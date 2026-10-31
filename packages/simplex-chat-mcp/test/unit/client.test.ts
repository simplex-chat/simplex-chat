import {mkdtempSync, readFileSync, rmSync, writeFileSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {DaemonClient, compareVersions, spawnDaemon} from "../../src/client"
import {logFile, serverToken, socketPath} from "../../src/files"
import {ExitError} from "../../src/log"
import {protocolVersion} from "../../src/protocol"
import {listen, sendJson} from "../../src/socket"
import {closeServer} from "./fixtures"

let root: string
const script = (body: string): [string, string[]] => {
  const file = join(root, "fake-daemon.js")
  writeFileSync(file, body)
  return [process.execPath, [file]]
}

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), "simplex-mcp-client-"))
})

afterEach(() => rmSync(root, {recursive: true, force: true}))

describe("spawning the daemon", () => {
  it("resolves on ready and running", async () => {
    await spawnDaemon(root, script(`require("fs").writeSync(Number(process.env.SIMPLEX_MCP_READY_FD), "ready\\n")`))
    await spawnDaemon(root, script(`require("fs").writeSync(3, "running\\n")`))
  })

  it("passes on the daemon's error and points to the log otherwise", async () => {
    await expect(spawnDaemon(root, script(`require("fs").writeSync(3, "error: Plugin option approve: bad\\n")`))).rejects.toEqual(new ExitError("Plugin option approve: bad", 1))
    await expect(spawnDaemon(root, script(`console.error("boom"); process.exit(1)`))).rejects.toEqual(new ExitError(`The SimpleX daemon exited; see ${logFile(root)}`, 1))
    expect(readFileSync(logFile(root), "utf8")).toContain("boom")
  })

  it("reports a command that cannot start", async () => {
    const missing = join(root, "no-such-command")
    await expect(spawnDaemon(root, [missing, []])).rejects.toEqual(new ExitError(`spawn ${missing} ENOENT`, 1))
  })

  it("gives the daemon the data root", async () => {
    await spawnDaemon(root, script(`require("fs").writeFileSync(process.env.SIMPLEX_MCP_HOME + "/seen", process.env.SIMPLEX_MCP_HOME); require("fs").writeSync(3, "ready\\n")`))
    expect(readFileSync(join(root, "seen"), "utf8")).toBe(root)
  })
})

describe("ensureDaemon", () => {
  const status = (version: string, protocol = protocolVersion) => ({version, protocol, mainGroup: "SimpleX MCP", owner: {owners: []}, sessions: []})

  it("spawns a daemon when none answers", async () => {
    const client = DaemonClient.forRoot(root)
    let server: Awaited<ReturnType<typeof listen>> | undefined
    const spawned = jest.fn(async () => {
      server = await listen(socketPath(root), serverToken(root), {"GET /status": async (_b, res) => sendJson(res, 200, status("0.2.0"))})
    })
    expect(await client.ensureDaemon("0.2.0", spawned)).toMatchObject({version: "0.2.0"})
    expect(spawned).toHaveBeenCalledTimes(1)
    await closeServer(server!)
  })

  it("stops an older daemon and starts its own, uses a newer one, refuses another protocol", async () => {
    const client = DaemonClient.forRoot(root)
    const stops = jest.fn()
    let server = await listen(socketPath(root), serverToken(root), {
      "GET /status": async (_b, res) => sendJson(res, 200, status("0.1.9")),
      "POST /stop": async (_b, res) => {
        stops()
        sendJson(res, 200, {})
        setImmediate(() => closeServer(server))
      },
    })
    const spawned = jest.fn(async () => {
      server = await listen(socketPath(root), serverToken(root), {"GET /status": async (_b, res) => sendJson(res, 200, status("0.2.0"))})
    })
    expect(await client.ensureDaemon("0.2.0", spawned)).toMatchObject({version: "0.2.0"})
    expect(stops).toHaveBeenCalledTimes(1)
    expect(await client.ensureDaemon("0.1.0", spawned)).toMatchObject({version: "0.2.0"})
    await closeServer(server)
    server = await listen(socketPath(root), serverToken(root), {"GET /status": async (_b, res) => sendJson(res, 200, status("0.3.0", protocolVersion + 1))})
    await expect(client.ensureDaemon("0.2.0", spawned)).rejects.toEqual(new ExitError("Restart Claude Code to update the SimpleX plugin.", 1))
    await closeServer(server)
  })

  it("tolerates an older daemon that another client already stopped", async () => {
    const client = DaemonClient.forRoot(root)
    let server = await listen(socketPath(root), serverToken(root), {
      "GET /status": async (_b, res) => {
        sendJson(res, 200, status("0.1.9"))
        setImmediate(() => closeServer(server))
      },
    })
    const spawned = jest.fn(async () => {
      server = await listen(socketPath(root), serverToken(root), {"GET /status": async (_b, res) => sendJson(res, 200, status("0.2.0"))})
    })
    expect(await client.ensureDaemon("0.2.0", spawned)).toMatchObject({version: "0.2.0"})
    expect(spawned).toHaveBeenCalledTimes(1)
    await closeServer(server)
  })

  it("compares versions numerically", () => {
    expect(compareVersions("0.2.0", "0.10.0")).toBeLessThan(0)
    expect(compareVersions("1.0.0", "0.9.9")).toBeGreaterThan(0)
    expect(compareVersions("0.2.0", "0.2.0")).toBe(0)
  })
})

describe("requests", () => {
  it("reach the daemon with the token", async () => {
    const client = DaemonClient.forRoot(root)
    const server = await listen(socketPath(root), serverToken(root), {
      "GET /status": async (_b, res) => sendJson(res, 200, {ok: true}),
      "POST /call": async (body, res) => sendJson(res, 200, body),
    })
    expect(await client.request("GET", "/status")).toEqual({ok: true})
    expect(await client.request("POST", "/call", {a: 1})).toEqual({a: 1})
    await closeServer(server)
  })
})
