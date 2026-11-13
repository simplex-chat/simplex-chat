import {mkdtempSync, readFileSync, rmSync, writeFileSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {ServiceClient, compareVersions, spawnService} from "../../src/client"
import {logFile, serviceToken, socketPath} from "../../src/files"
import {ExitError} from "../../src/log"
import {protocolVersion} from "../../src/protocol"
import {listen, sendJson} from "../../src/socket"
import {closeServer, descendants} from "./fixtures"

let root: string
const script = (body: string): [string, string[]] => {
  const file = join(root, "fake-service.js")
  writeFileSync(file, body)
  return [process.execPath, [file]]
}

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), "simplex-mcp-client-"))
})

afterEach(() => rmSync(root, {recursive: true, force: true}))

describe("spawning the service", () => {
  it("resolves on ready and running", async () => {
    await spawnService(root, script(`require("fs").writeSync(Number(process.env.SIMPLEX_MCP_READY_FD), "ready\\n")`))
    await spawnService(root, script(`require("fs").writeSync(3, "running\\n")`))
  })

  it("passes on the service's error and points to the log otherwise", async () => {
    await expect(spawnService(root, script(`require("fs").writeSync(3, "error: Plugin option approve: bad\\n")`))).rejects.toEqual(new ExitError("Plugin option approve: bad", 1))
    await expect(spawnService(root, script(`console.error("boom"); process.exit(1)`))).rejects.toEqual(new ExitError(`The SimpleX service exited; see ${logFile(root)}`, 1))
    expect(readFileSync(logFile(root), "utf8")).toContain("boom")
  })

  it("reports a command that cannot start", async () => {
    const missing = join(root, "no-such-command")
    await expect(spawnService(root, [missing, []])).rejects.toEqual(new ExitError(`spawn ${missing} ENOENT`, 1))
  })

  it("starts the service outside the process tree of its caller, so that ending the caller with its descendants leaves the service running", async () => {
    await spawnService(root, script(`require("fs").writeFileSync(process.env.SIMPLEX_MCP_HOME + "/pid", String(process.pid)); require("fs").writeSync(3, "ready\\n"); setTimeout(() => {}, 30_000)`))
    const pid = Number(readFileSync(join(root, "pid"), "utf8"))
    try {
      expect(descendants(process.pid)).not.toContain(pid)
    } finally {
      process.kill(pid)
    }
  })

  it("gives the service the data root", async () => {
    await spawnService(root, script(`require("fs").writeFileSync(process.env.SIMPLEX_MCP_HOME + "/seen", process.env.SIMPLEX_MCP_HOME); require("fs").writeSync(3, "ready\\n")`))
    expect(readFileSync(join(root, "seen"), "utf8")).toBe(root)
  })

  it("gives the service only the variables it uses", async () => {
    const given = {ANTHROPIC_API_KEY: "secret", GITHUB_TOKEN: "secret", CLAUDE_PLUGIN_OPTION_APPROVE: "host", SIMPLEX_LIBS_DIR: "/libs", HTTPS_PROXY: "http://proxy:3128", LC_CTYPE: "C.UTF-8", NODE_OPTIONS: "--no-deprecation"}
    Object.assign(process.env, given)
    try {
      await spawnService(root, script(`require("fs").writeFileSync(process.env.SIMPLEX_MCP_HOME + "/env", JSON.stringify(process.env)); require("fs").writeSync(3, "ready\\n")`))
    } finally {
      for (const name of Object.keys(given)) delete process.env[name]
    }
    const env = JSON.parse(readFileSync(join(root, "env"), "utf8"))
    expect(env).toMatchObject({CLAUDE_PLUGIN_OPTION_APPROVE: "host", SIMPLEX_LIBS_DIR: "/libs", HTTPS_PROXY: "http://proxy:3128", LC_CTYPE: "C.UTF-8", NODE_OPTIONS: "--no-deprecation", SIMPLEX_MCP_HOME: root, PATH: process.env.PATH})
    expect(env).not.toHaveProperty("ANTHROPIC_API_KEY")
    expect(env).not.toHaveProperty("GITHUB_TOKEN")
  })
})

describe("ensureService", () => {
  const status = (version: string, protocol = protocolVersion) => ({version, protocol, mainGroup: "SimpleX MCP", owner: {owners: []}, sessions: []})

  it("spawns a service when none answers", async () => {
    const client = ServiceClient.forRoot(root)
    let server: Awaited<ReturnType<typeof listen>> | undefined
    const spawned = jest.fn(async () => {
      server = await listen(socketPath(root), serviceToken(root), {"GET /status": async (_b, res) => sendJson(res, 200, status("0.2.0"))})
    })
    expect(await client.ensureService("0.2.0", spawned)).toMatchObject({version: "0.2.0"})
    expect(spawned).toHaveBeenCalledTimes(1)
    await closeServer(server!)
  })

  it("stops an older service and starts its own, uses a newer one, refuses another protocol", async () => {
    const client = ServiceClient.forRoot(root)
    const stops = jest.fn()
    let server = await listen(socketPath(root), serviceToken(root), {
      "GET /status": async (_b, res) => sendJson(res, 200, status("0.1.9")),
      "POST /stop": async (_b, res) => {
        stops()
        sendJson(res, 200, {})
        setImmediate(() => closeServer(server))
      },
    })
    const spawned = jest.fn(async () => {
      server = await listen(socketPath(root), serviceToken(root), {"GET /status": async (_b, res) => sendJson(res, 200, status("0.2.0"))})
    })
    expect(await client.ensureService("0.2.0", spawned)).toMatchObject({version: "0.2.0"})
    expect(stops).toHaveBeenCalledTimes(1)
    expect(await client.ensureService("0.1.0", spawned)).toMatchObject({version: "0.2.0"})
    await closeServer(server)
    server = await listen(socketPath(root), serviceToken(root), {"GET /status": async (_b, res) => sendJson(res, 200, status("0.3.0", protocolVersion + 1))})
    await expect(client.ensureService("0.2.0", spawned)).rejects.toEqual(new ExitError("Restart Claude Code to update the SimpleX plugin.", 1))
    await closeServer(server)
  })

  it("tolerates an older service that another client already stopped", async () => {
    const client = ServiceClient.forRoot(root)
    let server = await listen(socketPath(root), serviceToken(root), {
      "GET /status": async (_b, res) => {
        sendJson(res, 200, status("0.1.9"))
        setImmediate(() => closeServer(server))
      },
    })
    const spawned = jest.fn(async () => {
      server = await listen(socketPath(root), serviceToken(root), {"GET /status": async (_b, res) => sendJson(res, 200, status("0.2.0"))})
    })
    expect(await client.ensureService("0.2.0", spawned)).toMatchObject({version: "0.2.0"})
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
  it("reach the service with the token", async () => {
    const client = ServiceClient.forRoot(root)
    const server = await listen(socketPath(root), serviceToken(root), {
      "GET /status": async (_b, res) => sendJson(res, 200, {ok: true}),
      "POST /call": async (body, res) => sendJson(res, 200, body),
    })
    expect(await client.request("GET", "/status")).toEqual({ok: true})
    expect(await client.request("POST", "/call", {a: 1})).toEqual({a: 1})
    await closeServer(server)
  })
})
