import {once} from "events"
import {existsSync, mkdtempSync, writeFileSync} from "fs"
import {Server} from "http"
import {tmpdir} from "os"
import {join} from "path"
import {SocketError, closeServer, isNotRunning, listen, requestJson, sendJson, startLines, streamLines} from "../../src/socket"

let dir: string
let path: string
let server: Server | undefined
const token = "t0ken"

beforeEach(() => {
  dir = mkdtempSync(join(tmpdir(), "simplex-mcp-socket-"))
  path = join(dir, "daemon.sock")
})

afterEach(async () => {
  if (server) await closeServer(server)
  server = undefined
})

describe("socket", () => {
  it("checks the token, the route and the body", async () => {
    server = await listen(path, token, {
      "POST /echo": async (body, res) => sendJson(res, 200, body),
      "POST /fail": async () => {
        throw new Error("broken")
      },
    })
    expect(await requestJson(path, token, "POST", "/echo", {a: 1})).toEqual({a: 1})
    await expect(requestJson(path, "wrong", "POST", "/echo", {})).rejects.toEqual(new SocketError("Unauthorized", 401))
    await expect(requestJson(path, token, "GET", "/echo")).rejects.toEqual(new SocketError("Not found", 404))
    await expect(requestJson(path, token, "POST", "/fail", {})).rejects.toEqual(new SocketError("broken", 500))
    await expect(requestJson(path, token, "POST", "/echo", "x".repeat(1_048_577))).rejects.toEqual(new SocketError("Body too large", 413))
  })

  it("streams lines and aborts the request on the signal", async () => {
    let closedEarly: Promise<unknown> | undefined
    server = await listen(path, token, {
      "POST /lines": async (_body, res) => {
        const write = startLines(res)
        write({progress: "one"})
        write({progress: "two"})
        closedEarly = once(res, "close")
      },
    })
    const lines: unknown[] = []
    const controller = new AbortController()
    const streaming = streamLines(path, token, "/lines", {}, line => {
      lines.push(line)
      if (lines.length === 2) controller.abort()
    }, controller.signal)
    await expect(streaming).rejects.toThrow()
    expect(lines).toEqual([{progress: "one"}, {progress: "two"}])
    await closedEarly
  })

  it("reports a daemon that is not running by the error code", async () => {
    const error = await requestJson(path, token, "GET", "/status").catch(e => e)
    expect(isNotRunning(error)).toBe(true)
    expect(isNotRunning(new SocketError("x", 500))).toBe(false)
  })

  it("replaces a stale socket file", async () => {
    writeFileSync(path, "")
    server = await listen(path, token, {"GET /ok": async (_body, res) => sendJson(res, 200, {})})
    expect(existsSync(path)).toBe(true)
    expect(await requestJson(path, token, "GET", "/ok")).toEqual({})
  })
})
