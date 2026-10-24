import {once} from "events"
import {existsSync, mkdtempSync, rmSync, writeFileSync} from "fs"
import {Server, request} from "http"
import {tmpdir} from "os"
import {join} from "path"
import {SocketError, isNotRunning, listen, requestJson, sendJson, startLines, streamLines} from "../../src/socket"
import {closeServer} from "./fixtures"

let dir: string
let path: string
let server: Server | undefined
const token = "t0ken"

beforeEach(() => {
  dir = mkdtempSync(join(tmpdir(), "simplex-mcp-socket-"))
  path = join(dir, "daemon.sock")
})

afterEach(async () => {
  jest.restoreAllMocks()
  if (server) await closeServer(server)
  server = undefined
  rmSync(dir, {recursive: true, force: true})
})

function rawPost(route: string, body: string): Promise<{status: number | undefined; text: string}> {
  return new Promise((resolve, reject) => {
    const req = request({socketPath: path, method: "POST", path: route, headers: {authorization: `Bearer ${token}`}}, res => {
      let text = ""
      res.setEncoding("utf8")
      res.on("data", chunk => (text += chunk))
      res.on("end", () => resolve({status: res.statusCode, text}))
    })
    req.on("error", reject)
    req.end(body)
  })
}

describe("socket", () => {
  it("answers 400 to a body that is not JSON", async () => {
    server = await listen(path, token, {"POST /echo": async (body, res) => sendJson(res, 200, body)})
    expect(await rawPost("/echo", "not json")).toEqual({status: 400, text: JSON.stringify({error: "Invalid JSON"})})
  })

  it("keeps multi-byte characters across chunks in bodies and lines", async () => {
    const text = "€😀漢".repeat(40_000)
    server = await listen(path, token, {
      "POST /echo": async (body, res) => sendJson(res, 200, body),
      "POST /lines": async (body, res) => {
        startLines(res)({progress: (body as {text: string}).text})
        res.end()
      },
    })
    expect(await requestJson(path, token, "POST", "/echo", {text})).toEqual({text})
    const lines: unknown[] = []
    await streamLines(path, token, "/lines", {text}, line => lines.push(line))
    expect(lines).toEqual([{progress: text}])
  })

  it("survives a handler that throws after the stream started", async () => {
    server = await listen(path, token, {
      "POST /broken": async (_body, res) => {
        startLines(res)({progress: "one"})
        throw new SocketError("late", 409)
      },
      "GET /ok": async (_body, res) => sendJson(res, 200, {}),
    })
    const lines: unknown[] = []
    await expect(streamLines(path, token, "/broken", {}, line => lines.push(line))).rejects.toThrow()
    expect(await requestJson(path, token, "GET", "/ok")).toEqual({})
  })

  it("checks the token, the route and the body, and logs an unexpected handler error", async () => {
    const errors: string[] = []
    jest.spyOn(process.stderr, "write").mockImplementation(text => (errors.push(String(text)), true))
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
    expect(errors).toEqual(["POST /fail: broken\n"])
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
