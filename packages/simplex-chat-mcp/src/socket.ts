import {timingSafeEqual} from "crypto"
import {existsSync, unlinkSync} from "fs"
import {IncomingMessage, Server, ServerResponse, createServer, request} from "http"
import {logError} from "./log"

const maxBody = 1_048_576

export type Handler = (body: unknown, res: ServerResponse, req: IncomingMessage) => Promise<void>

export class SocketError extends Error {
  constructor(message: string, readonly status: number) {
    super(message)
  }
}

export function listen(path: string, token: string, routes: {[route: string]: Handler}): Promise<Server> {
  const expected = Buffer.from(`Bearer ${token}`)
  const authorized = (header: string | undefined): boolean => {
    const given = Buffer.from(header ?? "")
    return given.length === expected.length && timingSafeEqual(given, expected)
  }
  const server = createServer((req, res) => {
    if (!authorized(req.headers.authorization)) return sendJson(res, 401, {error: "Unauthorized"})
    const handler = routes[`${req.method} ${req.url}`]
    if (handler === undefined) return sendJson(res, 404, {error: "Not found"})
    readBody(req)
      .then(body => handler(body, res, req))
      .catch(error => {
        if (error instanceof SocketError) return sendJson(res, error.status, {error: error.message})
        logError(`${req.method} ${req.url}`, error)
        if (!res.headersSent) sendJson(res, 500, {error: error instanceof Error ? error.message : String(error)})
        else res.destroy()
      })
  })
  server.requestTimeout = 0
  server.headersTimeout = 60_000
  server.keepAliveTimeout = 5_000
  server.timeout = 0
  if (process.platform !== "win32" && existsSync(path)) unlinkSync(path)
  return new Promise((resolve, reject) => {
    server.once("error", reject)
    server.listen(path, () => {
      server.off("error", reject)
      resolve(server)
    })
  })
}

export function closeServer(server: Server): Promise<void> {
  const closed = new Promise<void>(resolve => server.close(() => resolve()))
  server.closeAllConnections()
  return closed
}

export function sendJson(res: ServerResponse, status: number, value: unknown): void {
  res.writeHead(status, {"content-type": "application/json"})
  res.end(JSON.stringify(value))
}

export function startLines(res: ServerResponse): (line: unknown) => void {
  res.writeHead(200, {"content-type": "application/x-ndjson"})
  return line => {
    res.write(`${JSON.stringify(line)}\n`)
  }
}

export function isNotRunning(error: unknown): boolean {
  const code = (error as NodeJS.ErrnoException | undefined)?.code
  return code === "ENOENT" || code === "ECONNREFUSED"
}

export async function requestJson<T>(path: string, token: string, method: string, route: string, body?: unknown): Promise<T> {
  const res = await send(path, token, method, route, body)
  const text = await readText(res)
  if (res.statusCode !== 200) throw new SocketError(errorOf(text), res.statusCode ?? 0)
  return JSON.parse(text) as T
}

export async function streamLines(path: string, token: string, route: string, body: unknown, onLine: (line: unknown) => void, signal?: AbortSignal): Promise<void> {
  const res = await send(path, token, "POST", route, body, signal)
  if (res.statusCode !== 200) throw new SocketError(errorOf(await readText(res)), res.statusCode ?? 0)
  let buffer = ""
  for await (const chunk of res) {
    buffer += chunk
    for (let i = buffer.indexOf("\n"); i >= 0; i = buffer.indexOf("\n")) {
      onLine(JSON.parse(buffer.slice(0, i)))
      buffer = buffer.slice(i + 1)
    }
  }
}

function send(path: string, token: string, method: string, route: string, body: unknown, signal?: AbortSignal): Promise<IncomingMessage> {
  return new Promise((resolve, reject) => {
    const req = request({socketPath: path, method, path: route, headers: {authorization: `Bearer ${token}`, "content-type": "application/json"}, signal}, resolve)
    req.on("error", reject)
    req.end(body === undefined ? undefined : JSON.stringify(body))
  })
}

async function readBody(req: IncomingMessage): Promise<unknown> {
  let text = ""
  let tooLarge = false
  for await (const chunk of req) {
    if (tooLarge) continue
    text += chunk
    tooLarge = text.length > maxBody
  }
  if (tooLarge) throw new SocketError("Body too large", 413)
  if (text === "") return undefined
  try {
    return JSON.parse(text)
  } catch {
    throw new SocketError("Invalid JSON", 400)
  }
}

async function readText(res: IncomingMessage): Promise<string> {
  let text = ""
  for await (const chunk of res) text += chunk
  return text
}

function errorOf(text: string): string {
  try {
    return (JSON.parse(text) as {error?: string}).error ?? text
  } catch {
    return text
  }
}
