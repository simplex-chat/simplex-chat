import {timingSafeEqual} from "crypto"
import {IncomingMessage, Server, ServerResponse, createServer} from "http"
import {json} from "stream/consumers"
import {localhostHostValidation, localhostOriginValidation, toNodeHandler} from "@modelcontextprotocol/node"
import {McpServerFactory, createMcpHandler} from "@modelcontextprotocol/server"
import {logError} from "./log"

export interface HttpOptions {
  port: number
  token: string
  factory: McpServerFactory
  ready: () => boolean
  hook: (event: unknown) => void
}

const maxHookBody = 4 * 1_048_576

export function startHttp({port, token, factory, ready, hook}: HttpOptions): Promise<Server> {
  const expectedAuthorization = Buffer.from(`Bearer ${token}`)
  const validHost = localhostHostValidation()
  const validOrigin = localhostOriginValidation()
  const mcp = toNodeHandler(createMcpHandler(factory, {onerror: error => logError("MCP", error)}), {onerror: error => logError("HTTP request", error)})
  const server = createServer((req, res) => {
    if (!validHost(req, res) || !validOrigin(req, res)) return
    if (!authorized(req.headers.authorization)) return plain(res, 401)
    const path = requestPath(req.url)
    if (path === "/hook") {
      receiveHook(req, res, hook).catch(() => res.destroy())
      return
    }
    if (path !== "/mcp") return plain(res, 404)
    if (!ready()) return plain(res, 503, {"Retry-After": "1"})
    mcp(req, res).catch(error => {
      logError("HTTP request", error)
      res.destroy()
    })
  })
  server.requestTimeout = 0
  server.headersTimeout = 60_000
  server.keepAliveTimeout = 5_000
  server.timeout = 0

  function authorized(authorization: string | undefined): boolean {
    const given = Buffer.from(authorization ?? "")
    return given.length === expectedAuthorization.length && timingSafeEqual(given, expectedAuthorization)
  }

  return new Promise((resolve, reject) => {
    server.once("error", reject)
    server.listen(port, "127.0.0.1", () => {
      server.off("error", reject)
      resolve(server)
    })
  })
}

export function stopHttp(server: Server): Promise<void> {
  const closed = new Promise<void>(resolve => server.close(() => resolve()))
  server.closeAllConnections()
  return closed
}

async function receiveHook(req: IncomingMessage, res: ServerResponse, hook: (event: unknown) => void): Promise<void> {
  if (req.method !== "POST") return plain(res, 405, {Allow: "POST"})
  const length = Number(req.headers["content-length"])
  if (Number.isNaN(length)) return plain(res, 411)
  if (length > maxHookBody) return plain(res, 413)
  const event = await json(req).catch(() => undefined)
  if (event === undefined) return plain(res, 400)
  hook(event)
  plain(res, 204)
}

function requestPath(target: string | undefined): string | undefined {
  try {
    return new URL(target ?? "/", "http://localhost").pathname
  } catch {
    return undefined
  }
}

function plain(res: ServerResponse, status: number, headers: {[name: string]: string} = {}): void {
  res.writeHead(status, headers)
  res.end()
}
