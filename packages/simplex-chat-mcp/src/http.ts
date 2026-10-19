import {timingSafeEqual} from "crypto"
import {Server, ServerResponse, createServer} from "http"
import {localhostHostValidation, localhostOriginValidation, toNodeHandler} from "@modelcontextprotocol/node"
import {McpServerFactory, createMcpHandler} from "@modelcontextprotocol/server"
import {logError} from "./log"

export interface HttpOptions {
  port: number
  token: string
  factory: McpServerFactory
}

export function startHttp({port, token, factory}: HttpOptions): Promise<Server> {
  const expectedAuthorization = Buffer.from(`Bearer ${token}`)
  const validHost = localhostHostValidation()
  const validOrigin = localhostOriginValidation()
  const mcp = toNodeHandler(createMcpHandler(factory, {onerror: error => logError("MCP", error)}), {onerror: error => logError("HTTP request", error)})
  const server = createServer((req, res) => {
    if (!validHost(req, res) || !validOrigin(req, res)) return
    if (!authorized(req.headers.authorization)) return plain(res, 401)
    if (requestPath(req.url) !== "/mcp") return plain(res, 404)
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

function requestPath(target: string | undefined): string | undefined {
  try {
    return new URL(target ?? "/", "http://localhost").pathname
  } catch {
    return undefined
  }
}

function plain(res: ServerResponse, status: number): void {
  res.writeHead(status)
  res.end()
}
