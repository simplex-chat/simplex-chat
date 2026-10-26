import {Server, request as httpRequest} from "http"
import {AddressInfo, connect} from "net"
import {McpServer} from "@modelcontextprotocol/server"
import {Actions, Mode} from "../../src/actions"
import {startHttp, stopHttp} from "../../src/http"
import {ChatItemsWatch} from "../../src/messages"
import {AgentTools} from "../../src/tools"
import {FakeChat, actionsWith, contact, direct, fakeHooks, user} from "./fixtures"

const token = "secret-token"

let server: Server
let url: string
let actions: Actions
let ready: boolean

async function start(mode: Mode = "requests"): Promise<void> {
  ready = true
  const chat = new FakeChat()
  chat.chats.set("@4", {chatInfo: direct(contact(4, "SimpleX Directory")), items: []})
  actions = actionsWith(mode, fakeHooks())
  const tools = new AgentTools({chat: chat.api(), user, actions, watch: new ChatItemsWatch(), controlChat: () => "#10", standingApprover: () => undefined}, mode)
  const factory = (): McpServer => {
    const mcp = new McpServer({name: "simplex-mcp", version: "0.1.0"}, {instructions: "Use the tools."})
    tools.register(mcp)
    return mcp
  }
  server = await startHttp({port: 0, token, factory, ready: () => ready})
  url = `http://127.0.0.1:${(server.address() as AddressInfo).port}`
}

beforeEach(() => {
  jest.spyOn(process.stdout, "write").mockImplementation(() => true)
})

afterEach(async () => {
  jest.restoreAllMocks()
  await stopHttp(server)
})

const auth = {authorization: `Bearer ${token}`, "content-type": "application/json", accept: "application/json, text/event-stream"}

function post(body: object, headers: {[name: string]: string} = auth, path = "/mcp", signal?: AbortSignal): Promise<Response> {
  return fetch(`${url}${path}`, {method: "POST", headers, body: JSON.stringify(body), signal})
}

function modern(method: string, params: object = {}, id = 1): object {
  return {jsonrpc: "2.0", id, method, params: {...params, _meta: {"io.modelcontextprotocol/protocolVersion": "2026-07-28", "io.modelcontextprotocol/clientCapabilities": {}, ...(params as {_meta?: object})._meta}}}
}

function modernHeaders(method: string, name?: string): {[name: string]: string} {
  return {...auth, "mcp-protocol-version": "2026-07-28", "mcp-method": method, ...(name === undefined ? {} : {"mcp-name": name})}
}

function statusWithHost(host: string): Promise<number> {
  return new Promise((resolve, reject) => {
    const req = httpRequest(`${url}/mcp`, {method: "POST", headers: {...modernHeaders("tools/list"), host}}, res => {
      res.resume()
      resolve(res.statusCode ?? 0)
    })
    req.on("error", reject)
    req.end(JSON.stringify(modern("tools/list")))
  })
}

function rawStatusLine(target: string): Promise<string> {
  return new Promise((resolve, reject) => {
    const {port} = server.address() as AddressInfo
    const socket = connect(port, "127.0.0.1", () => {
      socket.write(`POST ${target} HTTP/1.1\r\nHost: 127.0.0.1:${port}\r\nAuthorization: Bearer ${token}\r\nContent-Length: 0\r\nConnection: close\r\n\r\n`)
    })
    let response = ""
    socket.on("data", chunk => (response += chunk))
    socket.on("end", () => resolve(response.split("\r\n")[0]))
    socket.on("error", reject)
  })
}

async function body(response: Response): Promise<any> {
  const text = await response.text()
  if (!response.headers.get("content-type")?.startsWith("text/event-stream")) return JSON.parse(text)
  const data = text.split("\n").filter(line => line.startsWith("data: ")).map(line => JSON.parse(line.slice(6)))
  return data.find(message => "result" in message || "error" in message)
}

describe("HTTP", () => {
  it("refuses requests without the token", async () => {
    await start()
    expect((await post(modern("tools/list"), {...modernHeaders("tools/list"), authorization: ""})).status).toBe(401)
    expect((await post(modern("tools/list"), {...modernHeaders("tools/list"), authorization: "Bearer other"})).status).toBe(401)
  })

  it("refuses another Origin and another Host", async () => {
    await start()
    expect((await post(modern("tools/list"), {...modernHeaders("tools/list"), origin: "https://example.com"})).status).toBe(403)
    expect((await post(modern("tools/list"), {...modernHeaders("tools/list"), origin: "http://localhost:3000"})).status).toBe(200)
    expect(await statusWithHost("example.com")).toBe(403)
  })

  it("serves only /mcp", async () => {
    await start()
    expect((await post(modern("tools/list"), modernHeaders("tools/list"), "/other")).status).toBe(404)
    expect((await fetch(`${url}/mcp`, {headers: auth})).status).toBe(405)
  })

  it("answers a request target that is not a URL with 404, and keeps serving", async () => {
    await start()
    expect(await rawStatusLine("http://[")).toBe("HTTP/1.1 404 Not Found")
    expect((await post(modern("tools/list"), modernHeaders("tools/list"))).status).toBe(200)
  })

  it("answers /mcp with 503 and Retry-After until the server is ready, after the token check", async () => {
    await start()
    ready = false
    const starting = await post(modern("tools/list"), modernHeaders("tools/list"))
    expect(starting.status).toBe(503)
    expect(starting.headers.get("retry-after")).toBe("1")
    expect((await post(modern("tools/list"), {...modernHeaders("tools/list"), authorization: ""})).status).toBe(401)
    ready = true
    expect((await post(modern("tools/list"), modernHeaders("tools/list"))).status).toBe(200)
  })
})

describe("MCP", () => {
  it("initializes a legacy client with the instructions", async () => {
    await start()
    const response = await post({jsonrpc: "2.0", id: 1, method: "initialize", params: {protocolVersion: "2025-06-18", capabilities: {}, clientInfo: {name: "c", version: "1"}}})
    expect((await body(response)).result).toMatchObject({protocolVersion: "2025-06-18", serverInfo: {name: "simplex-mcp", version: "0.1.0"}, instructions: "Use the tools."})
  })

  it("lists the five tools with their schemas and caching fields", async () => {
    await start()
    const {result} = await body(await post(modern("tools/list"), modernHeaders("tools/list")))
    expect(result).toMatchObject({ttlMs: 0, cacheScope: "private"})
    expect(result.tools.map(({name}: {name: string}) => name)).toEqual(["connect", "send", "receive", "list", "leave"])
    const send = result.tools.find(({name}: {name: string}) => name === "send")
    expect(send.inputSchema.properties.idle).toEqual({type: "number", minimum: 1, maximum: 300, description: "Seconds without a new response that end the wait once a response arrived. Default 30."})
    expect(send.outputSchema.required).toEqual(["action", "status"])
    expect(send._meta).toBeUndefined()
  })

  it("asks the host for each action in mode host", async () => {
    await start("host")
    const {result} = await body(await post(modern("tools/list"), modernHeaders("tools/list")))
    expect(Object.fromEntries(result.tools.map(({name, _meta}: {name: string; _meta?: object}) => [name, _meta]))).toEqual({
      connect: {"anthropic/requiresUserInteraction": true},
      send: {"anthropic/requiresUserInteraction": true},
      receive: undefined,
      list: undefined,
      leave: {"anthropic/requiresUserInteraction": true},
    })
  })

  it("rejects input that fails the schema with isError", async () => {
    await start()
    const text = async (args: object): Promise<string> => {
      const {result} = await body(await post(modern("tools/call", {name: "send", arguments: args}), modernHeaders("tools/call", "send")))
      expect(result.isError).toBe(true)
      return result.content[0].text
    }
    expect(await text({chat: "@4", text: ""})).toBe("Input validation error: Invalid arguments for tool send: data/text must NOT have fewer than 1 characters")
    expect(await text({chat: "@4", text: "😀".repeat(12001)})).toMatch(/data\/text must NOT have more than 12000 characters$/)
    expect(await text({chat: "@4", text: "x", mention: [44, 44]})).toMatch(/data\/mention must NOT have duplicate items/)
    expect(await text({chat: "@4", text: "x", wait: -1})).toMatch(/data\/wait must be >= 0$/)
    expect(await text({chat: "@4", text: "x", colour: "red"})).toMatch(/data must NOT have additional properties$/)
    expect(await text({text: "x"})).toMatch(/data must have required property 'chat'$/)
    expect(actions.all()).toHaveLength(0)
  })

  it("streams progress, and aborts the call when the client closes the stream", async () => {
    await start()
    const controller = new AbortController()
    const response = await post(modern("tools/call", {name: "send", arguments: {chat: "@4", text: "hello"}, _meta: {progressToken: "p1"}}), modernHeaders("tools/call", "send"), "/mcp", controller.signal)
    expect(response.headers.get("content-type")).toMatch(/^text\/event-stream/)
    const reader = response.body!.getReader()
    let text = ""
    while (!text.includes("notifications/progress")) text += new TextDecoder().decode((await reader.read()).value)
    expect(text).toMatch(/"progressToken":"p1".*"message":"Action 1 waits for an approver until /)
    expect(actions.get(1)!.open).toBe(1)
    controller.abort()
    await until(() => actions.get(1)!.open === 0)
  })
})

async function until(condition: () => boolean): Promise<void> {
  for (let i = 0; i < 200 && !condition(); i++) await new Promise(resolve => setTimeout(resolve, 10))
  expect(condition()).toBe(true)
}
