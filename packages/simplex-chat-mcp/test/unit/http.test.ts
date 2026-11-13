import {Server, request as httpRequest} from "http"
import {AddressInfo, connect} from "net"
import {McpServer} from "@modelcontextprotocol/server"
import {Actions, Mode} from "../../src/actions"
import {startHttp, stopHttp} from "../../src/http"
import {ChatItemsWatch} from "../../src/messages"
import {AgentTools, instructions} from "../../src/tools"
import {FakeChat, actionsWith, contact, data, direct, fakeHooks, user} from "./fixtures"

const token = "secret-token"

let server: Server
let url: string
let actions: Actions
let ready: boolean
let hookEvents: unknown[]

async function start(mode: Mode = "requests"): Promise<void> {
  ready = true
  hookEvents = []
  const chat = new FakeChat()
  chat.chats.set("@4", {chatInfo: direct(contact(4, "SimpleX Directory")), items: []})
  actions = actionsWith(mode, fakeHooks())
  const tools = new AgentTools({chat: chat.api(), user, actions, watch: new ChatItemsWatch(), controlChat: () => "#10", standingApprover: () => undefined, refusedFile: () => undefined, data}, mode)
  const factory = (): McpServer => {
    const mcp = new McpServer({name: "simplex-mcp", version: "0.1.0"}, {instructions: "Use the tools."})
    tools.register(mcp)
    return mcp
  }
  server = await startHttp({port: 0, token, factory, ready: () => ready, hook: event => hookEvents.push(event)})
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

function rawStatusLine(target: string, headers = "Content-Length: 0\r\n"): Promise<string> {
  return new Promise((resolve, reject) => {
    const {port} = server.address() as AddressInfo
    const socket = connect(port, "127.0.0.1", () => {
      socket.write(`POST ${target} HTTP/1.1\r\nHost: 127.0.0.1:${port}\r\nAuthorization: Bearer ${token}\r\n${headers}Connection: close\r\n\r\n`)
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

describe("status hook endpoint", () => {
  const hookHeaders = {authorization: `Bearer ${token}`, "content-type": "application/json"}

  function postHook(body: string, headers: {[name: string]: string} = hookHeaders): Promise<Response> {
    return fetch(`${url}/hook`, {method: "POST", headers, body})
  }

  it("passes the parsed body to the hook and answers 204 without a body, also before the server is ready", async () => {
    await start()
    ready = false
    const event = {session_id: "s1", hook_event_name: "PreToolUse", tool_name: "Bash", tool_input: {command: "npm test"}, tool_use_id: "t1"}
    const response = await postHook(JSON.stringify(event))
    expect([response.status, await response.text()]).toEqual([204, ""])
    expect(hookEvents).toEqual([event])
    expect((await post(modern("tools/list"), modernHeaders("tools/list"))).status).toBe(503)
  })

  it("accepts a body of 4 MiB, and refuses a longer Content-Length and a request without it", async () => {
    await start()
    expect((await postHook(JSON.stringify({pad: "x".repeat(4 * 1_048_576 - 10)}))).status).toBe(204)
    expect(await rawStatusLine("/hook", `Content-Length: ${4 * 1_048_576 + 1}\r\n`)).toMatch(/^HTTP\/1\.1 413 /)
    expect(await rawStatusLine("/hook", "")).toBe("HTTP/1.1 411 Length Required")
    expect(hookEvents).toHaveLength(1)
  })

  it("refuses requests without the token, from another Origin, with another method and with invalid JSON, without calling the hook", async () => {
    await start()
    expect((await postHook("{}", {...hookHeaders, authorization: "Bearer other"})).status).toBe(401)
    expect((await postHook("{}", {...hookHeaders, origin: "https://example.com"})).status).toBe(403)
    const get = await fetch(`${url}/hook`, {headers: hookHeaders})
    expect([get.status, get.headers.get("allow")]).toEqual([405, "POST"])
    expect((await postHook("{")).status).toBe(400)
    expect((await postHook("")).status).toBe(400)
    expect(hookEvents).toEqual([])
  })
})

describe("MCP", () => {
  it("initializes a legacy client with the instructions", async () => {
    await start()
    const response = await post({jsonrpc: "2.0", id: 1, method: "initialize", params: {protocolVersion: "2025-06-18", capabilities: {}, clientInfo: {name: "c", version: "1"}}})
    expect((await body(response)).result).toMatchObject({protocolVersion: "2025-06-18", serverInfo: {name: "simplex-mcp", version: "0.1.0"}, instructions: "Use the tools."})
  })

  it("lists the six tools with their schemas and caching fields", async () => {
    await start()
    const {result} = await body(await post(modern("tools/list"), modernHeaders("tools/list")))
    expect(result).toMatchObject({ttlMs: 0, cacheScope: "private"})
    expect(result.tools.map(({name}: {name: string}) => name)).toEqual(["connect", "send", "receive", "download", "list", "leave"])
    const send = result.tools.find(({name}: {name: string}) => name === "send")
    expect(send.inputSchema.properties.idle).toEqual({type: "number", minimum: 1, maximum: 300, description: "Seconds without a new response that end the wait once a response arrived. Default 30."})
    expect(send.inputSchema.properties.live).toEqual({
      type: "boolean",
      description: "true: text is shown at once under the line of the live status message in this chat, until your next message there without live; status live is returned at once, and no action is created. Text only, at most 2000 characters, in a chat with a person, a group or a business chat that has approved: true in list. Default false.",
    })
    expect(send.outputSchema.required).toEqual(["status"])
    expect(send.outputSchema.properties.status.enum).toEqual(["done", "failed", "rejected", "expired", "live"])
    expect(send._meta).toBeUndefined()
    expect(send.description.split("\n")).toContain("With live: true, in a chat with a person, a group or a business chat that has approved: true in list, text is shown at once in the chat's live status message, and status live is returned without an action. Your next message there without live is sent as a new message, and the status message is then deleted.")
    const receive = result.tools.find(({name}: {name: string}) => name === "receive")
    expect(Object.keys(receive.inputSchema.properties)).toEqual(["chat", "wait", "limit"])
    expect(receive.description.split("\n").at(-1)).toBe("In a chat with a person, a group or a business chat that has approved: true in list, a live status message is shown there from the arrival of a message, while you work, until your next message there or a receive there that waits. In the chat where you receive prompts, send each answer with wait 0 and without live, so the next prompt stays unread for receive, and call receive again only after it. For a step that takes minutes, call send with live: true and a short note in the same response as the step's tool call; the note is shown in the status message.")
  })

  it("keeps each tool description, and the instructions for a 15-character name, within Claude Code's 2048 characters", async () => {
    await start()
    const {result} = await body(await post(modern("tools/list"), modernHeaders("tools/list")))
    for (const {description} of result.tools) expect(description.length).toBeLessThanOrEqual(2048)
    expect(instructions("Ridgeline Scout").length).toBeLessThanOrEqual(2048)
  })

  it("asks the host for each action in mode host", async () => {
    await start("host")
    const {result} = await body(await post(modern("tools/list"), modernHeaders("tools/list")))
    expect(Object.fromEntries(result.tools.map(({name, _meta}: {name: string; _meta?: object}) => [name, _meta]))).toEqual({
      connect: {"anthropic/requiresUserInteraction": true},
      send: {"anthropic/requiresUserInteraction": true},
      receive: undefined,
      download: {"anthropic/requiresUserInteraction": true},
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
    expect(await text({chat: "@4", text: "x", live: "yes"})).toMatch(/data\/live must be boolean$/)
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
