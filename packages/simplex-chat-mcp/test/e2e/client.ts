const modernVersion = "2026-07-28"

export interface ToolOutcome {
  isError?: boolean
  text: string
  result: any
}

export class McpClient {
  private nextId = 1

  constructor(private readonly url: string, private readonly token: string) {}

  async initialize(): Promise<any> {
    const id = this.nextId++
    const response = await fetch(this.url, {
      method: "POST",
      headers: {authorization: `Bearer ${this.token}`, "content-type": "application/json", accept: "application/json, text/event-stream"},
      body: JSON.stringify({jsonrpc: "2.0", id, method: "initialize", params: {protocolVersion: "2025-11-25", capabilities: {}, clientInfo: {name: "e2e", version: "1"}}}),
    })
    return reply(response, id, "initialize")
  }

  async request(method: string, params: object = {}, signal?: AbortSignal): Promise<any> {
    const id = this.nextId++
    const name = (params as {name?: string}).name
    const response = await fetch(this.url, {
      method: "POST",
      signal,
      headers: {
        authorization: `Bearer ${this.token}`,
        "content-type": "application/json",
        accept: "application/json, text/event-stream",
        "mcp-protocol-version": modernVersion,
        "mcp-method": method,
        ...(name === undefined ? {} : {"mcp-name": name}),
      },
      body: JSON.stringify({jsonrpc: "2.0", id, method, params: {...params, _meta: {"io.modelcontextprotocol/protocolVersion": modernVersion, "io.modelcontextprotocol/clientCapabilities": {}}}}),
    })
    return reply(response, id, method)
  }

  async tool(name: string, args: object, signal?: AbortSignal): Promise<ToolOutcome> {
    const result = await this.request("tools/call", {name, arguments: args}, signal)
    return {isError: result.isError, text: result.content[0].text, result: result.structuredContent}
  }
}

async function reply(response: Response, id: number, method: string): Promise<any> {
  const text = await response.text()
  const messages = response.headers.get("content-type")?.startsWith("text/event-stream")
    ? text.split("\n").filter(line => line.startsWith("data: ")).map(line => JSON.parse(line.slice(6)))
    : [JSON.parse(text)]
  const message = messages.find(({id: messageId}) => messageId === id)
  if (message.error) throw new Error(`${method}: ${JSON.stringify(message.error)}`)
  return message.result
}
