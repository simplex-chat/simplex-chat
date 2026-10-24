import {PassThrough} from "stream"
import {McpServer} from "@modelcontextprotocol/server"
import {StdioServerTransport} from "@modelcontextprotocol/server/stdio"
import {ToolCall} from "../../src/messages"
import {ToolName, instructions, registerTools} from "../../src/tools"
import {LineClient} from "./fixtures"

async function connectedServer(mode: "requests" | "host", handler: (name: ToolName, input: unknown, call: ToolCall) => Promise<any>): Promise<LineClient> {
  const input = new PassThrough()
  const output = new PassThrough()
  const server = new McpServer({name: "simplex-mcp", version: "test"}, {instructions: instructions()})
  registerTools(server, mode, handler)
  await server.connect(new StdioServerTransport(input, output))
  return new LineClient(input, output)
}

describe("tool registration", () => {
  it("lists the five tools, marking actions for the host in mode host", async () => {
    for (const mode of ["requests", "host"] as const) {
      const client = await connectedServer(mode, async () => undefined)
      expect((await client.initialize()).instructions).toBe(instructions())
      const {result} = await client.request("tools/list")
      expect(result.tools.map(({name}: {name: string}) => name)).toEqual(["connect", "send", "receive", "list", "leave"])
      const marked = result.tools.filter((tool: any) => tool._meta?.["anthropic/requiresUserInteraction"] === true).map(({name}: {name: string}) => name)
      expect(marked).toEqual(mode === "host" ? ["connect", "send", "leave"] : [])
    }
  })

  it("passes calls to the handler and returns Cancelled when it returns nothing", async () => {
    const calls: [string, unknown][] = []
    const client = await connectedServer("requests", async (name, input) => {
      calls.push([name, input])
      return name === "list" ? {content: [{type: "text", text: "{}"}], structuredContent: {chats: [], actions: []}} : undefined
    })
    await client.initialize()
    expect((await client.request("tools/call", {name: "list", arguments: {}})).result.structuredContent).toEqual({chats: [], actions: []})
    expect((await client.request("tools/call", {name: "receive", arguments: {chat: "@1"}})).result).toEqual({content: [{type: "text", text: "Cancelled."}], isError: true})
    expect(calls).toEqual([["list", {}], ["receive", {chat: "@1"}]])
  })

  it("names no profile in the instructions", () => {
    expect(instructions()).toContain("You act in SimpleX chats through these tools, as your agent profile.")
    expect(instructions()).not.toMatch(/as the agent \S/)
  })
})
