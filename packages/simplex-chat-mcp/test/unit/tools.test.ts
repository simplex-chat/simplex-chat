import {PassThrough} from "stream"
import {McpServer} from "@modelcontextprotocol/server"
import {StdioServerTransport} from "@modelcontextprotocol/server/stdio"
import {ToolCall} from "../../src/messages"
import {ToolName, instructions, registerTools} from "../../src/tools"
import {LineClient} from "./fixtures"

const pluginInstructions = instructions("Claude", "SimpleX daemon")

async function connectedServer(mode: "requests" | "host", handler: (name: ToolName, input: unknown, call: ToolCall) => Promise<any>): Promise<LineClient> {
  const input = new PassThrough()
  const output = new PassThrough()
  const server = new McpServer({name: "simplex-mcp", version: "test"}, {instructions: pluginInstructions})
  registerTools(server, mode, handler)
  await server.connect(new StdioServerTransport(input, output))
  return new LineClient(input, output)
}

describe("tool registration", () => {
  it("lists the six tools, marking actions for the host in mode host", async () => {
    for (const mode of ["requests", "host"] as const) {
      const client = await connectedServer(mode, async () => undefined)
      expect((await client.initialize()).instructions).toBe(pluginInstructions)
      const {result} = await client.request("tools/list")
      expect(result.tools.map(({name}: {name: string}) => name)).toEqual(["connect", "send", "receive", "download", "list", "leave"])
      const marked = result.tools.filter((tool: any) => tool._meta?.["anthropic/requiresUserInteraction"] === true).map(({name}: {name: string}) => name)
      expect(marked).toEqual(mode === "host" ? ["connect", "send", "download", "leave"] : [])
    }
  })

  it("describes the tools of the actions in the result of list as 0.2 did", async () => {
    const client = await connectedServer("requests", async () => undefined)
    await client.initialize()
    const {result} = await client.request("tools/list")
    const list = result.tools.find(({name}: {name: string}) => name === "list")
    expect(list.outputSchema.properties.actions.items.properties.tool).toEqual({type: "string", description: "connect, send, download or leave."})
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

  it("names the profile and the process whose restart cancels actions", () => {
    expect(pluginInstructions).toContain("You act in SimpleX chats through these tools, as the agent Claude.")
    expect(pluginInstructions).toContain("A SimpleX daemon restart cancels actions; a repeated call after it creates a new one.")
    expect(instructions("Scout")).toContain("A server restart cancels actions; a repeated call after it creates a new one.")
  })
})
