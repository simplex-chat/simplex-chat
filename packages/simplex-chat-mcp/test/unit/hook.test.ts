import {HookDeps, sessionStart} from "../../src/hook"
import {linkText, resetOwnersText} from "../../src/owner"
import {qrCodeText} from "../../src/qr"

const status = {version: "0.2.0", protocol: 1, mainGroup: "SimpleX MCP", sessions: []}
const input = JSON.stringify({session_id: "11111111-1111-4111-8111-111111111111", session_title: "auth", cwd: "/p/app", source: "startup"})

function fakeClient(owner: {owners: string[]; link?: string}, ensureError?: Error, mainGroup = "SimpleX MCP") {
  const requests: unknown[] = []
  const fake = {
    ensureDaemon: async () => {
      if (ensureError) throw ensureError
      return {...status, owner}
    },
    request: async (method: string, route: string, body: unknown) => {
      requests.push({method, route, body})
      return {mainGroup, owner, version: "0.2.0", protocol: 1}
    },
  }
  return {requests, client: fake as unknown as HookDeps["client"]}
}

describe("SessionStart hook", () => {
  it("records the session title and stays quiet when the Main group has an owner", async () => {
    const {client, requests} = fakeClient({owners: ["Alice"]})
    expect(await sessionStart(input, {}, {client, nodeVersion: "22.1.0"})).toEqual({})
    expect(requests).toEqual([
      {method: "POST", route: "/title", body: {id: "11111111-1111-4111-8111-111111111111", name: "auth", config: {mainGroup: "SimpleX MCP", approve: "requests", expireHours: 23}}},
    ])
  })

  it("shows the link of the daemon's Main group with a QR code while it has no owner", async () => {
    const link = "https://simplex.chat/g#link"
    const {client} = fakeClient({owners: [], link}, undefined, "Agents")
    const output = await sessionStart(input, {}, {client, nodeVersion: "22.1.0"})
    expect(output).toEqual({
      systemMessage: `SimpleX MCP: scan this QR code with the SimpleX app and tap Join to supervise your agents in "Agents":\n${qrCodeText(link, "plain")}\n${link}`,
      hookSpecificOutput: {hookEventName: "SessionStart", additionalContext: `The SimpleX Main group "Agents" has no owner yet. Tell your user to open this link in the SimpleX app and join: ${link}`},
    })
  })

  it("needs Node.js 22", async () => {
    const {client, requests} = fakeClient({owners: []})
    expect(await sessionStart(input, {}, {client, nodeVersion: "20.11.0"})).toEqual({systemMessage: "SimpleX MCP needs Node.js 22 or later; found 20.11.0."})
    expect(requests).toEqual([])
  })

  it("reports failures as a message and lets the session start", async () => {
    const {client} = fakeClient({owners: []})
    expect(await sessionStart("not json", {}, {client, nodeVersion: "22.1.0"})).toMatchObject({systemMessage: expect.stringMatching(/^SimpleX MCP: /)})
    expect(await sessionStart(input, {CLAUDE_PLUGIN_OPTION_APPROVE: "x"}, {client, nodeVersion: "22.1.0"})).toEqual({
      systemMessage: "SimpleX MCP: Plugin option approve: requests or host expected, got x. Change it in /plugin > Installed > simplex > Configure options.",
    })
  })

  it("explains a stopped daemon instead of the raw connect error", async () => {
    const error = Object.assign(new Error("connect ENOENT /root/daemon.sock"), {code: "ENOENT"})
    const {client} = fakeClient({owners: []}, error)
    expect(await sessionStart(input, {}, {client, nodeVersion: "22.1.0"})).toEqual({
      systemMessage: "SimpleX MCP: The SimpleX daemon stopped; it starts again with the next tool call.",
    })
  })

  it("reports other daemon errors with their text", async () => {
    const {client} = fakeClient({owners: []}, new Error("spawn failed"))
    expect(await sessionStart(input, {}, {client, nodeVersion: "22.1.0"})).toEqual({systemMessage: "SimpleX MCP: spawn failed"})
  })
})

describe("owner commands", () => {
  const link = "https://simplex.chat/g#link"
  it("show the owners, or the link with a QR code in a terminal", () => {
    expect(linkText({...status, owner: {owners: ["Alice", "Bob"]}}, false)).toBe('Owners of "SimpleX MCP": Alice, Bob. To replace them, run /simplex:reset-owners.')
    expect(linkText({...status, owner: {owners: [], link}}, false)).toBe(`Join "SimpleX MCP" in the SimpleX app. The link works once:\n${link}`)
    expect(linkText({...status, owner: {owners: [], link}}, true)).toBe(`Join "SimpleX MCP" in the SimpleX app. The link works once:\n${qrCodeText(link, "ansi")}\n${link}`)
    expect(linkText({...status, owner: {owners: []}}, false)).toBe("The Main group has no link. Run /simplex:reset-owners.")
    expect(resetOwnersText(link, "SimpleX MCP", false)).toBe(`"SimpleX MCP" was replaced. Join the new one in the SimpleX app. The link works once:\n${link}`)
  })
})
