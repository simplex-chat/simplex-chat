import {appendFileSync, mkdtempSync, rmSync, truncateSync, writeFileSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {ServiceClient} from "../../src/client"
import {HookDeps, sessionStart, turn} from "../../src/hook"
import {linkText, resetOwners, resetOwnersText, showLink} from "../../src/owner"
import {qrCodeText} from "../../src/qr"
import {SocketError} from "../../src/socket"
import {version} from "../../src/version"
import {recordedRun} from "./fixtures"

function captureStderr(): string[] {
  const errors: string[] = []
  jest.spyOn(process.stderr, "write").mockImplementation(text => (errors.push(String(text).trimEnd()), true))
  return errors
}

afterEach(() => jest.restoreAllMocks())

const status = {version: "0.2.0", protocol: 1, mainGroup: "SimpleX MCP", sessions: []}
const input = JSON.stringify({session_id: "11111111-1111-4111-8111-111111111111", session_title: "auth", cwd: "/p/app", source: "startup"})

function fakeClient(owner: {owners: string[]; link?: string}, ensureError?: Error, mainGroup = "SimpleX MCP") {
  const requests: unknown[] = []
  const fake = {
    ensureService: async () => {
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
  it("starts the session with its title, folder, model, Claude Code process and the time the hook started, and stays quiet when the Main group has an owner", async () => {
    const {client, requests} = fakeClient({owners: ["Alice"]})
    const started = JSON.stringify({...JSON.parse(input), model: "claude-opus-5"})
    expect(await sessionStart(started, {CLAUDE_PID: "4321"}, {client, nodeVersion: "22.1.0"}, 5)).toEqual({})
    expect(await sessionStart(started, {}, {client, nodeVersion: "22.1.0"})).toEqual({})
    expect(requests).toEqual([
      {method: "POST", route: "/title", body: {id: "11111111-1111-4111-8111-111111111111", pid: 4321, at: 5, name: "auth", project: "/p/app", model: "claude-opus-5", config: {mainGroup: "SimpleX MCP", approve: "requests", expireHours: 23}}},
      {method: "POST", route: "/title", body: {id: "11111111-1111-4111-8111-111111111111", at: performance.timeOrigin, name: "auth", project: "/p/app", model: "claude-opus-5", config: {mainGroup: "SimpleX MCP", approve: "requests", expireHours: 23}}},
    ])
  })

  it("shows the link of the service's Main group with a QR code to the person only, while it has no owner", async () => {
    const link = "https://simplex.chat/g#link"
    const {client} = fakeClient({owners: [], link}, undefined, "Agents")
    const output = await sessionStart(input, {}, {client, nodeVersion: "22.1.0"})
    expect(output).toEqual({systemMessage: `SimpleX MCP: scan this QR code with the SimpleX app and tap Join to supervise your agents in "Agents":\n${qrCodeText(link, "plain")}\n${link}`})
  })

  it("starts no service and opens no session in a non-interactive run, as with claude -p or the Agent SDK, and the turn hooks post nothing", async () => {
    const ensured = jest.fn()
    const requests: unknown[] = []
    const client = {ensureService: ensured, request: async (...args: unknown[]) => void requests.push(args)} as unknown as HookDeps["client"]
    for (const entrypoint of ["sdk-cli", "sdk-ts", "sdk-py"]) {
      const env = {CLAUDE_CODE_ENTRYPOINT: entrypoint, CLAUDE_PID: "4321"}
      expect(await sessionStart(input, env, {client, nodeVersion: "22.1.0"})).toEqual({})
      expect(await turn(JSON.stringify({session_id: "11111111-1111-4111-8111-111111111111", prompt: "Fix the build"}), env, client)).toEqual({})
    }
    expect(ensured).not.toHaveBeenCalled()
    expect(requests).toEqual([])
    const {client: interactive, requests: sent} = fakeClient({owners: ["Alice"]})
    await sessionStart(input, {CLAUDE_CODE_ENTRYPOINT: "cli"}, {client: interactive, nodeVersion: "22.1.0"})
    expect(sent).toHaveLength(1)
  })

  it("marks at the service the Claude Code process of a run that Claude Code marks unattended, as a claude -p run started by a command of a VS Code session, which keeps its entrypoint, shows nothing, and its turn hooks post nothing", async () => {
    const {env} = recordedRun("claude-vscode", "SessionStart")
    const {client, requests} = fakeClient({owners: [], link: "https://simplex.chat/g#link"})
    const ensured = jest.spyOn(client, "ensureService")
    expect(await sessionStart(input, env, {client, nodeVersion: "22.1.0"})).toEqual({})
    expect(ensured).toHaveBeenCalledTimes(1)
    expect(requests).toEqual([{method: "POST", route: "/title", body: {id: "11111111-1111-4111-8111-111111111111", pid: 1769596, project: "/p/app", config: {mainGroup: "SimpleX MCP", approve: "requests", expireHours: 23}, unattended: true}}])
    const {env: turnEnv} = recordedRun("claude-vscode", "UserPromptSubmit")
    expect(await turn(JSON.stringify({session_id: "11111111-1111-4111-8111-111111111111", prompt: "Fix the build"}), turnEnv, client)).toEqual({})
    expect(requests).toHaveLength(1)
    const errors = captureStderr()
    const failing = fakeClient({owners: []}, new Error("no service"))
    expect(await sessionStart(input, env, {client: failing.client, nodeVersion: "22.1.0"})).toEqual({})
    expect(errors).toEqual(["SimpleX MCP session start: no service"])
    const {client: attended, requests: sent} = fakeClient({owners: ["Alice"]})
    await sessionStart(input, {...env, CLAUDE_CODE_SESSION_ATTENDED: "1"}, {client: attended, nodeVersion: "22.1.0"})
    expect(sent).toMatchObject([{route: "/title", body: {name: "auth"}}])
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

  it("explains a stopped service instead of the raw connect error", async () => {
    const error = Object.assign(new Error("connect ENOENT /root/service.sock"), {code: "ENOENT"})
    const {client} = fakeClient({owners: []}, error)
    expect(await sessionStart(input, {}, {client, nodeVersion: "22.1.0"})).toEqual({
      systemMessage: "SimpleX MCP: The SimpleX service stopped; it starts again with the next tool call.",
    })
  })

  it("reports other service errors with their text", async () => {
    const {client} = fakeClient({owners: []}, new Error("spawn failed"))
    expect(await sessionStart(input, {}, {client, nodeVersion: "22.1.0"})).toEqual({systemMessage: "SimpleX MCP: spawn failed"})
  })
})

describe("turn hooks", () => {
  const id = "11111111-1111-4111-8111-111111111111"

  function turnClient(refusals = 0) {
    const requests: unknown[] = []
    let ensured = 0
    const client = {
      ensureService: async () => void ensured++,
      request: async (method: string, route: string, body: unknown) => {
        requests.push({method, route, body})
        if (requests.length <= refusals) throw new SocketError("Unknown session; restart the Claude Code session.", 409)
      },
    } as unknown as HookDeps["client"]
    return {client, requests, ensured: () => ensured}
  }

  it("pass the prompt, the final answer with the effort, and the new model to the session with the process and the time of the event, and stay quiet without a service", async () => {
    const errors = captureStderr()
    const {client, requests, ensured} = turnClient()
    expect(await turn(JSON.stringify({session_id: id, hook_event_name: "UserPromptSubmit", prompt: "fix the build"}), {CLAUDE_PID: "4321"}, client)).toEqual({})
    expect(await turn(JSON.stringify({session_id: id, hook_event_name: "Stop", last_assistant_message: "Fixed.", effort: {level: "high"}}), {}, client)).toEqual({})
    expect(await turn(JSON.stringify({session_id: id, hook_event_name: "PostModelSwitch", to_model: "claude-sonnet-5"}), {CLAUDE_PID: "x"}, client)).toEqual({})
    const at = performance.timeOrigin
    expect(requests).toEqual([
      {method: "POST", route: "/turn", body: {id, pid: 4321, at, prompt: "fix the build"}},
      {method: "POST", route: "/turn", body: {id, at, answer: "Fixed.", effort: "high"}},
      {method: "POST", route: "/turn", body: {id, at, model: "claude-sonnet-5"}},
    ])
    expect(ensured()).toBe(3)
    const stopped = {ensureService: async () => Promise.reject(Object.assign(new Error("connect ENOENT"), {code: "ENOENT"}))} as unknown as HookDeps["client"]
    expect(await turn(JSON.stringify({session_id: id, prompt: "x"}), {}, stopped)).toEqual({})
    expect(errors).toEqual(["SimpleX MCP turn: connect ENOENT"])
  })

  it("pass the model of a Stop hook's answer from Claude's last entry in the tail of the transcript, after that entry is written, and no model for another answer, a synthetic message or a missing transcript", async () => {
    const folder = mkdtempSync(join(tmpdir(), "simplex-mcp-transcript-"))
    try {
      const transcript = join(folder, "session.jsonl")
      const entry = (text: string, model: string, extra: object = {}) => `${JSON.stringify({type: "assistant", ...extra, message: {model, content: [{type: "text", text}]}})}\n`
      writeFileSync(transcript, "")
      truncateSync(transcript, 3 * 1024 ** 3)
      appendFileSync(transcript, `\n${JSON.stringify({type: "user", message: {role: "user", content: "fix the build"}})}\n${JSON.stringify({type: "assistant", message: {model: "claude-opus-5", content: [{type: "thinking", thinking: "The build."}]}})}\n${entry("Fixed.", "claude-opus-5")}`)
      const {client, requests} = turnClient()
      const stop = (answer: string, extra: object = {}) => turn(JSON.stringify({session_id: id, hook_event_name: "Stop", transcript_path: transcript, last_assistant_message: answer, ...extra}), {}, client, 0, 1)
      await stop("Fixed.")
      const later = stop("Tests pass.")
      const written = new Promise<void>(resolve => setTimeout(() => resolve(appendFileSync(transcript, `${entry("Tests pass.", "claude-sonnet-5")}${entry("Subagent done.", "claude-haiku-5", {isSidechain: true})}`)), 30))
      await later
      await written
      appendFileSync(transcript, entry("No response requested.", "<synthetic>"))
      await stop("No response requested.")
      await stop("Never written.")
      await stop("Lost.", {transcript_path: join(folder, "missing.jsonl")})
      await turn(JSON.stringify({session_id: id, hook_event_name: "UserPromptSubmit", transcript_path: transcript, prompt: "Run the tests"}), {}, client, 0, 1)
      await turn(JSON.stringify({session_id: id, hook_event_name: "PostModelSwitch", transcript_path: transcript, to_model: "claude-opus-5"}), {}, client, 0, 1)
      expect(requests).toEqual([
        {method: "POST", route: "/turn", body: {id, at: 1, answer: "Fixed.", model: "claude-opus-5"}},
        {method: "POST", route: "/turn", body: {id, at: 1, answer: "Tests pass.", model: "claude-sonnet-5"}},
        {method: "POST", route: "/turn", body: {id, at: 1, answer: "No response requested."}},
        {method: "POST", route: "/turn", body: {id, at: 1, answer: "Never written."}},
        {method: "POST", route: "/turn", body: {id, at: 1, answer: "Lost."}},
        {method: "POST", route: "/turn", body: {id, at: 1, prompt: "Run the tests"}},
        {method: "POST", route: "/turn", body: {id, at: 1, model: "claude-opus-5"}},
      ])
    } finally {
      rmSync(folder, {recursive: true, force: true})
    }
  })

  it("cut a prompt or an answer over 250,000 characters, so that the request fits the socket's 1 MiB", async () => {
    const {client, requests} = turnClient()
    await turn(JSON.stringify({session_id: id, last_assistant_message: `${"a".repeat(249_999)}😀${"b".repeat(1_000_000)}`}), {}, client)
    await turn(JSON.stringify({session_id: id, prompt: "x".repeat(250_000)}), {}, client)
    const [cutAnswer, whole] = requests.map(request => (request as {body: {answer?: string; prompt?: string}}).body)
    expect(cutAnswer.answer).toBe(`${"a".repeat(249_999)}…`)
    expect(whole.prompt).toBe("x".repeat(250_000))
  })

  it("start the service and repeat a turn of a session the service does not know yet, 15 times at most, past the 10 s a title waits for its session server", async () => {
    const errors = captureStderr()
    const late = turnClient(2)
    expect(await turn(JSON.stringify({session_id: id, prompt: "fix the build"}), {}, late.client, 0)).toEqual({})
    expect(late.requests).toHaveLength(3)
    expect(late.ensured()).toBe(1)
    expect(errors).toEqual([])
    const unknown = turnClient(Infinity)
    expect(await turn(JSON.stringify({session_id: id, prompt: "fix the build"}), {}, unknown.client, 0)).toEqual({})
    expect(unknown.requests).toHaveLength(15)
    expect(errors).toEqual(["SimpleX MCP turn: Unknown session; restart the Claude Code session."])
  })
})

describe("owner commands", () => {
  const link = "https://simplex.chat/g#link"
  it("read the Main group link from its own route only while the group has no owner, as the status has no link", async () => {
    const routes: string[] = []
    const client = (owners: string[]) => ({
      ensureService: async () => ({...status, owner: {owners}}),
      request: async (method: string, route: string) => (routes.push(`${method} ${route}`), {owners, link}),
    })
    const spy = jest.spyOn(ServiceClient, "forRoot")
    spy.mockReturnValue(client([]) as unknown as ServiceClient)
    expect(await showLink({SIMPLEX_MCP_HOME: "/r"}, true)).toBe(linkText({...status, owner: {owners: [], link}}, true))
    spy.mockReturnValue(client(["Alice"]) as unknown as ServiceClient)
    expect(await showLink({SIMPLEX_MCP_HOME: "/r"}, true)).toBe(`Owners of "SimpleX MCP": Alice. To replace them, run npx -y @simplex-chat/mcp@${version} reset-owners in a terminal.`)
    expect(routes).toEqual(["GET /link"])
  })

  it("show the owners, or the link with a QR code in a terminal only", () => {
    expect(linkText({...status, owner: {owners: ["Alice", "Bob"]}}, false)).toBe(`Owners of "SimpleX MCP": Alice, Bob. To replace them, run npx -y @simplex-chat/mcp@${version} reset-owners in a terminal.`)
    expect(linkText({...status, owner: {owners: [], link}}, false)).toBe(`Run npx -y @simplex-chat/mcp@${version} link in a terminal to see the link to "SimpleX MCP".`)
    expect(linkText({...status, owner: {owners: [], link}}, true)).toBe(`Join "SimpleX MCP" in the SimpleX app. The link works once:\n${qrCodeText(link, "small")}\n${link}`)
    expect(linkText({...status, owner: {owners: []}}, false)).toBe(`The Main group has no link. Run npx -y @simplex-chat/mcp@${version} reset-owners in a terminal.`)
    expect(resetOwnersText(link, "SimpleX MCP", false)).toBe(`"SimpleX MCP" was replaced. Run npx -y @simplex-chat/mcp@${version} link in a terminal to see the link to the new one.`)
    expect(resetOwnersText(link, "SimpleX MCP", true)).toBe(`"SimpleX MCP" was replaced. Join the new one in the SimpleX app. The link works once:\n${qrCodeText(link, "small")}\n${link}`)
  })

  it("reset the owners at once, with or without owners, and show the new link in a terminal only", async () => {
    const requests: string[] = []
    const client = (owners: string[]) => ({
      ensureService: async () => ({...status, owner: {owners}}),
      request: async (_method: string, route: string) => (requests.push(route), {link}),
    })
    const spy = jest.spyOn(ServiceClient, "forRoot")
    for (const owners of [[], ["Alice"]]) {
      spy.mockReturnValue(client(owners) as unknown as ServiceClient)
      expect(await resetOwners({SIMPLEX_MCP_HOME: "/r"}, false)).toBe(`"SimpleX MCP" was replaced. Run npx -y @simplex-chat/mcp@${version} link in a terminal to see the link to the new one.`)
      expect(await resetOwners({SIMPLEX_MCP_HOME: "/r"}, true)).toBe(`"SimpleX MCP" was replaced. Join the new one in the SimpleX app. The link works once:\n${qrCodeText(link, "small")}\n${link}`)
    }
    expect(requests).toEqual(["/reset-owners", "/reset-owners", "/reset-owners", "/reset-owners"])
    spy.mockRestore()
  })
})
