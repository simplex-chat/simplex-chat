import {appendFileSync, mkdirSync, mkdtempSync, rmSync, truncateSync, writeFileSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {ServiceClient} from "../../src/client"
import {HookDeps, ask, sessionStart, turn} from "../../src/hook"
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
  it("starts the session with its title, folder, model, Claude Code process and the time the hook started, without the model after a compaction, and stays quiet when the Main group has an owner", async () => {
    const {client, requests} = fakeClient({owners: ["Alice"]})
    const started = JSON.stringify({...JSON.parse(input), model: "claude-opus-5"})
    const compacted = JSON.stringify({...JSON.parse(input), source: "compact", model: "claude-haiku-5"})
    expect(await sessionStart(started, {CLAUDE_PID: "4321"}, {client, nodeVersion: "22.1.0"}, 5)).toEqual({})
    expect(await sessionStart(started, {}, {client, nodeVersion: "22.1.0"})).toEqual({})
    expect(await sessionStart(compacted, {CLAUDE_PID: "4321"}, {client, nodeVersion: "22.1.0"}, 6)).toEqual({})
    expect(requests).toEqual([
      {method: "POST", route: "/title", body: {id: "11111111-1111-4111-8111-111111111111", pid: 4321, at: 5, name: "auth", project: "/p/app", model: "claude-opus-5", config: {mainGroup: "Claude Control", approve: "requests", expireHours: 23}}},
      {method: "POST", route: "/title", body: {id: "11111111-1111-4111-8111-111111111111", at: performance.timeOrigin, name: "auth", project: "/p/app", model: "claude-opus-5", config: {mainGroup: "Claude Control", approve: "requests", expireHours: 23}}},
      {method: "POST", route: "/title", body: {id: "11111111-1111-4111-8111-111111111111", pid: 4321, at: 6, name: "auth", project: "/p/app", config: {mainGroup: "Claude Control", approve: "requests", expireHours: 23}}},
    ])
  })

  it("starts the session with the path of its transcript", async () => {
    const {client, requests} = fakeClient({owners: ["Alice"]})
    await sessionStart(JSON.stringify({...JSON.parse(input), transcript_path: "/c/projects/p/1.jsonl"}), {}, {client, nodeVersion: "22.1.0"})
    expect(requests).toMatchObject([{route: "/title", body: {transcript: "/c/projects/p/1.jsonl"}}])
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
    expect(requests).toEqual([{method: "POST", route: "/title", body: {id: "11111111-1111-4111-8111-111111111111", pid: 1769596, project: "/p/app", config: {mainGroup: "Claude Control", approve: "requests", expireHours: 23}, unattended: true}}])
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
    expect(await sessionStart(input, {SIMPLEX_MCP_MACHINE: "my.laptop"}, {client, nodeVersion: "22.1.0"})).toEqual({
      systemMessage: "SimpleX MCP: SIMPLEX_MCP_MACHINE: a valid SimpleX name, such as mylaptop expected, got my.laptop. Change it in the environment of this machine.",
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

describe("PermissionRequest hook", () => {
  const id = "11111111-1111-4111-8111-111111111111"

  function askClient(lines: unknown[]) {
    const requests: unknown[] = []
    const client = {
      ensureService: async () => undefined,
      stream: async (route: string, body: unknown, onLine: (line: unknown) => void) => {
        requests.push({route, body})
        lines.forEach(onLine)
      },
    } as unknown as HookDeps["client"]
    return {client, requests}
  }

  const prompt = (tool: string, fields: object, extra: object = {}) => JSON.stringify({session_id: id, hook_event_name: "PermissionRequest", tool_name: tool, tool_input: fields, ...extra})

  it("sends a tool prompt with its preview to the service and prints the decision of the stream", async () => {
    const deny = {behavior: "deny", message: "Alice rejected this in the SimpleX group."}
    const {client, requests} = askClient([{decision: deny}])
    const bash = prompt("Bash", {command: "npm test", description: "Run the unit tests"}, {agent_id: "a1", agent_type: "reviewer"})
    expect(await ask(bash, {CLAUDE_PID: "4321"}, client, 5)).toEqual({hookSpecificOutput: {hookEventName: "PermissionRequest", decision: deny}})
    await ask(prompt("Artifact", {action: "read", url: "https://claude.ai/artifact/abc"}), {}, client, 6)
    expect(requests).toEqual([
      {route: "/ask", body: {id, pid: 4321, at: 5, kind: "tool", tool: "Bash", description: "Run the unit tests", preview: "npm test", agent: "reviewer"}},
      {route: "/ask", body: {id, at: 6, kind: "tool", tool: "Artifact", description: "", preview: "action: read\nurl: https://claude.ai/artifact/abc", terminalOnly: true}},
    ])
  })

  it("sends Claude's question, the plan and an MCP server's link, and answers them with the input, the answers or the link's action", async () => {
    const questions = [{question: "Which library?", header: "Library", options: [{label: "date-fns", description: "Small"}, {label: "Luxon"}], multiSelect: false}]
    const answered = askClient([{decision: {behavior: "allow", answers: {"Which library?": "date-fns"}}}])
    expect(await ask(prompt("AskUserQuestion", {questions}), {}, answered.client, 1)).toEqual({hookSpecificOutput: {hookEventName: "PermissionRequest", decision: {behavior: "allow", updatedInput: {questions, answers: {"Which library?": "date-fns"}}}}})
    expect(answered.requests).toEqual([{route: "/ask", body: {id, at: 1, kind: "question", questions: [{question: "Which library?", options: [{label: "date-fns", description: "Small"}, {label: "Luxon", description: ""}], multiSelect: false}]}}])
    const approved = askClient([{decision: {behavior: "allow"}}])
    const plan = {plan: "## Retry uploads", planFilePath: "/p/plan.md"}
    expect(await ask(prompt("ExitPlanMode", plan), {}, approved.client, 2)).toEqual({hookSpecificOutput: {hookEventName: "PermissionRequest", decision: {behavior: "allow", updatedInput: plan}}})
    const link = JSON.stringify({session_id: id, hook_event_name: "Elicitation", mcp_server_name: "tracker", mode: "url", message: "Sign in to continue.", url: "https://tracker.example/oauth"})
    expect(await ask(link, {}, approved.client, 3)).toEqual({hookSpecificOutput: {hookEventName: "Elicitation", action: "accept"}})
    expect(approved.requests).toEqual([
      {route: "/ask", body: {id, at: 2, kind: "plan", plan: "## Retry uploads"}},
      {route: "/ask", body: {id, at: 3, kind: "link", server: "tracker", message: "Sign in to continue.", url: "https://tracker.example/oauth"}},
    ])
    const declined = askClient([{decision: {behavior: "deny", message: "Alice rejected this in the SimpleX group."}}])
    expect(await ask(link, {}, declined.client)).toEqual({hookSpecificOutput: {hookEventName: "Elicitation", action: "decline"}})
  })

  it("prints nothing without a decision, in a run that is not interactive, for the plugin's tools, a question without questions, a subagent's question, plan and artifact, a form, and on a failure", async () => {
    const errors = captureStderr()
    const {client, requests} = askClient([{}])
    expect(await ask(prompt("Bash", {command: "ls"}), {}, client)).toBeUndefined()
    expect(await ask(prompt("Bash", {command: "ls"}), {CLAUDE_CODE_ENTRYPOINT: "sdk-ts"}, client)).toBeUndefined()
    for (const tool of ["mcp__plugin_simplex_simplex__send", "AskUserQuestion"]) expect(await ask(prompt(tool, {}), {}, client)).toBeUndefined()
    const subagent = {agent_id: "a1", agent_type: "reviewer"}
    for (const [tool, fields] of [["WebFetch", {url: "https://claude.ai/code/artifact/abc"}], ["ExitPlanMode", {plan: "Plan"}]] as const) expect(await ask(prompt(tool, fields, subagent), {}, client)).toBeUndefined()
    expect(await ask(JSON.stringify({session_id: id, hook_event_name: "Elicitation", mcp_server_name: "tracker", mode: "form", message: "Your name"}), {}, client)).toBeUndefined()
    expect(requests).toHaveLength(1)
    expect(await ask("not json", {}, client)).toBeUndefined()
    expect(errors).toEqual([expect.stringMatching(/^SimpleX MCP ask: /)])
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

  it("pass the prompt, the final answer with the effort, and the new model as a switch to the session with the process and the time of the event, and stay quiet without a service", async () => {
    const errors = captureStderr()
    const {client, requests, ensured} = turnClient()
    expect(await turn(JSON.stringify({session_id: id, hook_event_name: "UserPromptSubmit", prompt: "fix the build"}), {CLAUDE_PID: "4321"}, client)).toEqual({})
    expect(await turn(JSON.stringify({session_id: id, hook_event_name: "Stop", last_assistant_message: "Fixed.", effort: {level: "high"}}), {}, client)).toEqual({})
    expect(await turn(JSON.stringify({session_id: id, hook_event_name: "PostModelSwitch", to_model: "claude-sonnet-5"}), {CLAUDE_PID: "x"}, client)).toEqual({})
    const at = performance.timeOrigin
    expect(requests).toEqual([
      {method: "POST", route: "/turn", body: {id, pid: 4321, at, prompt: "fix the build", typed: false}},
      {method: "POST", route: "/turn", body: {id, at, answer: "Fixed.", effort: "high"}},
      {method: "POST", route: "/turn", body: {id, at, model: "claude-sonnet-5", switched: true}},
    ])
    expect(ensured()).toBe(3)
    const stopped = {ensureService: async () => Promise.reject(Object.assign(new Error("connect ENOENT"), {code: "ENOENT"}))} as unknown as HookDeps["client"]
    expect(await turn(JSON.stringify({session_id: id, prompt: "x"}), {}, stopped)).toEqual({})
    expect(errors).toEqual(["SimpleX MCP turn: connect ENOENT"])
  })

  it("pass a failed turn without an answer, with the automatic continue noted after its error entry or the usage menu open in Claude Code's record, and a notification", async () => {
    const folder = mkdtempSync(join(tmpdir(), "simplex-mcp-failure-"))
    try {
      const transcript = join(folder, "session.jsonl")
      const limit = "You've hit your weekly limit · resets Nov 18, 3am (UTC)"
      const errorEntry = {type: "assistant", isApiErrorMessage: true, message: {model: "<synthetic>", content: [{type: "text", text: limit}]}}
      writeFileSync(transcript, `${JSON.stringify(errorEntry)}\n${JSON.stringify({type: "system", subtype: "informational", content: "Usage limit reached · continuing automatically at 1am · esc to cancel"})}\n`)
      mkdirSync(join(folder, "sessions"))
      writeFileSync(join(folder, "sessions", "4321.json"), JSON.stringify({status: "waiting", waitingFor: "dialog open"}))
      const {client, requests} = turnClient()
      const env = {CLAUDE_PID: "4321", CLAUDE_CONFIG_DIR: folder}
      const failed = (path: string) => turn(JSON.stringify({session_id: id, hook_event_name: "StopFailure", error: "rate_limit", transcript_path: path, last_assistant_message: limit}), env, client, 0, 1, 1)
      await failed(transcript)
      await failed(join(folder, "missing.jsonl"))
      await turn(JSON.stringify({session_id: id, hook_event_name: "Notification", notification_type: "permission_prompt", message: "Claude needs your permission"}), {}, client, 0, 1, 1)
      expect(requests).toEqual([
        {method: "POST", route: "/turn", body: {id, pid: 4321, at: 1, failure: {error: "rate_limit", text: limit, autoContinue: true, menu: false}}},
        {method: "POST", route: "/turn", body: {id, pid: 4321, at: 1, failure: {error: "rate_limit", text: limit, autoContinue: false, menu: true}}},
        {method: "POST", route: "/turn", body: {id, at: 1, waiting: {kind: "permission_prompt", text: "Claude needs your permission"}}},
      ])
    } finally {
      rmSync(folder, {recursive: true, force: true})
    }
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
      const stop = (answer: string, extra: object = {}) => turn(JSON.stringify({session_id: id, hook_event_name: "Stop", transcript_path: transcript, last_assistant_message: answer, ...extra}), {}, client, 0, 1, 10)
      await stop("Fixed.")
      const later = stop("Tests pass.")
      const written = new Promise<void>(resolve => setTimeout(() => resolve(appendFileSync(transcript, `${entry("Tests pass.", "claude-sonnet-5")}${entry("Subagent done.", "claude-haiku-5", {isSidechain: true})}`)), 30))
      await later
      await written
      appendFileSync(transcript, entry("No response requested.", "<synthetic>"))
      await stop("No response requested.")
      await stop("Never written.")
      await stop("Lost.", {transcript_path: join(folder, "missing.jsonl")})
      await turn(JSON.stringify({session_id: id, hook_event_name: "UserPromptSubmit", transcript_path: transcript, prompt: "fix the build"}), {}, client, 0, 1, 10)
      await turn(JSON.stringify({session_id: id, hook_event_name: "PostModelSwitch", transcript_path: transcript, to_model: "claude-opus-5"}), {}, client, 0, 1)
      expect(requests).toEqual([
        {method: "POST", route: "/turn", body: {id, at: 1, answer: "Fixed.", model: "claude-opus-5"}},
        {method: "POST", route: "/turn", body: {id, at: 1, answer: "Tests pass.", model: "claude-sonnet-5"}},
        {method: "POST", route: "/turn", body: {id, at: 1, answer: "No response requested."}},
        {method: "POST", route: "/turn", body: {id, at: 1, answer: "Never written."}},
        {method: "POST", route: "/turn", body: {id, at: 1, answer: "Lost."}},
        {method: "POST", route: "/turn", body: {id, at: 1, prompt: "fix the build", typed: false}},
        {method: "POST", route: "/turn", body: {id, at: 1, model: "claude-opus-5", switched: true}},
      ])
    } finally {
      rmSync(folder, {recursive: true, force: true})
    }
  })

  it("pass the usage of a Stop hook's turn from its transcript: the time from the turn's prompt entry to the answer, the output tokens of its distinct responses and the context of the answer, also when the prompt entry is more than 1 MiB back, and none past 64 MiB, without a prompt ID or for a synthetic answer", async () => {
    const folder = mkdtempSync(join(tmpdir(), "simplex-mcp-transcript-"))
    try {
      const line = (entry: object) => `${JSON.stringify(entry)}\n`
      const time = (seconds: number) => new Date(Date.UTC(2026, 10, 13, 10) + seconds * 1_000).toISOString()
      const prompt = (promptId: string, seconds: number, extra: object = {}) => line({type: "user", promptId, timestamp: time(seconds), ...extra, message: {role: "user", content: "Fix the build"}})
      const toolResult = (seconds: number, content = "ok") => line({type: "user", promptId: "p1", timestamp: time(seconds), message: {role: "user", content: [{type: "tool_result", tool_use_id: "t1", content}]}})
      const response = (messageId: string, seconds: number, output: number, content: object[], extra: object = {}, model = "claude-opus-5") =>
        line({type: "assistant", timestamp: time(seconds), ...extra, message: {id: messageId, model, content, usage: {input_tokens: 2, cache_creation_input_tokens: 1_099, cache_read_input_tokens: 534_289, output_tokens: output}}})
      const answer = (model = "claude-opus-5") => response("msg_3", 133.4, 580, [{type: "text", text: "Fixed."}], {}, model)
      const turnLines = (between = "") =>
        [
          prompt("p0", -100),
          response("msg_0", -90, 999, [{type: "text", text: "Earlier."}]),
          prompt("p1", 0, {isMeta: true}),
          prompt("p1", 0.1, {origin: {kind: "human"}}),
          response("msg_1", 2, 1_000, [{type: "thinking", thinking: "The build."}]),
          response("msg_1", 2.1, 1_000, [{type: "tool_use", id: "t1", name: "Bash", input: {}}]),
          toolResult(5),
          between,
          response("msg_2", 6, 5_000, [{type: "text", text: "Subagent done."}], {isSidechain: true}),
        ].join("")
      const write = (name: string, text: string) => (writeFileSync(join(folder, name), text), join(folder, name))
      const near = write("near.jsonl", `${turnLines()}${answer()}`)
      const far = write("far.jsonl", `${turnLines(toolResult(5.5, "x".repeat(1_600_000)))}${answer()}`)
      const beyond = write("beyond.jsonl", turnLines())
      truncateSync(beyond, 65 * 1_048_576)
      appendFileSync(beyond, `\n${answer()}`)
      const synthetic = write("synthetic.jsonl", `${turnLines()}${answer("<synthetic>")}`)
      const {client, requests} = turnClient()
      const stop = (transcript: string, extra: object = {prompt_id: "p1"}) => turn(JSON.stringify({session_id: id, hook_event_name: "Stop", transcript_path: transcript, last_assistant_message: "Fixed.", ...extra}), {}, client, 0, 1, 10)
      for (const transcript of [near, far, beyond]) await stop(transcript)
      await stop(near, {})
      await stop(synthetic)
      const usage = "2 min 13 s · 1.6K output tokens · 535K tokens in context"
      expect(requests.map(request => (request as {body: object}).body)).toEqual([
        {id, at: 1, answer: "Fixed.", model: "claude-opus-5", usage},
        {id, at: 1, answer: "Fixed.", model: "claude-opus-5", usage},
        {id, at: 1, answer: "Fixed.", model: "claude-opus-5"},
        {id, at: 1, answer: "Fixed.", model: "claude-opus-5"},
        {id, at: 1, answer: "Fixed."},
      ])
    } finally {
      rmSync(folder, {recursive: true, force: true})
    }
  })

  it("mark a prompt as typed only when its entry in the transcript, found by its prompt ID or else by its text, has the origin human, after that entry is written", async () => {
    const folder = mkdtempSync(join(tmpdir(), "simplex-mcp-transcript-"))
    try {
      const transcript = join(folder, "session.jsonl")
      const prompt = (promptId: string | undefined, content: unknown, origin?: object) => `${JSON.stringify({type: "user", ...(promptId === undefined ? {} : {promptId}), ...(origin === undefined ? {} : {origin}), message: {role: "user", content}})}\n`
      writeFileSync(transcript, [
        prompt("p1", "Fix the build", {kind: "human"}),
        prompt("p1", [{type: "tool_result", tool_use_id: "t1", content: "ok"}]),
        prompt("p2", "<task-notification>\n<task-id>b1</task-id>\n</task-notification>", {kind: "task-notification"}),
        prompt("p3", "Another Claude session sent a message: hi", {kind: "peer"}),
        prompt("p4", '<channel source="plugin:simplex:simplex" from="Alice" item="7">\nRun the tests\n</channel>', {kind: "channel", server: "plugin:simplex:simplex"}),
        prompt(undefined, [{type: "text", text: "Ship it"}, {type: "image", source: {}}], {kind: "human"}),
        `${JSON.stringify({type: "user", isSidechain: true, promptId: "p6", origin: {kind: "human"}, message: {role: "user", content: "Subagent task"}})}\n`,
      ].join(""))
      const {client, requests} = turnClient()
      const submit = (input: object) => turn(JSON.stringify({session_id: id, hook_event_name: "UserPromptSubmit", transcript_path: transcript, ...input}), {}, client, 0, 1, 1)
      await submit({prompt_id: "p1", prompt: "Fix the build"})
      await submit({prompt_id: "p2", prompt: "<task-notification>\n<task-id>b1</task-id>\n</task-notification>"})
      await submit({prompt_id: "p3", prompt: "Another Claude session sent a message: hi"})
      await submit({prompt_id: "p4", prompt: '<channel source="plugin:simplex:simplex" from="Alice" item="7">\nRun the tests\n</channel>'})
      await submit({prompt: "Ship it"})
      await submit({prompt_id: "p6", prompt: "Subagent task"})
      const later = submit({prompt_id: "p7", prompt: "Run the linter"})
      const written = new Promise<void>(resolve => setTimeout(() => resolve(appendFileSync(transcript, prompt("p7", "Run the linter", {kind: "human"}))), 30))
      await later
      await written
      await submit({prompt_id: "p8", prompt: "Never written."})
      expect(requests.map(request => (request as {body: {prompt: string; typed: boolean}}).body).map(({prompt, typed}) => [prompt.slice(0, 20), typed])).toEqual([
        ["Fix the build", true],
        ["<task-notification>\n", false],
        ["Another Claude sessi", false],
        ['<channel source="plu', false],
        ["Ship it", true],
        ["Subagent task", false],
        ["Run the linter", true],
        ["Never written.", false],
      ])
    } finally {
      rmSync(folder, {recursive: true, force: true})
    }
  })

  it("mark a prompt as typed when the transcript with the prompt's entry is created 30 reads after the hook starts, as behind a user's own prompt hook of 3 s", async () => {
    const folder = mkdtempSync(join(tmpdir(), "simplex-mcp-transcript-"))
    try {
      const transcript = join(folder, "session.jsonl")
      const {client, requests} = turnClient()
      const submitted = turn(JSON.stringify({session_id: id, hook_event_name: "UserPromptSubmit", transcript_path: transcript, prompt_id: "p1", prompt: "Fix the build"}), {}, client, 0, 1, 10)
      const written = new Promise<void>(resolve => setTimeout(() => resolve(writeFileSync(transcript, `${JSON.stringify({type: "user", promptId: "p1", origin: {kind: "human"}, message: {role: "user", content: "Fix the build"}})}\n`)), 300))
      await submitted
      await written
      expect(requests).toEqual([{method: "POST", route: "/turn", body: {id, at: 1, prompt: "Fix the build", typed: true}}])
    } finally {
      rmSync(folder, {recursive: true, force: true})
    }
  })

  const notification = (fields: string, after = "") => `<task-notification>\n${fields}\n</task-notification>${after}`
  const userEntry = (promptId: string, content: unknown, extra: object = {}) => `${JSON.stringify({type: "user", promptId, ...extra, message: {role: "user", content}})}\n`
  const queuedEntry = (prompt: string, origin: object) => `${JSON.stringify({type: "attachment", attachment: {type: "queued_command", prompt, origin, commandMode: "prompt"}})}\n`
  const enqueueEntry = (content: string) => `${JSON.stringify({type: "queue-operation", operation: "enqueue", content})}\n`
  const notes = (requests: unknown[]) => requests.map(request => (request as {body: {note?: string; typed: boolean; midTurn?: boolean}}).body)
  const kinds = (requests: unknown[]) => notes(requests).map(({note, typed, midTurn}) => [note?.replace(/^!- ([\s\S]*)!$/, "$1"), typed, midTurn])

  it("send a one-line note of each kind of Claude Code's own prompt, from the origin of its transcript entry, and none for a typed or a channel prompt", async () => {
    const folder = mkdtempSync(join(tmpdir(), "simplex-mcp-transcript-"))
    try {
      const transcript = join(folder, "session.jsonl")
      const long = "x".repeat(300)
      const prompts: [string, string, object][] = [
        ["p1", notification(`<task-id>b2gj1pdcw</task-id>\n<output-file>/p/tasks/b2gj1pdcw.output</output-file>\n<status>completed</status>\n<summary>Background command "Rebuild tests &amp;&amp; show errors" completed (exit code 0)</summary>`), {origin: {kind: "task-notification", producer: "session-task"}}],
        ["p2", notification(`<task-id>w5asfny36</task-id>\n<status>failed</status>\n<summary>Dynamic workflow "Research" failed: TypeError: null is not an object\n    at &lt;anonymous&gt; (workflow.js:144:22)</summary>`), {origin: {kind: "task-notification"}}],
        ["p3", notification(`<task-id>b4mg8bpso</task-id>\n<status>killed</status>\n<summary>Background command "Serve the docs" was stopped</summary>`), {origin: {kind: "task-notification"}}],
        ["p4", notification(`<task-id>boud456ze</task-id>\n<summary>Monitor event: "Exit status of each of the 4 flag-combination builds"</summary>\n<event>\nserverpg exit 0\nclientpg exit 0</event>`, "\nIf this event is something the user would act on now, send a PushNotification."), {origin: {kind: "task-notification"}}],
        ["p5", notification("<summary>Goal check-in: background work still running</summary>", "\n<system-reminder>\nThe goal is still active.\n</system-reminder>"), {origin: {kind: "task-notification"}}],
        ["p6", 'Another Claude session sent a message:\n<agent-message from="mcp-impl2">\nI read the evidence.\n\nDetails follow.\n</agent-message>', {origin: {kind: "peer", from: "mcp-impl2", senderTaskId: "amcp-impl2-1", name: "mcp-impl2", body: "I read the evidence.\n\nDetails follow."}}],
        ["p7", 'Another Claude session sent a message:\n<agent-message from="a43be993e71d0a22f">\n[Subagent hand-back] The report follows:\n  \n  ## Review of the race fix\n</agent-message>', {origin: {kind: "peer", from: "a43be993e71d0a22f", senderTaskId: "a43be993e71d0a22f", name: "general-purpose", body: "[Subagent hand-back] The report follows:\n  \n  ## Review of the race fix", handback: true, handbackReportText: "\n## Review of the race fix\n\nNo defect."}}],
        ["p8", 'Another Claude session sent a message:\n<agent-message from="ios-terminal">\n[Subagent hand-back] The report follows:\n  All done.\n</agent-message>', {origin: {kind: "peer", from: "ios-terminal", senderTaskId: "a1", name: "ios-terminal", body: "[Subagent hand-back] The report follows:\n  All done.", handback: true}}],
        ["p9", "Reply with the single word tick.", {isMeta: true, turnOrigin: "scheduled", scheduledTaskId: "d2657e37"}],
        ["p10", "Your claude.ai usage limit has reset. Continue the task you were working on when the limit was reached; do not repeat work that is already complete.", {isMeta: true, origin: {kind: "auto-continuation"}}],
        ["p11", "The coordinator sent a message while you were working:\nApproved.", {isMeta: true, origin: {kind: "coordinator"}}],
        ["p12", "Fix the build", {origin: {kind: "human"}}],
        ["p13", '<channel source="plugin:simplex:simplex" from="Alice" item="7">\nRun the tests\n</channel>', {isMeta: true, origin: {kind: "channel", server: "plugin:simplex:simplex"}}],
        ["p14", "Run the checks", {turnOrigin: "sdk"}],
        ["p15", `Another Claude session sent a message:\n<agent-message from="reviewer">\n${long}\n</agent-message>`, {origin: {kind: "peer", from: "reviewer", name: "reviewer", body: long}}],
      ]
      writeFileSync(transcript, prompts.map(([promptId, content, extra]) => userEntry(promptId, content, extra)).join(""))
      const {client, requests} = turnClient()
      for (const [promptId, content] of prompts) await turn(JSON.stringify({session_id: id, hook_event_name: "UserPromptSubmit", transcript_path: transcript, prompt_id: promptId, prompt: content}), {}, client, 0, 1, 1)
      const prefix = "✉️ Message from reviewer: "
      expect(kinds(requests)).toEqual([
        ['✅ Background command "Rebuild tests && show errors" completed (exit code 0)', false, undefined],
        ['❌ Dynamic workflow "Research" failed: TypeError: null is not an object', false, undefined],
        ['⏹️ Background command "Serve the docs" was stopped', false, undefined],
        ['📡 Monitor event: "Exit status of each of the 4 flag-combination builds": serverpg exit 0', false, undefined],
        ["🔔 Goal check-in: background work still running", false, undefined],
        ["✉️ Message from mcp-impl2: I read the evidence.", false, undefined],
        ["📋 Report from subagent general-purpose: ## Review of the race fix", false, undefined],
        ["📋 Report from subagent ios-terminal: [Subagent hand-back] The report follows:", false, undefined],
        ["⏰ Scheduled prompt: Reply with the single word tick.", false, undefined],
        ["🔄 Automatic prompt: Your claude.ai usage limit has reset. Continue the task you were working on when the limit was reached; do not repeat work that is already complete.", false, undefined],
        ["⚙️ Claude Code: The coordinator sent a message while you were working:", false, undefined],
        [undefined, true, undefined],
        [undefined, false, undefined],
        [undefined, false, undefined],
        [`${prefix}${"x".repeat(199 - [...prefix].length)}…`, false, undefined],
      ])
      expect(notes(requests).flatMap(({note}) => (note === undefined ? [] : [note.startsWith("!- ") && note.endsWith("!")]))).toEqual(Array(12).fill(true))
      expect(notes(requests)[5].note).toBe("!- ✉️ Message from mcp-impl2: I read the evidence.!")
    } finally {
      rmSync(folder, {recursive: true, force: true})
    }
  })

  it("find a prompt that reaches a running turn by its queued command or its own later entry after the entry of the turn's prompt ID, and mark it mid-turn", async () => {
    const folder = mkdtempSync(join(tmpdir(), "simplex-mcp-transcript-"))
    try {
      const transcript = join(folder, "session.jsonl")
      const alpha = notification('<task-id>buq1jepzn</task-id>\n<status>completed</status>\n<summary>Background command "Run the tests" completed (exit code 0)</summary>')
      const gamma = notification('<task-id>btjw2lm2a</task-id>\n<status>completed</status>\n<summary>Background command "Echo gamma" completed (exit code 0)</summary>')
      writeFileSync(transcript, [
        userEntry("t1", "Run the tests in the background, then wait.", {origin: {kind: "human"}}),
        userEntry("t1", [{type: "tool_result", tool_use_id: "toolu_1", content: "Command running in background with ID: buq1jepzn."}]),
        enqueueEntry(alpha),
        enqueueEntry("Also say the word bravo."),
      ].join(""))
      const {client, requests} = turnClient()
      const submit = (promptId: string, prompt: string) => turn(JSON.stringify({session_id: id, hook_event_name: "UserPromptSubmit", transcript_path: transcript, prompt_id: promptId, prompt}), {}, client, 0, 1, 10)
      const added = submit("t1", alpha)
      const queued = new Promise<void>(resolve => setTimeout(() => resolve(appendFileSync(transcript, `${queuedEntry(alpha, {kind: "task-notification", producer: "session-task"})}${queuedEntry("Also say the word bravo.", {kind: "human"})}`)), 30))
      await added
      await queued
      await submit("t1", "Also say the word bravo.")
      appendFileSync(transcript, [
        userEntry("t2", gamma, {origin: {kind: "task-notification"}}),
        enqueueEntry("Say the word charlie."),
        queuedEntry("Say the word charlie.", {kind: "human"}),
        userEntry("t3", "Then lint it.", {origin: {kind: "human"}}),
      ].join(""))
      await submit("t2", gamma)
      await submit("t2", "Say the word charlie.")
      await submit("t2", "Then lint it.")
      expect(kinds(requests)).toEqual([
        ['✅ Background command "Run the tests" completed (exit code 0)', false, true],
        [undefined, true, true],
        ['✅ Background command "Echo gamma" completed (exit code 0)', false, undefined],
        [undefined, true, true],
        [undefined, true, true],
      ])
    } finally {
      rmSync(folder, {recursive: true, force: true})
    }
  })

  it("send a note of a teammate's message with its sender and summary, or the first line without a summary, from the first tag that is not an idle notification, and none for idle notifications only", async () => {
    const folder = mkdtempSync(join(tmpdir(), "simplex-mcp-transcript-"))
    try {
      const transcript = join(folder, "session.jsonl")
      const tag = (attributes: string, body: string) => `<teammate-message teammate_id="lookalikes-builder" color="green"${attributes}>\n${body}\n</teammate-message>`
      const teammate = (...tags: string[]) => `Another Claude session sent a message:\n${tags.join("\n\n")}\n\nThis came from another Claude session — not typed by your user.`
      const idle = tag("", '{"type":"idle_notification","from":"lookalikes-builder","timestamp":"2026-11-12T19:00:02.715Z"}')
      const prompts: [string, string][] = [
        ["p1", teammate(tag(' summary="X Ads look-alike seed file &quot;v2&quot; written"', "The seed file is written.\n\nSeeds: 37 handles."))],
        ["p2", teammate(tag("", "\nThe seed file is written.\n\nSeeds: 37 handles."))],
        ["p3", teammate(idle)],
        ["p4", teammate(idle, tag(' summary="Handles checked"', "All 37 handles are live."), idle)],
        ["p5", teammate(idle, idle)],
      ]
      writeFileSync(transcript, prompts.map(([promptId, content]) => userEntry(promptId, content)).join(""))
      const {client, requests} = turnClient()
      for (const [promptId, content] of prompts) await turn(JSON.stringify({session_id: id, hook_event_name: "UserPromptSubmit", transcript_path: transcript, prompt_id: promptId, prompt: content}), {}, client, 0, 1, 1)
      expect(kinds(requests)).toEqual([
        ['✉️ Message from lookalikes-builder: X Ads look-alike seed file "v2" written', false, undefined],
        ["✉️ Message from lookalikes-builder: The seed file is written.", false, undefined],
        [undefined, false, undefined],
        ["✉️ Message from lookalikes-builder: Handles checked", false, undefined],
        [undefined, false, undefined],
      ])
    } finally {
      rmSync(folder, {recursive: true, force: true})
    }
  })

  it("keep a note on one line when the sender's name has a line break or a control character", async () => {
    const folder = mkdtempSync(join(tmpdir(), "simplex-mcp-transcript-"))
    try {
      const transcript = join(folder, "session.jsonl")
      const prompts: [string, string, object][] = [
        ["p1", 'Another Claude session sent a message:\n<agent-message from="a1">\nDone.\n</agent-message>', {origin: {kind: "peer", from: "a1", name: "mcp-impl2\n‮reviewer", body: "Done."}}],
        ["p2", 'Another Claude session sent a message:\n<agent-message from="a2">\nAll done.\n</agent-message>', {origin: {kind: "peer", from: "a2", name: "ios\r\nterminal", body: "All done.", handback: true, handbackReportText: "All done."}}],
        ["p3", 'Another Claude session sent a message:\n<teammate-message teammate_id="lookalikes\nbuilder" color="green">\nThe seed file is written.\n</teammate-message>', {}],
      ]
      writeFileSync(transcript, prompts.map(([promptId, content, extra]) => userEntry(promptId, content, extra)).join(""))
      const {client, requests} = turnClient()
      for (const [promptId, content] of prompts) await turn(JSON.stringify({session_id: id, hook_event_name: "UserPromptSubmit", transcript_path: transcript, prompt_id: promptId, prompt: content}), {}, client, 0, 1, 1)
      expect(kinds(requests)).toEqual([
        ["✉️ Message from mcp-impl2 reviewer: Done.", false, undefined],
        ["📋 Report from subagent ios terminal: All done.", false, undefined],
        ["✉️ Message from lookalikes builder: The seed file is written.", false, undefined],
      ])
    } finally {
      rmSync(folder, {recursive: true, force: true})
    }
  })

  it("find a prompt of a long turn whose first entry is out of the tail by the turn's tool results, and keep waiting for a prompt with the text of the turn's own prompt while it is queued", async () => {
    const folder = mkdtempSync(join(tmpdir(), "simplex-mcp-transcript-"))
    try {
      const transcript = join(folder, "session.jsonl")
      const toolResult = (promptId: string) => userEntry(promptId, [{type: "tool_result", tool_use_id: "toolu_1", content: "ok"}])
      writeFileSync(transcript, [
        toolResult("t1"),
        enqueueEntry("Run the linter"),
        userEntry("t2", "continue", {origin: {kind: "human"}}),
        toolResult("t2"),
        enqueueEntry("continue"),
      ].join(""))
      const {client, requests} = turnClient()
      const submit = (promptId: string, prompt: string, readMs = 1) => turn(JSON.stringify({session_id: id, hook_event_name: "UserPromptSubmit", transcript_path: transcript, prompt_id: promptId, prompt}), {}, client, 0, 1, readMs)
      await submit("t1", "Run the linter")
      const again = submit("t2", "continue", 10)
      const queued = new Promise<void>(resolve => setTimeout(() => resolve(appendFileSync(transcript, queuedEntry("continue", {kind: "human"}))), 30))
      await again
      await queued
      expect(kinds(requests)).toEqual([
        [undefined, true, true],
        [undefined, true, true],
      ])
    } finally {
      rmSync(folder, {recursive: true, force: true})
    }
  })

  it("classify a prompt by its record when a later prompt with its text is still queued after the reads: Claude Code's own prompt by its origin, as a repeated monitor event, and a typed prompt as typed and mid-turn", async () => {
    const folder = mkdtempSync(join(tmpdir(), "simplex-mcp-transcript-"))
    try {
      const transcript = join(folder, "session.jsonl")
      const event = notification('<task-id>bfmkogk6v</task-id>\n<summary>Monitor event: "Build errors"</summary>\n<event>error: missing import</event>')
      writeFileSync(transcript, [
        enqueueEntry(event),
        userEntry("t1", event, {origin: {kind: "task-notification"}}),
        enqueueEntry(event),
        userEntry("t2", "continue", {origin: {kind: "human"}}),
        enqueueEntry("continue"),
      ].join(""))
      const {client, requests} = turnClient()
      const submit = (promptId: string, prompt: string) => turn(JSON.stringify({session_id: id, hook_event_name: "UserPromptSubmit", transcript_path: transcript, prompt_id: promptId, prompt}), {}, client, 0, 1, 1)
      await submit("t1", event)
      await submit("t2", "continue")
      expect(kinds(requests)).toEqual([
        ['📡 Monitor event: "Build errors": error: missing import', false, undefined],
        [undefined, true, true],
      ])
    } finally {
      rmSync(folder, {recursive: true, force: true})
    }
  })

  it("send the first line of a task notification without a block, also from a queued command that has only its command mode", async () => {
    const folder = mkdtempSync(join(tmpdir(), "simplex-mcp-transcript-"))
    try {
      const transcript = join(folder, "session.jsonl")
      const stopped = '3 background agents were stopped by the user: "Duarte drafts the ad post", "Miller drafts the ad post".'
      const stoppedAgain = '2 background agents were stopped by the user: "Heath brothers draft the ad post".'
      writeFileSync(transcript, [
        userEntry("t1", stopped, {origin: {kind: "task-notification"}}),
        `${JSON.stringify({type: "attachment", attachment: {type: "queued_command", prompt: stoppedAgain, commandMode: "task-notification"}})}\n`,
      ].join(""))
      const {client, requests} = turnClient()
      for (const prompt of [stopped, stoppedAgain]) await turn(JSON.stringify({session_id: id, hook_event_name: "UserPromptSubmit", transcript_path: transcript, prompt_id: "t1", prompt}), {}, client, 0, 1, 1)
      expect(kinds(requests)).toEqual([
        [`🔔 ${stopped}`, false, undefined],
        [`🔔 ${stoppedAgain}`, false, true],
      ])
    } finally {
      rmSync(folder, {recursive: true, force: true})
    }
  })

  it("take the entry of the prompt ID whose text is the prompt, also when an older prompt has the same text, mark a prompt still queued after the reads as typed and mid-turn, and fall back to the origin of the prompt ID's entry", async () => {
    const folder = mkdtempSync(join(tmpdir(), "simplex-mcp-transcript-"))
    try {
      const transcript = join(folder, "session.jsonl")
      writeFileSync(transcript, [
        userEntry("p1", "<system-reminder>\nTools used before the summary.\n</system-reminder>", {isMeta: true}),
        userEntry("p1", "Fix the build", {origin: {kind: "human"}}),
        userEntry("p2", notification("<status>completed</status>\n<summary>Agent \"Review\" finished</summary>"), {origin: {kind: "task-notification"}}),
        enqueueEntry("Run the linter"),
      ].join(""))
      const {client, requests} = turnClient()
      const submit = (promptId: string, prompt: string) => turn(JSON.stringify({session_id: id, hook_event_name: "UserPromptSubmit", transcript_path: transcript, prompt_id: promptId, prompt}), {}, client, 0, 1, 1)
      await submit("p1", "Fix the build")
      await submit("p2", "Run the linter")
      await submit("p1", "Fix the build, then deploy")
      const again = submit("p3", "Fix the build")
      const written = new Promise<void>(resolve => setTimeout(() => resolve(appendFileSync(transcript, userEntry("p3", "Fix the build", {origin: {kind: "human"}}))), 30))
      await again
      await written
      expect(kinds(requests)).toEqual([
        [undefined, true, undefined],
        [undefined, true, true],
        [undefined, true, undefined],
        [undefined, true, undefined],
      ])
    } finally {
      rmSync(folder, {recursive: true, force: true})
    }
  })

  it("post one line for a compaction of the session's conversation, from its boundary in the transcript after the boundary is written, with its trigger and token counts, and nothing for a subagent's compaction, a boundary of a subagent, a boundary older than 60 s or one stamped after the hook started", async () => {
    const folder = mkdtempSync(join(tmpdir(), "simplex-mcp-transcript-"))
    try {
      const transcript = join(folder, "session.jsonl")
      const at = Date.now()
      const boundary = (age: number, compactMetadata: object, extra: object = {}) => `${JSON.stringify({type: "system", subtype: "compact_boundary", content: "Conversation compacted", ...extra, timestamp: new Date(at - age).toISOString(), compactMetadata})}\n`
      writeFileSync(transcript, `${boundary(61_000, {trigger: "auto", preTokens: 967_931, postTokens: 22_484})}${boundary(1_000, {trigger: "auto", preTokens: 972_401, postTokens: 9_145}, {isSidechain: true})}${boundary(-6_000, {trigger: "auto", preTokens: 968_000, postTokens: 30_000})}`)
      const {client, requests} = turnClient()
      const compact = (extra: object = {}) => turn(JSON.stringify({session_id: id, hook_event_name: "PostCompact", transcript_path: transcript, trigger: "auto", compact_summary: "Summary.", ...extra}), {}, client, 0, at, 1)
      expect(await compact()).toEqual({})
      expect(await compact({agent_id: "a1"})).toEqual({})
      expect(requests).toEqual([])
      const automatic = compact()
      const written = new Promise<void>(resolve => setTimeout(() => resolve(appendFileSync(transcript, boundary(500, {trigger: "auto", preTokens: 967_558, postTokens: 34_754}))), 300))
      await automatic
      await written
      appendFileSync(transcript, boundary(200, {trigger: "manual", preTokens: 398_983, postTokens: 25_399}))
      await compact({trigger: "manual"})
      appendFileSync(transcript, boundary(100, {trigger: "auto"}))
      await compact()
      expect(requests).toEqual([
        {method: "POST", route: "/turn", body: {id, at, note: "⚙️ Context summarised automatically: 968K → 35K tokens"}},
        {method: "POST", route: "/turn", body: {id, at, note: "⚙️ Context summarised on request: 399K → 25K tokens"}},
        {method: "POST", route: "/turn", body: {id, at, note: "⚙️ Context summarised automatically"}},
      ])
    } finally {
      rmSync(folder, {recursive: true, force: true})
    }
  })

  const toolUse =(event: string, tool: string, input: object, extra: object = {}) => JSON.stringify({session_id: id, hook_event_name: event, cwd: "/p/app", prompt_id: "p1", effort: {level: "high"}, tool_name: tool, tool_input: input, tool_use_id: `toolu_${tool}`, ...extra})
  const steps = (requests: unknown[]) => requests.map(request => (request as {body: {step?: unknown}}).body.step)

  it("send a step at the start of each tool use of the main conversation, with its text from the tool's input and paths relative to the session's folder, ✏️ for edits, notebook edits and writes, and 🔍 for searches, also of a tool whose name has search", async () => {
    const {client, requests} = turnClient()
    const started = (tool: string, input: object) => turn(toolUse("PreToolUse", tool, input), {CLAUDE_PID: "4321"}, client, 0, 1)
    await started("Bash", {command: "npm test\nnpm run lint", description: "Run the tests"})
    await started("Bash", {command: "\ngit status\ngit log"})
    await started("Read", {file_path: "/p/app/src/hook.ts"})
    await started("Edit", {file_path: "/etc/hosts", old_string: "a", new_string: "b"})
    await started("Write", {file_path: "/p/app/rfcs/2026-09-11-proof-of-work.md", content: "# Proof of work"})
    await started("Read", {file_path: "/p/app-old/src/hook.ts"})
    await started("WebFetch", {url: "https://spec.torproject.org/hspow-spec/common-protocol.html", prompt: "Summarize"})
    await started("WebSearch", {query: '"hs_pow.c" tor source'})
    await started("Agent", {description: "Review pass 6 usage stats", prompt: "Review it.", subagent_type: "general-purpose"})
    await started("ToolSearch", {query: "select:Monitor", max_results: 5})
    await started("Glob", {pattern: "**/*.ts", path: "/p/app/src"})
    await started("Grep", {pattern: "endSteps", path: "/p/app/src/hub.ts", output_mode: "content"})
    await started("Grep", {pattern: "TODO"})
    await started("mcp__claude_ai_Gmail__search_threads", {query: "from:alice"})
    await started("mcp__research__fetch_page", {url: "https://example.com/search"})
    await started("TodoWrite", {todos: [{content: "Test", status: "pending"}]})
    await started("NotebookEdit", {notebook_path: "/p/app/notes/a.ipynb", new_source: "print(1)", edit_mode: "replace"})
    await started("SlackPost", {description: " \n", notebook_path: "/p/app/a.ipynb", url: "https://example.com/a"})
    await started("Task", {description: `Review‮ ${"x".repeat(300)}`})
    await started("Bash", {description: "Say hi!", command: "echo hi"})
    expect(requests[0]).toEqual({method: "POST", route: "/turn", body: {id, pid: 4321, at: 1, effort: "high", step: {id: "toolu_Bash", text: "!- 💻 Run the tests!"}}})
    expect(steps(requests)).toEqual([
      {id: "toolu_Bash", text: "!- 💻 Run the tests!"},
      {id: "toolu_Bash", text: "!- 💻 git status!"},
      {id: "toolu_Read", text: "!- 📖 Read src/hook.ts!"},
      {id: "toolu_Edit", text: "!- ✏️ Edit /etc/hosts!"},
      {id: "toolu_Write", text: "!- ✏️ Write rfcs/2026-09-11-proof-of-work.md!"},
      {id: "toolu_Read", text: "!- 📖 Read /p/app-old/src/hook.ts!"},
      {id: "toolu_WebFetch", text: "!- 🌐 Fetch https://spec.torproject.org/hspow-spec/common-protocol.html!"},
      {id: "toolu_WebSearch", text: '!- 🔍 Search "hs_pow.c" tor source!'},
      {id: "toolu_ToolSearch", text: "!- 🔍 ToolSearch: select:Monitor!"},
      {id: "toolu_Glob", text: "!- 🔍 Glob **/*.ts in src!"},
      {id: "toolu_Grep", text: "!- 🔍 Grep endSteps in src/hub.ts!"},
      {id: "toolu_Grep", text: "!- 🔍 Grep TODO!"},
      {id: "toolu_mcp__claude_ai_Gmail__search_threads", text: "!- 🔍 mcp__claude_ai_Gmail__search_threads: from:alice!"},
      {id: "toolu_mcp__research__fetch_page", text: "!- 🔧 mcp__research__fetch_page: https://example.com/search!"},
      {id: "toolu_TodoWrite", text: "!- 🔧 TodoWrite!"},
      {id: "toolu_NotebookEdit", text: "!- ✏️ Edit notes/a.ipynb!"},
      {id: "toolu_SlackPost", text: "!- 🔧 SlackPost: https://example.com/a!"},
      {id: "toolu_Task", text: `!- 🔧 Task: Review  ${"x".repeat(199 - [..."🔧 Task: Review  "].length)}…!`},
      {id: "toolu_Bash", text: "!- 💻 Say hiǃ!"},
    ])
  })

  it("send a step at the end of each tool use with its outcome and duration, or in background for a launch, and nothing for a tool use of this plugin, a launch of an agent, or without its ID or tool", async () => {
    const {client, requests, ensured} = turnClient()
    const bash = {command: "npm test", description: "Run the tests"}
    const ended = (event: string, extra: object, tool = "Bash", input: object = bash) => turn(toolUse(event, tool, input, extra), {}, client, 0, 1)
    await ended("PostToolUse", {tool_response: {stdout: "ok"}, duration_ms: 2018})
    await ended("PostToolUseFailure", {error: "Exit code 1", is_interrupt: false, duration_ms: 39})
    await ended("PostToolUseFailure", {error: "Interrupted", is_interrupt: true, duration_ms: 252_000})
    await ended("PostToolUse", {duration_ms: 42_500})
    await ended("PostToolUse", {duration_ms: 3_900_000})
    await ended("PostToolUse", {})
    await ended("PostToolUse", {duration_ms: 41.6})
    await ended("PostToolUse", {duration_ms: 1999.6})
    await ended("PostToolUse", {duration_ms: 25}, "Bash", {...bash, run_in_background: true})
    await ended("PostToolUse", {duration_ms: 31}, "Agent", {description: "Review", prompt: "Review it.", run_in_background: true})
    await ended("PostToolUseFailure", {error: "Exit code 127", is_interrupt: false, duration_ms: 12}, "Bash", {...bash, run_in_background: true})
    await turn(toolUse("PreToolUse", "mcp__plugin_simplex_simplex__send", {chat: "@1", text: "hi"}), {}, client, 0, 1)
    await ended("PostToolUse", {duration_ms: 120}, "mcp__plugin_simplex_simplex__send")
    await turn(JSON.stringify({session_id: id, hook_event_name: "PreToolUse", tool_input: bash, tool_use_id: "toolu_1"}), {}, client, 0, 1)
    await turn(JSON.stringify({session_id: id, hook_event_name: "PostToolUse", tool_name: "Bash", tool_input: bash, duration_ms: 5}), {}, client, 0, 1)
    expect(steps(requests)).toEqual([
      {id: "toolu_Bash", text: "!- 💻 Run the tests 2.0 s!", end: true},
      {id: "toolu_Bash", text: "!- 💻 Run the tests ❌ 39 ms!", end: true},
      {id: "toolu_Bash", text: "!- 💻 Run the tests ⏹️ 4 min 12 s!", end: true},
      {id: "toolu_Bash", text: "!- 💻 Run the tests 42 s!", end: true},
      {id: "toolu_Bash", text: "!- 💻 Run the tests 1 h 5 min!", end: true},
      {id: "toolu_Bash", text: "!- 💻 Run the tests!", end: true},
      {id: "toolu_Bash", text: "!- 💻 Run the tests 42 ms!", end: true},
      {id: "toolu_Bash", text: "!- 💻 Run the tests 2.0 s!", end: true},
      {id: "toolu_Bash", text: "!- 💻 Run the tests (in background)!", end: true},
      {id: "toolu_Bash", text: "!- 💻 Run the tests ❌ 12 ms!", end: true},
    ])
    expect(ensured()).toBe(10)
  })

  it("send the start of a subagent with its type, the description and tool use of its record and its transcript, and nothing for a teammate or without a record", async () => {
    const folder = mkdtempSync(join(tmpdir(), "simplex-mcp-agents-"))
    try {
      const subagents = join(folder, "s1", "subagents")
      mkdirSync(subagents, {recursive: true})
      writeFileSync(join(subagents, "agent-a1.meta.json"), JSON.stringify({agentType: "general-purpose", description: "Audit the call stack", toolUseId: "toolu_9"}))
      writeFileSync(join(subagents, "agent-a2.meta.json"), JSON.stringify({agentType: "reviewer", description: "Review", taskKind: "in_process_teammate"}))
      const {client, requests} = turnClient()
      const started = (agent: string) => turn(JSON.stringify({session_id: id, hook_event_name: "SubagentStart", transcript_path: join(folder, "s1.jsonl"), agent_id: agent, agent_type: "Explore"}), {}, client, 0, 1, 1)
      for (const agent of ["a1", "a2", "a3"]) await started(agent)
      expect(requests).toEqual([{method: "POST", route: "/turn", body: {id, at: 1, agent: {id: "a1", start: {type: "Explore", description: "Audit the call stack", toolUse: "toolu_9", transcript: join(subagents, "agent-a1.jsonl")}}}}])
    } finally {
      rmSync(folder, {recursive: true, force: true})
    }
  })

  it("send a subagent's tool uses as its steps, its hand-back as its report and its stop with the first line of its answer, without effort and answer", async () => {
    const {client, requests} = turnClient()
    const bash = {command: "npm test", description: "Run the tests"}
    await turn(toolUse("PreToolUse", "Bash", bash, {agent_id: "a1"}), {}, client, 0, 1)
    await turn(toolUse("PostToolUse", "Bash", bash, {agent_id: "a1"}), {}, client, 0, 1)
    await turn(toolUse("PreToolUse", "SubagentHandback", {message: "Found 6 callers.\nDetails."}, {agent_id: "a1"}), {}, client, 0, 1)
    await turn(JSON.stringify({session_id: id, hook_event_name: "SubagentStop", agent_id: "a1", last_assistant_message: "Done.\nMore.", effort: {level: "high"}}), {}, client, 0, 1)
    expect(requests.map(request => (request as {body: unknown}).body)).toEqual([
      {id, at: 1, agent: {id: "a1", step: {id: "toolu_Bash", text: "💻 Run the tests"}}},
      {id, at: 1, agent: {id: "a1", step: {id: "toolu_Bash", text: "💻 Run the tests", end: true}}},
      {id, at: 1, agent: {id: "a1", report: "Found 6 callers."}},
      {id, at: 1, agent: {id: "a1", stop: "Done."}},
    ])
  })

  it("send the end of an agent from its result or its task notification with the figures, a workflow's run from its result, and no step for either", async () => {
    const {client, requests} = turnClient()
    await turn(toolUse("PreToolUse", "Agent", {description: "Review"}), {}, client, 0, 1)
    await turn(toolUse("PostToolUse", "Agent", {description: "Review"}, {tool_response: {status: "completed", agentId: "a1", totalTokens: 61_500, totalToolUseCount: 22, totalDurationMs: 160_000}}), {}, client, 0, 1)
    await turn(toolUse("PostToolUse", "Workflow", {}, {tool_response: {status: "async_launched", runId: "wf_1", workflowName: "review", transcriptDir: "/t/wf_1"}}), {}, client, 0, 1)
    const notification = "<task-notification>\n<task-id>a2</task-id>\n<tool-use-id>toolu_7</tool-use-id>\n<status>completed</status>\n<summary>Agent \"Map\" finished</summary>\n<usage><subagent_tokens>112485</subagent_tokens><tool_uses>27</tool_uses><duration_ms>165352</duration_ms></usage>\n</task-notification>"
    await turn(JSON.stringify({session_id: id, hook_event_name: "UserPromptSubmit", prompt: notification}), {}, client, 0, 1)
    expect(requests.map(request => (request as {body: {task?: unknown; workflow?: unknown}}).body).map(({task, workflow}) => task ?? workflow)).toEqual([
      {toolUse: "toolu_Agent", status: "completed", tokens: 61_500, steps: 22, ms: 160_000},
      {toolUse: "toolu_Workflow", run: "wf_1", name: "review", folder: "/t/wf_1"},
      {toolUse: "toolu_7", status: "completed", tokens: 112_485, steps: 27, ms: 165_352},
    ])
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
