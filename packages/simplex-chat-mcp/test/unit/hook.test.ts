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
      {method: "POST", route: "/title", body: {id: "11111111-1111-4111-8111-111111111111", pid: 4321, at: 5, name: "auth", project: "/p/app", model: "claude-opus-5", config: {mainGroup: "Claude Control", approve: "requests", expireHours: 23}}},
      {method: "POST", route: "/title", body: {id: "11111111-1111-4111-8111-111111111111", at: performance.timeOrigin, name: "auth", project: "/p/app", model: "claude-opus-5", config: {mainGroup: "Claude Control", approve: "requests", expireHours: 23}}},
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
      {method: "POST", route: "/turn", body: {id, pid: 4321, at, prompt: "fix the build", typed: false}},
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
        {method: "POST", route: "/turn", body: {id, at: 1, model: "claude-opus-5"}},
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
  const kinds = (requests: unknown[]) => requests.map(request => (request as {body: {note?: string; typed: boolean; midTurn?: boolean}}).body).map(({note, typed, midTurn}) => [note, typed, midTurn])

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

  const toolUse = (event: string, tool: string, input: object, extra: object = {}) => JSON.stringify({session_id: id, hook_event_name: event, cwd: "/p/app", prompt_id: "p1", effort: {level: "high"}, tool_name: tool, tool_input: input, tool_use_id: `toolu_${tool}`, ...extra})
  const steps = (requests: unknown[]) => requests.map(request => (request as {body: {step?: unknown}}).body.step)

  it("send a step at the start of each tool use of the main conversation, with its text from the tool's input and paths relative to the session's folder", async () => {
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
    await started("TodoWrite", {todos: [{content: "Test", status: "pending"}]})
    await started("NotebookEdit", {description: " \n", notebook_path: "/p/app/a.ipynb", url: "https://example.com/a"})
    await started("Task", {description: `Review‮ ${"x".repeat(300)}`})
    expect(requests[0]).toEqual({method: "POST", route: "/turn", body: {id, pid: 4321, at: 1, effort: "high", step: {id: "toolu_Bash", text: "💻 Run the tests"}}})
    expect(steps(requests)).toEqual([
      {id: "toolu_Bash", text: "💻 Run the tests"},
      {id: "toolu_Bash", text: "💻 git status"},
      {id: "toolu_Read", text: "📖 Read src/hook.ts"},
      {id: "toolu_Edit", text: "✏️ Edit /etc/hosts"},
      {id: "toolu_Write", text: "📝 Write rfcs/2026-09-11-proof-of-work.md"},
      {id: "toolu_Read", text: "📖 Read /p/app-old/src/hook.ts"},
      {id: "toolu_WebFetch", text: "🌐 Fetch https://spec.torproject.org/hspow-spec/common-protocol.html"},
      {id: "toolu_WebSearch", text: '🔎 Search "hs_pow.c" tor source'},
      {id: "toolu_Agent", text: "🤖 Agent: Review pass 6 usage stats"},
      {id: "toolu_ToolSearch", text: "🔧 ToolSearch: select:Monitor"},
      {id: "toolu_Glob", text: "🔍 Glob **/*.ts in src"},
      {id: "toolu_Grep", text: "🔍 Grep endSteps in src/hub.ts"},
      {id: "toolu_Grep", text: "🔍 Grep TODO"},
      {id: "toolu_TodoWrite", text: "🔧 TodoWrite"},
      {id: "toolu_NotebookEdit", text: "🔧 NotebookEdit: https://example.com/a"},
      {id: "toolu_Task", text: `🔧 Task: Review  ${"x".repeat(199 - [..."🔧 Task: Review  "].length)}…`},
    ])
  })

  it("send a step at the end of each tool use with its outcome and duration, or in background for a launch, and nothing for a tool use of a subagent or of this plugin, or without its ID or tool", async () => {
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
    await turn(toolUse("PreToolUse", "Bash", bash, {agent_id: "af945fc18f2e2d0d9", agent_type: "general-purpose"}), {}, client, 0, 1)
    await ended("PostToolUse", {agent_id: "af945fc18f2e2d0d9", duration_ms: 13})
    await turn(toolUse("PreToolUse", "mcp__plugin_simplex_simplex__send", {chat: "@1", text: "hi"}), {}, client, 0, 1)
    await ended("PostToolUse", {duration_ms: 120}, "mcp__plugin_simplex_simplex__send")
    await turn(JSON.stringify({session_id: id, hook_event_name: "PreToolUse", tool_input: bash, tool_use_id: "toolu_1"}), {}, client, 0, 1)
    await turn(JSON.stringify({session_id: id, hook_event_name: "PostToolUse", tool_name: "Bash", tool_input: bash, duration_ms: 5}), {}, client, 0, 1)
    expect(steps(requests)).toEqual([
      {id: "toolu_Bash", text: "💻 Run the tests ✅ 2.0 s", end: true},
      {id: "toolu_Bash", text: "💻 Run the tests ❌ 39 ms", end: true},
      {id: "toolu_Bash", text: "💻 Run the tests ⏹️ 4 min 12 s", end: true},
      {id: "toolu_Bash", text: "💻 Run the tests ✅ 42 s", end: true},
      {id: "toolu_Bash", text: "💻 Run the tests ✅ 1 h 5 min", end: true},
      {id: "toolu_Bash", text: "💻 Run the tests ✅", end: true},
      {id: "toolu_Bash", text: "💻 Run the tests ✅ 42 ms", end: true},
      {id: "toolu_Bash", text: "💻 Run the tests ✅ 2.0 s", end: true},
      {id: "toolu_Bash", text: "💻 Run the tests (in background)", end: true},
      {id: "toolu_Agent", text: "🤖 Agent: Review (in background)", end: true},
      {id: "toolu_Bash", text: "💻 Run the tests ❌ 12 ms", end: true},
    ])
    expect(ensured()).toBe(11)
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
