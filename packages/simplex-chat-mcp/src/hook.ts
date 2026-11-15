import {open, readFile} from "fs/promises"
import {isAbsolute, relative} from "path"
import {ServiceClient} from "./client"
import {interactive, interactiveEntrypoint, readConfig} from "./config"
import {dataRoot} from "./files"
import {errorText, logError, oneLine} from "./log"
import {duration} from "./messages"
import {Step, TeammateEvent, TitleReply, TurnRequest} from "./protocol"
import {qrCodeText} from "./qr"
import {claudePid} from "./service-request"
import {SocketError, isNotRunning} from "./socket"
import {version} from "./version"

const minNodeMajor = 22
const serviceStopped = "The SimpleX service stopped; it starts again with the next tool call."
const unknownSession = 409
const turnAttempts = 15
const turnRetryMs = 1_000
// spec: spec/api.md#plugin-commands
const maxText = 250_000
// spec: spec/api.md#plugin-options
const answerReads = 20
// spec: spec/api.md#plugin-options
const promptReads = 600
// spec: spec/api.md#plugin-options
const compactionReads = 120
// spec: spec/api.md#plugin-options
const compactionReadFactor = 10
// spec: spec/api.md#plugin-options
const compactionAge = 60_000
// spec: spec/api.md#plugin-options
const compactionSkew = 5_000
// spec: spec/api.md#plugin-options
const transcriptReadMs = 100
const transcriptTailBytes = 1_048_576
const usageTailBytes = [4, 16, 64].map(mebibytes => mebibytes * 1_048_576)
// spec: spec/api.md#plugin-options
const compactionTailBytes = 4 * transcriptTailBytes
// spec: spec/api.md#plugin-options
const boundaryMarker = '"compact_boundary"'
// spec: spec/api.md#plugin-options
const tokenCount = new Intl.NumberFormat("en", {notation: "compact"})
const syntheticModel = "<synthetic>"
const maxNote = 200
const statusEmoji = new Map([["completed", "✅"], ["failed", "❌"], ["stopped", "⏹️"], ["killed", "⏹️"]])
const teammatePrompt = /^Another Claude session sent a message[^:\n]*:\n<teammate-message /
const teammateMessage = /<teammate-message teammate_id="([^"]*)"([^>]*)>\n([\s\S]*?)\n<\/teammate-message>/g
const pluginTools = "mcp__plugin_simplex_simplex__"
const leadName = "team-lead"
const stepFields = ["description", "query", "url", "file_path", "path", "pattern", "command"]

// spec: spec/api.md#plugin-options
interface SessionStartInput {
  session_id: string
  session_title?: string
  cwd: string
  model?: string
  source?: string
}

// spec: spec/api.md#plugin-options
interface TurnInput {
  session_id: string
  hook_event_name?: string
  transcript_path?: string
  prompt_id?: string
  prompt?: string
  last_assistant_message?: string
  to_model?: string
  effort?: {level?: string}
  cwd?: string
  agent_id?: string
  agent_type?: string
  agent_transcript_path?: string
  tool_name?: string
  tool_input?: {[field: string]: unknown}
  tool_response?: {success?: unknown} | null
  tool_use_id?: string
  duration_ms?: number
  is_interrupt?: boolean
}

// spec: spec/api.md#plugin-options
interface Origin {
  kind?: unknown
  name?: unknown
  from?: unknown
  body?: unknown
  handback?: unknown
  handbackReportText?: unknown
  senderTaskId?: unknown
}

// spec: spec/api.md#plugin-options
interface TranscriptEntry {
  type?: unknown
  subtype?: unknown
  timestamp?: unknown
  compactMetadata?: {trigger?: unknown; preTokens?: unknown; postTokens?: unknown}
  isSidechain?: unknown
  promptId?: unknown
  origin?: Origin | null
  turnOrigin?: unknown
  operation?: unknown
  content?: unknown
  attachment?: {type?: unknown; prompt?: unknown; origin?: Origin | null; commandMode?: unknown}
  message?: {id?: unknown; model?: unknown; content?: unknown; usage?: Usage}
}

// spec: spec/api.md#plugin-options
interface Usage {
  input_tokens?: unknown
  cache_creation_input_tokens?: unknown
  cache_read_input_tokens?: unknown
  output_tokens?: unknown
}

interface Answer {
  text: string
  model?: string
  entry: TranscriptEntry
}

interface AnswerUsage {
  at: number
  usage: Usage
}

interface PromptKind {
  typed: boolean
  note?: string
  midTurn?: boolean
  teammates?: TeammateEvent[]
}

export interface HookOutput {
  systemMessage?: string
}

export interface HookDeps {
  client: Pick<ServiceClient, "ensureService" | "request">
  nodeVersion: string
}

// spec: spec/architecture.md#hook-sessionstart
export async function sessionStart(stdin: string, env: NodeJS.ProcessEnv = process.env, deps?: HookDeps, at = performance.timeOrigin): Promise<HookOutput> {
  if (!interactive(env)) {
    if (interactiveEntrypoint(env)) await markUnattended(stdin, env, deps?.client)
    return {}
  }
  const nodeVersion = deps?.nodeVersion ?? process.versions.node
  if (Number(nodeVersion.split(".")[0]) < minNodeMajor) return {systemMessage: `SimpleX MCP needs Node.js ${minNodeMajor} or later; found ${nodeVersion}.`}
  try {
    const input = JSON.parse(stdin) as SessionStartInput
    const config = readConfig(env)
    const client = deps?.client ?? ServiceClient.forRoot(dataRoot(env))
    await client.ensureService(version)
    const {mainGroup, owner} = await client.request<TitleReply>("POST", "/title", {id: input.session_id, pid: claudePid(env), at, name: input.session_title, project: input.cwd, model: input.source === "compact" ? undefined : input.model, config})
    const {link, owners} = owner
    if (owners.length > 0 || link === undefined) return {}
    return {systemMessage: `SimpleX MCP: scan this QR code with the SimpleX app and tap Join to supervise your agents in "${mainGroup}":\n${qrCodeText(link, "plain")}\n${link}`}
  } catch (error) {
    return {systemMessage: `SimpleX MCP: ${isNotRunning(error) ? serviceStopped : errorText(error)}`}
  }
}

// spec: spec/architecture.md#hook-markunattended
async function markUnattended(stdin: string, env: NodeJS.ProcessEnv, client: HookDeps["client"] = ServiceClient.forRoot(dataRoot(env))): Promise<void> {
  const pid = claudePid(env)
  if (pid === undefined) return
  try {
    const input = JSON.parse(stdin) as SessionStartInput
    await client.ensureService(version)
    await client.request("POST", "/title", {id: input.session_id, pid, project: input.cwd, config: readConfig(env), unattended: true})
  } catch (error) {
    logError("SimpleX MCP session start", error)
  }
}

// spec: spec/architecture.md#hook-turn
export async function turn(stdin: string, env: NodeJS.ProcessEnv = process.env, client: HookDeps["client"] = ServiceClient.forRoot(dataRoot(env)), retryMs = turnRetryMs, at = performance.timeOrigin, readMs = transcriptReadMs): Promise<HookOutput> {
  if (!interactive(env)) return {}
  try {
    const input = JSON.parse(stdin) as TurnInput
    const event = await eventFields(input, at, readMs)
    if (event === undefined) return {}
    const lead = input.agent_id === undefined
    const request = {id: input.session_id, pid: claudePid(env), at, prompt: cut(input.prompt), answer: lead ? cut(input.last_assistant_message) : undefined, model: input.to_model, switched: input.to_model === undefined ? undefined : true, effort: lead ? input.effort?.level : undefined, ...event}
    if (event.sessionEnd === true) return await endSession(client, request)
    await client.ensureService(version)
    for (let attempt = 1; ; attempt++) {
      try {
        await client.request("POST", "/turn", request)
        break
      } catch (error) {
        if (!(error instanceof SocketError && error.status === unknownSession) || attempt === turnAttempts) throw error
        await new Promise(resolve => setTimeout(resolve, retryMs))
      }
    }
  } catch (error) {
    logError("SimpleX MCP turn", error)
  }
  return {}
}

// spec: spec/architecture.md#hook-turn
async function endSession(client: HookDeps["client"], request: Partial<TurnRequest>): Promise<HookOutput> {
  await client.request("POST", "/turn", request).catch(error => {
    if (!isNotRunning(error) && !(error instanceof SocketError && error.status === unknownSession)) throw error
  })
  return {}
}

// spec: spec/architecture.md#hook-turn
async function eventFields(input: TurnInput, at: number, readMs: number): Promise<Partial<TurnRequest> | undefined> {
  switch (input.hook_event_name) {
    case "Stop": {
      const {model, usage} = await answerRecord(input, readMs)
      return {model: input.to_model ?? model, usage}
    }
    case "UserPromptSubmit":
      return promptKind(input, readMs)
    case "PreToolUse":
    case "PostToolUse":
    case "PostToolUseFailure": {
      const step = stepOf(input)
      const teammate = agentCall(input) ?? (await agentPost(input))
      if (step === undefined && teammate === undefined) return undefined
      return {...(step === undefined ? {} : {step}), ...(teammate === undefined ? {} : {teammates: [teammate]})}
    }
    case "SubagentStop": {
      const name = await endedAgent(input, readMs)
      return name === undefined ? undefined : {teammates: [{name, kind: "end"}]}
    }
    case "SessionEnd":
      return {sessionEnd: true}
    case "PostCompact": {
      const note = await compactionNote(input, at, readMs)
      return note === undefined ? undefined : {note}
    }
    default:
      return {}
  }
}

// spec: spec/architecture.md#hook-answermodel
async function answerRecord({transcript_path: path, prompt_id: id, last_assistant_message: message}: TurnInput, readMs: number): Promise<{model?: string; usage?: string}> {
  const answer = message?.trim()
  if (!path || !answer) return {}
  const last = await findInTranscript(path, answer, answerReads, readMs, tail => {
    const text = tail.toString()
    const entry = lastAnswer(text)
    return entry?.text === answer ? {...entry, tail: text} : undefined
  })
  if (last?.model === undefined) return {}
  const {usage} = last.entry.message ?? {}
  const answeredAt = timeOf(last.entry)
  const measured = id !== undefined && usage !== undefined && !Number.isNaN(answeredAt)
  return {model: last.model, usage: measured ? await turnUsage(path, id, {at: answeredAt, usage}, last.tail) : undefined}
}

// spec: spec/architecture.md#hook-turnusage
async function turnUsage(path: string, promptId: string, answer: AnswerUsage, tail: string): Promise<string | undefined> {
  let usage = usageOf(tail, promptId, answer)
  for (const bytes of usageTailBytes) {
    if (usage !== undefined) return usage
    usage = usageOf((await readTail(path, bytes).catch(() => Buffer.alloc(0))).toString(), promptId, answer)
  }
  return usage
}

// spec: spec/architecture.md#hook-turnusage
function usageOf(tail: string, promptId: string, {at, usage}: AnswerUsage): string | undefined {
  const lines = tail.split("\n")
  const first = lines.findIndex(line => line.includes(promptId) && startsTurn(transcriptEntry(line), promptId))
  const ms = first === -1 ? NaN : at - timeOf(transcriptEntry(lines[first]))
  if (Number.isNaN(ms)) return undefined
  const output = new Map<unknown, number>()
  for (const line of lines.slice(first + 1)) {
    const entry = line.includes('"type":"assistant"') ? transcriptEntry(line) : undefined
    if (entry?.type === "assistant" && entry.isSidechain !== true && entry.message?.model !== syntheticModel) output.set(entry.message?.id, count(entry.message?.usage?.output_tokens))
  }
  const written = [...output.values()].reduce((sum, tokens) => sum + tokens, 0)
  const context = count(usage.input_tokens) + count(usage.cache_creation_input_tokens) + count(usage.cache_read_input_tokens)
  return `${duration(ms)} · ${tokenCount.format(written)} output tokens · ${tokenCount.format(context)} tokens in context`
}

// spec: spec/architecture.md#hook-turnusage
function startsTurn(entry: TranscriptEntry | undefined, promptId: string): boolean {
  return entry !== undefined && isPrompt(entry) && entry.promptId === promptId
}

// spec: spec/architecture.md#hook-turnusage
function timeOf(entry: TranscriptEntry | undefined): number {
  return typeof entry?.timestamp === "string" ? Date.parse(entry.timestamp) : NaN
}

// spec: spec/architecture.md#hook-turnusage
function count(value: unknown): number {
  return typeof value === "number" ? value : 0
}

// spec: spec/architecture.md#hook-answermodel
async function promptKind({transcript_path: path, prompt_id: id, prompt}: TurnInput, readMs: number): Promise<PromptKind> {
  const text = prompt?.trim()
  if (!path || !text) return {typed: false}
  return (await findInTranscript(path, text, promptReads, readMs, (tail, last) => promptRecord(tail.toString(), id, text, last))) ?? {typed: false}
}

// spec: spec/architecture.md#hook-compactionnote
async function compactionNote({transcript_path: path, agent_id: agent}: TurnInput, at: number, readMs: number): Promise<string | undefined> {
  if (!path || agent !== undefined) return undefined
  const boundary = await findInTranscript(path, "", compactionReads, compactionReadFactor * readMs, tail => (tail.includes(boundaryMarker) ? recentBoundary(tail.toString(), at) : undefined), compactionTailBytes)
  if (boundary === undefined) return undefined
  const {trigger, preTokens, postTokens} = boundary.compactMetadata ?? {}
  const counts = typeof preTokens === "number" && typeof postTokens === "number" ? `: ${tokenCount.format(preTokens)} → ${tokenCount.format(postTokens)} tokens` : ""
  return `⚙️ Context summarised ${trigger === "manual" ? "on request" : "automatically"}${counts}`
}

// spec: spec/architecture.md#hook-compactionnote
function recentBoundary(tail: string, at: number): TranscriptEntry | undefined {
  const boundaries = tail.split("\n").flatMap(line => (line.includes(boundaryMarker) ? (transcriptEntry(line) ?? []) : []))
  const created = (entry: TranscriptEntry): number => Date.parse(String(entry.timestamp))
  return boundaries.filter(entry => entry.type === "system" && entry.subtype === "compact_boundary" && entry.isSidechain !== true && created(entry) >= at - compactionAge && created(entry) <= at + compactionSkew).at(-1)
}

// spec: spec/architecture.md#hook-answermodel
async function findInTranscript<T>(path: string, text: string, reads: number, readMs: number, find: (tail: Buffer, last: boolean) => T | undefined, tailBytes = transcriptTailBytes): Promise<T | undefined> {
  const bytes = tailBytes + 2 * Buffer.byteLength(text)
  for (let read = 1; ; read++) {
    const tail = await readTail(path, bytes).catch(() => Buffer.alloc(0))
    const found = find(tail, read === reads)
    if (found !== undefined || read === reads) return found
    await new Promise(resolve => setTimeout(resolve, readMs))
  }
}

// spec: spec/architecture.md#hook-answermodel
async function readTail(path: string, bytes: number): Promise<Buffer> {
  const file = await open(path, "r")
  try {
    const {size} = await file.stat()
    const length = Math.min(size, bytes)
    const buffer = Buffer.alloc(length)
    const {bytesRead} = await file.read(buffer, 0, length, size - length)
    return buffer.subarray(0, bytesRead)
  } finally {
    await file.close()
  }
}

// spec: spec/architecture.md#hook-answermodel
function lastAnswer(tail: string): Answer | undefined {
  const lines = tail.split("\n")
  for (let i = lines.length - 1; i >= 0; i--) {
    const entry = transcriptEntry(lines[i])
    if (entry?.type !== "assistant" || entry.isSidechain === true) continue
    const {model, content} = entry.message ?? {}
    return {text: textOf(content), model: typeof model === "string" && model !== syntheticModel ? model : undefined, entry}
  }
  return undefined
}

// spec: spec/architecture.md#hook-answermodel
function promptRecord(tail: string, id: string | undefined, text: string, last: boolean): PromptKind | undefined {
  const entries = tail.split("\n").flatMap(line => transcriptEntry(line) ?? [])
  const first = id === undefined ? -1 : entries.findIndex(entry => entry.type === "user" && entry.isSidechain !== true && entry.promptId === id)
  if (first === -1 && id !== undefined && !last) return undefined
  const after = entries.slice(Math.max(first, 0))
  const records = after.flatMap((entry, i) => (recordText(entry) === text ? [i] : []))
  const queuedAfter = (start: number): boolean => after.slice(start).some(entry => entry.type === "queue-operation" && entry.operation === "enqueue" && textOf(entry.content) === text)
  const record = records.at(-1)
  if (record !== undefined && !queuedAfter(record + 1)) return recordKind(after[record], id, text)
  if (!last) return undefined
  if (record !== undefined) {
    const kind = recordKind(after[record], id, text)
    return kind.typed ? {...kind, midTurn: true} : kind
  }
  if (queuedAfter(0)) return {typed: true, midTurn: true}
  return first === -1 ? undefined : {typed: after.some(entry => isPrompt(entry) && entry.promptId === id && entry.origin?.kind === "human")}
}

// spec: spec/architecture.md#hook-answermodel
function isPrompt(entry: TranscriptEntry): boolean {
  const content = entry.message?.content
  const toolResult = Array.isArray(content) && content.some((part: {type?: unknown} | null) => part?.type === "tool_result")
  return entry.type === "user" && entry.isSidechain !== true && !toolResult
}

// spec: spec/architecture.md#hook-answermodel
function recordText(entry: TranscriptEntry): string | undefined {
  if (isPrompt(entry)) return textOf(entry.message?.content)
  if (entry.type === "attachment" && entry.isSidechain !== true && entry.attachment?.type === "queued_command") return textOf(entry.attachment.prompt)
  return undefined
}

// spec: spec/architecture.md#hook-answermodel
function recordKind(record: TranscriptEntry, id: string | undefined, text: string): PromptKind {
  const origin = record.type === "attachment" ? (record.attachment?.origin ?? (record.attachment?.commandMode === "task-notification" ? {kind: "task-notification"} : undefined)) : record.origin
  const teammates = teammateEvents(origin, record.turnOrigin, text)
  const note = teammates === undefined ? noteOf(origin, record.turnOrigin, text) : undefined
  const midTurn = record.type !== "user" || (id !== undefined && record.promptId !== id)
  return {typed: origin?.kind === "human", ...(note === undefined ? {} : {note: small(shortened(note))}), ...(teammates === undefined ? {} : {teammates}), ...(midTurn ? {midTurn} : {})}
}

// spec: spec/architecture.md#hook-teammateevents
function teammateEvents(origin: Origin | null | undefined, turnOrigin: unknown, text: string): TeammateEvent[] | undefined {
  if (origin?.kind === "peer" && origin.handback === true) {
    const name = stringOf(origin.name) ?? stringOf(origin.from)
    if (name === undefined) return undefined
    const report = stringOf(origin.handbackReportText) ?? stringOf(origin.body) ?? text
    const agent = stringOf(origin.senderTaskId)
    return [{name, kind: "report", ...(agent === undefined ? {} : {id: reportId(agent)}), copy: true, text: report, note: reportNote(name, report)}]
  }
  if (origin?.kind !== undefined || turnOrigin === "scheduled" || !teammatePrompt.test(text)) return undefined
  const events = [...text.matchAll(teammateMessage)].flatMap(([, sender, attributes, body]): TeammateEvent[] => {
    if (idleNotification(body)) return []
    const name = decoded(sender)
    const summary = /\ssummary="([^"]*)"/.exec(attributes)?.[1]
    return [{name, kind: "message", copy: true, text: body, note: messageNote(name, summary === undefined ? body : decoded(summary))}]
  })
  return events.length === 0 ? undefined : events
}

// spec: spec/architecture.md#hook-teammateevents
function messageNote(name: string, summary: string): string {
  return small(shortened(`✉️ Message from ${oneLine(name)}: ${firstLine(summary)}`))
}

// spec: spec/architecture.md#hook-teammateevents
function reportNote(name: string, report: string): string {
  return small(shortened(`📋 Report from subagent ${oneLine(name)}: ${firstLine(report)}`))
}

// spec: spec/architecture.md#hook-teammateevents
function reportId(agent: string): string {
  return `report ${agent}`
}

// spec: spec/architecture.md#hook-noteof
function noteOf(origin: Origin | null | undefined, turnOrigin: unknown, text: string): string | undefined {
  switch (origin?.kind) {
    case "human":
    case "channel":
      return undefined
    case "task-notification":
      return taskNote(text)
    case "peer":
      return origin.handback === true
        ? `📋 Report from subagent ${peerName(origin)}: ${firstLine(stringOf(origin.handbackReportText) ?? stringOf(origin.body) ?? text)}`
        : `✉️ Message from ${peerName(origin)}: ${firstLine(stringOf(origin.body) ?? text)}`
    case "auto-continuation":
      return `🔄 Automatic prompt: ${firstLine(text)}`
    case undefined:
      return turnOrigin === "scheduled" ? `⏰ Scheduled prompt: ${firstLine(text)}` : undefined
    default:
      return typeof origin?.kind === "string" ? `⚙️ Claude Code: ${firstLine(text)}` : undefined
  }
}

// spec: spec/architecture.md#hook-noteof
function taskNote(text: string): string {
  const block = /<task-notification>([\s\S]*?)<\/task-notification>/.exec(text)?.[1]
  if (block === undefined) return `🔔 ${firstLine(text)}`
  const field = (name: string): string => firstLine(decoded(new RegExp(`<${name}>([\\s\\S]*?)</${name}>`).exec(block)?.[1] ?? ""))
  const summary = field("summary") || "Task notification"
  const event = field("event")
  const emoji = statusEmoji.get(field("status")) ?? (event === "" ? "🔔" : "📡")
  return `${emoji} ${summary}${event === "" ? "" : `: ${event}`}`
}

// spec: spec/architecture.md#hook-teammateevents
function idleNotification(body: string): boolean {
  try {
    return (JSON.parse(body) as {type?: unknown} | null)?.type === "idle_notification"
  } catch {
    return false
  }
}

// spec: spec/architecture.md#hook-noteof
function peerName(origin: Origin): string {
  return oneLine(stringOf(origin.name) ?? stringOf(origin.from) ?? "another session")
}

// spec: spec/architecture.md#hook-stepof
function stepOf({tool_use_id: id, tool_name: tool, agent_id: agent, hook_event_name: event, tool_input: fields = {}, cwd, is_interrupt: interrupted, duration_ms: ms}: TurnInput): Step | undefined {
  if (id === undefined || tool === undefined || agent !== undefined || tool.startsWith(pluginTools)) return undefined
  const text = shortened(stepText(tool, fields, cwd))
  if (event === "PreToolUse") return {id, text: small(text)}
  if (event === "PostToolUse" && fields.run_in_background === true) return {id, text: small(`${text} (in background)`), end: true}
  const outcome = event === "PostToolUse" ? "" : interrupted === true ? " ⏹️" : " ❌"
  return {id, text: small(`${text}${outcome}${ms === undefined ? "" : ` ${duration(ms)}`}`), end: true}
}

// spec: spec/architecture.md#hook-agentcall
function agentCall({tool_name: tool, agent_id: agent, hook_event_name: event, tool_input: fields = {}}: TurnInput): TeammateEvent | undefined {
  const name = stringOf(fields.name)?.trim()
  if (tool !== "Agent" || agent !== undefined || !name) return undefined
  if (event === "PostToolUseFailure") return {name, kind: "end"}
  if (event !== "PreToolUse") return undefined
  const description = stringOf(fields.description)?.trim()
  return {name, kind: "start", ...(description ? {description} : {})}
}

// spec: spec/architecture.md#hook-agentcall
async function agentPost({agent_id: agent, agent_type: type, hook_event_name: event, tool_name: tool, tool_input: fields = {}, tool_response: response, tool_use_id: toolUse, transcript_path: path}: TurnInput): Promise<TeammateEvent | undefined> {
  if (agent === undefined || event !== "PostToolUse" || response?.success !== true) return undefined
  if (tool !== "SubagentHandback" && (tool !== "SendMessage" || fields.to !== leadName)) return undefined
  const name = (await agentName(path, agent)) ?? stringOf(type)
  if (!name) return undefined
  if (fields.type === "shutdown_response") return fields.approve === true ? {name, kind: "end"} : undefined
  const text = stringOf(fields.message)
  if (text === undefined || text.trim() === "") return undefined
  if (tool === "SubagentHandback") return {name, kind: "report", id: reportId(agent), text, note: reportNote(name, text)}
  return {name, kind: "message", ...(toolUse === undefined ? {} : {id: `message ${toolUse}`}), text, note: messageNote(name, stringOf(fields.summary) ?? text)}
}

// spec: spec/architecture.md#hook-agentcall
async function agentName(path: string | undefined, agent: string): Promise<string | undefined> {
  if (path === undefined || !path.endsWith(".jsonl") || !/^[\w-]+$/.test(agent)) return undefined
  const meta = `${path.slice(0, -".jsonl".length)}/subagents/agent-${agent}.meta.json`
  const record = await readFile(meta, "utf8").then(text => JSON.parse(text) as {name?: unknown} | null, () => undefined)
  return stringOf(record?.name)
}

// spec: spec/architecture.md#hook-agentcall
async function endedAgent({agent_transcript_path: path}: TurnInput, readMs: number): Promise<string | undefined> {
  if (path === undefined || !path.endsWith(".jsonl")) return undefined
  const meta = `${path.slice(0, -".jsonl".length)}.meta.json`
  for (let read = 1; ; read++) {
    const record = await readFile(meta, "utf8").then(text => JSON.parse(text) as {name?: unknown; finished?: unknown} | null, () => undefined)
    if (record?.finished === true) return stringOf(record.name)
    if (read >= answerReads) return undefined
    await new Promise(resolve => setTimeout(resolve, readMs))
  }
}

// spec: spec/architecture.md#hook-stepof
function small(text: string): string {
  return `!- ${text.replaceAll("!", "ǃ")}!`
}

// spec: spec/architecture.md#hook-stepof
function stepText(tool: string, fields: {[field: string]: unknown}, cwd: string | undefined): string {
  const field = (name: string): string => firstLine(stringOf(fields[name]) ?? "")
  const path = (name: string): string => displayPath(field(name), cwd)
  switch (tool) {
    case "Bash":
      return `💻 ${field("description") || field("command")}`
    case "Read":
      return `📖 Read ${path("file_path")}`
    case "Edit":
      return `✏️ Edit ${path("file_path")}`
    case "Write":
      return `✏️ Write ${path("file_path")}`
    case "NotebookEdit":
      return `✏️ Edit ${path("notebook_path")}`
    case "Grep":
    case "Glob": {
      const where = path("path")
      return `🔍 ${tool} ${field("pattern")}${where === "" ? "" : ` in ${where}`}`
    }
    case "WebFetch":
      return `🌐 Fetch ${field("url")}`
    case "WebSearch":
      return `🔍 Search ${field("query")}`
    case "Agent": {
      const description = field("description")
      return description === "" ? "🤖 Agent" : `🤖 Agent: ${description}`
    }
    default: {
      const value = stepFields.map(field).find(text => text !== "")
      const icon = /search/i.test(tool.split("__").at(-1) ?? "") ? "🔍" : "🔧"
      return `${icon} ${oneLine(tool)}${value === undefined ? "" : `: ${value}`}`
    }
  }
}

// spec: spec/architecture.md#hook-stepof
function displayPath(path: string, cwd: string | undefined): string {
  if (cwd === undefined || path === "") return path
  const inside = relative(cwd, path)
  return isAbsolute(inside) || inside.startsWith("..") ? path : inside
}

// spec: spec/architecture.md#hook-noteof
function firstLine(text: string): string {
  return text.split("\n").map(line => oneLine(line).trim()).find(line => line !== "") ?? ""
}

// spec: spec/architecture.md#hook-noteof
function decoded(text: string): string {
  return text.replace(/&lt;/g, "<").replace(/&gt;/g, ">").replace(/&quot;/g, '"').replace(/&apos;/g, "'").replace(/&amp;/g, "&")
}

// spec: spec/architecture.md#hook-noteof
function stringOf(value: unknown): string | undefined {
  return typeof value === "string" ? value : undefined
}

// spec: spec/architecture.md#hook-noteof
function shortened(note: string): string {
  const chars = [...note]
  return chars.length <= maxNote ? note : `${chars.slice(0, maxNote - 1).join("")}…`
}

// spec: spec/api.md#plugin-options
function transcriptEntry(line: string): TranscriptEntry | undefined {
  if (!line.startsWith("{")) return undefined
  try {
    return JSON.parse(line)
  } catch {
    return undefined
  }
}

// spec: spec/api.md#plugin-options
function textOf(content: unknown): string {
  if (typeof content === "string") return content.trim()
  if (!Array.isArray(content)) return ""
  return content.flatMap((part: {type?: unknown; text?: unknown} | null) => (part?.type === "text" && typeof part.text === "string" ? [part.text] : [])).join("\n").trim()
}

// spec: spec/api.md#plugin-commands
function cut(text: string | undefined): string | undefined {
  return text === undefined || text.length <= maxText ? text : `${text.slice(0, maxText).replace(/[\ud800-\udbff]$/, "")}…`
}
