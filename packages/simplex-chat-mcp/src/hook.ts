import {open} from "fs/promises"
import {ServiceClient} from "./client"
import {interactive, interactiveEntrypoint, readConfig} from "./config"
import {dataRoot} from "./files"
import {errorText, logError, oneLine} from "./log"
import {TitleReply} from "./protocol"
import {qrCodeText} from "./qr"
import {SocketError, isNotRunning} from "./socket"
import {version} from "./version"

const minNodeMajor = 22
const serviceStopped = "The SimpleX service stopped; it starts again with the next tool call."
const unknownSession = 409
const turnAttempts = 15
const turnRetryMs = 1_000
const maxText = 250_000
const answerReads = 20
const promptReads = 600
const transcriptReadMs = 100
const transcriptTailBytes = 1_048_576
const syntheticModel = "<synthetic>"
const maxNote = 200
const statusEmoji = new Map([["completed", "✅"], ["failed", "❌"], ["stopped", "⏹️"], ["killed", "⏹️"]])
const teammatePrompt = /^Another Claude session sent a message[^:\n]*:\n<teammate-message /
const teammateMessage = /<teammate-message teammate_id="([^"]*)"([^>]*)>\n([\s\S]*?)\n<\/teammate-message>/g

interface SessionStartInput {
  session_id: string
  session_title?: string
  cwd: string
  model?: string
}

interface TurnInput {
  session_id: string
  hook_event_name?: string
  transcript_path?: string
  prompt_id?: string
  prompt?: string
  last_assistant_message?: string
  to_model?: string
  effort?: {level?: string}
}

interface Origin {
  kind?: unknown
  name?: unknown
  from?: unknown
  body?: unknown
  handback?: unknown
  handbackReportText?: unknown
}

interface TranscriptEntry {
  type?: unknown
  isSidechain?: unknown
  promptId?: unknown
  origin?: Origin | null
  turnOrigin?: unknown
  operation?: unknown
  content?: unknown
  attachment?: {type?: unknown; prompt?: unknown; origin?: Origin | null; commandMode?: unknown}
  message?: {model?: unknown; content?: unknown}
}

interface PromptKind {
  typed: boolean
  note?: string
  midTurn?: boolean
}

export interface HookOutput {
  systemMessage?: string
}

export interface HookDeps {
  client: Pick<ServiceClient, "ensureService" | "request">
  nodeVersion: string
}

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
    const {mainGroup, owner} = await client.request<TitleReply>("POST", "/title", {id: input.session_id, pid: claudePid(env), at, name: input.session_title, project: input.cwd, model: input.model, config})
    const {link, owners} = owner
    if (owners.length > 0 || link === undefined) return {}
    return {systemMessage: `SimpleX MCP: scan this QR code with the SimpleX app and tap Join to supervise your agents in "${mainGroup}":\n${qrCodeText(link, "plain")}\n${link}`}
  } catch (error) {
    return {systemMessage: `SimpleX MCP: ${isNotRunning(error) ? serviceStopped : errorText(error)}`}
  }
}

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

export async function turn(stdin: string, env: NodeJS.ProcessEnv = process.env, client: HookDeps["client"] = ServiceClient.forRoot(dataRoot(env)), retryMs = turnRetryMs, at = performance.timeOrigin, readMs = transcriptReadMs): Promise<HookOutput> {
  if (!interactive(env)) return {}
  try {
    const input = JSON.parse(stdin) as TurnInput
    const model = input.to_model ?? (input.hook_event_name === "Stop" ? await answerModel(input, readMs) : undefined)
    const kind = input.hook_event_name === "UserPromptSubmit" ? await promptKind(input, readMs) : undefined
    const request = {id: input.session_id, pid: claudePid(env), at, prompt: cut(input.prompt), ...kind, answer: cut(input.last_assistant_message), model, effort: input.effort?.level}
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

async function answerModel({transcript_path: path, last_assistant_message: message}: TurnInput, readMs: number): Promise<string | undefined> {
  const answer = message?.trim()
  if (!path || !answer) return undefined
  const last = await findInTranscript(path, answer, answerReads, readMs, tail => {
    const entry = lastAnswer(tail)
    return entry?.text === answer ? entry : undefined
  })
  return last?.model
}

async function promptKind({transcript_path: path, prompt_id: id, prompt}: TurnInput, readMs: number): Promise<PromptKind> {
  const text = prompt?.trim()
  if (!path || !text) return {typed: false}
  return (await findInTranscript(path, text, promptReads, readMs, (tail, last) => promptRecord(tail, id, text, last))) ?? {typed: false}
}

async function findInTranscript<T>(path: string, text: string, reads: number, readMs: number, find: (tail: string, last: boolean) => T | undefined): Promise<T | undefined> {
  const bytes = transcriptTailBytes + 2 * Buffer.byteLength(text)
  for (let read = 1; ; read++) {
    const tail = await readTail(path, bytes).catch(() => "")
    const found = find(tail, read === reads)
    if (found !== undefined || read === reads) return found
    await new Promise(resolve => setTimeout(resolve, readMs))
  }
}

async function readTail(path: string, bytes: number): Promise<string> {
  const file = await open(path, "r")
  try {
    const {size} = await file.stat()
    const length = Math.min(size, bytes)
    const buffer = Buffer.alloc(length)
    const {bytesRead} = await file.read(buffer, 0, length, size - length)
    return buffer.toString("utf8", 0, bytesRead)
  } finally {
    await file.close()
  }
}

function lastAnswer(tail: string): {text: string; model?: string} | undefined {
  const lines = tail.split("\n")
  for (let i = lines.length - 1; i >= 0; i--) {
    const entry = transcriptEntry(lines[i])
    if (entry?.type !== "assistant" || entry.isSidechain === true) continue
    const {model, content} = entry.message ?? {}
    return {text: textOf(content), model: typeof model === "string" && model !== syntheticModel ? model : undefined}
  }
  return undefined
}

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

function isPrompt(entry: TranscriptEntry): boolean {
  const content = entry.message?.content
  const toolResult = Array.isArray(content) && content.some((part: {type?: unknown} | null) => part?.type === "tool_result")
  return entry.type === "user" && entry.isSidechain !== true && !toolResult
}

function recordText(entry: TranscriptEntry): string | undefined {
  if (isPrompt(entry)) return textOf(entry.message?.content)
  if (entry.type === "attachment" && entry.isSidechain !== true && entry.attachment?.type === "queued_command") return textOf(entry.attachment.prompt)
  return undefined
}

function recordKind(record: TranscriptEntry, id: string | undefined, text: string): PromptKind {
  const origin = record.type === "attachment" ? (record.attachment?.origin ?? (record.attachment?.commandMode === "task-notification" ? {kind: "task-notification"} : undefined)) : record.origin
  const note = noteOf(origin, record.turnOrigin, text)
  const midTurn = record.type !== "user" || (id !== undefined && record.promptId !== id)
  return {typed: origin?.kind === "human", ...(note === undefined ? {} : {note: shortened(note)}), ...(midTurn ? {midTurn} : {})}
}

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
      return turnOrigin === "scheduled" ? `⏰ Scheduled prompt: ${firstLine(text)}` : teammateNote(text)
    default:
      return typeof origin?.kind === "string" ? `⚙️ Claude Code: ${firstLine(text)}` : undefined
  }
}

function taskNote(text: string): string {
  const block = /<task-notification>([\s\S]*?)<\/task-notification>/.exec(text)?.[1]
  if (block === undefined) return `🔔 ${firstLine(text)}`
  const field = (name: string): string => firstLine(decoded(new RegExp(`<${name}>([\\s\\S]*?)</${name}>`).exec(block)?.[1] ?? ""))
  const summary = field("summary") || "Task notification"
  const event = field("event")
  const emoji = statusEmoji.get(field("status")) ?? (event === "" ? "🔔" : "📡")
  return `${emoji} ${summary}${event === "" ? "" : `: ${event}`}`
}

function teammateNote(text: string): string | undefined {
  if (!teammatePrompt.test(text)) return undefined
  const match = [...text.matchAll(teammateMessage)].find(([, , , body]) => !idleNotification(body))
  if (match === undefined) return undefined
  const [, sender, attributes, body] = match
  const summary = /\ssummary="([^"]*)"/.exec(attributes)?.[1]
  return `✉️ Message from ${decoded(sender)}: ${firstLine(summary === undefined ? body : decoded(summary))}`
}

function idleNotification(body: string): boolean {
  try {
    return (JSON.parse(body) as {type?: unknown} | null)?.type === "idle_notification"
  } catch {
    return false
  }
}

function peerName(origin: Origin): string {
  return stringOf(origin.name) ?? stringOf(origin.from) ?? "another session"
}

function firstLine(text: string): string {
  return text.split("\n").map(line => oneLine(line).trim()).find(line => line !== "") ?? ""
}

function decoded(text: string): string {
  return text.replace(/&lt;/g, "<").replace(/&gt;/g, ">").replace(/&quot;/g, '"').replace(/&apos;/g, "'").replace(/&amp;/g, "&")
}

function stringOf(value: unknown): string | undefined {
  return typeof value === "string" ? value : undefined
}

function shortened(note: string): string {
  const chars = [...note]
  return chars.length <= maxNote ? note : `${chars.slice(0, maxNote - 1).join("")}…`
}

function transcriptEntry(line: string): TranscriptEntry | undefined {
  if (!line.startsWith("{")) return undefined
  try {
    return JSON.parse(line)
  } catch {
    return undefined
  }
}

function textOf(content: unknown): string {
  if (typeof content === "string") return content.trim()
  if (!Array.isArray(content)) return ""
  return content.flatMap((part: {type?: unknown; text?: unknown} | null) => (part?.type === "text" && typeof part.text === "string" ? [part.text] : [])).join("\n").trim()
}

function cut(text: string | undefined): string | undefined {
  return text === undefined || text.length <= maxText ? text : `${text.slice(0, maxText).replace(/[\ud800-\udbff]$/, "")}…`
}

function claudePid(env: NodeJS.ProcessEnv): number | undefined {
  const pid = Number(env.CLAUDE_PID)
  return Number.isInteger(pid) && pid > 0 ? pid : undefined
}
