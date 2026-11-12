import {open} from "fs/promises"
import {ServiceClient} from "./client"
import {interactive, interactiveEntrypoint, readConfig} from "./config"
import {dataRoot} from "./files"
import {errorText, logError} from "./log"
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
const transcriptReads = 20
const transcriptReadMs = 100
const transcriptTailBytes = 1_048_576
const syntheticModel = "<synthetic>"

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
  prompt?: string
  last_assistant_message?: string
  to_model?: string
  effort?: {level?: string}
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

export async function turn(stdin: string, env: NodeJS.ProcessEnv = process.env, client: HookDeps["client"] = ServiceClient.forRoot(dataRoot(env)), retryMs = turnRetryMs, at = performance.timeOrigin): Promise<HookOutput> {
  if (!interactive(env)) return {}
  try {
    const input = JSON.parse(stdin) as TurnInput
    const model = input.to_model ?? (input.hook_event_name === "Stop" ? await answerModel(input) : undefined)
    const request = {id: input.session_id, pid: claudePid(env), at, prompt: cut(input.prompt), answer: cut(input.last_assistant_message), model, effort: input.effort?.level}
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

async function answerModel({transcript_path: path, last_assistant_message: message}: TurnInput): Promise<string | undefined> {
  const answer = message?.trim()
  if (!path || !answer) return undefined
  const bytes = transcriptTailBytes + 2 * Buffer.byteLength(answer)
  for (let read = 1; ; read++) {
    const tail = await readTail(path, bytes).catch(() => undefined)
    if (tail === undefined) return undefined
    const last = lastAnswer(tail)
    if (last?.text === answer) return last.model
    if (read === transcriptReads) return undefined
    await new Promise(resolve => setTimeout(resolve, transcriptReadMs))
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

function transcriptEntry(line: string): {type?: unknown; isSidechain?: unknown; message?: {model?: unknown; content?: unknown}} | undefined {
  if (!line.startsWith("{")) return undefined
  try {
    return JSON.parse(line)
  } catch {
    return undefined
  }
}

function textOf(content: unknown): string {
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
