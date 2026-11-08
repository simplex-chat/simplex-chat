import {DaemonClient} from "./client"
import {readConfig} from "./config"
import {dataRoot} from "./files"
import {errorText, logError} from "./log"
import {TitleReply} from "./protocol"
import {qrCodeText} from "./qr"
import {SocketError, isNotRunning} from "./socket"
import {version} from "./version"

const minNodeMajor = 22
const daemonStopped = "The SimpleX daemon stopped; it starts again with the next tool call."
const unknownSession = 409
const turnAttempts = 10
const turnRetryMs = 1_000

interface SessionStartInput {
  session_id: string
  session_title?: string
  cwd: string
  model?: string
}

interface TurnInput {
  session_id: string
  prompt?: string
  last_assistant_message?: string
  to_model?: string
  effort?: {level?: string}
}

export interface HookOutput {
  systemMessage?: string
}

export interface HookDeps {
  client: Pick<DaemonClient, "ensureDaemon" | "request">
  nodeVersion: string
}

export async function sessionStart(stdin: string, env: NodeJS.ProcessEnv = process.env, deps?: HookDeps): Promise<HookOutput> {
  const nodeVersion = deps?.nodeVersion ?? process.versions.node
  if (Number(nodeVersion.split(".")[0]) < minNodeMajor) return {systemMessage: `SimpleX MCP needs Node.js ${minNodeMajor} or later; found ${nodeVersion}.`}
  try {
    const input = JSON.parse(stdin) as SessionStartInput
    const config = readConfig(env)
    const client = deps?.client ?? DaemonClient.forRoot(dataRoot(env))
    await client.ensureDaemon(version)
    const {mainGroup, owner} = await client.request<TitleReply>("POST", "/title", {id: input.session_id, pid: claudePid(env), name: input.session_title, project: input.cwd, model: input.model, config})
    const {link, owners} = owner
    if (owners.length > 0 || link === undefined) return {}
    return {systemMessage: `SimpleX MCP: scan this QR code with the SimpleX app and tap Join to supervise your agents in "${mainGroup}":\n${qrCodeText(link, "plain")}\n${link}`}
  } catch (error) {
    return {systemMessage: `SimpleX MCP: ${isNotRunning(error) ? daemonStopped : errorText(error)}`}
  }
}

export async function turn(stdin: string, env: NodeJS.ProcessEnv = process.env, client: HookDeps["client"] = DaemonClient.forRoot(dataRoot(env)), retryMs = turnRetryMs, at = performance.timeOrigin): Promise<HookOutput> {
  try {
    const input = JSON.parse(stdin) as TurnInput
    const request = {id: input.session_id, pid: claudePid(env), at, prompt: input.prompt, answer: input.last_assistant_message, model: input.to_model, effort: input.effort?.level}
    await client.ensureDaemon(version)
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

function claudePid(env: NodeJS.ProcessEnv): number | undefined {
  const pid = Number(env.CLAUDE_PID)
  return Number.isInteger(pid) && pid > 0 ? pid : undefined
}
