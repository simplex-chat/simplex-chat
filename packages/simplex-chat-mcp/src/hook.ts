import {DaemonClient} from "./client"
import {readConfig} from "./config"
import {dataRoot} from "./files"
import {errorText, logError} from "./log"
import {TitleReply} from "./protocol"
import {qrCodeText} from "./qr"
import {isNotRunning} from "./socket"
import {version} from "./version"

const minNodeMajor = 22
const daemonStopped = "The SimpleX daemon stopped; it starts again with the next tool call."

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
    const {mainGroup, owner} = await client.request<TitleReply>("POST", "/title", {id: input.session_id, name: input.session_title, project: input.cwd, model: input.model, config})
    const {link, owners} = owner
    if (owners.length > 0 || link === undefined) return {}
    return {systemMessage: `SimpleX MCP: scan this QR code with the SimpleX app and tap Join to supervise your agents in "${mainGroup}":\n${qrCodeText(link, "plain")}\n${link}`}
  } catch (error) {
    return {systemMessage: `SimpleX MCP: ${isNotRunning(error) ? daemonStopped : errorText(error)}`}
  }
}

export async function turn(stdin: string, env: NodeJS.ProcessEnv = process.env, client: Pick<DaemonClient, "request"> = DaemonClient.forRoot(dataRoot(env))): Promise<HookOutput> {
  try {
    const input = JSON.parse(stdin) as TurnInput
    await client.request("POST", "/turn", {id: input.session_id, prompt: input.prompt, answer: input.last_assistant_message, model: input.to_model, effort: input.effort?.level})
  } catch (error) {
    if (!isNotRunning(error)) logError("SimpleX MCP turn", error)
  }
  return {}
}
