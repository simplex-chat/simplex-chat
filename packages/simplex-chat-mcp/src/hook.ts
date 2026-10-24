import {DaemonClient} from "./client"
import {readConfig} from "./config"
import {dataRoot} from "./files"
import {errorText} from "./log"
import {SessionReply} from "./protocol"
import {qrCodeText} from "./qr"
import {isNotRunning} from "./socket"
import {version} from "./version"

const minNodeMajor = 22
const daemonStopped = "The SimpleX daemon stopped; it starts again with the next tool call."

interface SessionStartInput {
  session_id: string
  session_title?: string
  cwd?: string
}

export interface HookOutput {
  systemMessage?: string
  hookSpecificOutput?: {hookEventName: "SessionStart"; additionalContext: string}
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
    const reply = await client.request<SessionReply>("POST", "/session", {id: input.session_id, name: input.session_title, project: env.CLAUDE_PROJECT_DIR ?? input.cwd ?? process.cwd(), config})
    const {link, owners} = reply.owner
    if (owners.length > 0 || link === undefined) return {}
    const {mainGroup} = config
    return {
      systemMessage: `SimpleX MCP: scan this QR code with the SimpleX app and tap Join to supervise your agents in "${mainGroup}":\n${qrCodeText(link, "plain")}\n${link}`,
      hookSpecificOutput: {hookEventName: "SessionStart", additionalContext: `The SimpleX Main group "${mainGroup}" has no owner yet. Tell your user to open this link in the SimpleX app and join: ${link}`},
    }
  } catch (error) {
    return {systemMessage: `SimpleX MCP: ${isNotRunning(error) ? daemonStopped : errorText(error)}`}
  }
}
