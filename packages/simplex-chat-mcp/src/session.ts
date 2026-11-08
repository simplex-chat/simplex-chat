import {Readable, Writable} from "stream"
import {CallToolResult, JsonSchemaType, McpServer, fromJsonSchema} from "@modelcontextprotocol/server"
import {StdioServerTransport} from "@modelcontextprotocol/server/stdio"
import {DaemonClient, ProtocolError, SpawnDaemon, spawnDaemon} from "./client"
import {Config, ConfigError, defaultConfig, interactive, readConfig} from "./config"
import {dataRoot, isSessionId} from "./files"
import {ExitError, errorText, logError} from "./log"
import {ToolCall} from "./messages"
import {AttachLine, CallLine, PermissionRequest, SessionRequest, unattendedStatus} from "./protocol"
import {SocketError, isNotRunning} from "./socket"
import {ToolName, instructions, registerTools, replyTool} from "./tools"
import {version} from "./version"

const daemonStopped = "The SimpleX daemon stopped; repeat the call."
const unattendedRun = "This Claude Code run is not interactive, so it has no SimpleX group; the SimpleX tools work in interactive sessions."
const unknownSessionStatus = 409
const stoppingStatus = 503
const callAttempts = 2
const firstRetry = 1_000
const lastRetry = 30_000
const channelInstructions = "Messages in the simplex channel are prompts of the members of the SimpleX group of this session: from is the member's name, item the message ID, and file the path of a file they sent. Your final answer of each turn reaches them in that group; reply sends them a file."
const permissionFields = ["request_id", "tool_name", "description", "input_preview"] as const
const permissionRequest = fromJsonSchema({type: "object", properties: Object.fromEntries(permissionFields.map(field => [field, {type: "string"}])), required: [...permissionFields]} as JsonSchemaType)

class DaemonStoppedError extends Error {
  constructor() {
    super(daemonStopped)
  }
}

export interface SessionIo {
  stdin: Readable
  stdout: Writable
  spawnDaemon: SpawnDaemon
  version: string
}

const defaultIo: SessionIo = {stdin: process.stdin, stdout: process.stdout, spawnDaemon: root => spawnDaemon(root), version}

export async function startSession(env: NodeJS.ProcessEnv = process.env, io: SessionIo = defaultIo): Promise<void> {
  const id = env.CLAUDE_CODE_SESSION_ID
  if (!id) throw new ExitError("simplex-mcp session runs from the Claude Code plugin.", 2)
  if (!isSessionId(id)) throw new ExitError(`Not a Claude Code session ID: ${id}`, 2)
  let config: Config = defaultConfig
  let configError = interactive(env) ? undefined : unattendedRun
  try {
    config = readConfig(env)
  } catch (error) {
    if (!(error instanceof ConfigError)) throw error
    configError ??= error.message
  }
  const server = new McpServer({name: "simplex-mcp", version: io.version}, {instructions: `${instructions("Claude", "SimpleX daemon")}\n${channelInstructions}`, capabilities: {experimental: {"claude/channel": {}, "claude/channel/permission": {}}}})
  const link = new DaemonLink(DaemonClient.forRoot(dataRoot(env)), {id, pid: process.ppid, project: env.CLAUDE_PROJECT_DIR || process.cwd(), config}, io, line => {
    const notification = channelNotification(line as AttachLine)
    if (notification !== undefined) server.server.notification(notification).catch(error => logError("SimpleX channel", error))
  })
  registerTools(server, config.approve, async (name, input, call) => (configError === undefined ? link.call(name, input, call) : toolError(configError)), [replyTool])
  server.server.setNotificationHandler("notifications/claude/channel/permission_request", {params: permissionRequest}, params => {
    if (configError !== undefined) return
    const {request_id, tool_name, description, input_preview} = params as {[field in (typeof permissionFields)[number]]: string}
    link.permission({id, requestId: request_id, toolName: tool_name, description, inputPreview: input_preview}).catch(error => logError("permission relay", error))
  })
  const transport = new StdioServerTransport(io.stdin, io.stdout)
  transport.onclose = () => link.close()
  await server.connect(transport)
  if (configError === undefined) link.start()
}

class DaemonLink {
  private connecting?: Promise<void>
  private attach?: AbortController
  private closed = false
  private refused = false
  private retry = firstRetry
  private retryTimer?: NodeJS.Timeout

  constructor(private readonly client: DaemonClient, private readonly request: SessionRequest, private readonly io: SessionIo, private readonly onLine: (line: unknown) => void) {}

  connect(): Promise<void> {
    if (this.refused) return Promise.reject(new Error(unattendedRun))
    this.connecting ??= this.open().catch(error => {
      this.connecting = undefined
      throw error
    })
    return this.connecting
  }

  start(): void {
    this.connect().then(
      () => (this.retry = firstRetry),
      error => {
        if (this.closed || this.refused) return
        logError("SimpleX daemon", error)
        if (!(error instanceof ProtocolError)) this.reconnect()
      },
    )
  }

  close(): void {
    this.closed = true
    clearTimeout(this.retryTimer)
    this.attach?.abort()
  }

  private reconnect(): void {
    if (this.closed) return
    clearTimeout(this.retryTimer)
    this.retryTimer = setTimeout(() => {
      this.retry = Math.min(this.retry * 2, lastRetry)
      this.start()
    }, this.retry).unref()
  }

  async permission(request: PermissionRequest): Promise<void> {
    if (this.refused) return
    await this.connect()
    await this.client.request("POST", "/permission", request)
  }

  async call(name: ToolName, input: unknown, call: ToolCall): Promise<CallToolResult | undefined> {
    for (let attempt = 1; ; attempt++) {
      try {
        return await this.forward(name, input, call)
      } catch (error) {
        if (call.signal.aborted) return undefined
        if (attempt < callAttempts && error instanceof SocketError && error.status === unknownSessionStatus) continue
        return toolError(this.refused ? unattendedRun : failureText(error))
      }
    }
  }

  private async forward(name: ToolName, input: unknown, call: ToolCall): Promise<CallToolResult | undefined> {
    const connection = this.connect()
    await connection
    let result: CallToolResult | null | undefined
    try {
      await this.client.stream("/call", {id: this.request.id, tool: name, input}, line => {
        const parsed = line as CallLine
        if ("progress" in parsed) call.progress(parsed.progress)
        else result = parsed.result
      }, call.signal)
    } catch (error) {
      if (!call.signal.aborted) this.disconnect(connection)
      throw error instanceof SocketError ? error : new DaemonStoppedError()
    }
    if (result === undefined) {
      this.disconnect(connection)
      throw new DaemonStoppedError()
    }
    return result ?? undefined
  }

  private disconnect(connection: Promise<void>): void {
    if (this.connecting !== connection) return
    this.connecting = undefined
    this.attach?.abort()
    this.attach = undefined
  }

  private async open(): Promise<void> {
    await this.client.ensureDaemon(this.io.version, this.io.spawnDaemon)
    await this.client.request("POST", "/session", this.request).catch(error => {
      if (error instanceof SocketError && error.status === unattendedStatus) this.refused = true
      throw error
    })
    if (this.closed) return
    const controller = new AbortController()
    this.attach?.abort()
    this.attach = controller
    await new Promise<void>((resolve, reject) => {
      this.client
        .stream("/attach", {id: this.request.id}, line => {
          resolve()
          this.onLine(line)
        }, controller.signal)
        .then(() => reject(new DaemonStoppedError()), reject)
        .finally(() => {
          if (this.attach === controller) {
            this.attach = undefined
            this.connecting = undefined
          }
          this.reconnect()
        })
    })
  }
}

function failureText(error: unknown): string {
  if (isNotRunning(error) || (error instanceof SocketError && error.status === stoppingStatus)) return daemonStopped
  return errorText(error)
}

function toolError(text: string): CallToolResult {
  return {content: [{type: "text", text}], isError: true}
}

function channelNotification({prompt, permission}: AttachLine): {method: string; params: {[key: string]: unknown}} | undefined {
  if (prompt !== undefined) return {method: "notifications/claude/channel", params: {content: prompt.text, meta: {from: prompt.from, item: String(prompt.item), ...(prompt.file === undefined ? {} : {file: prompt.file})}}}
  if (permission !== undefined) return {method: "notifications/claude/channel/permission", params: {request_id: permission.requestId, behavior: permission.behavior}}
  return undefined
}
