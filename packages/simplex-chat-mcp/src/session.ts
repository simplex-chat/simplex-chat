import {Readable, Writable} from "stream"
import {CallToolResult, McpServer} from "@modelcontextprotocol/server"
import {StdioServerTransport} from "@modelcontextprotocol/server/stdio"
import {DaemonClient, SpawnDaemon, spawnDaemon} from "./client"
import {Config, ConfigError, defaultConfig, readConfig} from "./config"
import {dataRoot, isSessionId} from "./files"
import {ExitError, errorText, logError} from "./log"
import {ToolCall} from "./messages"
import {CallLine, SessionRequest} from "./protocol"
import {SocketError, isNotRunning} from "./socket"
import {ToolName, instructions, registerTools} from "./tools"
import {version} from "./version"

const daemonStopped = "The SimpleX daemon stopped; repeat the call."
const unknownSessionStatus = 409
const stoppingStatus = 503
const callAttempts = 2

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
  let configError: string | undefined
  try {
    config = readConfig(env)
  } catch (error) {
    if (!(error instanceof ConfigError)) throw error
    configError = error.message
  }
  const link = new DaemonLink(DaemonClient.forRoot(dataRoot(env)), {id, project: env.CLAUDE_PROJECT_DIR || process.cwd(), config}, io)
  const server = new McpServer({name: "simplex-mcp", version: io.version}, {instructions: instructions()})
  registerTools(server, config.approve, async (name, input, call) => (configError === undefined ? link.call(name, input, call) : toolError(configError)))
  const transport = new StdioServerTransport(io.stdin, io.stdout)
  transport.onclose = () => link.close()
  await server.connect(transport)
  if (configError === undefined) link.start()
}

class DaemonLink {
  private connecting?: Promise<void>
  private attach?: AbortController
  private closed = false

  constructor(private readonly client: DaemonClient, private readonly request: SessionRequest, private readonly io: SessionIo) {}

  connect(): Promise<void> {
    this.connecting ??= this.open().catch(error => {
      this.connecting = undefined
      throw error
    })
    return this.connecting
  }

  start(): void {
    this.connect().catch(error => {
      if (!this.closed) logError("SimpleX daemon", error)
    })
  }

  close(): void {
    this.closed = true
    this.attach?.abort()
  }

  async call(name: ToolName, input: unknown, call: ToolCall): Promise<CallToolResult | undefined> {
    for (let attempt = 1; ; attempt++) {
      try {
        return await this.forward(name, input, call)
      } catch (error) {
        if (call.signal.aborted) return undefined
        // the daemon refuses an unknown session before it runs the call, so repeating it is safe
        if (attempt < callAttempts && error instanceof SocketError && error.status === unknownSessionStatus) continue
        return toolError(failureText(error))
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

  // a failed call must not drop a newer connection that another call already opened
  private disconnect(connection: Promise<void>): void {
    if (this.connecting !== connection) return
    this.connecting = undefined
    this.attach?.abort()
    this.attach = undefined
  }

  private async open(): Promise<void> {
    await this.client.ensureDaemon(this.io.version, this.io.spawnDaemon)
    await this.client.request("POST", "/session", this.request)
    if (this.closed) return
    const controller = new AbortController()
    this.attach?.abort()
    this.attach = controller
    await new Promise<void>((resolve, reject) => {
      this.client
        .stream("/attach", {id: this.request.id}, () => resolve(), controller.signal)
        .then(() => reject(new DaemonStoppedError()), reject)
        .finally(() => {
          if (this.attach !== controller) return
          this.attach = undefined
          this.connecting = undefined
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
