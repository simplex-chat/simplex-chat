import {ChildProcess, spawn} from "child_process"
import {closeSync} from "fs"
import {join} from "path"
import {Readable} from "stream"
import {createDataFolder, logFile, openLog, serverToken, socketPath} from "./files"
import {ExitError} from "./log"
import {StatusReply, protocolVersion} from "./protocol"
import {isNotRunning, requestJson, streamLines} from "./socket"

const daemonStartTimeout = 5 * 60_000
const readyFd = 3
const errorPrefix = "error: "

export type SpawnDaemon = (root: string) => Promise<void>

export class DaemonClient {
  private constructor(readonly root: string, private readonly token: string, private readonly path: string) {}

  static forRoot(root: string): DaemonClient {
    createDataFolder(root)
    return new DaemonClient(root, serverToken(root), socketPath(root))
  }

  request<T>(method: "GET" | "POST", route: string, body?: unknown): Promise<T> {
    return requestJson<T>(this.path, this.token, method, route, body)
  }

  stream(route: string, body: unknown, onLine: (line: unknown) => void, signal?: AbortSignal): Promise<void> {
    return streamLines(this.path, this.token, route, body, onLine, signal)
  }

  async ensureDaemon(version: string, spawnIt: SpawnDaemon = root => spawnDaemon(root)): Promise<StatusReply> {
    let status = await this.status()
    if (status !== undefined && compareVersions(status.version, version) < 0) {
      await this.stopDaemon()
      status = undefined
    }
    if (status === undefined) {
      await spawnIt(this.root)
      status = await this.request<StatusReply>("GET", "/status")
    }
    if (status.protocol !== protocolVersion) throw new ExitError("Restart Claude Code to update the SimpleX plugin.", 1)
    return status
  }

  private async stopDaemon(): Promise<void> {
    try {
      await this.request("POST", "/stop")
    } catch (error) {
      if (!isNotRunning(error)) throw error
    }
  }

  private async status(): Promise<StatusReply | undefined> {
    try {
      return await this.request<StatusReply>("GET", "/status")
    } catch (error) {
      if (isNotRunning(error)) return undefined
      throw error
    }
  }
}

export function daemonCommand(): [string, string[]] {
  return [process.execPath, [join(__dirname, "cli.js"), "daemon"]]
}

export async function spawnDaemon(root: string, [command, args]: [string, string[]] = daemonCommand()): Promise<void> {
  const log = openLog(root)
  try {
    const child = spawn(command, args, {
      detached: true,
      windowsHide: true,
      stdio: ["ignore", log, log, "pipe"],
      env: {...process.env, SIMPLEX_MCP_HOME: root, SIMPLEX_MCP_READY_FD: String(readyFd)},
    })
    const channel = child.stdio[readyFd] as Readable
    try {
      const line = await firstLine(channel, child)
      if (line === "ready" || line === "running") return
      throw new ExitError(line.startsWith(errorPrefix) ? line.slice(errorPrefix.length) : `The SimpleX daemon exited; see ${logFile(root)}`, 1)
    } finally {
      channel.destroy()
      child.unref()
    }
  } finally {
    closeSync(log)
  }
}

function firstLine(stream: Readable, child: ChildProcess): Promise<string> {
  return new Promise((resolve, reject) => {
    let text = ""
    const finish = (settle: () => void): void => {
      clearTimeout(timer)
      stream.off("data", onData)
      stream.off("end", onEnd)
      child.off("error", onSpawnError)
      settle()
    }
    const onData = (chunk: string): void => {
      text += chunk
      const i = text.indexOf("\n")
      if (i >= 0) finish(() => resolve(text.slice(0, i)))
    }
    const onEnd = (): void => finish(() => resolve(text.trim()))
    const onSpawnError = (error: Error): void => finish(() => reject(new ExitError(error.message, 1)))
    const timer = setTimeout(() => finish(() => reject(new ExitError("The SimpleX daemon did not start in 5 minutes.", 1))), daemonStartTimeout)
    stream.setEncoding("utf8")
    stream.on("data", onData)
    stream.on("end", onEnd)
    stream.on("error", onEnd)
    child.once("error", onSpawnError)
  })
}

export function compareVersions(a: string, b: string): number {
  const parts = (version: string): number[] => version.split(/[.-]/).slice(0, 3).map(Number)
  const [x, y] = [parts(a), parts(b)]
  for (let i = 0; i < 3; i++) if (x[i] !== y[i]) return x[i] - y[i]
  return 0
}
