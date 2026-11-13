import {ChildProcess, spawn} from "child_process"
import {closeSync} from "fs"
import {join} from "path"
import {Readable} from "stream"
import {createDataFolder, logFile, openLog, serviceToken, socketPath} from "./files"
import {ExitError} from "./log"
import {StatusReply, protocolVersion} from "./protocol"
import {isNotRunning, requestJson, streamLines} from "./socket"

const serviceStartTimeout = 5 * 60_000
const readyFd = 3
const errorPrefix = "error: "
const serviceVariables = /^(PATH|PATHEXT|HOME|USERPROFILE|HOMEDRIVE|HOMEPATH|TMPDIR|TMP|TEMP|XDG_CACHE_HOME|LOCALAPPDATA|APPDATA|SYSTEMROOT|SYSTEMDRIVE|WINDIR|COMSPEC|LANG|LC_\w+|LD_LIBRARY_PATH|DYLD_LIBRARY_PATH|HTTPS?_PROXY|NO_PROXY|NODE_USE_ENV_PROXY|NODE_EXTRA_CA_CERTS|NODE_OPTIONS|SSL_CERT_(FILE|DIR)|SIMPLEX_\w+|CLAUDE_PLUGIN_OPTION_\w+)$/i
const launcher = [
  `const service = require("child_process").spawn(process.argv[1], process.argv.slice(2), {detached: true, windowsHide: true, stdio: ["ignore", 1, 2, ${readyFd}]})`,
  `service.on("error", error => require("fs").writeSync(${readyFd}, "${errorPrefix}" + error.message + "\\n"))`,
  "service.unref()",
].join("\n")

export type SpawnService = (root: string) => Promise<void>

export class ProtocolError extends ExitError {}

export class ServiceClient {
  private constructor(readonly root: string, private readonly token: string, private readonly path: string) {}

  // spec: spec/architecture.md#client-serviceclient-forroot
  static forRoot(root: string): ServiceClient {
    createDataFolder(root)
    return new ServiceClient(root, serviceToken(root), socketPath(root))
  }

  request<T>(method: "GET" | "POST", route: string, body?: unknown): Promise<T> {
    return requestJson<T>(this.path, this.token, method, route, body)
  }

  stream(route: string, body: unknown, onLine: (line: unknown) => void, signal?: AbortSignal): Promise<void> {
    return streamLines(this.path, this.token, route, body, onLine, signal)
  }

  // spec: spec/architecture.md#client-serviceclient-ensureservice
  async ensureService(version: string, spawnIt: SpawnService = root => spawnService(root)): Promise<StatusReply> {
    let status = await this.status()
    if (status !== undefined && compareVersions(status.version, version) < 0) {
      await this.stopService()
      status = undefined
    }
    if (status === undefined) {
      await spawnIt(this.root)
      status = await this.request<StatusReply>("GET", "/status")
    }
    if (status.protocol !== protocolVersion) throw new ProtocolError("Restart Claude Code to update the SimpleX plugin.", 1)
    return status
  }

  private async stopService(): Promise<void> {
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

export function serviceCommand(): [string, string[]] {
  return [process.execPath, [join(__dirname, "cli.js"), "service"]]
}

// spec: spec/architecture.md#client-spawnservice
export async function spawnService(root: string, [command, args]: [string, string[]] = serviceCommand()): Promise<void> {
  const log = openLog(root)
  try {
    const child = spawn(process.execPath, ["-e", launcher, command, ...args], {
      detached: true,
      windowsHide: true,
      stdio: ["ignore", log, log, "pipe"],
      env: {...Object.fromEntries(Object.entries(process.env).filter(([name]) => serviceVariables.test(name))), SIMPLEX_MCP_HOME: root, SIMPLEX_MCP_READY_FD: String(readyFd)},
    })
    const channel = child.stdio[readyFd] as Readable
    try {
      const line = await firstLine(channel, child)
      if (line === "ready" || line === "running") return
      throw new ExitError(line.startsWith(errorPrefix) ? line.slice(errorPrefix.length) : `The SimpleX service exited; see ${logFile(root)}`, 1)
    } finally {
      channel.destroy()
      child.unref()
    }
  } finally {
    closeSync(log)
  }
}

// spec: spec/architecture.md#client-firstline
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
    const timer = setTimeout(() => finish(() => reject(new ExitError("The SimpleX service did not start in 5 minutes.", 1))), serviceStartTimeout)
    stream.setEncoding("utf8")
    stream.on("data", onData)
    stream.on("end", onEnd)
    stream.on("error", onEnd)
    child.once("error", onSpawnError)
  })
}

// spec: spec/architecture.md#client-compareversions
export function compareVersions(a: string, b: string): number {
  const parts = (version: string): number[] => version.split(/[.-]/).slice(0, 3).map(Number)
  const [x, y] = [parts(a), parts(b)]
  for (let i = 0; i < 3; i++) if (x[i] !== y[i]) return x[i] - y[i]
  return 0
}
