import {createHash, randomBytes} from "crypto"
import {chmodSync, existsSync, mkdirSync, openSync, readFileSync, readdirSync, statSync, truncateSync, unlinkSync, writeFileSync} from "fs"
import {homedir, tmpdir} from "os"
import {join, resolve} from "path"
import {ExitError} from "./log"

const sessionIdPattern = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i
// sockaddr_un.sun_path holds 104 bytes on macOS (108 on Linux), including the terminating NUL
const maxSocketPath = 103
const maxLogBytes = 10 * 1_048_576

export interface SessionFile {
  number: number
  name: string
  project: string
  created: string
}

export class LockedError extends ExitError {}

export function dataRoot(env: NodeJS.ProcessEnv = process.env, home: string = homedir()): string {
  const given = env.SIMPLEX_MCP_HOME
  return given ? resolve(given) : join(home, ".simplex-mcp")
}

export function hubFolder(root: string): string {
  return join(root, "_hub")
}

export function isSessionId(id: string): boolean {
  return sessionIdPattern.test(id)
}

export function sessionFolder(root: string, id: string): string {
  if (!isSessionId(id)) throw new ExitError(`Not a Claude Code session ID: ${id}`, 2)
  return join(sessionsFolder(root), id.toLowerCase())
}

export function readSessionFile(folder: string): SessionFile | undefined {
  const file = join(folder, "session.json")
  return existsSync(file) ? JSON.parse(readFileSync(file, "utf8")) : undefined
}

export function writeSessionFile(folder: string, session: SessionFile): void {
  writePrivateFile(join(folder, "session.json"), `${JSON.stringify(session)}\n`)
}

export function sessionNumbers(root: string): number[] {
  const folder = sessionsFolder(root)
  if (!existsSync(folder)) return []
  return readdirSync(folder).flatMap(name => {
    const session = isSessionId(name) ? readSessionFile(join(folder, name)) : undefined
    return session === undefined ? [] : [session.number]
  })
}

export function socketPath(root: string, platform: NodeJS.Platform = process.platform): string {
  const hash = createHash("sha256").update(root).digest("hex").slice(0, 16)
  if (platform === "win32") return `\\\\.\\pipe\\simplex-mcp-${hash}`
  const path = join(root, "daemon.sock")
  return Buffer.byteLength(path) <= maxSocketPath ? path : join(tmpdir(), `simplex-mcp-${hash}.sock`)
}

export function logFile(root: string): string {
  return join(root, "daemon.log")
}

export function openLog(root: string): number {
  const file = logFile(root)
  if (existsSync(file) && statSync(file).size > maxLogBytes) truncateSync(file)
  return openSync(file, "a", 0o600)
}

export function createDataFolder(dir: string): void {
  mkdirSync(dir, {recursive: true, mode: 0o700})
  if (process.platform !== "win32") chmodSync(dir, 0o700)
}

export function acquireLock(dir: string): void {
  const file = join(dir, "lock")
  if (createLock(file)) return
  const pid = lockPid(file)
  if (pid !== undefined && pid !== process.pid && isRunning(pid)) throw lockedError(dir, file, pid)
  unlinkSync(file)
  if (!createLock(file)) throw lockedError(dir, file, lockPid(file))
}

export function releaseLock(dir: string): void {
  unlinkSync(join(dir, "lock"))
}

export function serverToken(dir: string): string {
  const file = join(dir, "token")
  const token = randomBytes(32).toString("base64url")
  try {
    writeFileSync(file, `${token}\n`, {flag: "wx", mode: 0o600})
    return token
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "EEXIST") throw error
    if (process.platform !== "win32") chmodSync(file, 0o600)
    return readFileSync(file, "utf8").trim()
  }
}

function sessionsFolder(root: string): string {
  return join(root, "_sessions")
}

function writePrivateFile(file: string, text: string): void {
  writeFileSync(file, text, {mode: 0o600})
  if (process.platform !== "win32") chmodSync(file, 0o600)
}

function lockedError(dir: string, file: string, pid: number | undefined): ExitError {
  return new LockedError(`Another daemon uses ${dir} (PID ${pid}). If none runs, remove ${file}.`, 1)
}

function createLock(file: string): boolean {
  try {
    writeFileSync(file, `${process.pid}\n`, {flag: "wx", mode: 0o600})
    return true
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === "EEXIST") return false
    throw error
  }
}

function lockPid(file: string): number | undefined {
  const pid = Number(readFileSync(file, "utf8").trim())
  return Number.isInteger(pid) && pid > 0 ? pid : undefined
}

function isRunning(pid: number): boolean {
  try {
    process.kill(pid, 0)
    return true
  } catch (error) {
    return (error as NodeJS.ErrnoException).code === "EPERM"
  }
}
