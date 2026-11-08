import {createHash, randomBytes} from "crypto"
import {chmodSync, existsSync, fchmodSync, mkdirSync, linkSync, openSync, readFileSync, readdirSync, renameSync, rmSync, statSync, truncateSync, unlinkSync, writeFileSync} from "fs"
import {homedir, tmpdir} from "os"
import {join, resolve} from "path"
import type {Mode} from "./actions"
import {ExitError, logError} from "./log"

const sessionIdPattern = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i
// sockaddr_un.sun_path holds 104 bytes on macOS (108 on Linux), including the terminating NUL
const maxSocketPath = 103
const maxLogBytes = 10 * 1_048_576

export interface SessionFile {
  number: number
  name: string
  project: string
  created: string
  group?: number
  mode?: Mode
  aliases?: string[]
  model?: string
  effort?: string
  hostDeclined?: boolean
}

export interface ServerFile {
  name: string
  port: number
  expireHours: number
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
  try {
    return existsSync(file) ? JSON.parse(readFileSync(file, "utf8")) : undefined
  } catch (error) {
    logError(file, error)
    return undefined
  }
}

export function writeSessionFile(folder: string, session: SessionFile): void {
  const file = join(folder, "session.json")
  writePrivateFile(`${file}.tmp`, `${JSON.stringify(session)}\n`)
  renameSync(`${file}.tmp`, file)
}

export function storedSessions(root: string): {id: string; session: SessionFile}[] {
  const folder = sessionsFolder(root)
  if (!existsSync(folder)) return []
  return readdirSync(folder).flatMap(id => {
    const session = isSessionId(id) ? readSessionFile(join(folder, id)) : undefined
    return session === undefined ? [] : [{id, session}]
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

export function trimLog(root: string): void {
  const file = logFile(root)
  if ((statSync(file, {throwIfNoEntry: false})?.size ?? 0) > maxLogBytes) truncateSync(file)
}

export function openLog(root: string): number {
  trimLog(root)
  const fd = openSync(logFile(root), "a", 0o600)
  if (process.platform !== "win32") fchmodSync(fd, 0o600)
  return fd
}

export function createDataFolder(dir: string): void {
  mkdirSync(dir, {recursive: true, mode: 0o700})
  if (process.platform !== "win32") chmodSync(dir, 0o700)
}

export function acquireLock(dir: string, user = "server"): void {
  const file = join(dir, "lock")
  if (createLock(file)) return
  const pid = lockPid(file)
  if (pid !== undefined && pid !== process.pid && isRunning(pid)) throw lockedError(dir, file, pid, user)
  if (!takeOver(file, pid)) throw lockedError(dir, file, pid, user)
}

export function releaseLock(dir: string): void {
  try {
    unlinkSync(join(dir, "lock"))
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error
  }
}

export function serverToken(dir: string, renew = false): string {
  const file = join(dir, "token")
  const token = randomBytes(32).toString("base64url")
  if (renew) {
    writePrivateFile(file, `${token}\n`)
    return token
  }
  if (createLock(file, `${token}\n`)) return token
  if (process.platform !== "win32" && statSync(file).uid === process.getuid?.()) chmodSync(file, 0o600)
  return readFileSync(file, "utf8").trim()
}

export function headersFile(dir: string): string {
  return join(dir, "headers.json")
}

export function writeHeadersFile(dir: string, token: string): void {
  writePrivateFile(headersFile(dir), `${JSON.stringify({Authorization: `Bearer ${token}`})}\n`)
}

export function writeServerFile(dir: string, server: ServerFile): void {
  writePrivateFile(join(dir, "server.json"), `${JSON.stringify(server)}\n`)
}

export function readServerFile(dir: string): ServerFile | undefined {
  const file = join(dir, "server.json")
  return existsSync(file) ? JSON.parse(readFileSync(file, "utf8")) : undefined
}

export function saveHeldTitles(root: string, titles: object[]): void {
  writePrivateFile(join(root, "titles.json"), `${JSON.stringify(titles)}\n`)
}

export function takeHeldTitles(root: string): unknown[] {
  const file = join(root, "titles.json")
  try {
    return existsSync(file) ? JSON.parse(readFileSync(file, "utf8")) : []
  } catch (error) {
    logError(file, error)
    return []
  } finally {
    rmSync(file, {force: true})
  }
}

function sessionsFolder(root: string): string {
  return join(root, "_sessions")
}

function writePrivateFile(file: string, text: string): void {
  writeFileSync(file, text, {mode: 0o600})
  if (process.platform !== "win32") chmodSync(file, 0o600)
}

function lockedError(dir: string, file: string, pid: number | undefined, user: string): ExitError {
  return new LockedError(`Another ${user} uses ${dir} (PID ${pid}). If no ${user} runs, remove ${file}.`, 1)
}

function createLock(file: string, text = `${process.pid}\n`): boolean {
  const temp = `${file}.${process.pid}.${randomBytes(6).toString("hex")}.tmp`
  try {
    writePrivateFile(temp, text)
    linkSync(temp, file)
    return true
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === "EEXIST") return false
    return createExclusive(file, text)
  } finally {
    rmSync(temp, {force: true})
  }
}

function createExclusive(file: string, text: string): boolean {
  try {
    writeFileSync(file, text, {flag: "wx", mode: 0o600})
    return true
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === "EEXIST") return false
    throw error
  }
}

function takeOver(file: string, pid: number | undefined): boolean {
  const claim = `${file}.${pid}.claim`
  try {
    linkSync(file, claim)
  } catch (error) {
    const {code} = error as NodeJS.ErrnoException
    if (code === "EEXIST" || code === "ENOENT") return false
  }
  try {
    if (existsSync(claim) && lockPid(claim) !== pid) return false
    unlinkSync(file)
    return createLock(file)
  } finally {
    rmSync(claim, {force: true})
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
