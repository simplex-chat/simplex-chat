import {createHash, randomBytes} from "crypto"
import {chmodSync, existsSync, fchmodSync, mkdirSync, linkSync, openSync, readFileSync, readdirSync, renameSync, rmSync, statSync, truncateSync, unlinkSync, writeFileSync} from "fs"
import {homedir, tmpdir} from "os"
import {join, resolve} from "path"
import {ExitError, logError} from "./log"

const sessionIdPattern = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i
const maxSocketPath = 103
const maxLogBytes = 10 * 1_048_576

// spec: spec/architecture.md#files-sessionfile
export interface SessionFile {
  number: number
  name: string
  project: string
  created: string
  group?: number
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

// spec: spec/architecture.md#files-dataroot
export function dataRoot(env: NodeJS.ProcessEnv = process.env, home: string = homedir()): string {
  const given = env.SIMPLEX_MCP_HOME
  return given ? resolve(given) : join(home, ".simplex-mcp")
}

// spec: spec/architecture.md#files-dataroot
export function hubFolder(root: string): string {
  return join(root, "_hub")
}

export function isSessionId(id: string): boolean {
  return sessionIdPattern.test(id)
}

// spec: spec/architecture.md#files-dataroot
export function sessionFolder(root: string, id: string): string {
  if (!isSessionId(id)) throw new ExitError(`Not a Claude Code session ID: ${id}`, 2)
  return join(sessionsFolder(root), id.toLowerCase())
}

// spec: spec/architecture.md#files-sessionfile
export function readSessionFile(folder: string): SessionFile | undefined {
  const file = join(folder, "session.json")
  try {
    return existsSync(file) ? JSON.parse(readFileSync(file, "utf8")) : undefined
  } catch (error) {
    logError(file, error)
    return undefined
  }
}

// spec: spec/architecture.md#files-sessionfile
export function writeSessionFile(folder: string, session: SessionFile): void {
  const file = join(folder, "session.json")
  writePrivateFile(`${file}.tmp`, `${JSON.stringify(session)}\n`)
  renameSync(`${file}.tmp`, file)
}

// spec: spec/architecture.md#files-sessionfile
export function storedSessions(root: string): {id: string; session: SessionFile}[] {
  const folder = sessionsFolder(root)
  if (!existsSync(folder)) return []
  return readdirSync(folder).flatMap(id => {
    const session = isSessionId(id) ? readSessionFile(join(folder, id)) : undefined
    return session === undefined ? [] : [{id, session}]
  })
}

// spec: spec/architecture.md#files-socketpath
export function socketPath(root: string, platform: NodeJS.Platform = process.platform): string {
  const hash = createHash("sha256").update(root).digest("hex").slice(0, 16)
  if (platform === "win32") return `\\\\.\\pipe\\simplex-mcp-${hash}`
  const path = join(root, "service.sock")
  return Buffer.byteLength(path) <= maxSocketPath ? path : join(tmpdir(), `simplex-mcp-${hash}.sock`)
}

// spec: spec/architecture.md#files-logfile
export function logFile(root: string): string {
  return join(root, "service.log")
}

// spec: spec/architecture.md#files-servicelock
export const serviceLock = "service.lock"

// spec: spec/architecture.md#files-servicelock
export function serviceToken(root: string): string {
  return serverToken(root, false, "service.token")
}

// spec: spec/architecture.md#files-logfile
export function trimLog(root: string): void {
  const file = logFile(root)
  if ((statSync(file, {throwIfNoEntry: false})?.size ?? 0) > maxLogBytes) truncateSync(file)
}

// spec: spec/architecture.md#files-logfile
export function openLog(root: string): number {
  trimLog(root)
  const fd = openSync(logFile(root), "a", 0o600)
  if (process.platform !== "win32") fchmodSync(fd, 0o600)
  return fd
}

// spec: spec/architecture.md#files-createdatafolder
export function createDataFolder(dir: string): void {
  mkdirSync(dir, {recursive: true, mode: 0o700})
  if (process.platform !== "win32") chmodSync(dir, 0o700)
}

// spec: spec/architecture.md#files-acquirelock
export function acquireLock(dir: string, user = "server", name = "lock"): void {
  const file = join(dir, name)
  if (createLock(file)) return
  const pid = lockPid(file)
  if (pid !== undefined && pid !== process.pid && isRunning(pid)) throw lockedError(dir, file, pid, user)
  if (!takeOver(file, pid)) throw lockedError(dir, file, pid, user)
}

// spec: spec/architecture.md#files-acquirelock
export function releaseLock(dir: string, name = "lock"): void {
  try {
    unlinkSync(join(dir, name))
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error
  }
}

// spec: spec/architecture.md#files-servertoken
export function serverToken(dir: string, renew = false, name = "token"): string {
  const file = join(dir, name)
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

// spec: spec/architecture.md#files-writeheadersfile
export function writeHeadersFile(dir: string, token: string): void {
  writePrivateFile(headersFile(dir), `${JSON.stringify({Authorization: `Bearer ${token}`})}\n`)
}

// spec: spec/architecture.md#files-writeserverfile
export function writeServerFile(dir: string, server: ServerFile): void {
  writePrivateFile(join(dir, "server.json"), `${JSON.stringify(server)}\n`)
}

// spec: spec/architecture.md#files-writeserverfile
export function readServerFile(dir: string): ServerFile | undefined {
  const file = join(dir, "server.json")
  return existsSync(file) ? JSON.parse(readFileSync(file, "utf8")) : undefined
}

// spec: spec/architecture.md#files-saveheldtitles
export function saveHeldTitles(root: string, titles: object[]): void {
  writePrivateFile(join(root, "titles.json"), `${JSON.stringify(titles)}\n`)
}

// spec: spec/architecture.md#files-saveheldtitles
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

// spec: spec/api.md#exit-codes
function lockedError(dir: string, file: string, pid: number | undefined, user: string): ExitError {
  return new LockedError(`Another ${user} uses ${dir} (PID ${pid}). If no ${user} runs, remove ${file}.`, 1)
}

// spec: spec/architecture.md#files-createlock
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

// spec: spec/architecture.md#files-createlock
function createExclusive(file: string, text: string): boolean {
  try {
    writeFileSync(file, text, {flag: "wx", mode: 0o600})
    return true
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === "EEXIST") return false
    throw error
  }
}

// spec: spec/architecture.md#files-takeover
function takeOver(file: string, pid: number | undefined): boolean {
  const claim = `${file}.${pid}.claim`
  try {
    linkSync(file, claim)
  } catch (error) {
    const {code} = error as NodeJS.ErrnoException
    if (code === "EEXIST" || code === "ENOENT" || !createExclusive(claim, "")) return false
  }
  try {
    if (!existsSync(file) || lockPid(file) !== pid) return false
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
