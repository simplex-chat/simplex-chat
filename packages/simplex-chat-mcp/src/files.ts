import {randomBytes} from "crypto"
import {chmodSync, existsSync, mkdirSync, readFileSync, unlinkSync, writeFileSync} from "fs"
import {join} from "path"
import {ExitError} from "./log"

export interface ServerFile {
  name: string
  port: number
  expireHours: number
}

export function createDataFolder(dir: string): void {
  mkdirSync(dir, {recursive: true, mode: 0o700})
  if (process.platform !== "win32") chmodSync(dir, 0o700)
}

export function acquireLock(dir: string): void {
  const file = join(dir, "lock")
  if (createLock(file)) return
  const pid = lockPid(file)
  if (pid !== undefined && pid !== process.pid && isRunning(pid)) throw new ExitError(`Another server uses ${dir} (PID ${pid}).`, 1)
  unlinkSync(file)
  if (!createLock(file)) throw new ExitError(`Another server uses ${dir} (PID ${lockPid(file)}).`, 1)
}

export function releaseLock(dir: string): void {
  unlinkSync(join(dir, "lock"))
}

export function serverToken(dir: string, renew: boolean): string {
  const file = join(dir, "token")
  if (!renew && existsSync(file)) return readFileSync(file, "utf8").trim()
  const token = randomBytes(32).toString("base64url")
  writeFileSync(file, `${token}\n`, {mode: 0o600})
  return token
}

export function headersFile(dir: string): string {
  return join(dir, "headers.json")
}

export function writeHeadersFile(dir: string, token: string): void {
  writeFileSync(headersFile(dir), `${JSON.stringify({Authorization: `Bearer ${token}`})}\n`, {mode: 0o600})
}

export function writeServerFile(dir: string, server: ServerFile): void {
  writeFileSync(join(dir, "server.json"), `${JSON.stringify(server)}\n`, {mode: 0o600})
}

export function readServerFile(dir: string): ServerFile | undefined {
  const file = join(dir, "server.json")
  return existsSync(file) ? JSON.parse(readFileSync(file, "utf8")) : undefined
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
