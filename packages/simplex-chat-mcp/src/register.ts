import {spawnSync} from "child_process"
import {existsSync, readFileSync} from "fs"
import {basename, dirname, join, resolve} from "path"
import {readServerFile, savedToken} from "./files"
import {ExitError} from "./log"

export type Scope = "user" | "local"

export interface RegisterOptions {
  name: string
  data: string
  scope: Scope
}

export interface RunResult {
  missing: boolean
  status: number | null
  stderr: string
}

export type Run = (command: string, args: string[]) => RunResult

export interface Entry {
  type: "http"
  url: string
  headers: {Authorization: string}
  timeout: number
}

export function register({name, data, scope}: RegisterOptions, run: Run = runCommand, projectFolder: () => string = projectRoot): string {
  const server = readServerFile(data)
  const token = savedToken(data)
  if (server === undefined || token === undefined) throw new ExitError(`Agent "${name}" was never started. Start it with: npx -y @simplex-chat/mcp serve ${name}`, 1)
  const serverName = `simplex-${name}`
  const entry = JSON.stringify(registryEntry(server.port, token, server.expireHours))
  const removed = run("claude", ["mcp", "remove", "--scope", scope, serverName])
  if (removed.missing) throw new ExitError(manualEntryText(scope, serverName, entry, projectFolder), 1)
  const added = run("claude", ["mcp", "add-json", "--scope", scope, serverName, entry])
  if (added.missing) throw new ExitError(manualEntryText(scope, serverName, entry, projectFolder), 1)
  if (added.status !== 0) throw new ExitError(`claude mcp add-json failed: ${added.stderr.trim()}`, 1)
  return `Added to Claude Code: scope ${scope}, server ${serverName}.`
}

export function registryEntry(port: number, token: string, expireHours: number): Entry {
  return {
    type: "http",
    url: `http://127.0.0.1:${port}/mcp`,
    headers: {Authorization: `Bearer ${token}`},
    timeout: (expireHours + 24) * 3_600_000 + 600_000,
  }
}

function manualEntryText(scope: Scope, serverName: string, entry: string, projectFolder: () => string): string {
  const location = scope === "user" ? `"mcpServers"` : `"projects" > "${projectFolder()}" > "mcpServers"`
  return `Add this entry under ${location} in ~/.claude.json: {"${serverName}": ${entry}}`
}

export function projectRoot(folder: string = process.cwd()): string {
  const start = resolve(folder)
  const root = gitRoot(start)
  const key = root === undefined ? start : repositoryRoot(root)
  return process.platform === "win32" ? key.replaceAll("\\", "/") : key
}

function gitRoot(folder: string): string | undefined {
  if (existsSync(join(folder, ".git"))) return folder
  const parent = dirname(folder)
  return parent === folder ? undefined : gitRoot(parent)
}

function repositoryRoot(root: string): string {
  try {
    const link = /^gitdir:(.*)$/s.exec(readFileSync(join(root, ".git"), "utf8").trim())
    if (link === null) return root
    const gitDir = resolve(root, link[1].trim())
    const commonDir = resolve(gitDir, readFileSync(join(gitDir, "commondir"), "utf8").trim())
    if (dirname(gitDir) !== join(commonDir, "worktrees")) return root
    return basename(commonDir) === ".git" ? dirname(commonDir) : commonDir
  } catch {
    return root
  }
}

function runCommand(command: string, args: string[]): RunResult {
  const result = spawnSync(command, args, {encoding: "utf8"})
  const missing = (result.error as NodeJS.ErrnoException | undefined)?.code === "ENOENT"
  return {missing, status: result.status, stderr: result.stderr ?? ""}
}
