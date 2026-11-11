import {spawnSync} from "child_process"
import {existsSync, mkdirSync, readFileSync, writeFileSync} from "fs"
import {homedir} from "os"
import {basename, dirname, join, resolve} from "path"
import {headersFile, hookHeadersFile, readServerFile} from "./files"
import {ExitError, shellPath} from "./log"

const cmdNotFound = 9009
const cmdMetaChars = /([()\][%!^"`<>&|;, *?])/g
const hookEvents = ["UserPromptSubmit", "PreToolUse", "PostToolUse", "PostToolUseFailure", "PostToolBatch", "PermissionRequest", "PermissionDenied", "SubagentStart", "SubagentStop", "PreCompact", "PostCompact", "Stop", "StopFailure", "SessionEnd"]

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
  headersHelper: string
  timeout: number
}

export function register({name, data, scope}: RegisterOptions, run: Run = runCommand, projectFolder: () => string = projectRoot, configFolder: string = process.env.CLAUDE_CONFIG_DIR || join(homedir(), ".claude")): string {
  const server = readServerFile(data)
  if (server === undefined) throw new ExitError(`Agent "${name}" was never started. Start it with: npx -y @simplex-chat/mcp serve ${name}`, 1)
  const serverName = `simplex-${name}`
  const entry = JSON.stringify(registryEntry(server.port, data, server.expireHours))
  const {servers, settings: file} = scopeLocations(scope, projectFolder, configFolder)
  const removed = run("claude", ["mcp", "remove", "--scope", scope, serverName])
  if (removed.missing) throw new ExitError(manualEntryText(servers, serverName, entry), 1)
  const added = run("claude", ["mcp", "add-json", "--scope", scope, serverName, entry])
  if (added.missing) throw new ExitError(manualEntryText(servers, serverName, entry), 1)
  if (added.status !== 0) throw new ExitError(`claude mcp add-json failed: ${added.stderr.trim()}`, 1)
  const hooks = statusHooks(server.port, data)
  const settings = withStatusHooks(settingsOf(file), hooks, `@${hookHeadersFile(data)}`)
  if (settings === undefined) throw new ExitError(`Added to Claude Code: scope ${scope}, server ${serverName}. ${file} is not a valid settings file; add these status hooks to it: ${JSON.stringify({hooks})}`, 1)
  mkdirSync(dirname(file), {recursive: true})
  writeFileSync(file, `${JSON.stringify(settings, null, 2)}\n`)
  return `Added to Claude Code: scope ${scope}, server ${serverName}, status hooks in ${file}.`
}

export function registryEntry(port: number, data: string, expireHours: number): Entry {
  return {
    type: "http",
    url: `http://127.0.0.1:${port}/mcp`,
    headersHelper: `${process.platform === "win32" ? "type" : "cat"} ${shellPath(headersFile(data))}`,
    timeout: (expireHours + 24) * 3_600_000 + 600_000,
  }
}

export function statusHooks(port: number, data: string): {[event: string]: object[]} {
  const handler = {type: "command", command: "curl", args: ["-q", "--noproxy", "*", "-s", "-m", "2", "-o", process.platform === "win32" ? "NUL" : "/dev/null", "-H", `@${hookHeadersFile(data)}`, "-H", "Content-Type: application/json", "--data-binary", "@-", `http://127.0.0.1:${port}/hook`], async: true}
  return Object.fromEntries(hookEvents.map(event => [event, [{hooks: [handler]}]]))
}

function scopeLocations(scope: Scope, projectFolder: () => string, configFolder: string): {servers: string; settings: string} {
  if (scope === "user") return {servers: `"mcpServers"`, settings: join(configFolder, "settings.json")}
  const project = projectFolder()
  return {servers: `"projects" > "${project}" > "mcpServers"`, settings: join(project, ".claude", "settings.local.json")}
}

function manualEntryText(location: string, serverName: string, entry: string): string {
  return `Add this entry under ${location} in ~/.claude.json: {"${serverName}": ${entry}}`
}

function settingsOf(file: string): unknown {
  const text = existsSync(file) ? readFileSync(file, "utf8") : "{}"
  try {
    return JSON.parse(text)
  } catch {
    return undefined
  }
}

function withStatusHooks(settings: unknown, hooks: {[event: string]: object[]}, headers: string): {[key: string]: unknown} | undefined {
  if (!isObject(settings)) return undefined
  const existing = settings.hooks ?? {}
  if (!isObject(existing) || Object.keys(hooks).some(event => existing[event] !== undefined && !Array.isArray(existing[event]))) return undefined
  const merged = Object.fromEntries(Object.entries(hooks).map(([event, groups]) => [event, [...withoutHandlers(existing[event], headers), ...groups]]))
  return {...settings, hooks: {...existing, ...merged}}
}

function withoutHandlers(groups: unknown, headers: string): unknown[] {
  return (Array.isArray(groups) ? groups : []).flatMap(group => {
    if (!isObject(group) || !Array.isArray(group.hooks)) return [group]
    const handlers = group.hooks.filter(handler => !(isObject(handler) && Array.isArray(handler.args) && handler.args.includes(headers)))
    return handlers.length > 0 ? [{...group, hooks: handlers}] : []
  })
}

function isObject(value: unknown): value is {[key: string]: unknown} {
  return typeof value === "object" && value !== null && !Array.isArray(value)
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
  if (missing && process.platform === "win32") return runInCmd(command, args)
  return {missing, status: result.status, stderr: result.stderr ?? ""}
}

function runInCmd(command: string, args: string[]): RunResult {
  const result = spawnSync([command, ...args.map(cmdArgument)].join(" "), {encoding: "utf8", shell: true})
  return {missing: result.status === cmdNotFound, status: result.status, stderr: result.stderr ?? ""}
}

export function cmdArgument(arg: string): string {
  const quoted = `"${arg.replace(/(\\*)"/g, '$1$1\\"').replace(/(\\*)$/, "$1$1")}"`
  return quoted.replace(cmdMetaChars, "^$1").replace(cmdMetaChars, "^$1")
}
