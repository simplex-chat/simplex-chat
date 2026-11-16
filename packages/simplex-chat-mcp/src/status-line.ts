import {statfsSync} from "fs"
import {totalmem} from "os"
import {dirname} from "path"
import {dataRoot} from "./paths"
import {claudePid, postToService} from "./service-request"

const requestWaitMs = 1_000
// spec: spec/architecture.md#status-line-statusline
const lowMemoryShare = 0.05
// spec: spec/architecture.md#status-line-statusline
const lowDiskBytes = 5_000_000_000
// spec: spec/architecture.md#status-line-statusline
const lowMark = "❗️"
const sizeUnits: [string, number][] = [
  ["TB", 1e12],
  ["GB", 1e9],
  ["MB", 1e6],
]

// spec: spec/architecture.md#status-line-statusline
export interface Machine {
  memoryFree: number
  memoryTotal: number
  diskFree?: number
}

interface Window {
  used_percentage?: unknown
  resets_at?: unknown
}

interface StatusLineInput {
  session_id?: unknown
  model?: {display_name?: unknown}
  effort?: {level?: unknown}
  context_window?: {used_percentage?: unknown}
  rate_limits?: {five_hour?: Window; seven_day?: Window}
}

interface Usage {
  percent: number
}

// spec: spec/architecture.md#status-line-statusline
export async function statusLine(stdin: string, write: (line: string) => void, env: NodeJS.ProcessEnv = process.env, at = performance.timeOrigin, machine: (root: string) => Machine | undefined = machineState): Promise<void> {
  const {session_id: id, model, effort, context_window: context, rate_limits: windows} = inputOf(stdin)
  const level = typeof effort?.level === "string" ? effort.level : undefined
  const used = context?.used_percentage
  const limits = {fiveHour: limitOf(windows?.five_hour), week: limitOf(windows?.seven_day)}
  const limited = limits.fiveHour !== undefined || limits.week !== undefined
  const parts = [typeof model?.display_name === "string" ? model.display_name : undefined, level === undefined ? undefined : `effort ${level}`, typeof used === "number" ? `${Math.floor(used)}% context` : undefined, ...limitParts(limits), ...machineParts(machine(dataRoot(env)))]
  write(parts.filter(part => part !== undefined).join(" · "))
  if (typeof id === "string" && (level !== undefined || limited)) await postToService(dataRoot(env), "/turn", {id, pid: claudePid(env), at, effort: level, limits: limited ? limits : undefined, statusLine: true}, requestWaitMs)
}

// spec: spec/architecture.md#status-line-statusline
export function limitParts({fiveHour, week}: {fiveHour?: Usage; week?: Usage}): string[] {
  return [fiveHour === undefined ? [] : [`5 h: ${Math.floor(fiveHour.percent)}%`], week === undefined ? [] : [`week: ${Math.floor(week.percent)}%`]].flat()
}

// spec: spec/architecture.md#status-line-statusline
export function machineState(root: string): Machine {
  const constrained = process.constrainedMemory()
  return {memoryFree: process.availableMemory(), memoryTotal: constrained > 0 ? Math.min(constrained, totalmem()) : totalmem(), diskFree: diskFree(root)}
}

// spec: spec/architecture.md#status-line-statusline
export function machineParts(machine: Machine | undefined): string[] {
  if (machine === undefined) return []
  const {memoryFree, memoryTotal, diskFree} = machine
  const memory = `RAM ${size(memoryFree)} free${memoryFree < memoryTotal * lowMemoryShare ? lowMark : ""}`
  return diskFree === undefined ? [memory] : [memory, `disk ${size(diskFree)} free${diskFree < lowDiskBytes ? lowMark : ""}`]
}

// spec: spec/architecture.md#status-line-statusline
export function runStatusLine(): void {
  let stdin = ""
  process.stdin.setEncoding("utf8")
  process.stdin.on("data", chunk => (stdin += chunk))
  process.stdin.on("end", () => void statusLine(stdin, line => process.stdout.write(`${line}\n`)))
}

function diskFree(path: string): number | undefined {
  for (let folder = path; ; folder = dirname(folder)) {
    try {
      const {bavail, bsize} = statfsSync(folder)
      return bavail * bsize
    } catch {
      if (dirname(folder) === folder) return undefined
    }
  }
}

function size(bytes: number): string {
  const [unit, scale] = sizeUnits.find(([, scale]) => bytes >= scale) ?? sizeUnits[sizeUnits.length - 1]
  const value = bytes / scale
  return `${value >= 10 ? Math.floor(value) : (Math.floor(value * 10) / 10).toFixed(1)} ${unit}`
}

function limitOf(window: Window | undefined): {percent: number; resetsAt: number} | undefined {
  const {used_percentage: percent, resets_at: resetsAt} = window ?? {}
  return typeof percent === "number" && typeof resetsAt === "number" ? {percent, resetsAt} : undefined
}

function inputOf(stdin: string): StatusLineInput {
  try {
    return (JSON.parse(stdin) as StatusLineInput | null) ?? {}
  } catch {
    return {}
  }
}

if (require.main === module) runStatusLine()
