import {dataRoot} from "./paths"
import {claudePid, postToService} from "./service-request"

const requestWaitMs = 1_000

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

export async function statusLine(stdin: string, write: (line: string) => void, env: NodeJS.ProcessEnv = process.env, at = performance.timeOrigin): Promise<void> {
  const {session_id: id, model, effort, context_window: context, rate_limits: windows} = inputOf(stdin)
  const level = typeof effort?.level === "string" ? effort.level : undefined
  const used = context?.used_percentage
  const limits = {fiveHour: limitOf(windows?.five_hour), week: limitOf(windows?.seven_day)}
  const limited = limits.fiveHour !== undefined || limits.week !== undefined
  write([typeof model?.display_name === "string" ? model.display_name : undefined, level === undefined ? undefined : `effort ${level}`, typeof used === "number" ? `${Math.floor(used)}% context` : undefined, ...limitParts(limits)].filter(part => part !== undefined).join(" · "))
  if (typeof id === "string" && (level !== undefined || limited)) await postToService(dataRoot(env), "/turn", {id, pid: claudePid(env), at, effort: level, limits: limited ? limits : undefined}, requestWaitMs)
}

export function limitParts({fiveHour, week}: {fiveHour?: Usage; week?: Usage}): string[] {
  return [fiveHour === undefined ? [] : [`5 h: ${Math.floor(fiveHour.percent)}%`], week === undefined ? [] : [`week: ${Math.floor(week.percent)}%`]].flat()
}

export function runStatusLine(): void {
  let stdin = ""
  process.stdin.setEncoding("utf8")
  process.stdin.on("data", chunk => (stdin += chunk))
  process.stdin.on("end", () => void statusLine(stdin, line => process.stdout.write(`${line}\n`)))
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
