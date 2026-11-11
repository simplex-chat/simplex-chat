import {log} from "./log"
import {utcMinutes} from "./messages"

const minute = 60_000
const cronRanges = [{min: 0, max: 59}, {min: 0, max: 23}, {min: 1, max: 31}, {min: 1, max: 12}, {min: 0, max: 7}]
const cronItem = /^(\*|\d+-\d+)(\/\d+)?$|^\d+$/

export type ScheduleEntry = {cron: string; prompt: string; by: string; byId: number; at: string}

type Due = {prompt: string; at: string}

export class Schedules {
  private readonly chats = new Map<string, {entry: ScheduleEntry; due?: Due}[]>()
  private readonly listeners = new Set<{chat: string; wake: () => void}>()
  private timer?: NodeJS.Timeout

  constructor(private readonly isApprover: (memberId: number) => boolean) {}

  set(chat: string, entries: ScheduleEntry[]): void {
    const current = this.chats.get(chat) ?? []
    this.chats.set(chat, entries.map(entry => ({entry, due: current.find(scheduled => scheduled.entry.at === entry.at)?.due})))
  }

  start(): void {
    this.tickAfter(Date.now())
  }

  stop(): void {
    clearTimeout(this.timer)
  }

  listen(chat: string, wake: () => void): () => void {
    const listener = {chat, wake}
    this.listeners.add(listener)
    return () => this.listeners.delete(listener)
  }

  take(chat: string): Due[] {
    const schedules = this.chats.get(chat) ?? []
    const taken = schedules.flatMap(({due}) => (due === undefined ? [] : [due]))
    for (const scheduled of schedules) scheduled.due = undefined
    return taken
  }

  pending(chat: string): number {
    return (this.chats.get(chat) ?? []).filter(({due}) => due !== undefined).length
  }

  private tickAfter(after: number): void {
    const boundary = after - (after % minute) + minute
    this.timer = setTimeout(() => {
      this.markDue(boundary)
      this.tickAfter(Math.max(Date.now(), boundary))
    }, boundary - Date.now()).unref()
  }

  private markDue(time: number): void {
    const local = new Date(time)
    for (const [chat, schedules] of this.chats) {
      for (const [i, scheduled] of schedules.entries()) {
        if (scheduled.due === undefined && this.isApprover(scheduled.entry.byId) && cronMatches(scheduled.entry.cron, local)) {
          scheduled.due = {prompt: scheduled.entry.prompt, at: utcMinutes(time)}
          log(`Scheduled prompt ${i + 1} of ${chat} is due.`)
          for (const {wake} of [...this.listeners].filter(listener => listener.chat === chat)) wake()
        }
      }
    }
  }
}

export function cronError(cron: string): string | undefined {
  const fields = cron.split(" ")
  if (fields.length !== cronRanges.length || !fields.every(field => field.split(",").every(item => cronItem.test(item)))) return "Schedule: 5 cron fields expected, such as 0 7 * * *."
  const outOfRange = fields.find((field, i) => fieldValues(field, cronRanges[i]).length === 0)
  return outOfRange === undefined ? undefined : `Schedule: cron field ${outOfRange} is out of range.`
}

function cronMatches(cron: string, time: Date): boolean {
  const fields = cron.split(" ")
  const [minutes, hours, days, months, weekdays] = fields.map((field, i) => fieldValues(field, cronRanges[i]))
  const day = days.includes(time.getDate())
  const weekday = weekdays.some(value => value % 7 === time.getDay())
  const eitherDay = !fields[2].startsWith("*") && !fields[4].startsWith("*")
  return minutes.includes(time.getMinutes()) && hours.includes(time.getHours()) && months.includes(time.getMonth() + 1) && (eitherDay ? day || weekday : day && weekday)
}

function fieldValues(field: string, {min, max}: {min: number; max: number}): number[] {
  const values: number[] = []
  for (const item of field.split(",")) {
    const [range, step = "1"] = item.split("/")
    const [first, last = first] = range === "*" ? [min, max] : range.split("-").map(Number)
    if (first < min || first > last || last > max || Number(step) === 0) return []
    for (let value = first; value <= last; value += Number(step)) values.push(value)
  }
  return values
}
