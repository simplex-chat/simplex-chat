export function isWeekend(timezone: string): boolean {
  const day = new Intl.DateTimeFormat("en-US", {timeZone: timezone, weekday: "short"}).format(new Date())
  return day === "Sat" || day === "Sun"
}

export function log(msg: string, ...args: unknown[]): void {
  const ts = new Date().toISOString()
  console.log(`[${ts}] ${msg}`, ...args)
}

export function logError(msg: string, err: unknown): void {
  const ts = new Date().toISOString()
  console.error(`[${ts}] ${msg}`, err)
}
