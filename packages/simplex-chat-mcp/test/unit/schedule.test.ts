import {Schedules, cronError} from "../../src/schedule"
import {clockStart, dueAt, scheduleEntry} from "./fixtures"

let logLines: string[]

beforeEach(() => {
  logLines = []
  jest.spyOn(process.stdout, "write").mockImplementation(text => (logLines.push(String(text).trimEnd()), true))
  jest.useFakeTimers({now: clockStart})
})

afterEach(() => {
  jest.restoreAllMocks()
  jest.useRealTimers()
})

function local(day: number, hour: number, minute: number): Date {
  return new Date(2026, 10, day, hour, minute)
}

function mismatches(cases: [string, Date, boolean][]): [string, Date, boolean][] {
  return cases.filter(([cron, time, due]) => {
    jest.setSystemTime(time.getTime() - 1)
    const schedules = new Schedules(() => true)
    schedules.set("#2", [scheduleEntry(cron)])
    schedules.start()
    jest.advanceTimersByTime(1)
    schedules.stop()
    return (schedules.pending("#2") === 1) !== due
  })
}

describe("cron", () => {
  it("accepts *, N, A-B, */S, A-B/S and comma lists of them within the range of each field", () => {
    for (const cron of ["0 7 * * *", "*/15 * * * *", "0-30/10 8-18 1,15,31 1-12/3 1-5", "59 23 31 12 7", "0 0 1 1 0", "5,10-20/5,*/30 07 * * *"]) expect(cronError(cron)).toBeUndefined()
  })

  it("refuses a cron that is not 5 fields of these forms", () => {
    for (const cron of ["0 7 * *", "0 7 * * * *", "0 7 * * MON", "0 7 ? * *", "5/10 * * * *", "0 7 * * 1,", "0 -7 * * *"]) expect(cronError(cron)).toBe("Schedule: 5 cron fields expected, such as 0 7 * * *.")
  })

  it("refuses a field out of its range, named as written", () => {
    const cases = [
      ["60 7 * * *", "60"],
      ["0 24 * * *", "24"],
      ["0 7 0 * *", "0"],
      ["0 7 32 * *", "32"],
      ["0 7 * 0 *", "0"],
      ["0 7 * 13 *", "13"],
      ["0 7 * * 8", "8"],
      ["0-60 * * * *", "0-60"],
      ["*/0 * * * *", "*/0"],
      ["0 7-5 * * *", "7-5"],
      ["0 7 * * 1,9", "1,9"],
    ]
    for (const [cron, field] of cases) expect(cronError(cron)).toBe(`Schedule: cron field ${field} is out of range.`)
  })
})

describe("cron matching", () => {
  it("matches the minute, hour, day of month and month of local time, with lists, ranges and steps", () => {
    expect(
      mismatches([
        ["0 7 * * *", local(11, 7, 0), true],
        ["0 7 * * *", local(11, 7, 1), false],
        ["0 7 * * *", local(11, 19, 0), false],
        ["*/15 * * * *", local(11, 7, 45), true],
        ["*/15 * * * *", local(11, 7, 50), false],
        ["10-40/15 * * * *", local(11, 7, 40), true],
        ["10-40/15 * * * *", local(11, 7, 30), false],
        ["5,10-20/5,*/30 7 * * *", local(11, 7, 15), true],
        ["5,10-20/5,*/30 7 * * *", local(11, 7, 25), false],
        ["0 7 11 11 *", local(11, 7, 0), true],
        ["0 7 11 12 *", local(11, 7, 0), false],
      ]),
    ).toEqual([])
  })

  it("takes day of week 0 and 7 as Sunday", () => {
    expect(
      mismatches([
        ["0 7 * * 0", local(15, 7, 0), true],
        ["0 7 * * 7", local(15, 7, 0), true],
        ["0 7 * * 7", local(11, 7, 0), false],
        ["0 7 * * 5-7", local(13, 7, 0), true],
        ["0 7 * * 5-7", local(15, 7, 0), true],
        ["0 7 * * 1-5", local(15, 7, 0), false],
      ]),
    ).toEqual([])
  })

  it("matches either day field when both are restricted, as vixie cron, and takes a day field starting with * as unrestricted", () => {
    expect(
      mismatches([
        ["0 7 2 * 3", local(2, 7, 0), true],
        ["0 7 2 * 3", local(11, 7, 0), true],
        ["0 7 2 * 3", local(12, 7, 0), false],
        ["0 7 2 * *", local(11, 7, 0), false],
        ["0 7 * * 3", local(2, 7, 0), false],
        ["0 7 * 11 3", local(11, 7, 0), true],
        ["0 7 */2 * 3", local(11, 7, 0), true],
        ["0 7 */2 * 3", local(4, 7, 0), false],
        ["0 7 */2 * 3", local(13, 7, 0), false],
      ]),
    ).toEqual([])
  })
})

describe("scheduled prompts", () => {
  it("checks the entries at each minute boundary of local time, makes the matching ones due, and logs each", () => {
    jest.setSystemTime(new Date(2026, 10, 11, 6, 58, 30))
    const schedules = new Schedules(() => true)
    schedules.set("#2", [scheduleEntry("* * * * *", "Read the news."), scheduleEntry("0 7 * * *", "Check the trail report.", "2026-11-10T09:00:00.000Z")])
    schedules.start()
    jest.advanceTimersByTime(29_999)
    expect(schedules.pending("#2")).toBe(0)
    jest.advanceTimersByTime(1)
    expect(schedules.take("#2")).toEqual([{prompt: "Read the news.", at: dueAt(6, 59)}])
    jest.advanceTimersByTime(60_000)
    expect(schedules.take("#2")).toEqual([
      {prompt: "Read the news.", at: dueAt(7, 0)},
      {prompt: "Check the trail report.", at: dueAt(7, 0)},
    ])
    expect(schedules.take("#2")).toEqual([])
    expect(logLines).toEqual(["Scheduled prompt 1 of #2 is due.", "Scheduled prompt 1 of #2 is due.", "Scheduled prompt 2 of #2 is due."])
    schedules.stop()
  })

  it("keeps one pending prompt per entry, with the minute it became due", () => {
    const schedules = new Schedules(() => true)
    schedules.set("#2", [scheduleEntry("* * * * *")])
    schedules.start()
    jest.advanceTimersByTime(150_000)
    expect(schedules.take("#2")).toEqual([{prompt: "Check the trail report.", at: dueAt(7, 0)}])
    expect(logLines).toEqual(["Scheduled prompt 1 of #2 is due."])
    jest.advanceTimersByTime(60_000)
    expect(schedules.take("#2")).toEqual([{prompt: "Check the trail report.", at: dueAt(7, 3)}])
    schedules.stop()
  })

  it("keeps the pending prompts of the entries set again, and drops those of removed entries", () => {
    const second = scheduleEntry("* * * * *", "second", "2026-11-10T09:00:00.000Z")
    const schedules = new Schedules(() => true)
    schedules.set("#2", [scheduleEntry("* * * * *", "first"), second])
    schedules.set("#5", [scheduleEntry("* * * * *", "other chat")])
    schedules.start()
    jest.advanceTimersByTime(30_000)
    schedules.set("#2", [{...second}, scheduleEntry("0 8 * * *", "third", "2026-11-10T10:00:00.000Z")])
    expect(schedules.take("#2")).toEqual([{prompt: "second", at: dueAt(7, 0)}])
    schedules.set("#5", [])
    expect(schedules.pending("#5")).toBe(0)
    schedules.stop()
  })

  it("makes due only the entries whose author is an approver at the due minute", () => {
    const approvers = new Set([3])
    const schedules = new Schedules(memberId => approvers.has(memberId))
    schedules.set("#2", [scheduleEntry("* * * * *", "by Alice"), {...scheduleEntry("* * * * *", "by Bob", "2026-11-10T09:00:00.000Z"), by: "Bob", byId: 7}])
    schedules.start()
    jest.advanceTimersByTime(30_000)
    expect(schedules.take("#2")).toEqual([{prompt: "by Alice", at: dueAt(7, 0)}])
    approvers.clear()
    approvers.add(7)
    jest.advanceTimersByTime(60_000)
    expect(schedules.take("#2")).toEqual([{prompt: "by Bob", at: dueAt(7, 1)}])
    schedules.stop()
  })

  it("wakes the listeners of the chat whose prompt becomes due, until they stop listening", () => {
    const schedules = new Schedules(() => true)
    const woken: string[] = []
    const stopListening = schedules.listen("#2", () => woken.push("#2"))
    schedules.listen("#5", () => woken.push("#5"))
    schedules.set("#2", [scheduleEntry("* * * * *")])
    schedules.start()
    jest.advanceTimersByTime(30_000)
    expect(woken).toEqual(["#2"])
    schedules.take("#2")
    stopListening()
    jest.advanceTimersByTime(60_000)
    expect([woken, schedules.pending("#2")]).toEqual([["#2"], 1])
    schedules.stop()
  })

  it("keeps one timer while it runs, and none after the stop", () => {
    const schedules = new Schedules(() => true)
    schedules.set("#2", [scheduleEntry("* * * * *")])
    schedules.start()
    expect(jest.getTimerCount()).toBe(1)
    jest.advanceTimersByTime(30_000)
    expect(jest.getTimerCount()).toBe(1)
    schedules.take("#2")
    schedules.stop()
    expect(jest.getTimerCount()).toBe(0)
    jest.advanceTimersByTime(120_000)
    expect(schedules.pending("#2")).toBe(0)
  })

  it("checks each minute once when its timer fires early, and the minute the timer was set for when it fires late", () => {
    const schedules = new Schedules(() => true)
    schedules.set("#2", [scheduleEntry("* * * * *", "every minute"), scheduleEntry("3 7 * * *", "at 07:03", "2026-11-10T09:00:00.000Z")])
    schedules.start()
    jest.setSystemTime(Date.now() - 5)
    jest.advanceTimersByTime(30_000)
    expect(schedules.take("#2")).toEqual([{prompt: "every minute", at: dueAt(7, 0)}])
    jest.advanceTimersByTime(5)
    expect(schedules.pending("#2")).toBe(0)
    jest.setSystemTime(new Date(2026, 10, 11, 7, 4, 30))
    jest.advanceTimersByTime(60_000)
    expect(schedules.take("#2")).toEqual([{prompt: "every minute", at: dueAt(7, 1)}])
    jest.advanceTimersByTime(30_000)
    expect(schedules.take("#2")).toEqual([{prompt: "every minute", at: dueAt(7, 6)}])
    schedules.stop()
  })
})
