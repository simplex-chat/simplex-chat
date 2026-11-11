import {T} from "@simplex-chat/types"
import {Actions, ToolError, stoppingMessage} from "../../src/actions"
import {InferenceState} from "../../src/inference"
import {Statuses} from "../../src/status"
import {FakeChat, contact, direct, item} from "./fixtures"

const start = Date.parse("2026-10-17T10:00:00Z")
const frozen = "◌ working since 2026-10-17 10:00 UTC"
const updateLimit = "live: the update limit of the status message of @4 is reached; send your answer without live."
let created: Statuses[]

beforeEach(() => {
  jest.useFakeTimers({now: start})
  created = []
})

afterEach(async () => {
  for (const statuses of created) await statuses.stop()
  jest.restoreAllMocks()
  jest.useRealTimers()
})

function statusesWith(agent: {allowed?: (chat: string) => Promise<boolean>; inference?: () => Promise<InferenceState | undefined>} = {}): {chat: FakeChat; statuses: Statuses; pending: {open: number}[]} {
  const chat = new FakeChat()
  const pending: {open: number}[] = []
  const actions = {pending: () => pending} as unknown as Actions
  const statuses = new Statuses({chat: chat.api(), actions, inference: agent.inference}, agent.allowed ?? (async () => true))
  created.push(statuses)
  return {chat, statuses, pending}
}

function deferred<V>(): {promise: Promise<V>; resolve: (value: V) => void; reject: (error: Error) => void} {
  let resolve: (value: V) => void = () => undefined
  let reject: (error: Error) => void = () => undefined
  const promise = new Promise<V>((onResolve, onReject) => {
    resolve = onResolve
    reject = onReject
  })
  return {promise, resolve, reject}
}

function logged(): string[] {
  const lines: string[] = []
  jest.spyOn(process.stderr, "write").mockImplementation(text => (lines.push(String(text).trimEnd()), true))
  return lines
}

function deletion(chatId: number, itemId: number): object {
  return {chatType: T.ChatType.Direct, chatId, itemIds: [itemId], mode: T.CIDeleteMode.Broadcast}
}

function texts(chat: FakeChat): string[] {
  return [...chat.sent.map(({messages: [{msgContent}]}) => msgContent.text), ...chat.updates.map(({text}) => text)]
}

function labelOf(text: string): string {
  return text.slice(2, text.lastIndexOf(" · "))
}

describe("status message", () => {
  it("posts the status after the eligibility read, then updates it 3 s apart for 30 s, then at a tenth of its age up to 20 s", async () => {
    const read = deferred<void>()
    const {chat, statuses} = statusesWith({
      allowed: async () => {
        await read.promise
        return true
      },
    })
    const times: number[] = []
    const update = chat.apiUpdateChatItem.bind(chat)
    chat.apiUpdateChatItem = async (...args) => {
      times.push(Date.now() - start)
      return update(...args)
    }
    statuses.start("@4")
    await jest.advanceTimersByTimeAsync(0)
    expect(chat.sent).toEqual([])
    read.resolve()
    await jest.advanceTimersByTimeAsync(0)
    expect(chat.sent).toEqual([{chat: [T.ChatType.Direct, 4], messages: [{msgContent: {type: "text", text: "◐ thinking · 0:00"}, mentions: {}}], live: true}])
    await jest.advanceTimersByTimeAsync(600_000)
    expect(times.slice(0, 12).map(time => Math.round(time / 100) / 10)).toEqual([3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36.3])
    const late = times.filter(time => time >= 200_000)
    expect(late.slice(1).map((time, i) => time - late[i])).toEqual(Array(19).fill(20_000))
    expect(late[0] - times[times.indexOf(late[0]) - 1]).toBeLessThan(20_000)
    expect(times).toHaveLength(49)
  })

  it("turns the frame at each update and shows the elapsed time", async () => {
    const {chat, statuses} = statusesWith()
    statuses.start("@4")
    await jest.advanceTimersByTimeAsync(65_000)
    expect(texts(chat).slice(0, 6)).toEqual(["◐ thinking · 0:00", "◓ thinking · 0:03", "◑ thinking · 0:06", "◒ thinking · 0:09", "◐ thinking · 0:12", "◓ thinking · 0:15"])
    expect(texts(chat).at(-1)).toBe("◑ thinking · 1:04")
  })

  it("shows the 50th update as working since its start time, and checks every 5 minutes after it", async () => {
    const allowed = jest.fn(async () => true)
    const {chat, statuses} = statusesWith({allowed})
    statuses.start("@4")
    await jest.advanceTimersByTimeAsync(600_000)
    expect(chat.updates).toHaveLength(49)
    expect(chat.updates[48].text).toBe(frozen)
    const reads = allowed.mock.calls.length
    await jest.advanceTimersByTimeAsync(300_000)
    expect(allowed.mock.calls.length).toBe(reads + 1)
    await jest.advanceTimersByTimeAsync(300_000)
    expect(allowed.mock.calls.length).toBe(reads + 2)
    expect(chat.updates).toHaveLength(49)
  })

  it("ranks labels: inference states, waiting for approval, using TOOL, working, thinking", async () => {
    let state: InferenceState | undefined = {type: "output", model: "qwen3-coder-30b", rate: 31}
    const {chat, statuses, pending} = statusesWith({inference: async () => state})
    const endDownload = statuses.startCall("download")
    pending.push({open: 1})
    statuses.start("@4")
    await jest.advanceTimersByTimeAsync(4_500)
    state = undefined
    await jest.advanceTimersByTimeAsync(3_000)
    pending[0].open = 0
    statuses.startCall("receive")
    await jest.advanceTimersByTimeAsync(3_000)
    endDownload()
    state = {type: "idle"}
    await jest.advanceTimersByTimeAsync(3_000)
    expect(chat.updates.map(({text}) => labelOf(text))).toEqual(["generating with `qwen3-coder-30b`, 31 tokens/s", "waiting for approval", "using download", "working"])
    const unpolled = statusesWith()
    unpolled.statuses.startCall("receive")
    unpolled.statuses.start("@4")
    await jest.advanceTimersByTimeAsync(4_500)
    expect(unpolled.chat.updates.map(({text}) => labelOf(text))).toEqual(["thinking"])
  })

  it("names the model and the counts in the inference labels", async () => {
    const states: InferenceState[] = [
      {type: "loading", model: "qwen3-coder-30b"},
      {type: "loading"},
      {type: "prompt", model: "qwen3-coder-30b", tokens: 6144},
      {type: "prompt", tokens: 0},
      {type: "output", model: "qwen3-coder-30b", rate: 30.6},
      {type: "output"},
    ]
    const {chat, statuses} = statusesWith({inference: async () => states.shift()})
    statuses.start("@4")
    await jest.advanceTimersByTimeAsync(19_500)
    expect(chat.updates.map(({text}) => labelOf(text))).toEqual([
      "loading `qwen3-coder-30b`",
      "loading the model",
      "reading the prompt with `qwen3-coder-30b`, 6144 tokens read",
      "reading the prompt",
      "generating with `qwen3-coder-30b`, 31 tokens/s",
      "generating",
    ])
  })

  it("deletes the status when the chat is not eligible or its read fails, and sends no update after it", async () => {
    const lines = logged()
    let eligible = true
    const ineligible = statusesWith({allowed: async () => eligible})
    let failure: Error | undefined
    const failing = statusesWith({
      allowed: async () => {
        if (failure !== undefined) throw failure
        return true
      },
    })
    ineligible.statuses.start("@4")
    failing.statuses.start("@4")
    await jest.advanceTimersByTimeAsync(1_500)
    eligible = false
    failure = new Error("database busy")
    await jest.advanceTimersByTimeAsync(3_000)
    expect([ineligible.chat.deletedItems, failing.chat.deletedItems]).toEqual([[deletion(4, 500)], [deletion(4, 500)]])
    expect(lines).toEqual(["status of @4: database busy"])
    await jest.advanceTimersByTimeAsync(60_000)
    expect([ineligible.chat.updates, failing.chat.updates]).toEqual([[], []])
  })

  it("deletes the status at 23 hours while a download runs, and when frozen 30 minutes after the last call while only a receive runs", async () => {
    const lasting = statusesWith()
    lasting.statuses.startCall("download")
    lasting.statuses.start("@4")
    const idle = statusesWith()
    idle.statuses.startCall("receive")
    idle.statuses.start("@4")
    await jest.advanceTimersByTimeAsync(30 * 60_000)
    expect([lasting.chat.deletedItems, idle.chat.deletedItems]).toEqual([[], []])
    await jest.advanceTimersByTimeAsync(5 * 60_000)
    expect([lasting.chat.deletedItems, idle.chat.deletedItems]).toEqual([[], [deletion(4, 500)]])
    await jest.advanceTimersByTimeAsync(23 * 3_600_000 - 35 * 60_000)
    expect(lasting.chat.deletedItems).toEqual([])
    await jest.advanceTimersByTimeAsync(5 * 60_000)
    expect(lasting.chat.deletedItems).toEqual([deletion(4, 500)])
  })

  it("logs nothing for a read that fails after the status was deleted", async () => {
    const lines = logged()
    const read = deferred<boolean>()
    const {chat, statuses} = statusesWith({allowed: () => read.promise})
    statuses.start("@4")
    await jest.advanceTimersByTimeAsync(0)
    await statuses.end("@4")
    read.reject(new Error("database busy"))
    await jest.advanceTimersByTimeAsync(3_000)
    expect(lines).toEqual([])
    expect([chat.sent, chat.deletedItems]).toEqual([[], []])
  })

  it("shows a note above the line at once, above the frozen line later, and refuses it at the 59th update", async () => {
    const {chat, statuses} = statusesWith()
    statuses.start("@4")
    await jest.advanceTimersByTimeAsync(1_000)
    statuses.show("@4", "Reading the file…")
    await jest.advanceTimersByTimeAsync(0)
    expect(texts(chat)).toEqual(["◐ thinking · 0:00", "Reading the file…\n◓ thinking · 0:01"])
    await jest.advanceTimersByTimeAsync(2_500)
    expect(texts(chat).at(-1)).toBe("Reading the file…\n◑ thinking · 0:03")
    await jest.advanceTimersByTimeAsync(600_000)
    expect(texts(chat).at(-1)).toBe(`Reading the file…\n${frozen}`)
    for (let n = 51; n <= 59; n++) {
      statuses.show("@4", `Step ${n}`)
      await jest.advanceTimersByTimeAsync(0)
    }
    expect(texts(chat).at(-1)).toBe(`Step 59\n${frozen}`)
    expect(chat.updates).toHaveLength(58)
    expect(() => statuses.show("@4", "Step 60")).toThrow(new ToolError(updateLimit))
    await jest.advanceTimersByTimeAsync(300_000)
    expect(chat.updates).toHaveLength(58)
  })

  it("replaces a status 23 hours old at a note", async () => {
    const {chat, statuses} = statusesWith()
    statuses.startCall("download")
    statuses.start("@4")
    await jest.advanceTimersByTimeAsync(23 * 3_600_000 + 1_000)
    expect(chat.deletedItems).toEqual([])
    statuses.show("@4", "Still reading…")
    await jest.advanceTimersByTimeAsync(0)
    expect(chat.deletedItems).toEqual([deletion(4, 500)])
    expect(chat.sent.at(-1)).toEqual({chat: [T.ChatType.Direct, 4], messages: [{msgContent: {type: "text", text: "Still reading…\n◐ thinking · 0:00"}, mentions: {}}], live: true})
    await jest.advanceTimersByTimeAsync(3_500)
    expect(chat.updates.at(-1)).toEqual({chatId: 4, itemId: 501, text: "Still reading…\n◓ using download · 0:03", live: true})
  })

  it("renders nothing after a label read during which notes reached 50 updates", async () => {
    const poll = deferred<InferenceState | undefined>()
    const {chat, statuses} = statusesWith({inference: () => poll.promise})
    const note = async (n: number): Promise<void> => {
      statuses.show("@4", `Step ${n}`)
      await jest.advanceTimersByTimeAsync(0)
    }
    statuses.start("@4")
    await jest.advanceTimersByTimeAsync(0)
    for (let n = 2; n <= 48; n++) await note(n)
    await jest.advanceTimersByTimeAsync(3_000)
    for (let n = 49; n <= 58; n++) await note(n)
    poll.resolve({type: "idle"})
    await jest.advanceTimersByTimeAsync(0)
    expect(chat.updates).toHaveLength(57)
    await note(59)
    expect(texts(chat).at(-1)).toBe(`Step 59\n${frozen}`)
    expect(() => statuses.show("@4", "Step 60")).toThrow(new ToolError(updateLimit))
  })

  it("keeps one status per chat for concurrent starts, and statuses of two chats apart", async () => {
    const {chat, statuses} = statusesWith()
    statuses.start("@4")
    statuses.start("@4")
    statuses.start("@5")
    await jest.advanceTimersByTimeAsync(3_500)
    expect(chat.sent.map(({chat: [, chatId]}) => chatId).sort()).toEqual([4, 5])
    expect(chat.updates.map(({chatId}) => chatId).sort()).toEqual([4, 5])
  })

  it("sends nothing for a tick that ends after the status was deleted", async () => {
    const reading = deferred<boolean>()
    let read = async (): Promise<boolean> => true
    const duringRead = statusesWith({allowed: () => read()})
    const polling = deferred<InferenceState | undefined>()
    const duringPoll = statusesWith({inference: () => polling.promise})
    duringRead.statuses.start("@4")
    duringPoll.statuses.start("@4")
    await jest.advanceTimersByTimeAsync(1_500)
    read = () => reading.promise
    await jest.advanceTimersByTimeAsync(3_000)
    await duringRead.statuses.end("@4")
    await duringPoll.statuses.end("@4")
    reading.resolve(true)
    polling.resolve({type: "idle"})
    await jest.advanceTimersByTimeAsync(60_000)
    for (const {chat} of [duringRead, duringPoll]) {
      expect(chat.updates).toEqual([])
      expect(chat.deletedItems).toEqual([deletion(4, 500)])
    }
  })

  it("deletes the status when a command fails and logs the failure, and a later note starts a new status", async () => {
    const lines = logged()
    const updating = statusesWith()
    jest.spyOn(updating.chat, "apiUpdateChatItem").mockRejectedValueOnce(new Error("chat item not found"))
    const posting = statusesWith()
    jest.spyOn(posting.chat, "apiSendMessages").mockRejectedValueOnce(new Error("no connection"))
    updating.statuses.start("@4")
    posting.statuses.start("@5")
    await jest.advanceTimersByTimeAsync(4_500)
    expect(lines).toEqual(["status of @5: no connection", "status of @4: chat item not found"])
    expect(updating.chat.deletedItems).toEqual([deletion(4, 500)])
    expect([posting.chat.sent, posting.chat.updates, posting.chat.deletedItems]).toEqual([[], [], []])
    await jest.advanceTimersByTimeAsync(60_000)
    expect([updating.chat.updates, posting.chat.updates]).toEqual([[], []])
    updating.statuses.show("@4", "Still reading…")
    await jest.advanceTimersByTimeAsync(0)
    expect(texts(updating.chat)).toEqual(["◐ thinking · 0:00", "Still reading…\n◐ thinking · 0:00"])
  })

  it("deletes every status at stop, returns the count of shown ones, and leaves no timer", async () => {
    const reading = deferred<boolean>()
    const {chat, statuses} = statusesWith({allowed: ref => (ref === "@6" ? reading.promise : Promise.resolve(true))})
    statuses.start("@4")
    statuses.start("@5")
    statuses.start("@6")
    await jest.advanceTimersByTimeAsync(1_500)
    statuses.show("@4", "Reading the file…")
    expect(await statuses.stop()).toBe(2)
    expect(chat.deletedItems.map(({chatId}) => chatId).sort()).toEqual([4, 5])
    expect(jest.getTimerCount()).toBe(0)
    reading.resolve(true)
    statuses.start("@7")
    expect(() => statuses.show("@4", "Still reading…")).toThrow(new ToolError(stoppingMessage))
    await jest.advanceTimersByTimeAsync(60_000)
    expect(chat.sent.map(({chat: [, chatId]}) => chatId).sort()).toEqual([4, 5])
    expect(chat.updates).toEqual([])
  })

  it("logs the first failed poll after a success only", async () => {
    const lines = logged()
    const failure = async (): Promise<InferenceState | undefined> => {
      throw new Error("GET http://127.0.0.1:8080/running: HTTP 401")
    }
    const polls: (() => Promise<InferenceState | undefined>)[] = [failure, failure, async () => ({type: "idle"}), failure]
    const {chat, statuses} = statusesWith({inference: () => (polls.shift() ?? failure)()})
    statuses.start("@4")
    await jest.advanceTimersByTimeAsync(7_500)
    expect(lines).toEqual(["inference endpoint: GET http://127.0.0.1:8080/running: HTTP 401"])
    await jest.advanceTimersByTimeAsync(6_000)
    expect(lines).toEqual(["inference endpoint: GET http://127.0.0.1:8080/running: HTTP 401", "inference endpoint: GET http://127.0.0.1:8080/running: HTTP 401"])
    expect(chat.updates.map(({text}) => labelOf(text))).toEqual(["thinking", "thinking", "working", "thinking"])
  })
})

describe("leftover status messages", () => {
  const hour = 3_600_000
  const deleteWindow = 30 * hour - 10 * 60_000
  const at = (age: number): string => new Date(start - age).toISOString()
  const own = (itemId: number, age: number): T.ChatItem => item(itemId, "◐ thinking · 0:00", {type: "directSnd"}, {live: true, ts: at(age)})

  it("are deleted when own, live and younger than 30 h − 10 min, page by page until an older page, and a failure is logged", async () => {
    const lines = logged()
    const {chat, statuses} = statusesWith()
    const marked = own(6, hour)
    marked.meta.itemDeleted = {type: "deleted", chatType: T.ChatType.Direct}
    const messages = Array.from({length: 100}, (_, i) => item(10 + i, `message ${i}`, {type: "directRcv"}, {ts: at(60_000)}))
    const final = item(4, "12:00", {type: "directSnd"}, {ts: at(hour)})
    const received = item(5, "Ridge", {type: "directRcv"}, {live: true, ts: at(hour)})
    chat.chats.set("@4", {chatInfo: direct(contact(4, "Alice")), items: [own(1, 31 * hour), own(2, deleteWindow), own(3, deleteWindow - 1_000), final, received, marked, ...messages]})
    await statuses.deleteLeftovers("@4")
    expect(chat.commands).toEqual(["/_get chat @4 count=100", "/_get chat @4 before=10 count=100"])
    expect(chat.deletedItems).toEqual([deletion(4, 3)])
    const older = Array.from({length: 60}, (_, i) => own(1 + i, 31 * hour))
    const newer = Array.from({length: 90}, (_, i) => item(61 + i, `message ${i}`, {type: "directRcv"}, {ts: at(hour)}))
    chat.chats.set("@5", {chatInfo: direct(contact(5, "Bob")), items: [...older, ...newer]})
    await statuses.deleteLeftovers("@5")
    expect(chat.commands.slice(2)).toEqual(["/_get chat @5 count=100"])
    expect(chat.deletedItems).toHaveLength(1)
    jest.spyOn(chat, "apiDeleteChatItems").mockRejectedValueOnce(new Error("database busy"))
    await statuses.deleteLeftovers("@4")
    expect(lines).toEqual(["status of @4: database busy"])
  })
})
