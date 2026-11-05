import {Action, Actions, NewAction, ToolError, lostMessage} from "../../src/actions"
import {Collected} from "../../src/messages"
import {actionsWith, fakeHooks, settled, signal} from "./fixtures"

let logLines: string[]

beforeEach(() => {
  logLines = []
  jest.spyOn(process.stdout, "write").mockImplementation(text => (logLines.push(String(text).trimEnd()), true))
})

afterEach(() => {
  jest.restoreAllMocks()
  jest.useRealTimers()
})

function sendFields(text = "trail running shoes"): NewAction {
  return {
    tool: "send",
    input: {chat: "@4", text},
    subject: {kind: "bot", name: "SimpleX Directory", simplexName: "@directory.simplex", incognito: false},
    chat: "@4",
    mentions: [],
    responses: new Collected(),
  }
}

function connectFields(): NewAction {
  return {
    tool: "connect",
    input: {target: "@directory.simplex"},
    subject: {kind: "bot", name: "SimpleX Directory", simplexName: "@directory.simplex", incognito: false},
    plan: {cardKind: "bot", type: "contact", takesMessage: true},
    messages: new Collected(),
  }
}

async function pendingAction(actions: Actions, key = "k"): Promise<Action> {
  const action = actions.create(1, "requests", key, sendFields())
  await settled()
  expect(action.card).toBe(1000 + action.n)
  return action
}

describe("actions in mode requests", () => {
  it("writes the counter before the card and logs the action", async () => {
    const fake = fakeHooks()
    const actions = actionsWith(fake, 7)
    const action = await pendingAction(actions)
    expect(action).toMatchObject({n: 7, state: "pending", open: 0})
    expect(actions.next).toBe(8)
    expect(fake.calls).toEqual(["counter 8", "card 7"])
    expect(logLines).toEqual(["Action 7 of session 1: send to bot SimpleX Directory (@4)"])
  })

  it("performs an approved action and edits its card", async () => {
    const fake = fakeHooks()
    const actions = actionsWith(fake)
    const action = await pendingAction(actions)
    actions.approve(action, "Alice")
    expect(action).toMatchObject({state: "performing", decidedBy: "Alice"})
    fake.performs[0].resolve()
    await settled()
    expect(action.state).toBe("done")
    expect(fake.calls).toEqual(["counter 2", "card 1", "perform 1", "outcome 1 done"])
    expect(logLines.at(-1)).toBe("Action 1 approved by Alice: sent.")
  })

  it("fails when performing fails", async () => {
    const fake = fakeHooks()
    const actions = actionsWith(fake)
    const action = await pendingAction(actions)
    actions.approve(action, "Alice")
    fake.performs[0].reject(new Error("no route"))
    await settled()
    expect(action).toMatchObject({state: "failed", error: "no route"})
    expect(logLines.at(-1)).toBe("Action 1 approved by Alice: failed: no route.")
    expect(fake.calls.at(-1)).toBe("outcome 1 failed")
  })

  it("logs a failure on one line", async () => {
    const fake = fakeHooks()
    const actions = actionsWith(fake)
    const action = await pendingAction(actions)
    actions.approve(action, "Alice")
    fake.performs[0].reject(new Error("bad\u001b]52;c;eA==\u0007\nnext"))
    await settled()
    expect(logLines.at(-1)).toBe("Action 1 approved by Alice: failed: bad ]52;c;eA== next.")
  })

  it("reports connect outcomes", async () => {
    const fake = fakeHooks()
    const actions = actionsWith(fake)
    const connected = actions.create(1, "requests", "a", connectFields())
    const requested = actions.create(1, "requests", "b", connectFields())
    await settled()
    actions.approve(connected, "Alice")
    actions.approve(requested, "Alice")
    if (connected.tool === "connect") connected.connected = true
    fake.performs[0].resolve()
    fake.performs[1].resolve()
    await settled()
    expect(logLines.slice(-2)).toEqual(["Action 1 approved by Alice: connected.", "Action 2 approved by Alice: connection requested."])
  })

  it("rejects", async () => {
    const fake = fakeHooks()
    const actions = actionsWith(fake)
    const action = await pendingAction(actions)
    actions.reject(action, "Bob")
    await settled()
    expect(action).toMatchObject({state: "rejected", decidedBy: "Bob"})
    expect(fake.calls.at(-1)).toBe("outcome 1 rejected")
    expect(logLines.at(-1)).toBe("Action 1 rejected by Bob.")
  })

  it("expires after --expire hours", async () => {
    jest.useFakeTimers()
    const fake = fakeHooks()
    const actions = actionsWith(fake, 1, 1)
    const action = await pendingAction(actions)
    jest.advanceTimersByTime(3_600_000 - 1)
    expect(action.state).toBe("pending")
    jest.advanceTimersByTime(1)
    await settled()
    expect(action.state).toBe("expired")
    expect(fake.calls.at(-1)).toBe("outcome 1 expired")
    expect(logLines.at(-1)).toBe("Action 1 expired.")
  })

  it("expires at its own expiry time, not at a fixed tick", async () => {
    jest.useFakeTimers()
    const actions = actionsWith(fakeHooks(), 1, 1)
    jest.advanceTimersByTime(30_000)
    const action = await pendingAction(actions)
    jest.advanceTimersByTime(3_600_000 - 1)
    expect(action.state).toBe("pending")
    jest.advanceTimersByTime(1)
    expect(action.state).toBe("expired")
  })

  it("expires by the wall clock after the computer sleeps", async () => {
    jest.useFakeTimers()
    const actions = actionsWith(fakeHooks(), 1, 1)
    const action = await pendingAction(actions)
    jest.setSystemTime(Date.now() + 3_600_000)
    expect(action.state).toBe("pending")
    actions.expireDue()
    expect(action.state).toBe("expired")
    expect(actions.all()).toEqual([])
    expect(jest.getTimerCount()).toBe(0)
    jest.advanceTimersByTime(3_600_000)
    expect(logLines.filter(line => line === "Action 1 expired.")).toHaveLength(1)
  })

  it("clears the timer of an action expired by the wall clock", async () => {
    jest.useFakeTimers()
    const fake = fakeHooks()
    const actions = actionsWith(fake, 1, 1)
    const action = await pendingAction(actions)
    actions.approve(action, "Alice")
    jest.setSystemTime(Date.now() + 3_600_000)
    actions.expireDue()
    expect(jest.getTimerCount()).toBe(0)
    fake.performs[0].resolve()
    await settled()
    expect(actions.all()).toEqual([])
  })

  it("expires nothing once stopping", async () => {
    jest.useFakeTimers()
    const fake = fakeHooks()
    const actions = actionsWith(fake, 1, 1)
    const action = await pendingAction(actions)
    await actions.stop()
    jest.setSystemTime(Date.now() + 3_600_000)
    actions.expireDue()
    expect(action.state).toBe("pending")
    expect(fake.calls.filter(call => call.startsWith("outcome"))).toEqual(["outcome 1 pending interrupted"])
  })

  it("waits for the expiry time when its timer fires early after the clock is set back", async () => {
    jest.useFakeTimers()
    const actions = actionsWith(fakeHooks(), 1, 1)
    const action = await pendingAction(actions)
    jest.setSystemTime(Date.now() - 60_000)
    jest.advanceTimersByTime(3_600_000)
    expect(action.state).toBe("pending")
    expect(jest.getTimerCount()).toBe(1)
    jest.advanceTimersByTime(60_000)
    expect(action.state).toBe("expired")
  })

  it("fails when the card can't be posted", async () => {
    const fake = fakeHooks()
    fake.hooks.postCard = async () => {
      throw new Error("not a member")
    }
    const actions = actionsWith(fake)
    const action = actions.create(1, "requests", "k", sendFields())
    await settled()
    expect(action).toMatchObject({state: "failed", error: "not a member"})
    expect(action.card).toBeUndefined()
    expect(logLines.at(-1)).toBe("Action 1: failed: not a member.")
  })

  it("fails without a card or a perform when the counter can't be written, and logs it on one line", async () => {
    const fake = fakeHooks()
    fake.hooks.saveCounter = async () => {
      throw new Error("disk\nfull")
    }
    const actions = actionsWith(fake)
    const action = actions.create(1, "host", "k", sendFields())
    await settled()
    expect(action).toMatchObject({state: "failed", error: "disk\nfull"})
    expect(fake.calls).toEqual([])
    expect(logLines).toEqual(["Action 1 of session 1: send to bot SimpleX Directory (@4)", "Action 1: failed: disk full."])
  })

  it("refuses new actions without approvers or control chat", () => {
    const fake = fakeHooks()
    fake.approvers = 0
    const actions = actionsWith(fake)
    expect(() => actions.create(1, "requests", "k", sendFields())).toThrow(new ToolError("no approver"))
    actions.lost = true
    expect(() => actions.create(1, "requests", "k", sendFields())).toThrow(new ToolError(lostMessage))
    expect(actions.next).toBe(1)
  })

  it("refuses new actions once stopping", async () => {
    const actions = actionsWith(fakeHooks())
    await actions.stop()
    expect(() => actions.create(1, "requests", "k", sendFields())).toThrow(new ToolError("The SimpleX daemon is stopping."))
    expect(actions.next).toBe(1)
  })

  it("asks for the refusal with the action it would create", () => {
    const fake = fakeHooks()
    const asked: Action[] = []
    fake.hooks.refusal = action => (asked.push(action), "too long")
    const actions = actionsWith(fake, 4)
    expect(() => actions.create(1, "requests", "k", sendFields("long"), "Alice")).toThrow(new ToolError("too long"))
    expect(asked).toEqual([expect.objectContaining({n: 4, tool: "send", input: {chat: "@4", text: "long"}, state: "performing", standing: true, decidedBy: "Alice"})])
    expect(actions.next).toBe(4)
    expect(actions.all()).toEqual([])
    expect(fake.calls).toEqual([])
  })

  it("refuses a new action while 20 are pending", async () => {
    const fake = fakeHooks()
    const actions = actionsWith(fake)
    for (let i = 0; i < 20; i++) actions.create(1, "requests", `k${i}`, sendFields(`text ${i}`))
    await settled()
    expect(() => actions.create(1, "requests", "k20", sendFields("one more"))).toThrow(new ToolError("20 actions are pending; wait for the owner's decisions."))
    expect(actions.next).toBe(21)
    actions.reject(actions.get(1)!, "Alice")
    expect(actions.create(1, "requests", "k20", sendFields("one more")).n).toBe(21)
  })

  it("fails pending actions when the control chat is lost and skips their cards", async () => {
    const fake = fakeHooks()
    const actions = actionsWith(fake)
    const pending = await pendingAction(actions, "a")
    const performing = await pendingAction(actions, "b")
    actions.approve(performing, "Alice")
    actions.loseControl()
    expect(pending).toMatchObject({state: "failed", error: lostMessage})
    fake.performs[0].resolve()
    await settled()
    expect(performing.state).toBe("done")
    expect(fake.calls.filter(call => call.startsWith("outcome"))).toEqual([])
  })

  it("stops: cancels pending and interrupts performing actions", async () => {
    const fake = fakeHooks()
    const actions = actionsWith(fake)
    await pendingAction(actions, "a")
    const performing = await pendingAction(actions, "b")
    const done = await pendingAction(actions, "c")
    actions.approve(performing, "Alice")
    actions.approve(done, "Alice")
    fake.performs[1].resolve()
    await settled()
    expect(await actions.stop()).toBe(2)
    expect(fake.calls.filter(call => call.startsWith("outcome"))).toEqual(["outcome 3 done", "outcome 1 pending interrupted", "outcome 2 performing interrupted"])
    fake.performs[0].resolve()
    await settled()
    expect(fake.calls.filter(call => call.startsWith("outcome"))).toHaveLength(3)
  })

  it("aborts the signal of the performs at the stop, and logs no line when an interrupted perform ends", async () => {
    const fake = fakeHooks()
    const signals: AbortSignal[] = []
    const perform = fake.hooks.perform
    fake.hooks.perform = (action, signal) => (signals.push(signal), perform(action, signal))
    const actions = actionsWith(fake)
    const action = await pendingAction(actions)
    actions.approve(action, "Alice")
    expect(signals.map(({aborted}) => aborted)).toEqual([false])
    await actions.stop()
    expect(signals.map(({aborted}) => aborted)).toEqual([true])
    const lines = [...logLines]
    fake.performs[0].resolve()
    await settled()
    expect(action.state).toBe("done")
    expect(logLines).toEqual(lines)
    expect(fake.calls.filter(call => call.startsWith("outcome"))).toEqual(["outcome 1 performing interrupted"])
  })

  it("stops after the outcome edit of a perform that ended before the stop", async () => {
    const fake = fakeHooks()
    let edited: () => void = () => undefined
    fake.hooks.outcome = () => new Promise(resolve => (edited = resolve))
    const actions = actionsWith(fake)
    const action = await pendingAction(actions)
    actions.approve(action, "Alice")
    fake.performs[0].resolve()
    await settled()
    expect(action.state).toBe("done")
    let stopped = false
    const stopping = actions.stop().then(() => (stopped = true))
    await settled()
    expect(stopped).toBe(false)
    edited()
    await stopping
    expect(stopped).toBe(true)
  })

  it("stops after a card being posted, and edits it", async () => {
    const fake = fakeHooks()
    let postCard: (card: number) => void = () => undefined
    fake.hooks.postCard = () => new Promise(resolve => (postCard = resolve))
    const actions = actionsWith(fake)
    const action = actions.create(1, "requests", "k", sendFields())
    await settled()
    const stopping = actions.stop()
    await settled()
    expect(fake.calls).toEqual(["counter 2"])
    postCard(77)
    expect(await stopping).toBe(1)
    expect(action.card).toBe(77)
    expect(fake.calls).toEqual(["counter 2", "outcome 1 pending interrupted"])
  })

  it("forgets an action whose result was returned, and stops its collection", async () => {
    const fake = fakeHooks()
    const actions = actionsWith(fake)
    const action = await pendingAction(actions, "k")
    const stopCollecting = jest.fn()
    action.stopCollecting = stopCollecting
    actions.reject(action, "Alice")
    actions.remove(action)
    expect(stopCollecting).toHaveBeenCalledTimes(1)
    expect(actions.get(action.n)).toBeUndefined()
    expect(actions.find("k")).toBeUndefined()
    expect(actions.all()).toEqual([])
  })

  it("attaches calls with the same key until the result is returned", async () => {
    const fake = fakeHooks()
    const actions = actionsWith(fake)
    const action = await pendingAction(actions, "same")
    expect(actions.find("same")).toBe(action)
    expect(actions.find("other")).toBeUndefined()
    expect(actions.all()).toEqual([action])
    actions.remove(action)
    expect(actions.find("same")).toBeUndefined()
    expect(actions.all()).toEqual([])
  })

  it("forgets decided actions without an open call once they expire", async () => {
    jest.useFakeTimers()
    const fake = fakeHooks()
    const actions = actionsWith(fake, 1, 1)
    const rejected = await pendingAction(actions, "a")
    const waited = await pendingAction(actions, "b")
    const performing = await pendingAction(actions, "c")
    const expiring = await pendingAction(actions, "d")
    actions.reject(rejected, "Alice")
    actions.reject(waited, "Alice")
    actions.approve(performing, "Alice")
    const stopCollecting = jest.fn()
    rejected.stopCollecting = stopCollecting
    actions.openCall(waited)
    jest.advanceTimersByTime(3_600_000 - 1)
    expect(actions.all()).toHaveLength(4)
    jest.advanceTimersByTime(1)
    expect(actions.all()).toEqual([waited, performing])
    expect(stopCollecting).toHaveBeenCalledTimes(1)
    expect(expiring.state).toBe("expired")
    expect(actions.find("d")).toBeUndefined()
    expect(jest.getTimerCount()).toBe(0)
  })

  it("forgets an expired decided action when its last open call ends", async () => {
    jest.useFakeTimers()
    const actions = actionsWith(fakeHooks(), 1, 1)
    const action = await pendingAction(actions)
    actions.reject(action, "Alice")
    actions.openCall(action)
    actions.openCall(action)
    jest.advanceTimersByTime(3_600_000)
    expect(actions.all()).toEqual([action])
    actions.closeCall(action)
    expect(action.open).toBe(1)
    expect(actions.all()).toEqual([action])
    actions.closeCall(action)
    expect(action.open).toBe(0)
    expect(actions.all()).toEqual([])
  })

  it("keeps a decided action whose call ends before it expires", async () => {
    jest.useFakeTimers()
    const actions = actionsWith(fakeHooks(), 1, 1)
    const action = await pendingAction(actions)
    actions.reject(action, "Alice")
    actions.openCall(action)
    actions.closeCall(action)
    expect(actions.all()).toEqual([action])
    jest.advanceTimersByTime(3_600_000)
    expect(actions.all()).toEqual([])
  })

  it("forgets an action performed after its expiry once it is decided", async () => {
    jest.useFakeTimers()
    const fake = fakeHooks()
    const actions = actionsWith(fake, 1, 1)
    const action = await pendingAction(actions)
    actions.approve(action, "Alice")
    jest.advanceTimersByTime(3_600_000)
    expect(actions.all()).toEqual([action])
    fake.performs[0].resolve()
    await settled()
    expect(action.state).toBe("done")
    expect(actions.all()).toEqual([])
    expect(fake.calls.at(-1)).toBe("outcome 1 done")
  })

  it("clears the expiry timer of a removed action", async () => {
    jest.useFakeTimers()
    const actions = actionsWith(fakeHooks())
    const kept = await pendingAction(actions, "a")
    const removed = await pendingAction(actions, "b")
    expect(jest.getTimerCount()).toBe(2)
    actions.remove(removed)
    expect(jest.getTimerCount()).toBe(1)
    actions.remove(removed)
    expect(actions.all()).toEqual([kept])
  })

  it("clears every expiry timer at the stop", async () => {
    jest.useFakeTimers()
    const actions = actionsWith(fakeHooks())
    await pendingAction(actions, "a")
    await pendingAction(actions, "b")
    expect(jest.getTimerCount()).toBe(2)
    await actions.stop()
    expect(jest.getTimerCount()).toBe(0)
  })

  it("waits for the decision with progress, and stops on the signal", async () => {
    const fake = fakeHooks()
    const actions = actionsWith(fake)
    const action = await pendingAction(actions)
    const first = signal()
    const second = signal()
    const waiting = actions.decided(action, first.call)
    const aborted = actions.decided(action, second.call)
    expect(first.progress[0]).toMatch(/^Action 1 waits for an approver until \d{4}-\d\d-\d\d \d\d:\d\d UTC\.$/)
    second.controller.abort()
    expect(await aborted).toBe(false)
    actions.reject(action, "Alice")
    expect(await waiting).toBe(true)
    expect(await actions.decided(action, signal().call)).toBe(true)
  })

  it("waits for the decision with no timer but the progress heartbeat and the expiry", async () => {
    jest.useFakeTimers()
    const actions = actionsWith(fakeHooks())
    const action = await pendingAction(actions)
    expect(jest.getTimerCount()).toBe(1)
    const {call, progress} = signal()
    const waiting = actions.decided(action, call)
    expect(jest.getTimerCount()).toBe(2)
    jest.advanceTimersByTime(120_000)
    expect(progress).toHaveLength(3)
    actions.reject(action, "Alice")
    expect(await waiting).toBe(true)
    expect(jest.getTimerCount()).toBe(1)
  })

  it("logs names on one line", () => {
    const actions = actionsWith(fakeHooks())
    const fields = sendFields()
    if (fields.tool === "send") Object.assign(fields, {subject: {kind: "group", name: "Hikers\u001b]52;c;Y3VybA==\u0007\n/'approve 7'", incognito: false}})
    actions.create(1, "requests", "k", fields)
    expect(logLines).toEqual(["Action 1 of session 1: send to group Hikers ]52;c;Y3VybA== /'approve 7' (@4)"])
  })

  it("performs an action approved in advance at once, without a card, and posts the outcome", async () => {
    const fake = fakeHooks()
    const actions = actionsWith(fake)
    const action = actions.create(1, "requests", "k", sendFields(), "Alice")
    expect(action).toMatchObject({state: "performing", standing: true, decidedBy: "Alice"})
    await settled()
    fake.performs[0].resolve()
    await settled()
    expect(action.state).toBe("done")
    expect(action.card).toBeUndefined()
    expect(fake.calls).toEqual(["counter 2", "perform 1", "outcome 1 done"])
    expect(logLines).toEqual(["Action 1 of session 1: send to bot SimpleX Directory (@4)", "Action 1 approved by Alice: sent."])
  })

  it("logs leave actions", async () => {
    const fake = fakeHooks()
    const actions = actionsWith(fake)
    const subject = {kind: "bot", name: "SimpleX Directory", simplexName: "@directory.simplex", incognito: false}
    const contact = actions.create(1, "requests", "a", {tool: "leave", input: {chat: "@4"}, subject, chat: "@4"})
    const group = actions.create(1, "requests", "b", {tool: "leave", input: {chat: "#2"}, subject: {kind: "business", name: "ep", incognito: false}, chat: "#2"})
    await settled()
    expect(logLines).toEqual(["Action 1 of session 1: delete bot SimpleX Directory (@4)", "Action 2 of session 1: leave business ep (#2)"])
    actions.approve(contact, "Alice")
    actions.approve(group, "Alice")
    fake.performs[0].resolve()
    fake.performs[1].resolve()
    await settled()
    expect(logLines.slice(-2)).toEqual(["Action 1 approved by Alice: deleted.", "Action 2 approved by Alice: left."])
  })
})

describe("actions in mode host", () => {
  it("performs at once and posts the outcome", async () => {
    const fake = fakeHooks()
    const actions = actionsWith(fake)
    const action = actions.create(1, "host", "k", connectFields())
    expect(action.state).toBe("performing")
    expect(logLines).toEqual(["Action 1 of session 1: connect to bot SimpleX Directory, as itself"])
    await settled()
    fake.performs[0].resolve()
    await settled()
    expect(action.state).toBe("done")
    expect(fake.calls).toEqual(["counter 2", "perform 1", "outcome 1 done"])
    expect(logLines.at(-1)).toBe("Action 1: performed without approval (mode host).")
  })

  it("cancels an action whose counter is being written at the stop, without performing it", async () => {
    const fake = fakeHooks()
    let written: () => void = () => undefined
    fake.hooks.saveCounter = () => new Promise(resolve => (written = resolve))
    const actions = actionsWith(fake)
    const action = actions.create(1, "host", "k", sendFields())
    const stopping = actions.stop()
    written()
    expect(await stopping).toBe(1)
    expect(action).toMatchObject({state: "pending", interrupted: true})
    expect(fake.calls).toEqual(["outcome 1 pending interrupted"])
  })

  it("signals the change when the stop sets an action back to pending", async () => {
    const fake = fakeHooks()
    let written: () => void = () => undefined
    fake.hooks.saveCounter = () => new Promise(resolve => (written = resolve))
    const actions = actionsWith(fake)
    const action = actions.create(1, "host", "k", sendFields())
    const changed = jest.fn()
    action.changes.subscribe(changed)
    const stopping = actions.stop()
    written()
    await stopping
    expect(action.state).toBe("pending")
    expect(changed).toHaveBeenCalledTimes(1)
  })

  it("refuses a new action while 20 are being performed", async () => {
    const fake = fakeHooks()
    const actions = actionsWith(fake)
    for (let i = 0; i < 20; i++) actions.create(1, "host", `k${i}`, sendFields(`text ${i}`))
    await settled()
    expect(() => actions.create(1, "host", "k20", sendFields("one more"))).toThrow(new ToolError("20 actions are being performed; wait for their results."))
    expect(actions.next).toBe(21)
    fake.performs[0].resolve()
    await settled()
    expect(actions.create(1, "host", "k20", sendFields("one more")).n).toBe(21)
  })

  it("logs a failure", async () => {
    const fake = fakeHooks()
    const actions = actionsWith(fake)
    actions.create(1, "host", "k", sendFields())
    await settled()
    fake.performs[0].reject(new Error("contact not ready"))
    await settled()
    expect(logLines.at(-1)).toBe("Action 1: performed without approval (mode host); failed: contact not ready.")
  })
})

describe("actions of several sessions", () => {
  it("counts the pending limit per session", async () => {
    const actions = actionsWith(fakeHooks())
    for (let i = 0; i < 20; i++) actions.create(1, "requests", `k${i}`, sendFields())
    expect(() => actions.create(1, "requests", "k20", sendFields())).toThrow(new ToolError("20 actions are pending; wait for the owner's decisions."))
    expect(actions.create(2, "requests", "k0", sendFields()).session).toBe(2)
    expect(actions.ofSession(2).map(({n}) => n)).toEqual([21])
    expect(actions.ofSession(1)).toHaveLength(20)
    await settled()
  })

  it("takes the expiry set after construction for new actions", async () => {
    const actions = actionsWith(fakeHooks())
    actions.expireHours = 2
    const before = Date.now()
    const action = actions.create(1, "requests", "k", sendFields())
    expect(action.expires - before).toBeGreaterThanOrEqual(2 * 3_600_000)
    expect(action.expires - before).toBeLessThan(2 * 3_600_000 + 1000)
    await settled()
  })

  it("keeps the mode each action was created under, for its state, its outcome and its log line", async () => {
    const fake = fakeHooks()
    const actions = actionsWith(fake)
    const host = actions.create(1, "host", "k", sendFields())
    const requested = actions.create(2, "requests", "k", sendFields())
    expect([host, requested]).toMatchObject([{session: 1, mode: "host", state: "performing"}, {session: 2, mode: "requests", state: "pending"}])
    await settled()
    actions.approve(requested, "Alice")
    await settled()
    fake.performs[0].resolve()
    fake.performs[1].resolve()
    await settled()
    expect(fake.calls).toEqual(["counter 2", "counter 3", "card 2", "perform 1", "perform 2", "outcome 1 done", "outcome 2 done"])
    expect(logLines.slice(-2)).toEqual(["Action 1: performed without approval (mode host).", "Action 2 approved by Alice: sent."])
  })

  it("logs the session of an action", async () => {
    const actions = actionsWith(fakeHooks())
    actions.create(2, "requests", "k", sendFields())
    expect(logLines).toEqual(["Action 1 of session 2: send to bot SimpleX Directory (@4)"])
    await settled()
  })

  it("regains control after a reset", () => {
    const actions = actionsWith(fakeHooks())
    actions.loseControl()
    expect(() => actions.create(1, "requests", "k", sendFields())).toThrow(new ToolError("The Main group is lost. Run /simplex:reset-owners in Claude Code."))
    actions.regainControl()
    expect(actions.create(1, "requests", "k", sendFields()).n).toBe(1)
  })
})
