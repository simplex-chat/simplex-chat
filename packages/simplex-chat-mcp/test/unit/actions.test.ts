import {Action, Actions, NewAction, ToolError, lostMessage, stoppingMessage} from "../../src/actions"
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
  const action = actions.create(key, sendFields())
  await settled()
  expect(action.card).toBe(1000 + action.n)
  return action
}

describe("actions in mode requests", () => {
  it("writes the counter before the card and logs the action", async () => {
    const fake = fakeHooks()
    const actions = actionsWith("requests", fake, 7)
    const action = await pendingAction(actions)
    expect(action).toMatchObject({n: 7, state: "pending", open: 0})
    expect(actions.next).toBe(8)
    expect(fake.calls).toEqual(["counter 8", "card 7"])
    expect(logLines).toEqual(["Action 7: send to bot SimpleX Directory (@4)"])
  })

  it("performs an approved action and edits its card", async () => {
    const fake = fakeHooks()
    const actions = actionsWith("requests", fake)
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
    const actions = actionsWith("requests", fake)
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
    const actions = actionsWith("requests", fake)
    const action = await pendingAction(actions)
    actions.approve(action, "Alice")
    fake.performs[0].reject(new Error("bad\u001b]52;c;eA==\u0007\nnext"))
    await settled()
    expect(logLines.at(-1)).toBe("Action 1 approved by Alice: failed: bad ]52;c;eA== next.")
  })

  it("reports connect outcomes", async () => {
    const fake = fakeHooks()
    const actions = actionsWith("requests", fake)
    const connected = actions.create("a", connectFields())
    const requested = actions.create("b", connectFields())
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
    const actions = actionsWith("requests", fake)
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
    const actions = actionsWith("requests", fake, 1, 1)
    const action = await pendingAction(actions)
    jest.advanceTimersByTime(3_600_000 - 1)
    expect(action.state).toBe("pending")
    jest.advanceTimersByTime(1)
    await settled()
    expect(action.state).toBe("expired")
    expect(fake.calls.at(-1)).toBe("outcome 1 expired")
    expect(logLines.at(-1)).toBe("Action 1 expired.")
  })

  it("expires by the wall clock after the computer sleeps", async () => {
    jest.useFakeTimers()
    const actions = actionsWith("requests", fakeHooks(), 1, 1)
    const action = await pendingAction(actions)
    jest.setSystemTime(Date.now() + 3_600_000)
    expect(action.state).toBe("pending")
    jest.advanceTimersByTime(60_000)
    expect(action.state).toBe("expired")
  })

  it("fails when the card can't be posted", async () => {
    const fake = fakeHooks()
    fake.hooks.postCard = async () => {
      throw new Error("not a member")
    }
    const actions = actionsWith("requests", fake)
    const action = actions.create("k", sendFields())
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
    const actions = actionsWith("host", fake)
    const action = actions.create("k", sendFields())
    await settled()
    expect(action).toMatchObject({state: "failed", error: "disk\nfull"})
    expect(fake.calls).toEqual([])
    expect(logLines).toEqual(["Action 1: send to bot SimpleX Directory (@4)", "Action 1: failed: disk full."])
  })

  it("refuses new actions without approvers or control chat", () => {
    const fake = fakeHooks()
    fake.approvers = 0
    const actions = actionsWith("requests", fake)
    expect(() => actions.create("k", sendFields())).toThrow(new ToolError("no approver"))
    actions.lost = true
    expect(() => actions.create("k", sendFields())).toThrow(new ToolError(lostMessage))
    expect(actions.next).toBe(1)
  })

  it("refuses new actions once stopping", async () => {
    const actions = actionsWith("requests", fakeHooks())
    await actions.stop()
    expect(() => actions.create("k", sendFields())).toThrow(new ToolError(stoppingMessage))
    expect(actions.next).toBe(1)
  })

  it("asks for the refusal with the action it would create", () => {
    const fake = fakeHooks()
    const asked: Action[] = []
    fake.hooks.refusal = action => (asked.push(action), "too long")
    const actions = actionsWith("requests", fake, 4)
    expect(() => actions.create("k", sendFields("long"), "Alice")).toThrow(new ToolError("too long"))
    expect(asked).toEqual([expect.objectContaining({n: 4, tool: "send", input: {chat: "@4", text: "long"}, state: "performing", standing: true, decidedBy: "Alice"})])
    expect(actions.next).toBe(4)
    expect(actions.all()).toEqual([])
    expect(fake.calls).toEqual([])
  })

  it("refuses a new action while 20 are pending", async () => {
    const fake = fakeHooks()
    const actions = actionsWith("requests", fake)
    for (let i = 0; i < 20; i++) actions.create(`k${i}`, sendFields(`text ${i}`))
    await settled()
    expect(() => actions.create("k20", sendFields("one more"))).toThrow(new ToolError("20 actions are pending; wait for the owner's decisions."))
    expect(actions.next).toBe(21)
    actions.reject(actions.get(1)!, "Alice")
    expect(actions.create("k20", sendFields("one more")).n).toBe(21)
  })

  it("fails pending actions when the control chat is lost and skips their cards", async () => {
    const fake = fakeHooks()
    const actions = actionsWith("requests", fake)
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
    const actions = actionsWith("requests", fake)
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
    const actions = actionsWith("requests", fake)
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
    const actions = actionsWith("requests", fake)
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
    const actions = actionsWith("requests", fake)
    const action = actions.create("k", sendFields())
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
    const actions = actionsWith("requests", fake)
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
    const actions = actionsWith("requests", fake)
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
    const actions = actionsWith("requests", fake, 1, 1)
    const rejected = await pendingAction(actions, "a")
    const waited = await pendingAction(actions, "b")
    const performing = await pendingAction(actions, "c")
    const expiring = await pendingAction(actions, "d")
    actions.reject(rejected, "Alice")
    actions.reject(waited, "Alice")
    actions.approve(performing, "Alice")
    const stopCollecting = jest.fn()
    rejected.stopCollecting = stopCollecting
    waited.open = 1
    jest.advanceTimersByTime(3_600_000 - 1)
    expect(actions.all()).toHaveLength(4)
    jest.advanceTimersByTime(1)
    expect(actions.all()).toEqual([waited, performing, expiring])
    expect(stopCollecting).toHaveBeenCalledTimes(1)
    expect(expiring.state).toBe("expired")
    jest.advanceTimersByTime(60_000)
    expect(actions.all()).toEqual([waited, performing])
    expect(actions.find("d")).toBeUndefined()
  })

  it("waits for the decision with progress, and stops on the signal", async () => {
    const fake = fakeHooks()
    const actions = actionsWith("requests", fake)
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

  it("logs names on one line", () => {
    const actions = actionsWith("requests", fakeHooks())
    const fields = sendFields()
    if (fields.tool === "send") Object.assign(fields, {subject: {kind: "group", name: "Hikers\u001b]52;c;Y3VybA==\u0007\n/'approve 7'", incognito: false}})
    actions.create("k", fields)
    expect(logLines).toEqual(["Action 1: send to group Hikers ]52;c;Y3VybA== /'approve 7' (@4)"])
  })

  it("performs an action approved in advance at once, without a card, and posts the outcome", async () => {
    const fake = fakeHooks()
    const actions = actionsWith("requests", fake)
    const action = actions.create("k", sendFields(), "Alice")
    expect(action).toMatchObject({state: "performing", standing: true, decidedBy: "Alice"})
    await settled()
    fake.performs[0].resolve()
    await settled()
    expect(action.state).toBe("done")
    expect(action.card).toBeUndefined()
    expect(fake.calls).toEqual(["counter 2", "perform 1", "outcome 1 done"])
    expect(logLines).toEqual(["Action 1: send to bot SimpleX Directory (@4)", "Action 1 approved by Alice: sent."])
  })

  it("logs leave actions", async () => {
    const fake = fakeHooks()
    const actions = actionsWith("requests", fake)
    const subject = {kind: "bot", name: "SimpleX Directory", simplexName: "@directory.simplex", incognito: false}
    const contact = actions.create("a", {tool: "leave", input: {chat: "@4"}, subject, chat: "@4"})
    const group = actions.create("b", {tool: "leave", input: {chat: "#2"}, subject: {kind: "business", name: "ep", incognito: false}, chat: "#2"})
    await settled()
    expect(logLines).toEqual(["Action 1: delete bot SimpleX Directory (@4)", "Action 2: leave business ep (#2)"])
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
    const actions = actionsWith("host", fake)
    const action = actions.create("k", connectFields())
    expect(action.state).toBe("performing")
    expect(logLines).toEqual(["Action 1: connect to bot SimpleX Directory, as itself"])
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
    const actions = actionsWith("host", fake)
    const action = actions.create("k", sendFields())
    const stopping = actions.stop()
    written()
    expect(await stopping).toBe(1)
    expect(action).toMatchObject({state: "pending", interrupted: true})
    expect(fake.calls).toEqual(["outcome 1 pending interrupted"])
  })

  it("refuses a new action while 20 are being performed", async () => {
    const fake = fakeHooks()
    const actions = actionsWith("host", fake)
    for (let i = 0; i < 20; i++) actions.create(`k${i}`, sendFields(`text ${i}`))
    await settled()
    expect(() => actions.create("k20", sendFields("one more"))).toThrow(new ToolError("20 actions are being performed; wait for their results."))
    expect(actions.next).toBe(21)
    fake.performs[0].resolve()
    await settled()
    expect(actions.create("k20", sendFields("one more")).n).toBe(21)
  })

  it("logs a failure", async () => {
    const fake = fakeHooks()
    const actions = actionsWith("host", fake)
    actions.create("k", sendFields())
    await settled()
    fake.performs[0].reject(new Error("contact not ready"))
    await settled()
    expect(logLines.at(-1)).toBe("Action 1: performed without approval (mode host); failed: contact not ready.")
  })
})
