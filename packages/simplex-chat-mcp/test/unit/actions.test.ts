import {Action, Actions, NewAction, ToolError, lostMessage, pendingLine, stoppingMessage} from "../../src/actions"
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
    expect(action).toMatchObject({n: 7, state: "pending", returned: false, open: 0})
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

  it("forgets an action whose result was returned", async () => {
    const fake = fakeHooks()
    const actions = actionsWith("requests", fake)
    const action = await pendingAction(actions, "k")
    actions.reject(action, "Alice")
    actions.markReturned(action)
    expect(action.returned).toBe(true)
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
    expect(actions.unreturned()).toEqual([action])
    actions.markReturned(action)
    expect(actions.find("same")).toBeUndefined()
    expect(actions.unreturned()).toEqual([])
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

  it("lists pending actions", async () => {
    const fake = fakeHooks()
    const actions = actionsWith("requests", fake)
    const action = await pendingAction(actions)
    expect(pendingLine(action)).toBe(`1  send to bot SimpleX Directory  expires ${new Date(action.expires).toISOString().slice(11, 16)}`)
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

  it("logs and lists leave actions", async () => {
    const fake = fakeHooks()
    const actions = actionsWith("requests", fake)
    const subject = {kind: "bot", name: "SimpleX Directory", simplexName: "@directory.simplex", incognito: false}
    const contact = actions.create("a", {tool: "leave", input: {chat: "@4"}, subject, chat: "@4"})
    const group = actions.create("b", {tool: "leave", input: {chat: "#2"}, subject: {kind: "business", name: "ep", incognito: false}, chat: "#2"})
    await settled()
    expect(logLines).toEqual(["Action 1: delete bot SimpleX Directory (@4)", "Action 2: leave business ep (#2)"])
    expect(pendingLine(contact)).toMatch(/^1 {2}delete bot SimpleX Directory {2}expires \d\d:\d\d$/)
    expect(pendingLine(group)).toMatch(/^2 {2}leave business ep {2}expires \d\d:\d\d$/)
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

  it("ignores a standing approval", () => {
    const actions = actionsWith("host", fakeHooks())
    expect(actions.create("k", sendFields(), "Alice")).toMatchObject({state: "performing", standing: false, decidedBy: undefined})
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
