import {ChatEvent, T} from "@simplex-chat/types"
import {Actions} from "../../src/actions"
import {Control} from "../../src/control"
import {Hub} from "../../src/hub"
import {Collected} from "../../src/messages"
import {FakeChat, actionsWith, fakeHooks, settled, user} from "./fixtures"

type HubConstructor = new (chat: FakeChat, user: T.User, actions: Actions, control: Pick<Control, "groupId">) => Hub

function hubWith(actions: Actions): {onEvent(event: ChatEvent): Promise<void>} {
  return new (Hub as unknown as HubConstructor)(new FakeChat(), user, actions, {groupId: 1}) as unknown as {onEvent(event: ChatEvent): Promise<void>}
}

beforeEach(() => {
  jest.spyOn(process.stdout, "write").mockImplementation(() => true)
})

afterEach(() => {
  jest.restoreAllMocks()
  jest.useRealTimers()
})

describe("hub events", () => {
  it("expire the actions whose expiry passed while the computer slept, and edit each card once", async () => {
    jest.useFakeTimers()
    const fake = fakeHooks()
    const actions = actionsWith(fake, 1, 1)
    const subject = {kind: "bot", name: "SimpleX Directory", incognito: false}
    const action = actions.create(1, "requests", "k", {tool: "send", input: {chat: "@4", text: "hi"}, subject, chat: "@4", mentions: [], responses: new Collected()})
    await settled()
    jest.setSystemTime(Date.now() + 3_600_000)
    expect(action.state).toBe("pending")
    await hubWith(actions).onEvent({type: "hostConnected"} as unknown as ChatEvent)
    await settled()
    expect(action.state).toBe("expired")
    jest.advanceTimersByTime(3_600_000)
    await settled()
    expect(fake.calls.filter(call => call.startsWith("outcome"))).toEqual(["outcome 1 expired"])
  })
})
