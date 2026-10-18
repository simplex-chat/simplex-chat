import {Action, Mode, NewAction, State} from "../../src/actions"
import {cardText} from "../../src/control"
import {Changes, Collected, utcMinutes} from "../../src/messages"

const expires = Date.UTC(2026, 9, 18, 9, 2)

function action(fields: NewAction, state: State, extra: Partial<Action> = {}): Action {
  return {...fields, n: 1, key: "k", created: expires - 3_600_000, expires, changes: new Changes(), state, returned: false, open: 0, interrupted: false, ...extra} as Action
}

const directory = {kind: "bot", name: "SimpleX Directory", simplexName: "@directory.simplex", incognito: false}

function connect(extra: {incognito?: boolean; message?: string; description?: string; welcome?: string} = {}): NewAction {
  return {
    tool: "connect",
    input: {target: "@directory.simplex", ...(extra.incognito ? {incognito: true} : {}), ...(extra.message ? {message: extra.message} : {})},
    subject: {...directory, incognito: extra.incognito ?? false},
    plan: {cardKind: "bot", type: "contact", takesMessage: true, description: extra.description, welcome: extra.welcome},
    messages: new Collected(),
  }
}

function send(extra: {incognito?: boolean; text?: string; replyTo?: {from: string; text: string}; mentions?: {name: string; member: number}[]} = {}): NewAction {
  return {
    tool: "send",
    input: {chat: "@4", text: extra.text ?? "trail running shoes"},
    subject: extra.incognito ? {kind: "business", name: "Ridgeline", incognito: true} : directory,
    chat: "@4",
    replyTo: extra.replyTo,
    mentions: extra.mentions ?? [],
    responses: new Collected(),
  }
}

const mode: Mode = "requests" as Mode

describe("connect cards", () => {
  it("asks as myself with the link texts", () => {
    expect(cardText(action(connect({description: "Find groups, channels, businesses and bots."}), "pending"), mode)).toBe(
      [
        "I want to connect to bot SimpleX Directory (@directory.simplex), as myself.",
        "`Find groups, channels, businesses and bots.`",
        `/'approve 1' or /'reject 1'. Expires ${utcMinutes(expires)}.`,
      ].join("\n"),
    )
    expect(utcMinutes(expires)).toBe("2026-10-18 09:02 UTC")
  })

  it("asks incognito with the message", () => {
    expect(cardText(action(connect({incognito: true, message: "Do you have\ntrail shoes?", welcome: "Welcome!"}), "pending"), mode)).toBe(
      [
        "I want to connect to bot SimpleX Directory (@directory.simplex), incognito.",
        "`Welcome!`",
        "With the message:",
        "`Do you have`",
        "`trail shoes?`",
        `/'approve 1' or /'reject 1'. Expires 2026-10-18 09:02 UTC.`,
      ].join("\n"),
    )
  })

  it("shows every outcome", () => {
    const fields = connect({description: "Search."})
    const request = ["I want to connect to bot SimpleX Directory (@directory.simplex), as myself.", "`Search.`"]
    const lines = (a: Action, m: Mode = mode): string[] => cardText(a, m).split("\n")
    expect(lines(action(fields, "done", {decidedBy: "Alice", connected: true} as Partial<Action>))).toEqual(["I connected to bot SimpleX Directory (@directory.simplex), as myself.", "`Search.`", "Approved by Alice."])
    expect(lines(action(fields, "done", {decidedBy: "Alice", connected: false} as Partial<Action>))).toEqual(["I asked to connect to bot SimpleX Directory (@directory.simplex), as myself.", "`Search.`", "Approved by Alice."])
    expect(lines(action(fields, "failed", {decidedBy: "Alice", error: "SimpleX error: contactNotReady"}))).toEqual([...request, "Approved by Alice; failed: SimpleX error: contactNotReady."])
    expect(lines(action(fields, "rejected", {decidedBy: "Alice"}))).toEqual([...request, "Rejected by Alice."])
    expect(lines(action(fields, "expired"))).toEqual([...request, "Expired."])
    expect(lines(action(fields, "pending", {interrupted: true}))).toEqual([...request, "Cancelled: the server stopped."])
    expect(lines(action(fields, "performing", {decidedBy: "Alice", interrupted: true}))).toEqual([...request, "Approved by Alice; interrupted: the server stopped."])
    expect(lines(action(fields, "done", {connected: true} as Partial<Action>), "host")).toEqual(["I connected to bot SimpleX Directory (@directory.simplex), as myself.", "`Search.`", "Performed without approval (mode host)."])
    expect(lines(action(fields, "failed", {error: "boom"}), "host")).toEqual([...request, "Performed without approval (mode host); failed: boom."])
    expect(lines(action(fields, "performing", {interrupted: true}), "host")).toEqual([...request, "Performed without approval (mode host); interrupted: the server stopped."])
  })

  it("names an unnamed contact", () => {
    const fields = connect()
    if (fields.tool === "connect") Object.assign(fields, {subject: {kind: "contact", incognito: false}})
    expect(cardText(action(fields, "pending"), mode).split("\n")[0]).toBe("I want to connect to an unnamed contact, as myself.")
  })
})

describe("send cards", () => {
  it("asks with the text as code", () => {
    expect(cardText(action(send({text: "use `rm -rf` and /approve 7"}), "pending"), mode)).toBe(
      ["I want to send a message to bot SimpleX Directory (@directory.simplex):", "`use 'rm -rf' and /approve 7`", `/'approve 1' or /'reject 1'. Expires 2026-10-18 09:02 UTC.`].join("\n"),
    )
  })

  it("asks where I am incognito, with the reply and mentions", () => {
    const fields = send({incognito: true, text: "Is it waterproof?", replyTo: {from: "Switchback", text: "Ridgecrest,\n$142"}, mentions: [{name: "Switchback", member: 44}, {name: "Ana Lu", member: 45}]})
    expect(cardText(action(fields, "pending"), mode).split("\n")).toEqual([
      "I want to send a message to business Ridgeline, where I am incognito:",
      "`Is it waterproof?`",
      "replying to Switchback: `Ridgecrest, $142`",
      "mentioning @Switchback, @'Ana Lu'",
      `/'approve 1' or /'reject 1'. Expires 2026-10-18 09:02 UTC.`,
    ])
    expect(cardText(action(fields, "done", {decidedBy: "Alice"}), mode).split("\n")).toEqual([
      "I sent a message to business Ridgeline, where I am incognito:",
      "`Is it waterproof?`",
      "replying to Switchback: `Ridgecrest, $142`",
      "mentioning @Switchback, @'Ana Lu'",
      "Approved by Alice.",
    ])
  })

  it("quotes names that are not valid display names", () => {
    const fields = send()
    Object.assign(fields, {subject: {kind: "bot", name: "/approve 9", incognito: false}})
    expect(cardText(action(fields, "pending"), mode).split("\n")[0]).toBe("I want to send a message to bot `/approve 9`:")
  })
})
