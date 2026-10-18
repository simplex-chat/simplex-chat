import * as qrcode from "qrcode"
import {Action, Mode, NewAction, State} from "../../src/actions"
import {cardText, qrCodeText} from "../../src/control"
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

describe("QR codes", () => {
  const link = "https://smp4.simplex.im/g#1ai9d8WS140_IxvPDYWLigJNZU-z7GsDUhp0JoJ7kbU"
  const cells = (line: string): string[] => [...line.matchAll(/\x1b\[[0-9;]+m[^\x1b]+/g)].map(([cell]) => cell)
  const visible = (line: string): string => line.replace(/\x1b\[[0-9;]*m/g, "")

  it("draws each module as two cells in large", () => {
    const lines = qrCodeText(link, "large").split("\n")
    expect(lines).toHaveLength(37)
    for (const line of lines) {
      expect(visible(line)).toHaveLength(74)
      expect(line.endsWith("\x1b[0m")).toBe(true)
    }
    for (const line of [...lines.slice(0, 2), ...lines.slice(-2)]) expect(cells(line)).toEqual(Array(37).fill("\x1b[47m  "))
    expect(cells(lines[2]).slice(0, 10)).toEqual([...Array(2).fill("\x1b[47m  "), ...Array(7).fill("\x1b[40m  "), "\x1b[47m  "])
  })

  it("draws two module rows per line with lower half blocks in small", () => {
    const lines = qrCodeText(link, "small").split("\n")
    expect(lines).toHaveLength(19)
    for (const line of lines) {
      expect(visible(line)).toHaveLength(37)
      expect(line.endsWith("\x1b[0m")).toBe(true)
      expect([...cells(line).slice(0, 2), ...cells(line).slice(-2)]).toEqual(Array(4).fill("\x1b[47m "))
    }
    for (const line of [lines[0], lines.at(-1)!]) expect(cells(line)).toEqual(Array(37).fill("\x1b[47m "))
    expect(cells(lines[1]).slice(2, 4)).toEqual(["\x1b[40m ", "\x1b[37;40m▄"])
    const {modules} = qrcode.create(link, {errorCorrectionLevel: "L"})
    const row = Array.from({length: 16}, (_, i) => 2 * i).find(r => Array.from({length: modules.size}, (_, c) => c).some(c => !modules.get(r, c) && modules.get(r + 1, c)))!
    const col = Array.from({length: modules.size}, (_, c) => c).find(c => !modules.get(row, c) && modules.get(row + 1, c))!
    expect(cells(lines[row / 2 + 1])[col + 2]).toBe("\x1b[30;47m▄")
  })
})
