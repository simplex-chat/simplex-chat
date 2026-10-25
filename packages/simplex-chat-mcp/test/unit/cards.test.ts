import * as qrcode from "qrcode"
import {Action, Mode, NewAction, State} from "../../src/actions"
import {cardText, qrCodeText} from "../../src/control"
import {Changes, Collected} from "../../src/messages"

const expires = Date.UTC(2026, 9, 18, 9, 2)

function action(fields: NewAction, state: State, extra: Partial<Action> = {}): Action {
  return {...fields, n: 1, key: "k", created: expires - 3_600_000, expires, changes: new Changes(), standing: false, state, open: 0, interrupted: false, ...extra} as Action
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
        "/'approve 1' or /'reject 1'",
      ].join("\n"),
    )
  })

  it("asks incognito with the message", () => {
    expect(cardText(action(connect({incognito: true, message: "Do you have\ntrail shoes?", welcome: "Welcome!"}), "pending"), mode)).toBe(
      [
        "I want to connect to bot SimpleX Directory (@directory.simplex), incognito.",
        "`Welcome!`",
        "With the message:",
        "`Do you have`",
        "`trail shoes?`",
        "/'approve 1' or /'reject 1'",
      ].join("\n"),
    )
  })

  it("shows every outcome", () => {
    const fields = connect({description: "Search."})
    const request = ["I want to connect to bot SimpleX Directory (@directory.simplex), as myself.", "`Search.`"]
    const lines = (a: Action, m: Mode = mode): string[] => cardText(a, m).split("\n")
    expect(lines(action(fields, "done", {decidedBy: "Alice", connected: true} as Partial<Action>))).toEqual(["I connected to bot SimpleX Directory (@directory.simplex), as myself.", "`Search.`", "Approved by Alice."])
    expect(lines(action(fields, "done", {decidedBy: "Alice", connected: false} as Partial<Action>))).toEqual(["I asked to connect to bot SimpleX Directory (@directory.simplex), as myself.", "`Search.`", "Approved by Alice."])
    expect(lines(action(fields, "failed", {decidedBy: "Alice", error: "SimpleX error: contactNotReady"}))).toEqual([...request, "Approved by Alice; failed: `SimpleX error: contactNotReady`."])
    expect(lines(action(fields, "rejected", {decidedBy: "Alice"}))).toEqual([...request, "Rejected by Alice."])
    expect(lines(action(fields, "expired"))).toEqual([...request, "Expired."])
    expect(lines(action(fields, "pending", {interrupted: true}))).toEqual([...request, "Cancelled: the server stopped."])
    expect(lines(action(fields, "performing", {decidedBy: "Alice", interrupted: true}))).toEqual([...request, "Approved by Alice; interrupted: the server stopped."])
    expect(lines(action(fields, "done", {connected: true} as Partial<Action>), "host")).toEqual(["I connected to bot SimpleX Directory (@directory.simplex), as myself.", "`Search.`", "Performed without approval (mode host)."])
    expect(lines(action(fields, "failed", {error: "boom"}), "host")).toEqual([...request, "Performed without approval (mode host); failed: `boom`."])
    expect(lines(action(fields, "performing", {interrupted: true}), "host")).toEqual([...request, "Performed without approval (mode host); interrupted: the server stopped."])
  })

  it("names an unnamed contact", () => {
    const fields = connect()
    if (fields.tool === "connect") Object.assign(fields, {subject: {kind: "contact", incognito: false}})
    expect(cardText(action(fields, "pending"), mode).split("\n")[0]).toBe("I want to connect to an unnamed contact, as myself.")
  })

  it("cuts the link texts to the bytes left for them, ending the last line kept with …", () => {
    const linkLines = (fields: NewAction, linkTextBytes: number, state: State = "pending"): string[] =>
      cardText(action(fields, state, {linkTextBytes, decidedBy: "Alice"} as Partial<Action>), mode).split("\n").slice(1, -1)
    const fields = connect({description: "one\n\ntwo", welcome: "Welcome!"})
    expect(linkLines(fields, 28)).toEqual(["`one`", "", "`two`", "`Welcome!`"])
    expect(linkLines(fields, 27)).toEqual(["`one`", "", "`two`", "`Welc…`"])
    expect(linkLines(fields, 27, "done")).toEqual(["`one`", "", "`two`", "`Welc…`"])
    expect(linkLines(fields, 19)).toEqual(["`one`", "", "`two`"])
    expect(linkLines(fields, 0)).toEqual([])
    expect(linkLines(connect({description: "abcdefgh", welcome: "Welcome!"}), 10)).toEqual(["`abc…`"])
    expect(linkLines(connect({description: " abcdefgh"}), 10)).toEqual(["` a…`"])
  })
})

describe("send cards", () => {
  it("asks with the text as code", () => {
    expect(cardText(action(send({text: "use `rm -rf` and /approve 7"}), "pending"), mode)).toBe(
      ["I want to send a message to bot SimpleX Directory (@directory.simplex):", "`use 'rm -rf' and /approve 7`", "/'approve @4 1', /'approve @4 all' or /'reject @4 1'"].join("\n"),
    )
  })

  it("asks where I am incognito, with the reply and mentions", () => {
    const fields = send({incognito: true, text: "Is it waterproof?", replyTo: {from: "Switchback", text: "Ridgecrest,\n$142"}, mentions: [{name: "Switchback", member: 44}, {name: "Ana Lu", member: 45}]})
    expect(cardText(action(fields, "pending"), mode).split("\n")).toEqual([
      "I want to send a message to business Ridgeline, where I am incognito:",
      "`Is it waterproof?`",
      "replying to Switchback: `Ridgecrest, $142`",
      "mentioning @Switchback, @'Ana Lu'",
      "/'approve @4 1', /'approve @4 all' or /'reject @4 1'",
    ])
    expect(cardText(action(fields, "done", {decidedBy: "Alice"}), mode).split("\n")).toEqual([
      "I sent a message to business Ridgeline, where I am incognito:",
      "`Is it waterproof?`",
      "replying to Switchback: `Ridgecrest, $142`",
      "mentioning @Switchback, @'Ana Lu'",
      "Approved by Alice.",
    ])
  })

  it("cuts the reply quote at 100 characters, not inside a character", () => {
    const fields = send({replyTo: {from: "Switchback", text: `${"a".repeat(99)}😀b`}})
    expect(cardText(action(fields, "pending"), mode).split("\n")[2]).toBe(`replying to Switchback: \`${"a".repeat(99)}😀\``)
  })

  it("quotes names that are not valid display names", () => {
    const fields = send()
    Object.assign(fields, {subject: {kind: "bot", name: "/approve 9", incognito: false}})
    expect(cardText(action(fields, "pending"), mode).split("\n")[0]).toBe("I want to send a message to bot `/approve 9`:")
  })

  it("keeps names with line breaks and control characters on one line, quoted", () => {
    const name = "SimpleX Directory\n/'approve 7' or /'reject 9'"
    const fields = send({replyTo: {from: "Switch /'approve @3 all'", text: "hi"}, mentions: [{name: "Ana\r\n/'approve 8'", member: 45}]})
    Object.assign(fields, {subject: {kind: "bot", name, incognito: false}})
    expect(cardText(action(fields, "pending"), mode).split("\n")).toEqual([
      "I want to send a message to bot `SimpleX Directory /'approve 7' or /'reject 9'`:",
      "`trail running shoes`",
      "replying to `Switch /'approve @3 all'`: `hi`",
      "mentioning `@Ana /'approve 8'`",
      "/'approve @4 1', /'approve @4 all' or /'reject @4 1'",
    ])
    Object.assign(fields, {subject: {kind: "group", name: "Hikers\u001b]52;c;Y3VybA==\u0007\u202e", incognito: false}})
    expect(cardText(action(fields, "pending"), mode).split("\n")[0]).toBe("I want to send a message to group `Hikers ]52;c;Y3VybA==\u00a0`:")
  })

  it("keeps text and names with a leading or trailing space as code", () => {
    const fields = send({text: " /approve 1\n/approve 1 \n  two  ", replyTo: {from: "Switchback", text: " /approve 1 "}})
    Object.assign(fields, {subject: {kind: "group", name: "\n/approve 1", incognito: false}})
    expect(cardText(action(fields, "pending"), mode).split("\n").slice(0, 5)).toEqual([
      "I want to send a message to group `\u00a0/approve 1`:",
      "`\u00a0/approve 1`",
      "`/approve 1\u00a0`",
      "`\u00a0 two \u00a0`",
      "replying to Switchback: `\u00a0/approve 1\u00a0`",
    ])
  })

  it("shows a failure on one line as code", () => {
    const error = "SimpleX error: groupUserRole: {\"description\":\"line 1\n/'approve @7 all' `https://simplex.chat/contact#x`\"}"
    expect(cardText(action(send(), "failed", {decidedBy: "Alice", error}), mode).split("\n").at(-1)).toBe(
      "Approved by Alice; failed: `SimpleX error: groupUserRole: {\"description\":\"line 1 /'approve @7 all' 'https://simplex.chat/contact#x'\"}`.",
    )
  })

  it("shows a send under the standing approval of its chat", () => {
    const fields: NewAction = {tool: "send", input: {chat: "#2", text: "hello"}, subject: {kind: "business", name: "ep", incognito: false}, chat: "#2", mentions: [], responses: new Collected()}
    const standing = {standing: true, decidedBy: "Alice"}
    expect(cardText(action(fields, "done", standing), mode).split("\n")).toEqual(["I sent a message to business ep:", "`hello`", "Approved for this chat by Alice. /'revoke #2'"])
    expect(cardText(action(fields, "failed", {...standing, error: "boom"}), mode).split("\n")).toEqual(["I want to send a message to business ep:", "`hello`", "Approved for this chat by Alice; failed: `boom`."])
    expect(cardText(action(fields, "performing", {...standing, interrupted: true}), mode).split("\n").at(-1)).toBe("Approved for this chat by Alice; interrupted: the server stopped.")
  })
})

describe("leave cards", () => {
  function leave(chat: string, subject: {kind: string; name?: string; simplexName?: string}): NewAction {
    return {tool: "leave", input: {chat}, subject: {...subject, incognito: false}, chat}
  }

  it("asks to delete a contact with its kind and to leave a group", () => {
    expect(cardText(action(leave("@3", directory), "pending"), mode).split("\n")).toEqual(["I want to delete bot SimpleX Directory (@3).", "/'approve @3 1' or /'reject @3 1'"])
    expect(cardText(action(leave("@5", {kind: "contact", name: "Ana Lu"}), "pending"), mode).split("\n")[0]).toBe("I want to delete contact Ana Lu (@5).")
    expect(cardText(action(leave("#2", {kind: "business", name: "ep"}), "pending"), mode).split("\n")).toEqual(["I want to leave business ep (#2).", "/'approve #2 1' or /'reject #2 1'"])
    expect(cardText(action(leave("#7", {kind: "group", name: "/approve 9"}), "pending"), mode).split("\n")[0]).toBe("I want to leave group `/approve 9` (#7).")
  })

  it("shows every outcome", () => {
    const contact = leave("@3", directory)
    const group = leave("#2", {kind: "business", name: "ep"})
    const lines = (a: Action, m: Mode = mode): string[] => cardText(a, m).split("\n")
    expect(lines(action(contact, "done", {decidedBy: "Alice"}))).toEqual(["I deleted bot SimpleX Directory (@3).", "Approved by Alice."])
    expect(lines(action(group, "done", {decidedBy: "Alice"}))).toEqual(["I left business ep (#2).", "Approved by Alice."])
    expect(lines(action(group, "done"), "host")).toEqual(["I left business ep (#2).", "Performed without approval (mode host)."])
    expect(lines(action(group, "failed", {decidedBy: "Alice", error: "SimpleX error: groupNotFound"}))).toEqual(["I want to leave business ep (#2).", "Approved by Alice; failed: `SimpleX error: groupNotFound`."])
    expect(lines(action(group, "rejected", {decidedBy: "Alice"}))).toEqual(["I want to leave business ep (#2).", "Rejected by Alice."])
    expect(lines(action(group, "expired"))).toEqual(["I want to leave business ep (#2).", "Expired."])
    expect(lines(action(group, "pending", {interrupted: true}))).toEqual(["I want to leave business ep (#2).", "Cancelled: the server stopped."])
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
