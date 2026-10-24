import {T} from "@simplex-chat/types"
import {core} from "simplex-chat"
import {deletePresetContacts, deliveries} from "../../src/core"
import {FakeChat, contact, direct, user} from "./fixtures"

beforeEach(() => {
  jest.spyOn(process.stdout, "write").mockImplementation(() => true)
})

afterEach(() => jest.restoreAllMocks())

describe("delivery wait at the stop", () => {
  it("logs an error of the SQL query and ends", async () => {
    const errors: string[] = []
    jest.spyOn(process.stderr, "write").mockImplementation(text => (errors.push(String(text).trimEnd()), true))
    const chat = new FakeChat()
    chat.sendChatCmd = async () => {
      throw new core.ChatAPIError("Chat command error", {type: "errorStore", storeError: {type: "dBException", message: "no such table: connections"}} as unknown as T.ChatError)
    }
    await deliveries(chat.api(), 10)
    expect(errors).toEqual(['delivery wait: SimpleX error: errorStore: dBException: {"message":"no such table: connections"}'])
  })
})

describe("preset contact of a new profile", () => {
  const preset = (contactId: number): T.Contact => ({...contact(contactId, "Ask SimpleX Team"), activeConn: undefined})

  function chatWith(...contacts: T.Contact[]): FakeChat {
    const chat = new FakeChat()
    for (const c of contacts) chat.chats.set(`@${c.contactId}`, {chatInfo: direct(c), items: []})
    return chat
  }

  it("deletes the contact without a connection, silently", async () => {
    const chat = chatWith(preset(1))
    await deletePresetContacts(chat.api(), user.userId)
    expect(chat.deleted).toEqual([{chatType: T.ChatType.Direct, chatId: 1, mode: {type: "full", notify: false}}])
  })

  it("keeps a connected contact with the same name and other contacts", async () => {
    const chat = chatWith(contact(2, "Ask SimpleX Team"), contact(3, "Alice"), {...preset(4), profile: {...preset(4).profile, displayName: "Bob"}})
    await deletePresetContacts(chat.api(), user.userId)
    expect(chat.deleted).toEqual([])
  })

  it("logs a failed deletion and goes on with the rest", async () => {
    const errors: string[] = []
    jest.spyOn(process.stderr, "write").mockImplementation(text => (errors.push(String(text).trimEnd()), true))
    const chat = chatWith(preset(1), preset(5))
    const remove = chat.apiDeleteChat.bind(chat)
    chat.apiDeleteChat = async (type, id, mode) => {
      if (id === 1) throw new Error("db locked")
      await remove(type, id, mode)
    }
    await deletePresetContacts(chat.api(), user.userId)
    expect(chat.deleted.map(({chatId}) => chatId)).toEqual([5])
    expect(errors).toEqual(["preset contact 1: db locked"])
  })
})
