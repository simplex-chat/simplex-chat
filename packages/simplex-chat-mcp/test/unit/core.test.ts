import {T} from "@simplex-chat/types"
import {api} from "simplex-chat"
import {deletePresetContacts, updatedUser} from "../../src/core"
import {FakeChat, contact, direct, user} from "./fixtures"

beforeEach(() => {
  jest.spyOn(process.stdout, "write").mockImplementation(() => true)
})

afterEach(() => jest.restoreAllMocks())

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

describe("display name and peer type of the profile at each start", () => {
  const stored = {...user, profile: {...user.profile, displayName: "User", fullName: "", peerType: T.ChatPeerType.Agent}} as T.User

  function profileChat(): {chat: api.ChatApi; updates: T.Profile[]} {
    const updates: T.Profile[] = []
    let current = stored
    const chat = {
      apiUpdateProfile: async (_userId: number, profile: T.Profile) => {
        updates.push(profile)
        current = {...current, profile: {...current.profile, ...profile}}
      },
      apiGetActiveUser: async () => current,
    }
    return {chat: chat as unknown as api.ChatApi, updates}
  }

  it("updates a profile whose peer type differs", async () => {
    const {chat, updates} = profileChat()
    const updated = await updatedUser(chat, stored, "User", T.ChatPeerType.Human)
    expect(updates).toEqual([expect.objectContaining({displayName: "User", peerType: T.ChatPeerType.Human})])
    expect(updated.profile.peerType).toBe(T.ChatPeerType.Human)
  })

  it("keeps a profile with its name and peer type", async () => {
    const {chat, updates} = profileChat()
    expect(await updatedUser(chat, stored, "User", T.ChatPeerType.Agent)).toBe(stored)
    expect(updates).toEqual([])
  })
})
