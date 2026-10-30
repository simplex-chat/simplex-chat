import {T} from "@simplex-chat/types"
import {api, bot} from "simplex-chat"
import {ServeOptions} from "../../src/serve"

export const timeout = 60_000

export function captureLog(): string[] {
  const lines: string[] = []
  const write = process.stdout.write.bind(process.stdout) as (chunk: string) => boolean
  jest.spyOn(process.stdout, "write").mockImplementation(((chunk: string) => {
    lines.push(...String(chunk).split("\n"))
    return write(chunk)
  }) as typeof process.stdout.write)
  return lines
}

export async function profileWithServer(smpAddress: string, filePrefix: string, profile: T.Profile, xftpAddress?: string): Promise<void> {
  const chat = await api.ChatApi.init({type: "sqlite", filePrefix})
  await chat.apiCreateActiveUser(profile)
  await chat.startChat()
  expect(await chat.sendChatCmd(`/smp ${smpAddress}`)).toMatchObject({type: "cmdOk"})
  if (xftpAddress !== undefined) expect(await chat.sendChatCmd(`/xftp ${xftpAddress}`)).toMatchObject({type: "cmdOk"})
  await chat.close()
}

export async function person(smpAddress: string, filePrefix: string, displayName: string, xftpAddress?: string): Promise<{chat: api.ChatApi; user: T.User}> {
  await profileWithServer(smpAddress, filePrefix, {displayName, fullName: ""}, xftpAddress)
  const chat = await api.ChatApi.init({type: "sqlite", filePrefix})
  const user = (await chat.apiGetActiveUser())!
  await chat.startChat()
  return {chat, user}
}

export interface Bot {
  chat: api.ChatApi
  user: T.User
  link: string
}

export async function startBot(smpAddress: string, filePrefix: string, displayName: string, addressSettings: api.BotAddressSettings): Promise<Bot> {
  await profileWithServer(smpAddress, filePrefix, {displayName, fullName: "", peerType: T.ChatPeerType.Bot})
  const [chat, user, address] = await bot.run({
    profile: {displayName, fullName: "", shortDescr: "Find groups and bots."},
    dbOpts: {type: "sqlite", filePrefix},
    options: {addressSettings, commands: [{type: "command", keyword: "help", label: "Help"}], logContacts: false},
    onMessage: async (ci, content, botChat) => {
      await botChat.apiSendTextReply(ci, `result 1: ${content.text}`)
      await botChat.apiSendTextReply(ci, `result 2: ${content.text}`)
    },
    onCommands: {
      help: async (ci, _command, botChat) => {
        await botChat.apiSendTextReply(ci, "Send a search, for example: trail shoes")
      },
    },
  })
  return {chat, user, link: address!.connLinkContact.connShortLink ?? address!.connLinkContact.connFullLink}
}

export function serveOptions(data: string, port: number, extra: Partial<ServeOptions> = {}): ServeOptions {
  return {name: "agent", data, dataGiven: true, approve: "requests", expireHours: 23, resetOwners: false, newToken: false, port, qrCode: "small", ...extra}
}

export function lastLink(lines: string[]): string {
  return lines.filter(line => /^(https:\/\/|simplex:)/.test(line)).at(-1)!
}

export async function joinByLink({chat, user}: {chat: api.ChatApi; user: T.User}, link: string): Promise<number> {
  const joined = chat.wait("userJoinedGroup", timeout)
  const [, connLink] = await chat.apiConnectPlan(user.userId, link)
  await chat.apiConnect(user.userId, false, connLink)
  const event = await joined
  if (event === undefined) throw new Error("not joined")
  return event.groupInfo.groupId
}

export async function until(condition: () => boolean | Promise<boolean>, ms = timeout): Promise<void> {
  const deadline = Date.now() + ms
  while (!(await condition())) {
    if (Date.now() > deadline) throw new Error("condition timed out")
    await new Promise(resolve => setTimeout(resolve, 100))
  }
}

export type ItemCheck = (ci: T.AChatItem) => boolean

export function received(chat: api.ChatApi, check: ItemCheck): Promise<T.AChatItem> {
  return chat.wait("newChatItems", ({chatItems}) => chatItems.some(check), timeout).then(event => {
    if (event === undefined) throw new Error("no matching item")
    return event.chatItems.find(check)!
  })
}

export function updated(chat: api.ChatApi, check: ItemCheck): Promise<T.AChatItem> {
  return chat.wait("chatItemUpdated", ({chatItem}) => check(chatItem), timeout).then(event => {
    if (event === undefined) throw new Error("no matching update")
    return event.chatItem
  })
}

export const textStarts = (prefix: string): ItemCheck => ci => ci.chatItem.meta.itemText.startsWith(prefix)

export function cardNumber(ci: T.AChatItem): number {
  return Number(/\/'approve (?:[#@]\d+ )?(\d+)'/.exec(ci.chatItem.meta.itemText)![1])
}

export function lines(ci: T.AChatItem): string[] {
  return ci.chatItem.meta.itemText.split("\n")
}

export function isActive({memberStatus}: T.GroupMember): boolean {
  return memberStatus === T.GroupMemberStatus.Connected || memberStatus === T.GroupMemberStatus.Complete
}

export async function controlGroupOf(chat: api.ChatApi, userId: number): Promise<T.GroupInfo> {
  return (await chat.apiListGroups(userId)).find(({customData}) => (customData as {simplexMcp?: {control?: boolean}} | undefined)?.simplexMcp?.control === true)!
}
