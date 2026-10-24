import {join} from "path"
import {T} from "@simplex-chat/types"
import {api, bot} from "simplex-chat"
import {DaemonClient} from "../../src/client"
import {createDataFolder, hubFolder, sessionFolder} from "../../src/files"
import {CallLine} from "../../src/protocol"

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

export async function profileWithServer(smpAddress: string, filePrefix: string, profile: T.Profile): Promise<void> {
  const chat = await api.ChatApi.init({type: "sqlite", filePrefix})
  await chat.apiCreateActiveUser(profile)
  await chat.startChat()
  expect(await chat.sendChatCmd(`/smp ${smpAddress}`)).toMatchObject({type: "cmdOk"})
  await chat.close()
}

export async function prepareRoot(smpAddress: string, root: string, sessions: {id: string; name: string}[]): Promise<void> {
  const folders = [{folder: hubFolder(root), displayName: "SimpleX MCP"}, ...sessions.map(({id, name}) => ({folder: sessionFolder(root, id), displayName: `SimpleX MCP - ${name}`}))]
  for (const {folder, displayName} of folders) {
    createDataFolder(folder)
    await profileWithServer(smpAddress, join(folder, "simplex"), {displayName, fullName: "", peerType: T.ChatPeerType.Agent})
  }
}

export async function callTool(client: DaemonClient, id: string, tool: string, input: object): Promise<{isError?: boolean; text: string; result: any}> {
  let result: any
  await client.stream("/call", {id, tool, input}, line => {
    const parsed = line as CallLine
    if ("result" in parsed) result = parsed.result
  })
  return {isError: result?.isError, text: result?.content[0].text, result: result?.structuredContent}
}

export async function person(smpAddress: string, filePrefix: string, displayName: string): Promise<{chat: api.ChatApi; user: T.User}> {
  await profileWithServer(smpAddress, filePrefix, {displayName, fullName: ""})
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
  return Number(/\/'approve (?:\d+:[#@]\d+ )?(\d+)'/.exec(ci.chatItem.meta.itemText)![1])
}

export function lines(ci: T.AChatItem): string[] {
  return ci.chatItem.meta.itemText.split("\n")
}
