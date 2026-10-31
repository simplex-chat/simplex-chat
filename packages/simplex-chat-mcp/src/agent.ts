import {basename} from "path"
import {T} from "@simplex-chat/types"
import {api} from "simplex-chat"
import {Actions, Mode} from "./actions"
import {Core, openCore, updatedUser} from "./core"
import {ChatItemsWatch, validName} from "./messages"
import {AgentTools} from "./tools"

const profilePrefix = "SimpleX MCP - "
const unnamedSession = "Claude Code"

export interface SessionAgent {
  readonly number: number
  readonly chat: api.ChatApi
  readonly tools: AgentTools
  readonly displayName: string
  readonly mode: Mode
  rename(name: string): Promise<void>
  close(): Promise<void>
}

export interface SessionAgentOptions {
  folder: string
  number: number
  name: string
  mode: () => Mode
  actions: Actions
  standingApprover: (chatInfo: T.ChatInfo, mode: Mode) => string | undefined
}

export function sessionName(title: string | undefined, project: string): string {
  const named = title?.trim()
  if (named) return named
  return basename(project) || unnamedSession
}

export function profileName(name: string): string {
  return validName(`${profilePrefix}${name}`)
}

export async function openSessionAgent({folder, number, name, mode, actions, standingApprover}: SessionAgentOptions): Promise<SessionAgent> {
  const core: Core = await openCore(folder, profileName(name))
  const {chat, user} = core
  const watch = new ChatItemsWatch()
  const tools = new AgentTools({chat, user, actions, watch, session: number, mode, standingApprover: chatInfo => standingApprover(chatInfo, mode())})
  await core.events(async event => {
    switch (event.type) {
      case "newChatItems":
        return watch.deliver(event.chatItems)
      case "chatItemUpdated":
        return watch.update(event.chatItem)
      case "chatItemsDeleted":
        return watch.remove(event.chatItemDeletions.map(({deletedChatItem}) => deletedChatItem))
      case "contactConnected":
        return tools.onContactConnected(event)
      case "userJoinedGroup":
        return tools.onUserJoinedGroup(event)
    }
  })
  const agent: SessionAgent = {
    number,
    chat,
    tools,
    get displayName() {
      return user.profile.displayName
    },
    get mode() {
      return mode()
    },
    async rename(newName) {
      const renamed = await updatedUser(chat, user, profileName(newName))
      Object.assign(user.profile, renamed.profile)
    },
    close: () => chat.close(),
  }
  return agent
}
