import {Server} from "http"
import {join} from "path"
import {McpServer, McpServerFactory} from "@modelcontextprotocol/server"
import {ChatEvent, T} from "@simplex-chat/types"
import {api, util} from "simplex-chat"
import {Actions, Mode} from "./actions"
import {Control, controlDataOf, createControlChat, findControlGroup, isControlEvent, resetControlChat} from "./control"
import {acquireLock, createDataFolder, releaseLock, serverToken, writeHeadersFile, writeServerFile} from "./files"
import {startHttp, stopHttp} from "./http"
import {ExitError, log, logError, shellPath} from "./log"
import {ChatItemsWatch, chatCommand} from "./messages"
import {AgentTools, instructions} from "./tools"

export const version: string = require("../package.json").version

const deliveryTimeout = 10_000
const deliveryPoll = 100

export type QrCodeSize = "small" | "large"

export interface OwnerEntry {
  memberId: number
  name: string
}

export interface ServeOptions {
  name: string
  data: string
  dataGiven: boolean
  displayName?: string
  approve: Mode
  owners?: OwnerEntry[]
  expireHours: number
  resetOwners: boolean
  newToken: boolean
  port: number
  qrCode: QrCodeSize
}

export interface RunningServer {
  token: string
  chat: api.ChatApi
  user: T.User
  actions: Actions
  stop(): Promise<void>
}

export async function serve(options: ServeOptions): Promise<void> {
  const server = await startServer(options)
  let stopping: Promise<void> | undefined
  const stop = (): void => {
    stopping ??= server.stop().then(
      () => process.exit(0),
      error => {
        logError("stop", error)
        process.exit(1)
      },
    )
  }
  process.on("SIGINT", stop)
  process.on("SIGTERM", stop)
}

export async function startServer(options: ServeOptions): Promise<RunningServer> {
  const {name, data} = options
  createDataFolder(data)
  acquireLock(data)
  try {
    const token = serverToken(data, options.newToken)
    writeHeadersFile(data, token)
    const chat = await api.ChatApi.init({type: "sqlite", filePrefix: join(data, "simplex")})
    log(`SimpleX MCP server ${version}, agent "${name}"`)
    log(`Data folder: ${data}`)
    log(`Approval mode: ${options.approve}`)
    try {
      return await startAgent(chat, options, token)
    } catch (error) {
      await chat.close().catch(() => undefined)
      throw error
    }
  } catch (error) {
    releaseLock(data)
    throw error
  }
}

async function startAgent(chat: api.ChatApi, options: ServeOptions, token: string): Promise<RunningServer> {
  const {name, data, port, approve, expireHours} = options
  const created = (await chat.apiGetActiveUser()) ?? (await chat.apiCreateActiveUser({displayName: options.displayName ?? name, fullName: "", peerType: T.ChatPeerType.Agent}))
  await chatCommand(chat, "/network socks=off smp-proxy=always smp-proxy-fallback=protected", "networkConfig")
  const events = bufferEvents(chat)
  await chat.startChat()
  const user = await updatedUser(chat, created, options.displayName)
  const displayName = user.profile.displayName
  const group = await controlGroup(chat, user.userId, displayName, options.resetOwners)
  const actions: Actions = new Actions(approve, expireHours, controlDataOf(group.customData)?.nextAction ?? 1, {
    refusal: action => control.refusal(action),
    saveCounter: next => control.saveCounter(next),
    postCard: action => control.postCard(action),
    outcome: action => control.outcome(action),
    perform: (action, signal) => tools.perform(action, signal),
  })
  const watch = new ChatItemsWatch()
  const tools = new AgentTools({chat, user, actions, watch, controlChat: () => control.chatRef, standingApprover: chatInfo => control.standingApprover(chatInfo)}, approve)
  const control: Control = new Control(chat, group, {displayName, owners: options.owners, registerHint: registerHint(options), qrCode: options.qrCode}, actions, tools)
  await control.load()
  const factory = (): McpServer => {
    const mcp = new McpServer({name: "simplex-mcp", version}, {instructions: instructions(displayName)})
    tools.register(mcp)
    return mcp
  }
  let started = false
  const server = await listen(port, token, factory, () => started)
  writeServerFile(data, {name, port, expireHours})
  try {
    await control.start()
  } catch (error) {
    await stopHttp(server)
    throw error
  }
  started = true
  log(`Listening on http://127.0.0.1:${port}/mcp`)
  let controlEvents = Promise.resolve()
  let stopped = false
  await events(async event => {
    if (stopped) return
    switch (event.type) {
      case "newChatItems":
        for (const ci of event.chatItems) {
          if (ci.chatInfo.type === "group" && ci.chatInfo.groupInfo.groupId === control.groupId && ci.chatInfo.groupChatScope === undefined) control.onMessage(ci)
        }
        watch.deliver(event.chatItems)
        return
      case "chatItemUpdated":
        return watch.update(event.chatItem)
      case "chatItemsDeleted":
        return watch.remove(event.chatItemDeletions.map(({deletedChatItem}) => deletedChatItem))
      case "contactConnected":
        return tools.onContactConnected(event)
      case "userJoinedGroup":
        return tools.onUserJoinedGroup(event)
    }
    if (isControlEvent(event, control.groupId)) {
      controlEvents = controlEvents.then(() => control.onEvent(event)).catch(error => logError(`${event.type} in the agent control chat`, error))
    }
  })
  return {
    token,
    chat,
    user,
    actions,
    async stop() {
      stopped = true
      const cancelling = actions.stop()
      await stopHttp(server)
      const cancelled = await cancelling
      if (cancelled > 0) log(`Cancelled ${cancelled} action${cancelled === 1 ? "" : "s"}.`)
      await deliveries(chat, control.groupId)
      await chat.close()
      releaseLock(data)
      log("Stopped.")
    },
  }
}

export function registerHint({name, data, dataGiven}: ServeOptions): string {
  return `Add the agent to Claude Code:\n  npx -y @simplex-chat/mcp register ${name}${dataGiven ? ` --data ${shellPath(data)}` : ""}`
}

export async function deliveries(chat: api.ChatApi, groupId: number): Promise<void> {
  const connIds = (await sqlRows(chat, `/sql chat SELECT hex(agent_conn_id) FROM connections WHERE group_member_id IN (SELECT group_member_id FROM group_members WHERE group_id = ${groupId})`)).slice(1)
  if (connIds.length === 0) return
  const pending = `/sql agent SELECT count(*) FROM snd_message_deliveries WHERE failed = 0 AND hex(conn_id) IN (${connIds.map(id => `'${id}'`).join(",")})`
  const deadline = Date.now() + deliveryTimeout
  while (Date.now() < deadline && Number((await sqlRows(chat, pending)).at(-1) ?? 0) > 0) {
    await new Promise(resolve => setTimeout(resolve, deliveryPoll))
  }
}

async function sqlRows(chat: api.ChatApi, query: string): Promise<string[]> {
  try {
    const {rows} = (await chat.sendChatCmd(query)) as unknown as {rows?: string[]}
    return rows ?? []
  } catch (error) {
    logError("delivery wait", error)
    return []
  }
}

async function updatedUser(chat: api.ChatApi, user: T.User, displayName: string | undefined): Promise<T.User> {
  const {profile} = user
  if ((displayName === undefined || displayName === profile.displayName) && profile.peerType === T.ChatPeerType.Agent) return user
  await chat.apiUpdateProfile(user.userId, {...util.fromLocalProfile(profile), displayName: displayName ?? profile.displayName, peerType: T.ChatPeerType.Agent})
  return (await chat.apiGetActiveUser())!
}

async function controlGroup(chat: api.ChatApi, userId: number, displayName: string, resetOwners: boolean): Promise<T.GroupInfo> {
  const group = await findControlGroup(chat, userId)
  if (group === undefined) return createControlChat(chat, userId, displayName, 1)
  const nextAction = controlDataOf(group.customData)?.nextAction ?? 1
  return resetOwners ? resetControlChat(chat, userId, group, displayName, nextAction) : group
}

async function listen(port: number, token: string, factory: McpServerFactory, ready: () => boolean): Promise<Server> {
  try {
    return await startHttp({port, token, factory, ready})
  } catch (error) {
    if ((error as NodeJS.ErrnoException).code === "EADDRINUSE") throw new ExitError(`Port ${port} is in use. Start with --port N.`, 1)
    throw error
  }
}

function bufferEvents(chat: api.ChatApi): (handler: (event: ChatEvent) => Promise<void>) => Promise<void> {
  const buffered: ChatEvent[] = []
  let eventHandler: ((event: ChatEvent) => Promise<void>) | undefined
  chat.onAny(async event => {
    if (eventHandler) await eventHandler(event)
    else buffered.push(event)
  })
  return async handler => {
    for (let event = buffered.shift(); event !== undefined; event = buffered.shift()) {
      const {type} = event
      await handler(event).catch(error => logError(`${type} event`, error))
    }
    eventHandler = handler
  }
}
