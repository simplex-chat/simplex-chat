import {Server} from "http"
import {join} from "path"
import {McpServer, McpServerFactory} from "@modelcontextprotocol/server"
import {T} from "@simplex-chat/types"
import {api} from "simplex-chat"
import {Action, Actions, Mode} from "./actions"
import {AudioOptions} from "./audio"
import {Control, controlDataOf, createControlChat, findControlGroup, hasOutcomeCard, isControlEvent, isGroupMessage, resetControlChat} from "./control"
import {bufferEvents, prepareCore, updatedUser} from "./core"
import {acquireLock, createDataFolder, releaseLock, serverToken, writeHeadersFile, writeServerFile} from "./files"
import {startHttp, stopHttp} from "./http"
import {ExitError, log, logError, shellPath} from "./log"
import {ChatItemsWatch} from "./messages"
import {AgentTools, instructions} from "./tools"
import {version} from "./version"

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
  stopWait: number
  audio?: AudioOptions
}

export interface RunningServer {
  token: string
  chat: api.ChatApi
  user: T.User
  actions: Actions
  stop(endWait?: AbortSignal): Promise<void>
}

export async function serve(options: ServeOptions): Promise<void> {
  const server = await startServer(options)
  const endWait = new AbortController()
  let stopping: Promise<void> | undefined
  const stop = (): void => {
    if (stopping !== undefined) return endWait.abort()
    stopping = server.stop(endWait.signal).then(
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
  const {name, data, audio} = options
  createDataFolder(data)
  acquireLock(data)
  try {
    const token = serverToken(data, options.newToken)
    writeHeadersFile(data, token)
    const chat = await api.ChatApi.init({type: "sqlite", filePrefix: join(data, "simplex")})
    log(`SimpleX MCP server ${version}, agent "${name}"`)
    log(`Data folder: ${data}`)
    log(`Approval mode: ${options.approve}`)
    if (audio !== undefined) log(`Audio: ${audio.url}, speech-to-text ${audio.stt}, text-to-speech ${audio.tts}${audio.voice === undefined ? "" : `, voice ${audio.voice}`}`)
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
  await prepareCore(chat, data)
  const events = bufferEvents(chat)
  await chat.startChat()
  const user = await updatedUser(chat, created, options.displayName ?? created.profile.displayName)
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
  const tools = new AgentTools({chat, user, actions, watch, controlChat: () => control.chatRef, standingApprover: (chatInfo, standing) => control.standingApprover(chatInfo, standing), refusedFile: (path, chatInfo, chat) => control.refusedFile(path, chatInfo, chat), data, audio: options.audio}, approve)
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
    if (event.type === "newChatItems") {
      for (const ci of event.chatItems) {
        if (isGroupMessage(ci, control.groupId)) control.onMessage(ci)
      }
    }
    tools.onEvent(event)
    if (event.type === "sndFileError") control.fileNotSent(event)
    if (isControlEvent(event, control.groupId)) {
      controlEvents = controlEvents.then(() => control.onEvent(event)).catch(error => logError(`${event.type} in the agent control chat`, error))
    }
  })
  return {
    token,
    chat,
    user,
    actions,
    async stop(endWait = new AbortController().signal) {
      stopped = true
      const cancelling = actions.stop()
      await stopHttp(server)
      const cancelled = await cancelling
      if (cancelled.length > 0) log(`Cancelled ${cancelled.length} action${cancelled.length === 1 ? "" : "s"}.`)
      await waitForCancelledCards(actions.lost ? [] : cancelled, options.stopWait, endWait)
      await chat.close()
      releaseLock(data)
      log("Stopped.")
    },
  }
}

export function registerHint({name, data, dataGiven}: ServeOptions): string {
  return `Add the agent to Claude Code:\n  npx -y @simplex-chat/mcp register ${name}${dataGiven ? ` --data ${shellPath(data)}` : ""}`
}

export async function waitForCancelledCards(stopped: Action[], seconds: number, endWait: AbortSignal): Promise<void> {
  const cards = stopped.filter(hasOutcomeCard).length
  if (cards === 0 || seconds === 0) return
  log(`Stopping: sending ${cards} cancelled card${cards === 1 ? "" : "s"}, up to ${seconds} s. Ctrl-C to stop now.`)
  await new Promise<void>(resolve => {
    const end = (): void => {
      clearTimeout(timer)
      endWait.removeEventListener("abort", end)
      resolve()
    }
    const timer = setTimeout(end, seconds * 1000)
    if (endWait.aborted) end()
    else endWait.addEventListener("abort", end, {once: true})
  })
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
