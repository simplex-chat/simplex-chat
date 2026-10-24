import {ChatEvent, T} from "@simplex-chat/types"
import {api} from "simplex-chat"
import {Action, Actions} from "./actions"
import {Config} from "./config"
import {Control, Sessions, controlDataOf, createControlChat, findControlGroup, isControlEvent} from "./control"
import {deliveries, openCore, updatedUser} from "./core"
import {hubFolder} from "./files"
import {logError} from "./log"
import {OwnerState} from "./protocol"

const firstAction = 1

export type Perform = (action: Action, signal: AbortSignal) => Promise<void>

export class Hub {
  private stopped = false
  private controlEvents: Promise<void> = Promise.resolve()

  private constructor(
    private readonly chat: api.ChatApi,
    private user: T.User,
    readonly actions: Actions,
    readonly control: Control,
  ) {}

  static async open(root: string, config: Config, sessions: Sessions, perform: Perform): Promise<Hub> {
    const core = await openCore(hubFolder(root), config.mainGroup)
    try {
      const {chat, user} = core
      const group = (await findControlGroup(chat, user.userId)) ?? (await createControlChat(chat, user.userId, config.mainGroup, firstAction))
      const actions: Actions = new Actions(config.approve, config.expireHours, controlDataOf(group.customData)?.nextAction ?? firstAction, {
        refusal: action => control.refusal(action),
        saveCounter: next => control.saveCounter(next),
        postCard: action => control.postCard(action),
        outcome: action => control.outcome(action),
        perform,
      })
      const control: Control = new Control(chat, group, {displayName: config.mainGroup}, actions, sessions)
      await control.load()
      const hub = new Hub(chat, user, actions, control)
      await control.start()
      await core.events(event => hub.onEvent(event))
      return hub
    } catch (error) {
      await core.chat.close().catch(() => undefined)
      throw error
    }
  }

  get mainGroup(): string {
    return this.user.profile.displayName
  }

  owner(): OwnerState {
    const {link} = this.control
    return {owners: this.control.owners(), ...(link === undefined ? {} : {link})}
  }

  standingApprover(chatInfo: T.ChatInfo): string | undefined {
    return this.control.standingApprover(chatInfo)
  }

  async configure(config: Config): Promise<void> {
    this.actions.mode = config.approve
    this.actions.expireHours = config.expireHours
    if (config.mainGroup === this.mainGroup) return
    await this.serialize("Main group rename", async () => {
      await this.control.rename(config.mainGroup)
      this.user = await updatedUser(this.chat, this.user, config.mainGroup)
    })
  }

  resetOwners(): Promise<string> {
    return this.serialize("owner reset", () => this.control.reset(this.user.userId))
  }

  async stop(): Promise<number> {
    this.stopped = true
    const cancelled = await this.actions.stop()
    await this.controlEvents
    await deliveries(this.chat, this.control.groupId)
    await this.chat.close()
    return cancelled
  }

  private serialize<R>(context: string, work: () => Promise<R>): Promise<R> {
    const result = this.controlEvents.then(work)
    this.controlEvents = result.then(() => undefined, error => logError(context, error))
    return result
  }

  private async onEvent(event: ChatEvent): Promise<void> {
    if (this.stopped) return
    const {control} = this
    if (event.type === "newChatItems") {
      for (const ci of event.chatItems) {
        if (ci.chatInfo.type === "group" && ci.chatInfo.groupInfo.groupId === control.groupId && ci.chatInfo.groupChatScope === undefined) control.onMessage(ci)
      }
      return
    }
    if (isControlEvent(event, control.groupId)) {
      this.controlEvents = this.controlEvents.then(() => control.onEvent(event)).catch(error => logError(`${event.type} in the Main group`, error))
    }
  }
}
