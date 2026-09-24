import {T} from "@simplex-chat/types"
import {api, bot, util} from "simplex-chat"
import {Calc, calculatorText, evaluate, initialCalc, keyWord, keypad, press, textKeys} from "./calculator.js"

const anyTextCommandsVersion = 21
const idleMinutes = 10

const welcomeMessage = `Tap the keys, or send an expression like 12 × 3 + 4.\nKeys are applied left to right, as on a pocket calculator.\nThe calculator turns off after ${idleMinutes} minutes.`
const offText = "*Off*\nTap /calc or send an expression."
const hint = "Send a number or an expression like 12 × 3 + 4."

interface Session {
  calc: Calc
  itemId: number
  timer: NodeJS.Timeout
}

const sessions = new Map<number, Session>()

function groupSender({chatInfo, chatItem}: T.AChatItem): {groupId: number, member: T.GroupMember} | undefined {
  return chatInfo.type === "group" && chatItem.chatDir.type === "groupRcv"
    ? {groupId: chatInfo.groupInfo.groupId, member: chatItem.chatDir.groupMember}
    : undefined
}

function currentCalc(groupId: number): Calc {
  return sessions.get(groupId)?.calc ?? initialCalc
}

async function showCalculator(chat: api.ChatApi, groupId: number, member: T.GroupMember, calc: Calc): Promise<void> {
  const symbolKeys = member.memberChatVRange.maxVersion >= anyTextCommandsVersion
  const [sent] = await chat.apiSendTextMessage([T.ChatType.Group, groupId], calculatorText(calc, symbolKeys))
  const itemId = sent.chatItem.meta.itemId
  const previous = sessions.get(groupId)
  sessions.set(groupId, {calc, itemId, timer: setTimeout(() => turnOff(chat, groupId, itemId), idleMinutes * 60_000)})
  if (previous) {
    clearTimeout(previous.timer)
    await chat.apiDeleteChatItems(T.ChatType.Group, groupId, [previous.itemId], T.CIDeleteMode.Broadcast)
  }
}

function turnOff(chat: api.ChatApi, groupId: number, itemId: number): void {
  sessions.delete(groupId)
  chat.apiUpdateChatItem(T.ChatType.Group, groupId, itemId, {type: "text", text: offText}, false)
    .catch(e => console.log("error turning calculator off", e))
}

function tapCommand(update: (calc: Calc) => [Calc, string?]) {
  return async (ci: T.AChatItem, _command: util.BotCommand, chat: api.ChatApi): Promise<void> => {
    const sender = groupSender(ci)
    if (!sender) return
    await chat.apiDeleteMemberChatItem(sender.groupId, [ci.chatItem.meta.itemId])
    const [calc, logLine] = update(currentCalc(sender.groupId))
    if (logLine) await chat.apiSendTextMessage([T.ChatType.Group, sender.groupId], logLine)
    await showCalculator(chat, sender.groupId, sender.member, calc)
  }
}

const keyCommands = Object.fromEntries(
  keypad.flat().flatMap(key => [key, keyWord(key)].map(keyword => [keyword, tapCommand(calc => press(calc, key))]))
)

async function onMessage(ci: T.AChatItem, content: T.MsgContent, chat: api.ChatApi): Promise<void> {
  const sender = groupSender(ci)
  if (!sender || content.type !== "text") return
  const keys = textKeys(content.text)
  if (keys) await showCalculator(chat, sender.groupId, sender.member, evaluate(keys))
  else await chat.apiSendTextReply(ci, hint)
}

export function runCalculatorBot(dbOpts: bot.BotDbOpts): Promise<[api.ChatApi, T.User, T.UserContactLink | undefined]> {
  return bot.run({
    profile: {displayName: "SimpleX Calculator", fullName: "", preferences: {fullDelete: {allow: T.FeatureAllowed.Yes}}},
    dbOpts,
    options: {
      addressSettings: {businessAddress: true, welcomeMessage},
      commands: [{type: "command", keyword: "calc", label: "Show calculator"}],
    },
    onMessage,
    onCommands: {
      ...keyCommands,
      calc: tapCommand(calc => [calc]),
      "": async (ci, _command, chat) => { await chat.apiSendTextReply(ci, hint) },
    },
    events: {
      joinedGroupMember: ({groupInfo, member}, chat) => showCalculator(chat, groupInfo.groupId, member, initialCalc),
    },
  })
}
