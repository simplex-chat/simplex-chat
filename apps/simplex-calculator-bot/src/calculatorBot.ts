import {T} from "@simplex-chat/types"
import {api, bot, util} from "simplex-chat"
import {Calc, Update, calculatorText, initialCalc, keyNames, press, textInput} from "./calculator.js"
import {calculatorIcon} from "./icon.js"

const anyTextCommandsVersion = 21
const idleMinutes = 10

const welcomeMessage = `Tap the keys, or send numbers, keys like + or =, and expressions like 12 × 3 + 4.\nKeys are applied left to right, as on a pocket calculator.\nThe calculator turns off after ${idleMinutes} minutes, any key turns it on.`
const hint = "Send a number, a key like + or =, or an expression like 12 × 3 + 4."

interface Session {
  calc?: Calc
  itemId: number
  symbolKeys: boolean
  timer?: NodeJS.Timeout
}

interface Sender {
  groupId: number
  member: T.GroupMember
}

const sessions = new Map<number, Session>()

function groupSender({chatInfo, chatItem}: T.AChatItem): Sender | undefined {
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
  const timer = setTimeout(() => turnOff(chat, groupId, itemId, symbolKeys), idleMinutes * 60_000)
  sessions.set(groupId, {calc, itemId, symbolKeys, timer})
  if (previous) {
    clearTimeout(previous.timer)
    await chat.apiDeleteChatItems(T.ChatType.Group, groupId, [previous.itemId], T.CIDeleteMode.Broadcast)
  }
}

function turnOff(chat: api.ChatApi, groupId: number, itemId: number, symbolKeys: boolean): void {
  sessions.set(groupId, {itemId, symbolKeys})
  chat.apiUpdateChatItem(T.ChatType.Group, groupId, itemId, {type: "text", text: calculatorText(undefined, symbolKeys)}, false)
    .catch(e => console.log("error turning calculator off", e))
}

async function updateCalculator(chat: api.ChatApi, {groupId, member}: Sender, update: Update): Promise<void> {
  const [calc, logLine] = update(currentCalc(groupId))
  if (logLine) await chat.apiSendTextMessage([T.ChatType.Group, groupId], logLine)
  await showCalculator(chat, groupId, member, calc)
}

function tapCommand(update: Update) {
  return async (ci: T.AChatItem, _command: util.BotCommand, chat: api.ChatApi): Promise<void> => {
    const sender = groupSender(ci)
    if (!sender) return
    await chat.apiDeleteMemberChatItem(sender.groupId, [ci.chatItem.meta.itemId])
    await updateCalculator(chat, sender, update)
  }
}

const keyCommands = Object.fromEntries([...keyNames].map(([name, key]) => [name, tapCommand(calc => press(calc, key))]))

async function onMessage(ci: T.AChatItem, content: T.MsgContent, chat: api.ChatApi): Promise<void> {
  const sender = groupSender(ci)
  if (!sender || content.type !== "text") return
  const update = textInput(content.text)
  if (update) await updateCalculator(chat, sender, update)
  else await chat.apiSendTextReply(ci, hint)
}

export function runCalculatorBot(dbOpts: bot.BotDbOpts): Promise<[api.ChatApi, T.User, T.UserContactLink | undefined]> {
  return bot.run({
    profile: {displayName: "SimpleX Calculator", fullName: "", image: calculatorIcon, preferences: {fullDelete: {allow: T.FeatureAllowed.Yes}}},
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
