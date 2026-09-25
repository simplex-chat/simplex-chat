import {T} from "@simplex-chat/types"
import {api, bot, util} from "simplex-chat"
import {Calc, Update, calculatorText, initialCalc, keyNames, press, textInput} from "./calculator.js"
import {calculatorIcon} from "./icon.js"

const anyTextCommandsVersion = 21
const idleMinutes = 10

const welcomeMessage = `Tap the keys or send numbers, keys like + and =, or expressions like (2 + 3) × 4.
The calculator turns off after ${idleMinutes} minutes; any key turns it on.`
const hint = "Send a number, a key like + or =, or an expression like (2 + 3) × 4."

interface Session {
  calc?: Calc
  itemId: number
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

async function showCalculator(chat: api.ChatApi, {groupId, member}: Sender, calc: Calc): Promise<void> {
  const symbolKeys = member.memberChatVRange.maxVersion >= anyTextCommandsVersion
  const [sent] = await chat.apiSendTextMessage([T.ChatType.Group, groupId], calculatorText(calc, symbolKeys))
  const itemId = sent.chatItem.meta.itemId
  const previous = sessions.get(groupId)
  const timer = setTimeout(() => turnOff(chat, groupId, itemId, symbolKeys), idleMinutes * 60_000)
  sessions.set(groupId, {calc, itemId, timer})
  if (previous) {
    clearTimeout(previous.timer)
    await chat.apiDeleteChatItems(T.ChatType.Group, groupId, [previous.itemId], T.CIDeleteMode.Broadcast)
  }
}

function turnOff(chat: api.ChatApi, groupId: number, itemId: number, symbolKeys: boolean): void {
  sessions.set(groupId, {itemId})
  chat.apiUpdateChatItem(T.ChatType.Group, groupId, itemId, {type: "text", text: calculatorText(undefined, symbolKeys)}, false)
    .catch(e => console.log("error turning calculator off", e))
}

async function updateCalculator(chat: api.ChatApi, sender: Sender, update: Update): Promise<void> {
  const [calc, logLine] = update(sessions.get(sender.groupId)?.calc ?? initialCalc)
  if (logLine) await chat.apiSendTextMessage([T.ChatType.Group, sender.groupId], logLine)
  await showCalculator(chat, sender, calc)
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
  const input = textInput(content.text)
  if (!input) {
    await chat.apiSendTextReply(ci, hint)
    return
  }
  if (input.result) await chat.apiSendTextReply(ci, input.result)
  else await chat.apiDeleteMemberChatItem(sender.groupId, [ci.chatItem.meta.itemId])
  await updateCalculator(chat, sender, input.update)
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
      joinedGroupMember: ({groupInfo, member}, chat) => showCalculator(chat, {groupId: groupInfo.groupId, member}, initialCalc),
    },
  })
}
