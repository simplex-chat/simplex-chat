import {T} from "@simplex-chat/types"
import {bot, util} from "../dist"

(async function run() {
  const welcomeMsgText = "Hello! I am a simple squaring bot.\n\nIf you send me a number, I will calculate its square."
  const [chat, _user] = await bot.run({
    profile: {displayName: "Squaring bot example", fullName: ""},
    dbOpts: {dbFilePrefix: "./squaring_bot", dbKey: ""},
    options: {
      addressSettings: {autoAccept: true, welcomeMessage: {type: "text", text: welcomeMsgText}},
      commands: [ // commands to show in client UI
        {type: "command", keyword: "help", label: "Send welcome message"},
        {type: "command", keyword: "info", label: "More information (not implemented)"}
      ],
      logContacts: true,
      logNetwork: false
    },
    onMessage: async (ci, content) => {
      const n = +content.text
      const reply = typeof n === "number" && !isNaN(n) ? `${n} * ${n} = ${n * n}` : `this is not a number`
      await chat.apiSendTextReply(ci, reply)
    },
    onCommands: { // command handlers can be different from commands to be shown in client UI
      "help": async (ci: T.AChatItem, _cmd: util.BotCommand) => {
        await chat.apiSendTextMessage(ci.chatInfo, welcomeMsgText)
      },
      // fallback handler that will be called for all other commands
      "": async (ci: T.AChatItem, _cmd: util.BotCommand) => {
        await chat.apiSendTextReply(ci, "This command is not supported")
      }
    },
    events: {
      "chatItemReaction": ({added, reaction}) => {
        console.log(`${util.senderName(reaction.chatInfo, reaction.chatReaction.chatDir)} ${added ? "added" : "removed"} reaction ${util.reactionText(reaction)}`)
      }
    },
  })
})()
