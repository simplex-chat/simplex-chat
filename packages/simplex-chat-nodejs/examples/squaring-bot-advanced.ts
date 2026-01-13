import {T} from "@simplex-chat/types"
import {api, util} from "../dist"

const dbPath = "./squaring_bot_advanced"
const botProfile = {displayName: "Squaring bot example", fullName: ""}

run()

async function run() {
  const bot = await api.ChatApi.init(dbPath)
  const user = (await bot.apiGetActiveUser()) || (await bot.apiCreateActiveUser(botProfile))
  console.log("Bot user: ", user.profile.displayName)
  const {userId} = user
  await bot.startChat()
  const address = (await bot.apiGetUserAddress(userId))?.connLinkContact || (await bot.apiCreateUserAddress(userId))
  console.log(`Bot address: ${util.contactAddressStr(address)}`)
  await bot.apiSetAddressSettings(userId, {autoAccept: true, welcomeMessage: {type: "text", text: "Hello! I am a simple squaring bot.\n\nIf you send me a number, I will calculate its square."}})
  bot.on("newChatItems", async (evt) => {
    for (const {chatInfo, chatItem} of evt.chatItems) {
      if (chatInfo.type !== T.ChatType.Direct) continue
      const msg = util.ciContentText(chatItem)
      if (msg) {
        const n = +msg
        const reply = typeof n === "number" && !isNaN(n) ? `${n} * ${n} = ${n * n}` : `this is not a number`
        await bot.apiSendTextMessage(chatInfo, reply)        
      }
    }
  })
  bot.on({
    "contactConnected": ({contact}) => console.log(`${contact.profile.displayName} connected`),
    "contactDeletedByContact": ({contact}) => console.log(`${contact.profile.displayName} deleted connection with bot`),
    "hostConnected": ({transportHost}) => console.log(`connected server ${transportHost}`),
    "hostDisconnected": ({transportHost}) => console.log(`diconnected server ${transportHost}`),
    "subscriptionStatus": ({subscriptionStatus, connections}) => console.log(`${connections.length} subscription(s) ${subscriptionStatus.type}`)
  })
}
