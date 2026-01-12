import {T} from "@simplex-chat/types"
import {api} from ".."

const dbPath = "./squaring_bot"
const botProfile = {displayName: "Squaring bot example", fullName: ""}

run()

async function run() {
  const bot = await api.ChatApi.init(dbPath)
  const user = (await bot.apiGetActiveUser()) || (await bot.apiCreateActiveUser(botProfile))
  console.log("Bot user: ", user.profile.displayName)
  const {userId} = user
  await bot.startChat()
  const address = (await bot.apiGetUserAddress(userId)) || (await bot.apiCreateUserAddress(userId))
  console.log(`Bot address: ${address}`)
  await bot.enableAddressAutoAccept(userId, {type: "text", text: "Hello! I am a simple squaring bot.\n\nIf you send me a number, I will calculate its square."})
  bot.on("contactConnected", async ({contact}) => console.log(`${contact.profile.displayName} connected`))
  bot.on("newChatItems", async (evt) => {
    for (const {chatInfo, chatItem} of evt.chatItems) {
      if (chatInfo.type !== T.ChatType.Direct) continue
      const msg = ciContentText(chatItem.content)
      if (msg) {
        const n = +msg
        const reply = typeof n === "number" && !isNaN(n) ? `${n} * ${n} = ${n * n}` : `this is not a number`
        await bot.apiSendTextMessage(T.ChatType.Direct, chatInfo.contact.contactId, reply)        
      }
    }
  })
  
  function ciContentText(content: T.CIContent): string | undefined {
    switch (content.type) {
      case "sndMsgContent": return content.msgContent.text;
      case "rcvMsgContent": return content.msgContent.text;
      default: return undefined;
    }
  }
}
