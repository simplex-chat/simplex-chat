const {ChatClient} = require("..")
const {ChatType} = require("../dist/command")
const {ciContentText, ChatInfoType} = require("../dist/response")

run()

async function run() {
  const chat = await ChatClient.create("ws://localhost:5225")
  const user = await chat.apiGetActiveUser()
  if (!user) {
    console.log("no user profile")
    return
  }
  console.log(`Bot profile: ${user.profile.displayName} (${user.profile.fullName})`)
  const address = (await chat.apiGetUserAddress()) || (await chat.apiCreateUserAddress())
  console.log(`Bot address: ${address}`)
  await chat.sendChatCmdStr("/auto_accept on")
  await processMessages(chat)

  async function processMessages(chat) {
    for await (const r of chat.msgQ) {
      const resp = r instanceof Promise ? await r : r
      switch (resp.type) {
        case "contactConnected": {
          const {contact} = resp
          console.log(`${contact.profile.displayName} connected`)
          await chat.apiSendTextMessage(
            ChatType.Direct,
            contact.contactId,
            "Hello! I am a simple squaring bot - if you send me a number, I will calculate its square"
          )
          continue
        }
        case "newChatItem": {
          const {chatInfo} = resp.chatItem
          if (chatInfo.type !== ChatInfoType.Direct) continue
          const msg = ciContentText(resp.chatItem.chatItem.content)
          let reply
          if (msg) {
            const n = +msg
            reply = typeof n === "number" ? `${n} * ${n} = ${n * n}` : `${n} is not a number`
          } else {
            reply = "no message text"
          }
          await chat.apiSendTextMessage(ChatType.Direct, chatInfo.contact.contactId, reply)
        }
      }
    }
  }
}
