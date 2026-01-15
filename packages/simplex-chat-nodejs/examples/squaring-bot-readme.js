(async () => {
  const {bot} = await import("../dist/index.js")
  const [chat, _user, _address] = await bot.run({
    profile: {displayName: "Squaring bot example", fullName: ""},
    dbOpts: {dbFilePrefix: "./squaring_bot", dbKey: ""},
    options: {
      addressSettings: {welcomeMessage: "Send a number, I will square it."},
    },
    onMessage: async (ci, content) => {
      const n = +content.text
      const reply = typeof n === "number" && !isNaN(n)
                    ? `${n} * ${n} = ${n * n}`
                    : `this is not a number`
      await chat.apiSendTextReply(ci, reply)
    }
  })
})()
