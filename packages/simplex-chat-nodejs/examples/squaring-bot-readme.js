(async () => {
  const {bot} = await import("../dist/index.js")
  await bot.run({
    profile: {displayName: "Squaring bot example", fullName: ""},
    dbOpts: {type: "sqlite", filePrefix: "./squaring_bot"},
    options: {
      addressSettings: {welcomeMessage: "Send a number, I will square it."},
    },
    onMessage: async (ci, content, chat) => {
      const n = +content.text
      const reply = typeof n === "number" && !isNaN(n)
                    ? `${n} * ${n} = ${n * n}`
                    : `this is not a number`
      await chat.apiSendTextReply(ci, reply)
    }
  })
})()
