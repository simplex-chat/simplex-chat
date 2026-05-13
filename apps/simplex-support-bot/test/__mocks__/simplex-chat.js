// Mock for simplex-chat — prevents native addon from loading

function ciContentText(chatItem) {
  const c = chatItem.content
  if (c.type === "sndMsgContent" || c.type === "rcvMsgContent") return c.msgContent.text
  return undefined
}

function ciBotCommand(chatItem) {
  const text = ciContentText(chatItem)?.trim()
  if (text) {
    const r = text.match(/^\/([^\s]+)(.*)/)
    if (r && r.length >= 3) return {keyword: r[1], params: r[2].trim()}
  }
  return undefined
}

function contactAddressStr(link) {
  return link.connShortLink || link.connFullLink
}

// Mirrors core.ChatAPIError so isChatNotFound's instanceof check passes when
// MockChatApi throws. Tests should construct these directly.
class ChatAPIError extends Error {
  constructor(message, chatError) {
    super(message)
    this.chatError = chatError
  }
}

module.exports = {
  api: {ChatApi: {}},
  bot: {},
  core: {ChatAPIError},
  util: {ciContentText, ciBotCommand, contactAddressStr},
}
