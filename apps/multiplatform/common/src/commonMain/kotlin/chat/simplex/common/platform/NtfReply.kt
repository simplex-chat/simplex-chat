package chat.simplex.common.platform

import chat.simplex.common.model.*

// Parses a stored notification chatId ("@123", "#45", "*7") into chat type + numeric api id.
fun chatIdToChatTypeAndId(chatId: String): Pair<ChatType, Long>? {
  val type = ChatType.values()
    .filter { it.type.isNotEmpty() }
    .sortedByDescending { it.type.length } // match "<@" before "@"
    .firstOrNull { chatId.startsWith(it.type) } ?: return null
  val id = chatId.removePrefix(type.type).toLongOrNull() ?: return null
  return type to id
}

fun ntfReplyAllowed(quickReplyEnabled: Boolean, appLockEnabled: Boolean, allowWhenLocked: Boolean): Boolean =
  quickReplyEnabled && (!appLockEnabled || allowWhenLocked)

fun shouldOfferReply(replyAllowed: Boolean, previewModeName: String?, cInfo: ChatInfo): Boolean =
  replyAllowed &&
    previewModeName != NotificationPreviewMode.HIDDEN.name &&
    (cInfo is ChatInfo.Direct || cInfo is ChatInfo.Group) &&
    cInfo.sendMsgEnabled

// Sends a plain-text reply to the chat identified by a notification's chatId.
// Returns true on success. rhId is null (local host) for now.
suspend fun sendNtfReply(rhId: Long?, userId: Long?, chatId: String, text: String): Boolean {
  if (text.isBlank()) return false
  val m = chatModel
  // the reply may be delivered to a freshly cold-started process (after OOM kill / non-instant mode);
  // wait for the chat controller before sending, as openChatAction does.
  ntfManager.awaitChatStartedIfNeeded(m)
  if (userId != null && userId != m.currentUser.value?.userId && m.currentUser.value != null) {
    m.controller.changeActiveUser(rhId, userId, null)
  }
  val cInfo = m.getChat(chatId)?.chatInfo
  if (cInfo != null && !cInfo.sendMsgEnabled) return false
  val typeAndId = when {
    cInfo != null -> cInfo.chatType to cInfo.apiId
    else -> chatIdToChatTypeAndId(chatId) ?: return false
  }
  val composed = ComposedMessage(
    fileSource = null,
    quotedItemId = null,
    msgContent = MsgContent.MCText(text),
    mentions = emptyMap()
  )
  val sent = m.controller.apiSendMessages(
    rh = rhId,
    type = typeAndId.first,
    id = typeAndId.second,
    scope = null,
    composedMessages = listOf(composed)
  )
  return !sent.isNullOrEmpty()
}
