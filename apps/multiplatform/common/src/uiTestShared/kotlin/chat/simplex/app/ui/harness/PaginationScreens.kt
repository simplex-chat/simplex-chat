package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatType
import chat.simplex.common.model.ComposedMessage
import chat.simplex.common.model.MsgContent
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import kotlinx.coroutines.runBlocking

private const val API_BATCH = 100

fun sendTextsViaApi(contact: String, texts: List<String>) =
  texts.chunked(API_BATCH).forEach { batch -> sendViaApi(contact, batch.map { ComposedMessage(null, null, MsgContent.MCText(it), emptyMap()) }) }

fun replyViaApi(contact: String, quotedItemId: Long, text: String) =
  sendViaApi(contact, listOf(ComposedMessage(null, quotedItemId, MsgContent.MCText(text), emptyMap())))

private fun sendViaApi(contact: String, messages: List<ComposedMessage>) {
  val contactId = contactOf(contact).contactId
  runBlocking { chatController.apiSendMessages(null, ChatType.Direct, contactId, null, composedMessages = messages) } ?: error("apiSendMessages failed")
}

fun loadedText(text: String): Boolean = chatModel.chatsContext.chatItems.value.any { it.content.text == text }

fun UiScope.shownText(text: String): Boolean =
  chatModel.chatsContext.chatItems.value.lastOrNull { it.content.text == text }?.let { exists(Ui.Chat.Item(it.id)) } == true

fun loadedNumbered(prefix: String): List<String> =
  Regex("${Regex.escape(prefix)}\\d+").let { r -> chatModel.chatsContext.chatItems.value.map { it.content.text }.filter { r.matches(it) } }
