package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.model.replaceAll
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withContext
import kotlin.test.Test
import kotlin.test.assertEquals

class ChatSettingsEventTest {

  private val changedSettings = ChatSettings(enableNtfs = MsgFilter.Mentions, sendRcpts = null, favorite = true)

  private suspend fun chatsWith(chatInfo: ChatInfo): ChatModel.ChatsContext {
    val chatsCtx = ChatModel.ChatsContext(null)
    withContext(Dispatchers.Main) {
      chatsCtx.chats.replaceAll(listOf(Chat(remoteHostId = null, chatInfo = chatInfo, chatItems = emptyList())))
    }
    return chatsCtx
  }

  @Test
  fun groupEventWithEarlierCopyKeepsChangedSettings() = runBlocking {
    val groupInfo = GroupInfo.sampleData
    val chatsCtx = chatsWith(ChatInfo.Group(groupInfo.copy(chatSettings = changedSettings), groupChatScope = null))
    withContext(Dispatchers.Main) {
      chatsCtx.updateGroup(null, groupInfo)
    }
    assertEquals(changedSettings, chatsCtx.chats.value.single().chatInfo.chatSettings)
  }

  @Test
  fun contactEventWithEarlierCopyKeepsChangedSettings() = runBlocking {
    val contact = Contact.sampleData
    val chatsCtx = chatsWith(ChatInfo.Direct(contact.copy(chatSettings = changedSettings)))
    withContext(Dispatchers.Main) {
      chatsCtx.updateContact(null, contact)
    }
    assertEquals(changedSettings, chatsCtx.chats.value.single().chatInfo.chatSettings)
  }
}
