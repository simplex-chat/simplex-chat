package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.model.replaceAll
import chat.simplex.common.platform.NtfManager
import chat.simplex.common.platform.NotificationAction
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.call.RcvCallInvitation
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

  @Test
  fun messageEventWithEarlierCopyFollowsChangedSettings() = runBlocking {
    val groupInfo = GroupInfo.sampleData
    val user = User.sampleData
    val ntfs = RecordingNtfManager()
    val item = ChatItem.getSampleData(1, CIDirection.GroupRcv(GroupMember.sampleData))
      .copy(content = CIContent.RcvMsgContent(MsgContent.MCText("hello")))
    try {
      withContext(Dispatchers.Main) {
        chatModel.currentUser.value = user
        chatModel.chatsContext.chats.replaceAll(listOf(Chat(remoteHostId = null, chatInfo = ChatInfo.Group(groupInfo.copy(chatSettings = changedSettings), groupChatScope = null), chatItems = emptyList())))
      }
      ntfs.notifyMessageReceived(null, user, ChatInfo.Group(groupInfo, groupChatScope = null), item)
      assertEquals(emptyList(), ntfs.shownChatIds)
    } finally {
      withContext(Dispatchers.Main) {
        chatModel.chatsContext.chats.replaceAll(emptyList())
        chatModel.currentUser.value = null
      }
    }
  }

  private class RecordingNtfManager : NtfManager() {
    val shownChatIds = mutableListOf<String>()
    override fun displayNotification(user: UserLike, chatId: String, displayName: String, msgText: String, image: String?, actions: List<Pair<NotificationAction, () -> Unit>>) {
      shownChatIds.add(chatId)
    }
    override fun notifyCallInvitation(invitation: RcvCallInvitation) = false
    override fun hasNotificationsForChat(chatId: String) = false
    override fun cancelNotificationsForChat(chatId: String) {}
    override fun cancelNotificationsForUser(userId: Long) {}
    override fun cancelCallNotification() {}
    override fun cancelAllNotifications() {}
    override fun showMessage(title: String, text: String) {}
    override fun androidCreateNtfChannelsMaybeShowAlert() {}
  }
}
