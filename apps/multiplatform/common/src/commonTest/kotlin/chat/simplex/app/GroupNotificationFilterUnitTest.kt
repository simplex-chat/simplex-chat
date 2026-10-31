package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.res.*
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class GroupNotificationFilterUnitTest {

  private fun groupWithNtfs(filter: MsgFilter): ChatInfo.Group =
    ChatInfo.Group(
      GroupInfo.sampleData.copy(chatSettings = ChatSettings(enableNtfs = filter, sendRcpts = null, favorite = false)),
      groupChatScope = null
    )

  private fun groupChat(filter: MsgFilter, stats: Chat.ChatStats): Chat =
    Chat(remoteHostId = null, chatInfo = groupWithNtfs(filter), chatItems = emptyList(), chatStats = stats)

  @Test
  fun groupMuteCycleGoesAllMentionsNoneAll() {
    assertEquals(MsgFilter.Mentions, MsgFilter.All.nextMode(mentions = true))
    assertEquals(MsgFilter.None, MsgFilter.Mentions.nextMode(mentions = true))
    assertEquals(MsgFilter.All, MsgFilter.None.nextMode(mentions = true))
  }

  @Test
  fun directMuteCycleSkipsMentions() {
    assertEquals(MsgFilter.None, MsgFilter.All.nextMode(mentions = false))
    assertEquals(MsgFilter.All, MsgFilter.None.nextMode(mentions = false))
  }

  @Test
  fun muteButtonLabelNamesTheNextMode() {
    assertEquals(MR.strings.unmute_chat, MsgFilter.All.text(mentions = true))
    assertEquals(MR.strings.unmute_chat, MsgFilter.All.text(mentions = false))
    assertEquals(MR.strings.mute_chat, MsgFilter.Mentions.text(mentions = true))
    assertEquals(MR.strings.mute_all_chat, MsgFilter.None.text(mentions = true))
    assertEquals(MR.strings.mute_chat, MsgFilter.None.text(mentions = false))
  }

  @Test
  fun allNotifiesForEveryMessage() {
    assertTrue(groupWithNtfs(MsgFilter.All).ntfsEnabled(userMention = false))
    assertTrue(groupWithNtfs(MsgFilter.All).ntfsEnabled(userMention = true))
  }

  @Test
  fun mentionsNotifiesOnlyForMentions() {
    assertTrue(groupWithNtfs(MsgFilter.Mentions).ntfsEnabled(userMention = true))
    assertFalse(groupWithNtfs(MsgFilter.Mentions).ntfsEnabled(userMention = false))
  }

  @Test
  fun noneNeverNotifies() {
    assertFalse(groupWithNtfs(MsgFilter.None).ntfsEnabled(userMention = true))
    assertFalse(groupWithNtfs(MsgFilter.None).ntfsEnabled(userMention = false))
  }

  @Test
  fun chatWithoutSettingsNeverNotifies() {
    assertFalse(ChatInfo.Local.sampleData.ntfsEnabled(userMention = true))
  }

  @Test
  fun unreadTagFollowsTheNotificationFilter() {
    assertTrue(groupChat(MsgFilter.All, Chat.ChatStats(unreadCount = 1)).unreadTag)
    assertFalse(groupChat(MsgFilter.All, Chat.ChatStats()).unreadTag)
    assertFalse(groupChat(MsgFilter.Mentions, Chat.ChatStats(unreadCount = 3)).unreadTag)
    assertTrue(groupChat(MsgFilter.Mentions, Chat.ChatStats(unreadCount = 3, unreadMentions = 1)).unreadTag)
    assertFalse(groupChat(MsgFilter.None, Chat.ChatStats(unreadCount = 3, unreadMentions = 1)).unreadTag)
    assertTrue(groupChat(MsgFilter.None, Chat.ChatStats(unreadChat = true)).unreadTag)
  }
}
