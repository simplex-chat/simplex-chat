package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.chatlist.ActiveFilter
import chat.simplex.common.views.chatlist.PresetTagKind
import kotlin.test.AfterTest
import kotlin.test.BeforeTest
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

class ChatListTagCountersUnitTest {
  private val addressLink = "https://simplex.chat/contact#/?v=2-7&smp=smp%3A%2F%2Fabc%40smp.example.com%2Fq"
  private val workTag = ChatTag(chatTagId = 10, chatTagText = "work", chatTagEmoji = null)
  private val homeTag = ChatTag(chatTagId = 11, chatTagText = "home", chatTagEmoji = null)

  private fun settings(enableNtfs: MsgFilter = MsgFilter.All, favorite: Boolean = false) =
    ChatSettings(enableNtfs = enableNtfs, sendRcpts = null, favorite = favorite)

  private fun direct(
    id: Long,
    stats: Chat.ChatStats = Chat.ChatStats(),
    enableNtfs: MsgFilter = MsgFilter.All,
    favorite: Boolean = false,
    chatTags: List<Long> = emptyList(),
    remoteHostId: Long? = null,
  ): Chat =
    Chat(
      remoteHostId = remoteHostId,
      chatInfo = ChatInfo.Direct(Contact.sampleData.copy(contactId = id, chatSettings = settings(enableNtfs, favorite), chatTags = chatTags)),
      chatItems = emptyList(),
      chatStats = stats
    )

  private fun group(
    id: Long,
    stats: Chat.ChatStats = Chat.ChatStats(),
    enableNtfs: MsgFilter = MsgFilter.All,
    businessChatType: BusinessChatType? = null,
    chatTags: List<Long> = emptyList(),
  ): Chat =
    Chat(
      remoteHostId = null,
      chatInfo = ChatInfo.Group(
        GroupInfo.sampleData.copy(
          groupId = id,
          chatSettings = settings(enableNtfs),
          businessChat = businessChatType?.let { BusinessChatInfo(chatType = it, businessId = "business", customerId = "customer") },
          chatTags = chatTags
        ),
        groupChatScope = null
      ),
      chatItems = emptyList(),
      chatStats = stats
    )

  private fun channel(id: Long): Chat =
    Chat(
      remoteHostId = null,
      chatInfo = ChatInfo.Group(
        GroupInfo.sampleData.copy(
          groupId = id,
          groupProfile = GroupProfile.sampleData.copy(
            publicGroup = PublicGroupProfile(groupType = GroupType.Channel, groupLink = addressLink, publicGroupId = "channel")
          )
        ),
        groupChatScope = null
      ),
      chatItems = emptyList()
    )

  private fun notes(): Chat = Chat(remoteHostId = null, chatInfo = ChatInfo.Local(NoteFolder.sampleData), chatItems = emptyList())

  private fun contactCard(id: Long): Chat =
    Chat(
      remoteHostId = null,
      chatInfo = ChatInfo.Direct(Contact.sampleData.copy(contactId = id, activeConn = null, profile = LocalProfile.sampleData.copy(contactLink = addressLink))),
      chatItems = emptyList()
    )

  private fun setChats(vararg chats: Chat) {
    chatModel.chatsContext.chats.replaceAll(chats.toList())
  }

  private fun resetTagState() {
    chatModel.chatId.value = null
    chatModel.chatsContext.chats.replaceAll(emptyList())
    chatModel.presetTags.clear()
    chatModel.unreadTags.clear()
    chatModel.userTags.value = emptyList()
    chatModel.activeChatTagFilter.value = null
  }

  @BeforeTest
  fun setUp() = resetTagState()

  @AfterTest
  fun tearDown() = resetTagState()

  @Test
  fun updateChatTagsCountsEveryPresetKindForTheHost() {
    setChats(
      direct(1),
      direct(2, favorite = true),
      group(3, Chat.ChatStats(reportsCount = 1)),
      group(4, businessChatType = BusinessChatType.Business),
      group(5, businessChatType = BusinessChatType.Customer),
      notes(),
      contactCard(6),
      direct(7, remoteHostId = 1),
      channel(8),
    )
    chatModel.updateChatTags(rhId = null)
    assertEquals(
      mapOf(
        PresetTagKind.CONTACTS to 3,
        PresetTagKind.FAVORITES to 1,
        PresetTagKind.GROUPS to 1,
        PresetTagKind.CHANNELS to 1,
        PresetTagKind.GROUP_REPORTS to 1,
        PresetTagKind.BUSINESS to 1,
        PresetTagKind.NOTES to 1,
      ),
      chatModel.presetTags.toMap()
    )
  }

  @Test
  fun updateChatTagsCountsOnlyUnreadTagChatsPerUserTag() {
    setChats(
      direct(1, Chat.ChatStats(unreadCount = 1), chatTags = listOf(10, 11)),
      group(2, Chat.ChatStats(unreadCount = 3, unreadMentions = 0), enableNtfs = MsgFilter.Mentions, chatTags = listOf(10)),
      direct(3, Chat.ChatStats(unreadChat = true), enableNtfs = MsgFilter.None, chatTags = listOf(11)),
      direct(4, chatTags = listOf(10)),
      direct(5, Chat.ChatStats(unreadCount = 1), chatTags = listOf(10), remoteHostId = 1),
    )
    chatModel.updateChatTags(rhId = null)
    assertEquals(mapOf(10L to 1, 11L to 2), chatModel.unreadTags.toMap())
  }

  @Test
  fun updateChatTagsReplacesPreviousCounts() {
    chatModel.presetTags[PresetTagKind.CHANNELS] = 4
    chatModel.unreadTags[12] = 3
    setChats(direct(1))
    chatModel.updateChatTags(rhId = null)
    assertEquals(mapOf(PresetTagKind.CONTACTS to 1), chatModel.presetTags.toMap())
    assertEquals(emptyMap<Long, Int>(), chatModel.unreadTags.toMap())
  }

  @Test
  fun updateChatTagsClearsPresetFilterWithNoChatsLeft() {
    chatModel.activeChatTagFilter.value = ActiveFilter.PresetTag(PresetTagKind.FAVORITES)
    setChats(direct(1))
    chatModel.updateChatTags(rhId = null)
    assertNull(chatModel.activeChatTagFilter.value)
  }

  @Test
  fun updateChatTagsKeepsPresetFilterWithChats() {
    chatModel.activeChatTagFilter.value = ActiveFilter.PresetTag(PresetTagKind.CONTACTS)
    setChats(direct(1))
    chatModel.updateChatTags(rhId = null)
    assertEquals(ActiveFilter.PresetTag(PresetTagKind.CONTACTS), chatModel.activeChatTagFilter.value)
  }

  @Test
  fun userTagFilterIsClearedOnlyWhenTheTagIsGone() {
    chatModel.userTags.value = listOf(workTag)
    chatModel.activeChatTagFilter.value = ActiveFilter.UserTag(workTag)
    chatModel.clearActiveChatFilterIfNeeded()
    assertEquals(ActiveFilter.UserTag(workTag), chatModel.activeChatTagFilter.value)
    chatModel.activeChatTagFilter.value = ActiveFilter.UserTag(homeTag)
    chatModel.clearActiveChatFilterIfNeeded()
    assertNull(chatModel.activeChatTagFilter.value)
  }

  @Test
  fun unreadFilterIsNeverCleared() {
    chatModel.activeChatTagFilter.value = ActiveFilter.Unread
    chatModel.clearActiveChatFilterIfNeeded()
    assertEquals(ActiveFilter.Unread, chatModel.activeChatTagFilter.value)
  }

  @Test
  fun favoritingAddsToFavoritesCount() {
    chatModel.updateChatFavorite(favorite = true, wasFavorite = false)
    assertEquals(1, chatModel.presetTags[PresetTagKind.FAVORITES])
    chatModel.updateChatFavorite(favorite = true, wasFavorite = false)
    assertEquals(2, chatModel.presetTags[PresetTagKind.FAVORITES])
  }

  @Test
  fun unfavoritingLastChatClearsFavoritesFilter() {
    chatModel.presetTags[PresetTagKind.FAVORITES] = 1
    chatModel.activeChatTagFilter.value = ActiveFilter.PresetTag(PresetTagKind.FAVORITES)
    chatModel.updateChatFavorite(favorite = false, wasFavorite = true)
    assertEquals(0, chatModel.presetTags[PresetTagKind.FAVORITES])
    assertNull(chatModel.activeChatTagFilter.value)
  }

  @Test
  fun unchangedFavoriteLeavesCountAlone() {
    chatModel.presetTags[PresetTagKind.FAVORITES] = 2
    chatModel.updateChatFavorite(favorite = true, wasFavorite = true)
    chatModel.updateChatFavorite(favorite = false, wasFavorite = false)
    assertEquals(2, chatModel.presetTags[PresetTagKind.FAVORITES])
  }

  @Test
  fun unfavoritingWithoutCountDoesNotCreateIt() {
    chatModel.updateChatFavorite(favorite = false, wasFavorite = true)
    assertNull(chatModel.presetTags[PresetTagKind.FAVORITES])
  }

  @Test
  fun movingUnreadChatMovesTagCount() {
    chatModel.unreadTags[10] = 1
    chatModel.moveChatTagUnread(direct(1, Chat.ChatStats(unreadCount = 1)), oldTags = listOf(10), newTags = listOf(11))
    assertEquals(mapOf(10L to 0, 11L to 1), chatModel.unreadTags.toMap())
  }

  @Test
  fun movingReadChatLeavesTagCounts() {
    chatModel.unreadTags[10] = 1
    chatModel.moveChatTagUnread(direct(1), oldTags = listOf(10), newTags = listOf(11))
    assertEquals(mapOf(10L to 1), chatModel.unreadTags.toMap())
  }

  @Test
  fun movingUnreadChatClampsAtZeroAndSkipsUncountedTags() {
    chatModel.unreadTags[10] = 0
    chatModel.moveChatTagUnread(direct(1, Chat.ChatStats(unreadChat = true)), oldTags = listOf(10, 12), newTags = emptyList())
    assertEquals(mapOf(10L to 0), chatModel.unreadTags.toMap())
  }

  @Test
  fun chatBecomingUnreadIncrementsItsTags() {
    chatModel.unreadTags[10] = 1
    chatModel.chatsContext.updateChatTagReadInPrimaryContext(direct(1, Chat.ChatStats(unreadCount = 1), chatTags = listOf(10, 11)), wasUnread = false)
    assertEquals(mapOf(10L to 2, 11L to 1), chatModel.unreadTags.toMap())
  }

  @Test
  fun chatBecomingReadDecrementsItsTagsClampedAtZero() {
    chatModel.unreadTags[10] = 2
    chatModel.unreadTags[11] = 0
    chatModel.chatsContext.updateChatTagReadInPrimaryContext(direct(1, chatTags = listOf(10, 11)), wasUnread = true)
    assertEquals(mapOf(10L to 1, 11L to 0), chatModel.unreadTags.toMap())
  }

  @Test
  fun unchangedReadStateLeavesTagCounts() {
    chatModel.unreadTags[10] = 1
    chatModel.chatsContext.updateChatTagReadInPrimaryContext(direct(1, Chat.ChatStats(unreadCount = 1), chatTags = listOf(10)), wasUnread = true)
    chatModel.chatsContext.updateChatTagReadInPrimaryContext(direct(2, chatTags = listOf(10)), wasUnread = false)
    assertEquals(mapOf(10L to 1), chatModel.unreadTags.toMap())
  }

  @Test
  fun secondaryContextDoesNotChangeTagCounts() {
    val reportsContext = ChatModel.ChatsContext(SecondaryContextFilter.MsgContentTagContext(MsgContentTag.Report))
    chatModel.unreadTags[10] = 1
    reportsContext.updateChatTagReadInPrimaryContext(direct(1, Chat.ChatStats(unreadCount = 1), chatTags = listOf(10)), wasUnread = false)
    assertEquals(mapOf(10L to 1), chatModel.unreadTags.toMap())
    reportsContext.markChatTagRead(direct(2, Chat.ChatStats(unreadCount = 1), chatTags = listOf(10)))
    assertEquals(mapOf(10L to 1), chatModel.unreadTags.toMap())
  }

  @Test
  fun markingUnreadChatReadDecrementsItsTags() {
    chatModel.unreadTags[10] = 2
    chatModel.unreadTags[11] = 1
    chatModel.chatsContext.markChatTagRead(direct(1, Chat.ChatStats(unreadCount = 3), chatTags = listOf(10, 11)))
    assertEquals(mapOf(10L to 1, 11L to 0), chatModel.unreadTags.toMap())
  }

  @Test
  fun markingAlreadyReadChatReadLeavesTagCounts() {
    chatModel.unreadTags[10] = 2
    chatModel.chatsContext.markChatTagRead(direct(1, chatTags = listOf(10)))
    assertEquals(mapOf(10L to 2), chatModel.unreadTags.toMap())
  }

  @Test
  fun groupReportsPresetCountsGroupsWithReportsNotReports() {
    val reported = group(3)
    setChats(direct(1), reported)
    chatModel.chatsContext.increaseGroupReportsCounter(rhId = null, chatId = reported.id)
    assertEquals(1, chatModel.chatsContext.getChat(reported.id)?.chatStats?.reportsCount)
    assertEquals(1, chatModel.presetTags[PresetTagKind.GROUP_REPORTS])
    chatModel.chatsContext.increaseGroupReportsCounter(rhId = null, chatId = reported.id)
    assertEquals(2, chatModel.chatsContext.getChat(reported.id)?.chatStats?.reportsCount)
    assertEquals(1, chatModel.presetTags[PresetTagKind.GROUP_REPORTS])
  }

  @Test
  fun resolvingAllReportsClearsReportsFilter() {
    val reported = group(3, Chat.ChatStats(reportsCount = 2))
    setChats(reported)
    chatModel.presetTags[PresetTagKind.GROUP_REPORTS] = 1
    chatModel.activeChatTagFilter.value = ActiveFilter.PresetTag(PresetTagKind.GROUP_REPORTS)
    chatModel.chatsContext.decreaseGroupReportsCounter(rhId = null, chatId = reported.id, by = 2)
    assertEquals(0, chatModel.chatsContext.getChat(reported.id)?.chatStats?.reportsCount)
    assertEquals(0, chatModel.presetTags[PresetTagKind.GROUP_REPORTS])
    assertNull(chatModel.activeChatTagFilter.value)
  }

  @Test
  fun decreasingReportsBelowZeroClampsAtZero() {
    val reported = group(3, Chat.ChatStats(reportsCount = 1))
    setChats(reported)
    chatModel.presetTags[PresetTagKind.GROUP_REPORTS] = 1
    chatModel.chatsContext.decreaseGroupReportsCounter(rhId = null, chatId = reported.id, by = 5)
    assertEquals(0, chatModel.chatsContext.getChat(reported.id)?.chatStats?.reportsCount)
    assertEquals(0, chatModel.presetTags[PresetTagKind.GROUP_REPORTS])
  }
}
