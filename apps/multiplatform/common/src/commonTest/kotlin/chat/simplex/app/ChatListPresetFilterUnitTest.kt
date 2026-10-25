package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.chatlist.PresetTagKind
import chat.simplex.common.views.chatlist.presetTagMatchesChat
import kotlinx.datetime.Clock
import kotlin.test.Test
import kotlin.test.assertEquals

class ChatListPresetFilterUnitTest {
  private fun matchingTags(chatInfo: ChatInfo, chatStats: Chat.ChatStats = Chat.ChatStats()): Set<PresetTagKind> =
    PresetTagKind.entries.filter { presetTagMatchesChat(it, chatInfo, chatStats) }.toSet()

  private val addressLink = "https://simplex.chat/contact#/?v=2-7&smp=smp%3A%2F%2Fabc%40smp.example.com%2Fq"

  private fun contactCard(): Contact =
    Contact.sampleData.copy(activeConn = null, profile = LocalProfile.sampleData.copy(contactLink = addressLink))

  private fun group(businessChatType: BusinessChatType? = null, groupType: GroupType? = null): ChatInfo.Group =
    ChatInfo.Group(
      GroupInfo.sampleData.copy(
        businessChat = businessChatType?.let { BusinessChatInfo(chatType = it, businessId = "business", customerId = "customer") },
        groupProfile = GroupProfile.sampleData.copy(
          publicGroup = groupType?.let { PublicGroupProfile(groupType = it, groupLink = addressLink, publicGroupId = "public-group") }
        )
      ),
      groupChatScope = null
    )

  @Test
  fun readyContactIsInContactsOnly() {
    assertEquals(setOf(PresetTagKind.CONTACTS), matchingTags(ChatInfo.Direct(Contact.sampleData)))
  }

  @Test
  fun contactCardIsInNoPresetFilter() {
    assertEquals(emptySet<PresetTagKind>(), matchingTags(ChatInfo.Direct(contactCard())))
  }

  @Test
  fun contactWithPendingRequestIsNotACardAndIsInContacts() {
    assertEquals(setOf(PresetTagKind.CONTACTS), matchingTags(ChatInfo.Direct(contactCard().copy(contactRequestId = 5))))
  }

  @Test
  fun deletedContactIsInNoPresetFilter() {
    assertEquals(emptySet<PresetTagKind>(), matchingTags(ChatInfo.Direct(Contact.sampleData.copy(chatDeleted = true))))
  }

  @Test
  fun pendingConnectionIsInContacts() {
    assertEquals(setOf(PresetTagKind.CONTACTS), matchingTags(ChatInfo.ContactConnection(PendingContactConnection.getSampleData())))
  }

  @Test
  fun contactRequestIsInContacts() {
    assertEquals(setOf(PresetTagKind.CONTACTS), matchingTags(ChatInfo.ContactRequest(UserContactRequest.sampleData)))
  }

  @Test
  fun plainGroupIsInGroupsOnly() {
    assertEquals(setOf(PresetTagKind.GROUPS), matchingTags(group()))
  }

  @Test
  fun groupWithUnknownPublicGroupTypeIsInGroups() {
    assertEquals(setOf(PresetTagKind.GROUPS), matchingTags(group(groupType = GroupType.Unknown("forum"))))
  }

  @Test
  fun customerBusinessChatIsInContactsNotGroups() {
    assertEquals(setOf(PresetTagKind.CONTACTS), matchingTags(group(businessChatType = BusinessChatType.Customer)))
  }

  @Test
  fun businessChatIsInBusinessesNotGroups() {
    assertEquals(setOf(PresetTagKind.BUSINESS), matchingTags(group(businessChatType = BusinessChatType.Business)))
  }

  @Test
  fun channelIsInChannelsNotGroups() {
    assertEquals(setOf(PresetTagKind.CHANNELS), matchingTags(group(groupType = GroupType.Channel)))
  }

  @Test
  fun noteFolderIsInNotesOnly() {
    assertEquals(setOf(PresetTagKind.NOTES), matchingTags(ChatInfo.Local(NoteFolder.sampleData)))
  }

  @Test
  fun favoriteNoteFolderIsNotInFavorites() {
    assertEquals(setOf(PresetTagKind.NOTES), matchingTags(ChatInfo.Local(NoteFolder(noteFolderId = 2, favorite = true, unread = false, createdAt = Clock.System.now(), updatedAt = Clock.System.now(), chatTs = Clock.System.now()))))
  }

  @Test
  fun groupWithReportsIsAlsoInReports() {
    assertEquals(setOf(PresetTagKind.GROUPS, PresetTagKind.GROUP_REPORTS), matchingTags(group(), Chat.ChatStats(reportsCount = 2)))
  }

  @Test
  fun favoriteContactIsAlsoInFavorites() {
    val favorite = Contact.sampleData.copy(chatSettings = ChatSettings(enableNtfs = MsgFilter.All, sendRcpts = null, favorite = true))
    assertEquals(setOf(PresetTagKind.CONTACTS, PresetTagKind.FAVORITES), matchingTags(ChatInfo.Direct(favorite)))
  }

  @Test
  fun favoriteGroupIsAlsoInFavorites() {
    val favorite = GroupInfo.sampleData.copy(chatSettings = ChatSettings(enableNtfs = MsgFilter.All, sendRcpts = null, favorite = true))
    assertEquals(setOf(PresetTagKind.GROUPS, PresetTagKind.FAVORITES), matchingTags(ChatInfo.Group(favorite, groupChatScope = null)))
  }
}
