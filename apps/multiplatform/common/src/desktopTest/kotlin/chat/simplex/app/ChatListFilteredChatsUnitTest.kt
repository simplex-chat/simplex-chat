package chat.simplex.app

import androidx.compose.runtime.mutableStateOf
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.chatlist.ActiveFilter
import chat.simplex.common.views.chatlist.PresetTagKind
import chat.simplex.common.views.chatlist.filteredChats
import kotlinx.datetime.Clock
import kotlin.test.AfterTest
import kotlin.test.BeforeTest
import kotlin.test.Test
import kotlin.test.assertEquals

class ChatListFilteredChatsUnitTest {
  private val addressLink = "https://simplex.chat/contact#/?v=2-7&smp=smp%3A%2F%2Fabc%40smp.example.com%2Fq"

  private fun contact(
    id: Long,
    displayName: String,
    fullName: String = "",
    localAlias: String = "",
    chatDeleted: Boolean = false,
    enableNtfs: MsgFilter = MsgFilter.All,
    chatTags: List<Long> = emptyList(),
  ): Contact =
    Contact.sampleData.copy(
      contactId = id,
      localDisplayName = displayName,
      profile = LocalProfile.sampleData.copy(profileId = id, displayName = displayName, fullName = fullName, localAlias = localAlias),
      chatSettings = ChatSettings(enableNtfs = enableNtfs, sendRcpts = null, favorite = false),
      chatDeleted = chatDeleted,
      chatTags = chatTags,
    )

  private fun chat(chatInfo: ChatInfo, stats: Chat.ChatStats = Chat.ChatStats()): Chat =
    Chat(remoteHostId = null, chatInfo = chatInfo, chatItems = emptyList(), chatStats = stats)

  private fun direct(contact: Contact, stats: Chat.ChatStats = Chat.ChatStats()): Chat = chat(ChatInfo.Direct(contact), stats)

  private val bob = direct(contact(2, "bob", fullName = "Robert Smith"))
  private val dentist = direct(contact(3, "carol", localAlias = "dentist"))
  private val deleted = direct(contact(4, "bobby", chatDeleted = true))
  private val card = direct(contact(5, "bobcard").copy(activeConn = null, profile = LocalProfile.sampleData.copy(profileId = 5, displayName = "bobcard", fullName = "", localAlias = "", contactLink = addressLink)))
  private val team = chat(ChatInfo.Group(GroupInfo.sampleData.copy(groupId = 6), groupChatScope = null))
  private val plumberConnection = chat(
    ChatInfo.ContactConnection(
      PendingContactConnection(
        pccConnId = 7,
        pccAgentConnId = "agent-7",
        pccConnStatus = ConnStatus.New,
        viaContactUri = false,
        localAlias = "plumber",
        createdAt = Clock.System.now(),
        updatedAt = Clock.System.now()
      )
    )
  )
  private val request = chat(ChatInfo.ContactRequest(UserContactRequest.sampleData))

  private val allChats = listOf(bob, dentist, deleted, card, team, plumberConnection, request)

  private fun search(text: String, chats: List<Chat> = allChats, activeFilter: ActiveFilter? = null, showingLink: Boolean = false, linkChatIds: Set<String> = emptySet()): List<String> =
    filteredChats(mutableStateOf(showingLink), mutableStateOf(linkChatIds), text, chats, activeFilter).map { it.id }

  @BeforeTest
  fun resetOpenChat() {
    chatModel.chatId.value = null
  }

  @AfterTest
  fun clearOpenChat() {
    chatModel.chatId.value = null
  }

  @Test
  fun emptySearchHidesDeletedChatsAndContactCards() {
    assertEquals(listOf(bob.id, dentist.id, team.id, plumberConnection.id, request.id), search(""))
  }

  @Test
  fun blankSearchActsLikeEmpty() {
    assertEquals(search(""), search("   "))
  }

  @Test
  fun searchIsCaseInsensitiveOnDisplayName() {
    assertEquals(listOf(bob.id), search("BO"))
  }

  @Test
  fun searchIsTrimmed() {
    assertEquals(listOf(bob.id), search("  bob  "))
  }

  @Test
  fun searchMatchesFullName() {
    assertEquals(listOf(bob.id), search("smith"))
  }

  @Test
  fun searchMatchesContactAlias() {
    assertEquals(listOf(dentist.id), search("dent"))
  }

  @Test
  fun searchMatchesPendingConnectionAlias() {
    assertEquals(listOf(plumberConnection.id), search("PLUM"))
    assertEquals(emptyList<String>(), search("plux"))
  }

  @Test
  fun searchMatchesGroupNameAndContactRequestName() {
    assertEquals(listOf(team.id), search("my team"))
    assertEquals(listOf(request.id), search("alice"))
  }

  @Test
  fun searchMatchesNotesName() {
    val notes = chat(ChatInfo.Local(NoteFolder.sampleData))
    assertEquals(listOf(notes.id), search(notes.chatInfo.displayName.uppercase(), allChats + notes))
  }

  @Test
  fun searchExcludesDeletedContactsAndContactCardsEvenWhenNameMatches() {
    assertEquals(listOf(bob.id), search("bob"))
  }

  @Test
  fun searchWithNoMatchIsEmpty() {
    assertEquals(emptyList<String>(), search("zzz"))
  }

  @Test
  fun openChatIsAlwaysKept() {
    chatModel.chatId.value = deleted.id
    assertEquals(listOf(bob.id, dentist.id, deleted.id, team.id, plumberConnection.id, request.id), search(""))
    assertEquals(listOf(deleted.id), search("zzz"))
    assertEquals(listOf(deleted.id, team.id), search("", activeFilter = ActiveFilter.PresetTag(PresetTagKind.GROUPS)))
  }

  @Test
  fun chatsFoundByLinkOverrideSearchTextAndHiddenRules() {
    assertEquals(listOf(card.id, team.id), search("bob", linkChatIds = setOf(team.id, card.id)))
  }

  @Test
  fun searchTextIsIgnoredWhileShowingSimplexLink() {
    assertEquals(search(""), search("zzz", showingLink = true))
  }

  @Test
  fun presetFilterAppliesWhenSearchIsEmpty() {
    assertEquals(listOf(team.id), search("", activeFilter = ActiveFilter.PresetTag(PresetTagKind.GROUPS)))
    assertEquals(listOf(bob.id, dentist.id, plumberConnection.id, request.id), search("", activeFilter = ActiveFilter.PresetTag(PresetTagKind.CONTACTS)))
  }

  @Test
  fun activeFilterIsIgnoredWhileSearching() {
    assertEquals(listOf(team.id), search("team", activeFilter = ActiveFilter.PresetTag(PresetTagKind.CONTACTS)))
  }

  @Test
  fun userTagFilterKeepsOnlyTaggedChats() {
    val work = direct(contact(10, "worker", chatTags = listOf(1L, 2L)))
    val home = direct(contact(11, "homer", chatTags = listOf(3L)))
    val workGroup = chat(ChatInfo.Group(GroupInfo.sampleData.copy(groupId = 12, chatTags = listOf(2L)), groupChatScope = null))
    val chats = listOf(work, home, workGroup, plumberConnection)
    assertEquals(listOf(work.id, workGroup.id), search("", chats, ActiveFilter.UserTag(ChatTag(chatTagId = 2, chatTagText = "work", chatTagEmoji = null))))
  }

  @Test
  fun unreadFilterKeepsChatsWithUnreadTag() {
    val unread = direct(contact(20, "unread"), Chat.ChatStats(unreadCount = 2))
    val markedUnread = direct(contact(21, "marked"), Chat.ChatStats(unreadChat = true))
    val mutedUnread = direct(contact(22, "muted", enableNtfs = MsgFilter.None), Chat.ChatStats(unreadCount = 5))
    val read = direct(contact(23, "read"))
    val mentioned = direct(contact(24, "mentioned", enableNtfs = MsgFilter.Mentions), Chat.ChatStats(unreadCount = 1, unreadMentions = 1))
    val unmentioned = direct(contact(25, "unmentioned", enableNtfs = MsgFilter.Mentions), Chat.ChatStats(unreadCount = 3, unreadMentions = 0))
    assertEquals(
      listOf(unread.id, markedUnread.id, mentioned.id),
      search("", listOf(unread, markedUnread, mutedUnread, read, mentioned, unmentioned), ActiveFilter.Unread)
    )
  }
}
