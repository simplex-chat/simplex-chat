package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.businessChat
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.app.ui.harness.ownGroup
import chat.simplex.app.ui.harness.presetFilter
import chat.simplex.app.ui.harness.quiesce
import chat.simplex.common.model.MsgFilter
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.chatlist.ActiveFilter
import chat.simplex.common.views.chatlist.PresetTagKind
import org.junit.Test

class ChatListFiltersListsTest : UiTest() {
  @Test fun unreadFilterToggle() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    alice.chatList.row("bob").row("cath").row("Private notes")
    alice.chatList.rowMenu("cath", "ntfs.None")
    poll(what = "cath muted") { alice.chatList.chat("cath").chatInfo.chatSettings?.enableNtfs == MsgFilter.None }
    cath sends "@alice muted hello"
    bob sends "@alice hello"
    poll(what = "unread count 1 on bob and on muted cath") { "1" in alice.chatList.rowTexts("bob") && "1" in alice.chatList.rowTexts("cath") }
    click(Ui.ChatList.UnreadFilter)
    poll(what = "only bob listed") {
      chatModel.activeChatTagFilter.value == ActiveFilter.Unread &&
        alice.chatList.shown("bob") && !alice.chatList.shown("cath") && !alice.chatList.shown("Private notes")
    }
    click(Ui.ChatList.UnreadFilter)
    poll(what = "all chats listed again") {
      chatModel.activeChatTagFilter.value == null && alice.chatList.shown("bob") && alice.chatList.shown("cath") && alice.chatList.shown("Private notes")
    }
  }

  @Test fun unreadFilterEmptyState() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.row("bob").row("Private notes")
    quiesce()
    click(Ui.ChatList.UnreadFilter)
    poll(what = "`No unread chats` and no rows") {
      seesText("No unread chats") && exists(Ui.ChatList.NoUnreadChats) && !alice.chatList.shown("bob") && !alice.chatList.shown("Private notes")
    }
    click(Ui.ChatList.NoUnreadChats)
    poll(what = "the filter cleared and the chats back") {
      chatModel.activeChatTagFilter.value == null && alice.chatList.shown("bob") && alice.chatList.shown("Private notes")
    }
  }

  @Test fun presetFilterFavorites() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    alice.chatList.row("bob").row("cath").row("Private notes")
    alice.chatList.rowMenu("bob", "favorite")
    poll(what = "bob a favorite") { alice.chatList.chat("bob").chatInfo.chatSettings?.favorite == true }
    presetFilter(PresetTagKind.FAVORITES, "Favorites")
    poll(what = "only bob listed") { alice.chatList.shown("bob") && !alice.chatList.shown("cath") && !alice.chatList.shown("Private notes") }
  }

  @Test fun presetFilterContacts() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    hostGroup(bob, "team", "alice")
    alice.chatList.row("bob").row("team").row("Private notes")
    presetFilter(PresetTagKind.CONTACTS, "Contacts")
    poll(what = "only bob listed") { alice.chatList.shown("bob") && !alice.chatList.shown("team") && !alice.chatList.shown("Private notes") }
  }

  @Test fun presetFilterGroups() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val biz = cli("biz")
    connect(bob)
    hostGroup(bob, "team", "alice")
    businessChat(biz)
    alice.chatList.row("bob").row("team").row("biz").row("Private notes")
    presetFilter(PresetTagKind.GROUPS, "Groups")
    poll(what = "only team listed") {
      alice.chatList.shown("team") && !alice.chatList.shown("biz") && !alice.chatList.shown("bob") && !alice.chatList.shown("Private notes")
    }
  }

  @Test fun presetFilterBusinesses() = uiTest {
    val alice = app("alice"); val biz = cli("biz")
    businessChat(biz)
    alice.chatList.row("biz").row("Private notes")
    presetFilter(PresetTagKind.BUSINESS, "Businesses")
    poll(what = "only biz listed") { alice.chatList.shown("biz") && !alice.chatList.shown("Private notes") }
  }

  @Test fun presetFilterNotes() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.row("bob").row("Private notes")
    presetFilter(PresetTagKind.NOTES, "Notes")
    poll(what = "only Private notes listed") { alice.chatList.shown("Private notes") && !alice.chatList.shown("bob") }
  }

  @Test fun presetFilterReports() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    ownGroup(alice, "team", bob, cath)
    bob.eventually("#team: new member cath is connected", CONNECT_TIMEOUT_MS)
    cath.eventually("#team: member bob is connected", CONNECT_TIMEOUT_MS)
    cath sends "#team bad joke"
    bob.eventuallyMessage("#team cath> bad joke", CONNECT_TIMEOUT_MS)
    bob sends "/report #team content bad joke"
    bob.eventuallyMessage("#team (support) > cath bad joke")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the report counted on team") {
      chatModel.chats.value.any { it.chatInfo.chatViewName == "team" && it.chatStats.reportsCount > 0 }
    }
    backToChatList()
    alice.chatList.row("bob").row("cath").row("team").row("Private notes")
    presetFilter(PresetTagKind.GROUP_REPORTS, "Reports")
    poll(what = "only team listed") {
      alice.chatList.shown("team") && !alice.chatList.shown("bob") && !alice.chatList.shown("cath") && !alice.chatList.shown("Private notes")
    }
  }
}
