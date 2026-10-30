package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.businessChat
import chat.simplex.app.ui.harness.businessTypeOf
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.createBusinessAddress
import chat.simplex.app.ui.harness.hostGroup
import chat.simplex.common.platform.appPlatform
import chat.simplex.app.ui.harness.switchAwayFromChat
import chat.simplex.app.ui.harness.keepOneTimeLink
import chat.simplex.app.ui.harness.lists
import chat.simplex.app.ui.harness.ownGroup
import chat.simplex.app.ui.harness.presetFilter
import chat.simplex.app.ui.harness.quiesce
import chat.simplex.common.model.BusinessChatType
import chat.simplex.common.model.ChatInfo
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
    poll(what = "only bob listed with the Favorites chip") {
      exists(Ui.ChatList.PresetTag(PresetTagKind.FAVORITES.name)) && alice.chatList.shown("bob") && !alice.chatList.shown("cath") && !alice.chatList.shown("Private notes")
    }
    alice.chatList.rowMenu("bob", "unfavorite")
    poll(what = "the Favorites filter cleared with its chip gone and all chats listed") {
      chatModel.activeChatTagFilter.value == null && !exists(Ui.ChatList.PresetTag(PresetTagKind.FAVORITES.name)) &&
        exists(Ui.ChatList.PresetTag(PresetTagKind.CONTACTS.name)) &&
        alice.chatList.shown("bob") && alice.chatList.shown("cath") && alice.chatList.shown("Private notes")
    }
  }

  @Test fun presetFilterContacts() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val biz = cli("biz"); val cust = cli("cust")
    val address = createBusinessAddress(alice)
    connect(bob)
    hostGroup(bob, "team", "alice")
    cust sends "/c $address"
    cust expects "connection request sent!"
    cust.eventually("#alice: joining the group...", CONNECT_TIMEOUT_MS)
    cust.eventually("#alice: you joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "a customer business chat named cust") { businessTypeOf("cust") == BusinessChatType.Customer }
    businessChat(biz)
    keepOneTimeLink(alice)
    val pending = Ui.ChatList.Row(chatModel.chats.value.first { it.chatInfo is ChatInfo.ContactConnection }.id)
    alice.chatList.row("bob").row("team").row("biz").row("cust").row("Private notes")
    waitFor(pending)
    presetFilter(PresetTagKind.CONTACTS, "Contacts")
    poll(what = "only bob, the customer chat cust and the pending connection listed") {
      alice.chatList.shown("bob") && alice.chatList.shown("cust") && exists(pending) &&
        !alice.chatList.shown("team") && !alice.chatList.shown("biz") && !alice.chatList.shown("Private notes")
    }
  }

  @Test fun presetFilterReports() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    ownGroup(alice, "team", bob, cath)
    bob.eventually("#team: new member cath is connected", CONNECT_TIMEOUT_MS)
    cath.eventually("#team: member bob is connected", CONNECT_TIMEOUT_MS)
    hostGroup(bob, "club", "alice")
    cath sends "#team bad joke"
    bob.eventuallyMessage("#team cath> bad joke", CONNECT_TIMEOUT_MS)
    bob sends "/report #team content bad joke"
    bob.eventuallyMessage("#team (support) > cath bad joke")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the report counted on team") {
      chatModel.chats.value.any { it.chatInfo.chatViewName == "team" && it.chatStats.reportsCount > 0 }
    }
    backToChatList()
    switchAwayFromChat(alice)
    check(chatModel.chatId.value != alice.chatList.chatId("team")) { "team is the open chat, which is always listed" }
    alice.chatList.row("bob").row("cath").row("team").row("club").row("Private notes")
    presetFilter(PresetTagKind.GROUP_REPORTS, "Reports")
    poll(what = "only team listed") {
      alice.chatList.shown("team") && !alice.chatList.shown("club") && !alice.chatList.shown("bob") && !alice.chatList.shown("cath") &&
        (appPlatform.isDesktop || !alice.chatList.shown("Private notes"))
    }
  }

  @Test fun presetFiltersCollapsedMenuWhenMany() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    hostGroup(bob, "team", "alice")
    alice.chatList.row("bob").row("team").row("Private notes")
    quiesce()
    poll(what = "Contacts, Groups and Notes as chips and no preset menu") {
      listOf(PresetTagKind.CONTACTS, PresetTagKind.GROUPS, PresetTagKind.NOTES).all { exists(Ui.ChatList.PresetTag(it.name)) } && !exists(Ui.ChatList.PresetMenu)
    }
    alice.chatList.rowMenu("bob", "favorite")
    poll(what = "the presets collapsed into the `All` menu") {
      exists(Ui.ChatList.PresetMenu) && "All" in descriptionsOf(Ui.ChatList.PresetMenu) && PresetTagKind.entries.none { exists(Ui.ChatList.PresetTag(it.name)) }
    }
    click(Ui.ChatList.PresetMenu)
    val labels = mapOf(PresetTagKind.FAVORITES to "Favorites", PresetTagKind.CONTACTS to "Contacts", PresetTagKind.GROUPS to "Groups", PresetTagKind.NOTES to "Notes")
    poll(what = "Favorites, Contacts, Groups and Notes in the menu and no `All` item") {
      labels.all { (kind, label) -> exists(Ui.ChatList.PresetMenuItem(kind.name)) && label in textsOf(Ui.ChatList.PresetMenuItem(kind.name)) } &&
        !exists(Ui.ChatList.PresetMenuItem(PresetTagKind.CHANNELS.name)) && !exists(Ui.ChatList.PresetMenuAll)
    }
    click(Ui.ChatList.PresetMenuItem(PresetTagKind.GROUPS.name))
    poll(what = "the Groups filter active and shown as the menu icon") {
      chatModel.activeChatTagFilter.value == ActiveFilter.PresetTag(PresetTagKind.GROUPS) && "Groups" in descriptionsOf(Ui.ChatList.PresetMenu) &&
        alice.chatList.shown("team") && !alice.chatList.shown("bob")
    }
  }

  @Test fun presetFilterAllResets() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    hostGroup(bob, "team", "alice")
    alice.chatList.row("bob").row("team").row("Private notes")
    alice.chatList.rowMenu("bob", "favorite")
    poll(what = "the preset menu") { exists(Ui.ChatList.PresetMenu) }
    alice.chatList.search("bob")
    poll(what = "only bob found") { alice.chatList.shown("bob") && !alice.chatList.shown("team") }
    click(Ui.ChatList.PresetMenu)
    click(Ui.ChatList.PresetMenuAll)
    poll(what = "the search cleared and all chats listed") {
      editableText(Ui.ChatList.Search).isNullOrEmpty() && alice.chatList.shown("bob") && alice.chatList.shown("team") && alice.chatList.shown("Private notes")
    }
    presetFilter(PresetTagKind.CONTACTS, "Contacts")
    poll(what = "only bob listed") { alice.chatList.shown("bob") && !alice.chatList.shown("team") && !alice.chatList.shown("Private notes") }
    click(Ui.ChatList.PresetMenu)
    click(Ui.ChatList.PresetMenuAll)
    poll(what = "the filter cleared and all chats listed") {
      chatModel.activeChatTagFilter.value == null && "All" in descriptionsOf(Ui.ChatList.PresetMenu) &&
        alice.chatList.shown("bob") && alice.chatList.shown("team") && alice.chatList.shown("Private notes")
    }
  }

  @Test fun presetTagsHiddenWhenOnlyOneKind() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    alice.chatList.row("Private notes")
    quiesce()
    poll(what = "only the Notes kind, the tags row shown, and no preset chips or menu") {
      chatModel.presetTags.filterValues { it > 0 }.keys == setOf(PresetTagKind.NOTES) && exists(Ui.ChatList.AddList) &&
        PresetTagKind.entries.none { exists(Ui.ChatList.PresetTag(it.name)) } && !exists(Ui.ChatList.PresetMenu)
    }
    connect(bob)
    alice.chatList.row("bob")
    poll(what = "Contacts and Notes chips once there are two kinds") {
      exists(Ui.ChatList.PresetTag(PresetTagKind.CONTACTS.name)) && exists(Ui.ChatList.PresetTag(PresetTagKind.NOTES.name)) && !exists(Ui.ChatList.PresetMenu)
    }
  }

  @Test fun createListFromAddList() = uiTest {
    val alice = app("alice")
    alice.chatList.row("Private notes")
    val lists = lists()
    poll(what = "the `Add list` button") { "Add list" in textsOf(Ui.ChatList.AddList) }
    val editor = lists.addList()
    poll(what = "an empty list editor with `Save list`") { editor.nameText().isNullOrEmpty() && seesText("List name...") && editor.saveLabel() == "Save list" }
    editor.typeName("work")
    editor.save()
    val work = lists.tag("work")
    poll(what = "the `work` chip and the `Add list` button turned into +") {
      "work" in textsOf(Ui.ChatList.UserTag(work.chatTagId)) && "Add list" !in textsOf(Ui.ChatList.AddList) && "Add list" in descriptionsOf(Ui.ChatList.AddList)
    }
    check(work.chatTagEmoji == null) { "list created with emoji ${work.chatTagEmoji}" }
  }

  @Test fun createListDuplicateNameError() = uiTest {
    val alice = app("alice")
    alice.chatList.row("Private notes")
    val lists = lists()
    lists.create("work")
    val editor = lists.addList()
    poll(what = "the list editor") { editor.shown() }
    editor.typeName("work")
    editor.saveIgnored()
    check(!seesText("Error creating chat list") && !exists(Ui.Alert.Confirm(Ui.AlertSource.Shared))) { "an error alert after saving a duplicate list name" }
    check(chatModel.userTags.value.map { it.chatTagText } == listOf("work")) { "lists: ${chatModel.userTags.value}" }
    editor.replaceName("work2")
    editor.save()
    poll(what = "lists work and work2") { chatModel.userTags.value.map { it.chatTagText }.toSet() == setOf("work", "work2") }
  }

  @Test fun addChatToListFromContextMenu() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    alice.chatList.row("bob").row("cath")
    val lists = lists()
    alice.chatList.rowMenu("bob", "tagList", "Add to list")
    val editor = lists.editor()
    poll(what = "the list editor with `Add to list`") { editor.shown() && editor.saveLabel() == "Add to list" }
    editor.typeName("work")
    editor.save()
    val work = lists.tag("work").chatTagId
    poll(what = "bob in work") { lists.tagsOf("bob") == listOf(work) }
    alice.chatList.rowMenu("cath", "tagList", "Add to list")
    poll(what = "the list sheet with work") { exists(Ui.TagList.Tag(work)) && "work" in textsOf(Ui.TagList.Tag(work)) && !editor.shown() }
    click(Ui.TagList.Tag(work))
    poll(what = "cath in work and the sheet closed") { lists.tagsOf("cath") == listOf(work) && !exists(Ui.TagList.Tag(work)) }
  }

  @Test fun changeChatListFromContextMenu() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.row("bob")
    val lists = lists()
    val work = lists.create("work").chatTagId
    val family = lists.create("family").chatTagId
    lists.assign("bob", "work")
    alice.chatList.rowMenu("bob", "tagList", "Change list")
    poll(what = "the list sheet with work and family") { exists(Ui.TagList.Tag(work)) && exists(Ui.TagList.Tag(family)) }
    click(Ui.TagList.Tag(family))
    poll(what = "bob moved from work to family and the sheet closed") { lists.tagsOf("bob") == listOf(family) && !exists(Ui.TagList.Tag(family)) }
  }

  @Test fun removeChatFromList() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.row("bob")
    val lists = lists()
    val work = lists.create("work").chatTagId
    lists.assign("bob", "work")
    alice.chatList.rowMenu("bob", "tagList", "Change list")
    click(Ui.TagList.Tag(work))
    poll(what = "bob out of work and the sheet closed") { lists.tagsOf("bob").isEmpty() && !exists(Ui.TagList.Tag(work)) }
    alice.chatList.rowMenu("bob", "tagList", "Add to list")
    waitFor(Ui.TagList.Tag(work))
    escape()
    poll(what = "the list sheet closed with the work list kept") { !exists(Ui.TagList.Tag(work)) && exists(Ui.ChatList.UserTag(work)) }
  }

  @Test fun filterByUserList() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    alice.chatList.row("bob").row("cath").row("Private notes")
    val lists = lists()
    lists.create("work")
    lists.assign("bob", "work")
    lists.filter("work")
    poll(what = "only bob listed") { alice.chatList.shown("bob") && !alice.chatList.shown("cath") && !alice.chatList.shown("Private notes") }
    click(lists.chip("work"))
    poll(what = "the filter off and all chats listed") {
      lists.activeTagId() == null && alice.chatList.shown("bob") && alice.chatList.shown("cath") && alice.chatList.shown("Private notes")
    }
  }

  @Test fun emptyListShowsNoChatsInList() = uiTest {
    val alice = app("alice")
    alice.chatList.row("Private notes")
    val lists = lists()
    lists.create("work")
    lists.filter("work")
    poll(what = "`No chats in list work.` and no rows") { seesText("No chats in list work.") && !alice.chatList.shown("Private notes") }
  }

  @Test fun listUnreadDot() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.row("bob")
    val lists = lists()
    lists.create("work")
    lists.assign("bob", "work")
    quiesce()
    poll(what = "the work chip without the unread dot") { "work" in lists.chipTexts("work") && "work ●" !in lists.chipTexts("work") }
    bob sends "@alice hi"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the unread dot on the work chip") { "work ●" in lists.chipTexts("work") }
    alice.chatList.open("bob")
    poll(what = "bob's message read") { alice.chatList.chat("bob").chatStats.unreadCount == 0 }
    backToChatList()
    poll(what = "the unread dot gone") { "work" in lists.chipTexts("work") && "work ●" !in lists.chipTexts("work") }
  }

  @Test fun deleteListConfirm() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.row("bob").row("Private notes")
    val lists = lists()
    val work = lists.create("work").chatTagId
    lists.assign("bob", "work")
    lists.filter("work")
    poll(what = "only bob listed in work") { alice.chatList.shown("bob") && !alice.chatList.shown("Private notes") }
    lists.menu("work", "delete")
    poll(what = "the `Delete list?` alert") { seesText("Delete list?") && seesText("All chats will be removed from the list work, and the list deleted") }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the work chip and its filter gone, bob still listed without a list, and the notes folder back") {
      chatModel.userTags.value.none { it.chatTagId == work } && lists.activeTagId() == null && !exists(Ui.ChatList.UserTag(work)) &&
        alice.chatList.shown("bob") && alice.chatList.shown("Private notes") && lists.tagsOf("bob").isEmpty()
    }
  }

  @Test fun listsArePerUser() = uiTest {
    val alice = app("alice")
    alice.chatList.row("Private notes")
    val lists = lists()
    val work = lists.create("work").chatTagId
    alice.userPicker().profiles().apply { add("alice2"); close() }
    poll(what = "no lists for alice2") {
      chatModel.userTags.value.isEmpty() && !exists(Ui.ChatList.UserTag(work)) && exists(Ui.ChatList.AddList) && "Add list" in textsOf(Ui.ChatList.AddList)
    }
    val home = lists.create("home").chatTagId
    alice.userPicker().switchTo("alice")
    poll(what = "alice's work list back without alice2's home list") {
      chatModel.userTags.value.map { it.chatTagText } == listOf("work") && exists(Ui.ChatList.UserTag(work)) && !exists(Ui.ChatList.UserTag(home))
    }
  }
}
