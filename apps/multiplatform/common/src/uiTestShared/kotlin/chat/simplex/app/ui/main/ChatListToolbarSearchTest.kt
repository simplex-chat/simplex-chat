package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.chatListToolbar
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.createAddress
import chat.simplex.app.ui.harness.createGroupLink
import chat.simplex.app.ui.harness.enterLinkInSearch
import chat.simplex.app.ui.harness.goBack
import chat.simplex.app.ui.harness.joinLink
import chat.simplex.app.ui.harness.keepOneTimeLink
import chat.simplex.app.ui.harness.namePendingConnection
import chat.simplex.app.ui.harness.planConfirm
import chat.simplex.app.ui.harness.presetFilter
import chat.simplex.app.ui.harness.quiesce
import chat.simplex.app.ui.harness.searchShowsLink
import chat.simplex.app.ui.harness.switchAwayFromChat
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.GroupProfile
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.chatlist.PresetTagKind
import chat.simplex.common.views.database.stopChatAsync
import kotlinx.coroutines.runBlocking
import org.junit.Test

class ChatListToolbarSearchTest : UiTest() {
  @Test fun searchFiltersByDisplayName() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    alice.chatList.row("bob").row("cath").row("Private notes")
    alice.chatList.search("BO")
    poll(what = "only bob listed for `BO`") { alice.chatList.shown("bob") && !alice.chatList.shown("cath") && !alice.chatList.shown("Private notes") }
  }

  @Test fun chatListScrollToTopOnTitleClick() = uiTest {
    app("alice")
    repeat(GROUPS) { i ->
      runBlocking { chatController.apiNewGroup(null, incognito = false, GroupProfile("group$i", "", null)) } ?: error("apiNewGroup failed")
    }
    restart()
    var ids = emptyList<String>()
    poll(what = "all chats in the model") { ids = chatModel.chats.value.map { it.id }; ids.size > GROUPS }
    val bar = chatListToolbar()
    poll(what = "the title `Chats`") { "Chats" in bar.titleTexts() }
    waitFor(Ui.ChatList.Row(ids.first()))
    check(!bar.canScrollToTop()) { "the title is clickable while the list is at the top" }
    scrollTo(Ui.ChatList.Row(ids.last()))
    poll(what = "the title to become clickable with the list scrolled") { bar.canScrollToTop() }
    bar.scrollToTop()
    poll(what = "the list back at the top with the first chat shown") { !bar.canScrollToTop() && exists(Ui.ChatList.Row(ids.first())) }
  }

  @Test fun chatListUnreadDotOnProfileButtonForOtherUsers() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    alice.userPicker().profiles().apply { add("work"); close() }
    connect(bob)
    alice.userPicker().switchTo("alice")
    val bar = chatListToolbar()
    quiesce()
    check(!bar.profileBadge()) { "the profile button has a badge before bob writes" }
    bob sends "@work hello"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the badge on the profile button") { bar.profileBadge() }
    alice.userPicker().switchTo("work")
    alice.chatList.open("bob")
    poll(what = "bob's chat to be read") { alice.chatList.chat("bob").chatStats.unreadCount == 0 }
    backToChatList()
    alice.userPicker().switchTo("alice")
    poll(what = "the badge gone") { !bar.profileBadge() }
  }

  @Test fun chatListStoppedChatIndicator() = uiTest {
    app("alice")
    check(!exists(Ui.ChatList.Stopped)) { "the stopped indicator is shown while chat runs" }
    runBlocking { stopChatAsync(chatModel) }
    poll(what = "the `Chat is stopped` indicator") { exists(Ui.ChatList.Stopped) && "Chat is stopped" in descriptionsOf(Ui.ChatList.Stopped) }
    click(Ui.ChatList.Stopped)
    poll(what = "the chat stopped alert") { seesText("Chat is stopped") && seesText(STOPPED_ALERT_TEXT) }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the alert to close") { !seesText(STOPPED_ALERT_TEXT) }
  }

  @Test fun subscriptionIndicatorPercentageToggle() = uiTest {
    app("alice")
    val bar = chatListToolbar()
    waitFor(Ui.ChatList.Subscriptions)
    check(bar.percentage() == null) { "the percentage is shown by default" }
    bar.toggleShowPercentage()
    poll(what = "`%` next to the indicator with no subscriptions") { bar.percentage() == "%" && storedSetting("ShowSubscriptionPercentage") == "true" }
    runBlocking { chatController.apiAddContact(null, incognito = false) }.first ?: error("apiAddContact failed")
    poll(what = "`100%` with the invitation queue subscribed") { bar.percentage() == "100%" }
    bar.toggleShowPercentage()
    poll(what = "the percentage hidden") { bar.percentage() == null && storedSetting("ShowSubscriptionPercentage") == "false" }
  }

  private companion object {
    const val GROUPS = 30
    const val STOPPED_ALERT_TEXT = "You can start chat via app Settings / Database or by restarting the app."
  }

  @Test fun newChatButtonOpensNewChatSheet() = uiTest {
    app("alice")
    poll(what = "the toolbar button `Start new chat` and no floating button") {
      exists(Ui.ChatList.NewChat) && !exists(Ui.ChatList.NewChatFab) && "Start new chat" in descriptionsOf(Ui.ChatList.NewChat)
    }
    click(Ui.ChatList.NewChat)
    poll(what = "the New chat sheet") { chatModel.newChatSheetVisible.value && exists(Ui.NewChat.CreateLink) && seesText("New chat") }
    goBack()
    poll(what = "the New chat sheet to close") { !chatModel.newChatSheetVisible.value && !exists(Ui.NewChat.CreateLink) }
    appPrefs.oneHandUI.set(false)
    poll(what = "the floating button `Start new chat` and no toolbar button") {
      exists(Ui.ChatList.NewChatFab) && !exists(Ui.ChatList.NewChat) && "Start new chat" in descriptionsOf(Ui.ChatList.NewChatFab)
    }
    click(Ui.ChatList.NewChatFab)
    poll(what = "the New chat sheet from the floating button") { chatModel.newChatSheetVisible.value && exists(Ui.NewChat.CreateLink) }
  }

  @Test fun searchFiltersByAlias() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val link = keepOneTimeLink(alice)
    namePendingConnection("friend")
    bob sends "/c $link"; bob expects "confirmation sent!"
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    poll(what = "the contact bob listed under the alias `friend`") {
      chatModel.chats.value.any { it.chatInfo is ChatInfo.Direct && it.chatInfo.chatViewName == "friend" } &&
        chatModel.chats.value.none { it.chatInfo is ChatInfo.ContactConnection }
    }
    quiesce()
    switchAwayFromChat(alice)
    val friend = alice.chatList.chatId("friend")
    check(chatModel.chatId.value != friend) { "`friend` is the open chat, which is listed whatever the search text" }
    alice.chatList.row("friend")
    alice.chatList.search("frie")
    poll(what = "`friend` listed for part of its alias") { editableText(Ui.ChatList.Search) == "frie" && alice.chatList.shown("friend") }
    replaceText(Ui.ChatList.Search, "frix")
    poll(what = "`friend` hidden for text its alias does not contain") { editableText(Ui.ChatList.Search) == "frix" && !alice.chatList.shown("friend") }
  }

  @Test fun searchMatchesPendingConnectionAlias() = uiTest {
    val alice = app("alice")
    keepOneTimeLink(alice)
    namePendingConnection("dentist")
    alice.chatList.row("dentist").row("Private notes")
    alice.chatList.search("dent")
    poll(what = "only the pending connection listed") { alice.chatList.shown("dentist") && !alice.chatList.shown("Private notes") }
  }

  @Test fun searchNoChatsFound() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.row("bob").row("Private notes")
    alice.chatList.search("qq")
    poll(what = "`No chats found` and no rows") {
      seesText("No chats found") && !alice.chatList.shown("bob") && !alice.chatList.shown("Private notes")
    }
  }

  @Test fun searchClearsWhenFilterChanges() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.row("bob").row("Private notes")
    alice.chatList.search("bo")
    poll(what = "only bob listed") { alice.chatList.shown("bob") && !alice.chatList.shown("Private notes") }
    presetFilter(PresetTagKind.CONTACTS, "Contacts")
    poll(what = "the search text cleared") { editableText(Ui.ChatList.Search).isNullOrEmpty() }
  }

  @Test fun searchNameShowsConnectToNameRow() = uiTest {
    val alice = app("alice")
    alice.chatList.search("bobby")
    poll(what = "the row `Connect to bobby.testing`") {
      exists(Ui.ChatList.ConnectByName) && "Connect to bobby.testing" in textsOf(Ui.ChatList.ConnectByName)
    }
    replaceText(Ui.ChatList.Search, "@bob.simplex")
    poll(what = "the row `Connect to @bob.simplex`") {
      exists(Ui.ChatList.ConnectByName) && "Connect to @bob.simplex" in textsOf(Ui.ChatList.ConnectByName)
    }
    replaceText(Ui.ChatList.Search, "bob")
    poll(what = "no connect row for `bob`") { !exists(Ui.ChatList.ConnectByName) }
  }

  @Test fun searchPasteLinkTriggersConnectPlan() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = bob.createAddress()
    alice.chatList.row("Private notes")
    enterLinkInSearch(address)
    poll(what = "the address description in the read-only search field") { searchShowsLink("SimpleX contact address") }
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the plan alert for bob with `Open new chat`") {
      exists(planConfirm) && "Open new chat" in textsOf(planConfirm) && seesText("bob")
    }
    click(Ui.Alert.Dismiss(Ui.AlertSource.PrivacySensitive))
    poll(what = "the search field cleared and editable") { editableText(Ui.ChatList.Search).isNullOrEmpty() && isEnabled(Ui.ChatList.Search) }
  }

  @Test fun searchPasteKnownContactLinkFiltersToContact() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = bob.createAddress()
    joinLink(address)
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    alice.chatList.row("bob").row("Private notes")
    quiesce()
    enterLinkInSearch(address)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "only bob listed for his address") {
      searchShowsLink("SimpleX contact address") && alice.chatList.shown("bob") && !alice.chatList.shown("Private notes")
    }
    settle()
    poll(what = "no plan alert for a known contact") { !exists(planConfirm) }
  }

  @Test fun searchPasteKnownGroupLinkFiltersToGroup() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val link = bob.createGroupLink("team")
    joinLink(link)
    bob.eventually("#team: alice joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice an active member of team") {
      chatModel.chats.value.any { (it.chatInfo as? ChatInfo.Group)?.takeIf { g -> g.chatViewName == "team" }?.groupInfo?.membership?.memberActive == true }
    }
    alice.chatList.row("team").row("Private notes")
    quiesce()
    enterLinkInSearch(link)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "only team listed for its link") {
      searchShowsLink("SimpleX group link") && alice.chatList.shown("team") && !alice.chatList.shown("Private notes")
    }
    settle()
    poll(what = "no plan alert for a known group") { !exists(planConfirm) }
  }
}
