package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.keepOneTimeLink
import chat.simplex.app.ui.harness.namePendingConnection
import chat.simplex.app.ui.harness.presetFilter
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.chatlist.PresetTagKind
import org.junit.Test

class ChatListToolbarSearchTest : UiTest() {
  @Test fun newChatButtonOpensNewChatSheet() = uiTest {
    app("alice")
    poll(what = "the toolbar button `Start new chat` and no floating button") {
      exists(Ui.ChatList.NewChat) && !exists(Ui.ChatList.NewChatFab) && "Start new chat" in descriptionsOf(Ui.ChatList.NewChat)
    }
    click(Ui.ChatList.NewChat)
    poll(what = "the New chat sheet") { chatModel.newChatSheetVisible.value && exists(Ui.NewChat.CreateLink) && seesText("New chat") }
    escape()
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
    alice.chatList.row("friend").row("Private notes")
    alice.chatList.search("frie")
    poll(what = "only `friend` listed") { alice.chatList.shown("friend") && !alice.chatList.shown("Private notes") }
    replaceText(Ui.ChatList.Search, "frix")
    poll(what = "`friend` hidden for text it does not contain") { !alice.chatList.shown("friend") }
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
}
