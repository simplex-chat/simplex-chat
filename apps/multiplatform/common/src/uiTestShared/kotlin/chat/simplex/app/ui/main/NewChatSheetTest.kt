package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.ChatScreen
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.contactOf
import chat.simplex.app.ui.harness.described
import chat.simplex.app.ui.harness.dismissNewChatSheet
import chat.simplex.app.ui.harness.goBack
import chat.simplex.app.ui.harness.labelled
import chat.simplex.app.ui.harness.openContextMenu
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

class NewChatSheetTest : UiTest() {
  @Test fun newChatSheetShowsActions() = uiTest {
    val alice = app("alice")
    alice.newChat()
    awaitSheet()
    check(
      "Create 1-time link" in textsOf(Ui.NewChat.CreateLink) &&
        (if (appPlatform.isAndroid) "Paste link / Scan" else "Paste link") in textsOf(Ui.NewChat.PasteLink) &&
        "Create group" in textsOf(Ui.NewChat.CreateGroup) &&
        "Create public channel" in textsOf(Ui.NewChat.CreateChannel)
    ) { "the New chat actions are not all shown with their labels" }
    check(!exists(Ui.NewGroup.Name) && !seesText("Channel name")) { "a creation screen is shown before its action is clicked" }

    click(Ui.NewChat.CreateGroup)
    poll(what = "the `Create secret group` screen in place of the sheet") {
      exists(Ui.NewGroup.Name) && seesText("Create secret group") && !exists(Ui.NewChat.CreateLink) && !seesText("Channel name")
    }
    leave(what = "the group screen") { !exists(Ui.NewGroup.Name) }
    awaitSheet()

    click(Ui.NewChat.CreateChannel)
    poll(what = "the `Create public channel` screen in place of the sheet") {
      seesText("Channel name") && seesText("Configure relays") && !exists(Ui.NewChat.CreateLink) && !exists(Ui.NewGroup.Name)
    }
    leave(what = "the channel screen") { !seesText("Channel name") }
    awaitSheet()
  }

  @Test fun newChatSheetSearchFiltersContacts() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob)
    connect(cath)
    val bobId = alice.chatList.chatId("bob"); val cathId = alice.chatList.chatId("cath")
    alice.newChat()
    poll(what = "`Your contacts` listing bob and cath") {
      seesText("Your contacts") && exists(Ui.NewChat.Contact(bobId)) && exists(Ui.NewChat.Contact(cathId))
    }
    typeInto(Ui.NewChat.Search, "CA")
    poll(what = "only cath listed for `CA`") {
      exists(Ui.NewChat.Contact(cathId)) && !exists(Ui.NewChat.Contact(bobId)) && !seesText("Your contacts") && !seesText("No filtered contacts")
    }
    replaceText(Ui.NewChat.Search, "zzz")
    poll(what = "`No filtered contacts` and no contact listed for `zzz`") {
      seesText("No filtered contacts") && !exists(Ui.NewChat.Contact(cathId)) && !exists(Ui.NewChat.Contact(bobId))
    }
    replaceText(Ui.NewChat.Search, "ca")
    poll(what = "only cath listed for `ca`") { exists(Ui.NewChat.Contact(cathId)) && !exists(Ui.NewChat.Contact(bobId)) }
    hideKeyboard()
    click(Ui.NewChat.Contact(cathId))
    poll(what = "cath's chat open and the New chat sheet closed") {
      chatModel.chatId.value == cathId && !chatModel.newChatSheetVisible.value && !exists(Ui.NewChat.Search)
    }
    ChatScreen(this).send("found you")
    cath.expectsMessage("alice> found you", CONNECT_TIMEOUT_MS)
  }

  @Test fun archivedContactsSendMessageRestores() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob)
    connect(cath)
    val bobId = alice.chatList.chatId("bob"); val cathId = alice.chatList.chatId("cath")
    alice.chatList.row("cath")
    alice.newChat()
    poll(what = "bob and cath under `Your contacts` and no `Archived contacts` row") {
      exists(Ui.NewChat.Contact(bobId)) && exists(Ui.NewChat.Contact(cathId)) && exists(Ui.NewChat.CreateLink) && !exists(Ui.NewChat.Archived)
    }
    dismissNewChatSheet()
    alice.chatList.rowMenu("bob", "delete")
    click(ONLY_CONVERSATION)
    poll(what = "bob archived and gone from the chat list") { contactOf("bob").chatDeleted && !exists(Ui.ChatList.Row(bobId)) }

    alice.newChat()
    poll(what = "the `Archived contacts` row, with cath but not bob under `Your contacts`") {
      labelled(Ui.NewChat.Archived, "Archived contacts") && exists(Ui.NewChat.Contact(cathId)) && !exists(Ui.NewChat.Contact(bobId))
    }
    typeInto(Ui.NewChat.Search, "bo")
    poll(what = "the archived bob found by `bo`, cath not") {
      exists(Ui.NewChat.Contact(bobId)) && !exists(Ui.NewChat.Contact(cathId)) && !exists(Ui.NewChat.Archived)
    }
    hideKeyboard()
    click(described("Close button"))
    poll(what = "the search cleared, bob hidden again behind `Archived contacts`") {
      editableText(Ui.NewChat.Search).isNullOrEmpty() && exists(Ui.NewChat.Archived) && exists(Ui.NewChat.Contact(cathId)) && !exists(Ui.NewChat.Contact(bobId))
    }
    click(Ui.NewChat.Archived)
    poll(what = "the `Archived contacts` list with bob only, in place of the sheet") {
      exists(Ui.NewChat.Contact(bobId)) && !exists(Ui.NewChat.Contact(cathId)) && !exists(Ui.NewChat.CreateLink)
    }
    click(Ui.NewChat.Contact(bobId))
    poll(what = "bob's chat open with the archived list and the sheet closed") {
      chatModel.chatId.value == bobId && !exists(Ui.NewChat.Contact(bobId)) && !exists(Ui.NewChat.Search)
    }
    ChatScreen(this).send("hi")
    bob.expectsMessage("alice> hi", CONNECT_TIMEOUT_MS)
    backToChatList()
    poll(what = "bob back in the chat list with `hi`") {
      !contactOf("bob").chatDeleted && exists(Ui.ChatList.Row(bobId)) && "hi" in alice.chatList.rowTexts("bob")
    }
    alice.newChat()
    poll(what = "bob under `Your contacts` and no `Archived contacts` row") {
      exists(Ui.NewChat.Contact(bobId)) && exists(Ui.NewChat.Contact(cathId)) && exists(Ui.NewChat.CreateLink) && !exists(Ui.NewChat.Archived)
    }
  }

  @Test fun newChatSheetFavoritesFilterToggle() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob)
    connect(cath)
    val bobId = alice.chatList.chatId("bob"); val cathId = alice.chatList.chatId("cath")
    alice.chatList.row("cath")
    alice.chatList.rowMenu("bob", "favorite", "Favorite")
    poll(what = "bob favourite and cath not") {
      alice.chatList.chat("bob").chatInfo.chatSettings?.favorite == true && alice.chatList.chat("cath").chatInfo.chatSettings?.favorite == false
    }
    alice.newChat()
    poll(what = "bob and cath listed with the filter off") { exists(Ui.NewChat.Contact(bobId)) && exists(Ui.NewChat.Contact(cathId)) && exists(Ui.NewChat.Favorites) }
    check(storedSetting(FAVORITES_FILTER) != "true") { "the favourites filter is stored as on before it is toggled" }
    click(Ui.NewChat.Favorites)
    poll(what = "only the favourite bob listed and the filter stored as on") {
      exists(Ui.NewChat.Contact(bobId)) && !exists(Ui.NewChat.Contact(cathId)) && storedSetting(FAVORITES_FILTER) == "true"
    }
    click(Ui.NewChat.Favorites)
    poll(what = "bob and cath listed again and the filter stored as off") {
      exists(Ui.NewChat.Contact(bobId)) && exists(Ui.NewChat.Contact(cathId)) && storedSetting(FAVORITES_FILTER) == "false"
    }
  }

  @Test fun newChatSheetContactWithRequestOpensChat() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    val address = alice.userPicker().createAddress()
    connect(bob)
    cath sends "/c $address"
    cath expects "connection request sent!"
    alice.chatList.request("cath")
    val bobId = alice.chatList.chatId("bob"); val cathId = alice.chatList.chatId("cath")
    alice.newChat()
    poll(what = "bob and cath's request under `Your contacts`") { exists(Ui.NewChat.Contact(bobId)) && exists(Ui.NewChat.Contact(cathId)) }

    openContextMenu(Ui.NewChat.Contact(bobId))
    click(Ui.NewChat.ContactMenu(bobId, "delete"))
    click(DELETE_CONTACT)
    click(AND_NOTIFY)
    bob.eventually("alice deleted contact with you", CONNECT_TIMEOUT_MS)
    poll(what = "bob gone from the sheet, cath still listed") {
      chatModel.chats.value.none { it.id == bobId } && !exists(Ui.NewChat.Contact(bobId)) && exists(Ui.NewChat.Contact(cathId))
    }

    click(Ui.NewChat.Contact(cathId))
    poll(what = "cath's chat open with the `Reject` / `Accept` bar and the sheet closed") {
      chatModel.chatId.value == cathId && exists(Ui.Chat.RequestAccept) && exists(Ui.Chat.RequestReject) && !exists(Ui.Chat.Message) && !exists(Ui.NewChat.Search)
    }
  }

  private fun UiScope.awaitSheet() =
    poll(what = "the New chat sheet with its actions") {
      exists(Ui.NewChat.CreateLink) && exists(Ui.NewChat.CreateGroup) && exists(Ui.NewChat.CreateChannel)
    }

  private fun UiScope.leave(what: String, closed: () -> Boolean) {
    hideKeyboard()
    goBack()
    poll(what = "$what to close", cond = closed)
  }

  private companion object {
    val ONLY_CONVERSATION = Ui.Alert.Option(Ui.AlertSource.Shared, "onlyConversation")
    val DELETE_CONTACT = Ui.Alert.Option(Ui.AlertSource.Shared, "deleteContact")
    val AND_NOTIFY = Ui.Alert.Option(Ui.AlertSource.Shared, "deleteAndNotify")
    const val FAVORITES_FILTER = "ShowUnreadAndFavorites"
  }
}
