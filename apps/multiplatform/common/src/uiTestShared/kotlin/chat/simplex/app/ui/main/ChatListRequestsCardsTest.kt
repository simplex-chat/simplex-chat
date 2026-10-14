package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.quiesce
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

class ChatListRequestsCardsTest : UiTest() {
  @Test fun contactWithRequestOpenToAccept() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = alice.userPicker().createAddress()
    bob sends "/c $address"; bob expects "connection request sent!"
    alice.chatList.request("bob").open("bob")
    poll(what = "bob's chat with the `Reject` / `Accept` bar instead of the message field") {
      chatModel.chatId.value == alice.chatList.chatId("bob") &&
        exists(Ui.Chat.RequestReject) && "Reject" in textsOf(Ui.Chat.RequestReject) &&
        exists(Ui.Chat.RequestAccept) && "Accept" in textsOf(Ui.Chat.RequestAccept) &&
        !exists(Ui.Chat.Message)
    }
  }

  @Test fun contactWithRequestComposeAccept() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = alice.userPicker().createAddress()
    bob sends "/c $address"; bob expects "connection request sent!"
    alice.chatList.request("bob").open("bob")
    click(Ui.Chat.RequestAccept)
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    poll(what = "the accept bar replaced by the message field") {
      !(alice.chatList.chat("bob").chatInfo as ChatInfo.Direct).contact.nextAcceptContactRequest &&
        !exists(Ui.Chat.RequestAccept) && exists(Ui.Chat.Message) && !seesText("Accept contact request")
    }
  }

  @Test fun contactWithRequestComposeReject() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = alice.userPicker().createAddress()
    bob sends "/c $address"; bob expects "connection request sent!"
    alice.chatList.request("bob").open("bob")
    click(Ui.Chat.RequestReject)
    poll(what = "the `Reject contact request` alert") { seesText("Reject contact request") && seesText("The sender will NOT be notified.") }
    poll(what = "the alert's `Reject` button") { "Reject" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    alice.chatList.gone("bob")
    poll(what = "back on the chat list with no chat open") { chatModel.chatId.value == null && exists(Ui.ChatList.Search) }
  }

  @Test fun oneHandToolbarCardSelectBottomBar() = uiTest {
    app("alice")
    appPrefs.oneHandUICardShown.set(false)
    poll(what = "the `Bottom bar` / `Top bar` card") {
      exists(Ui.ChatList.OneHandCard) && "Bottom bar" in textsOf(Ui.ChatList.OneHandCard) && "Top bar" in textsOf(Ui.ChatList.OneHandCard)
    }
    click(Ui.ChatList.OneHandCard.part("top"))
    poll(what = "top toolbars: the floating new-chat button and no toolbar button") {
      !appPrefs.oneHandUI.get() && storedSetting("OneHandUI") == "false" && exists(Ui.ChatList.NewChatFab) && !exists(Ui.ChatList.NewChat)
    }
    click(Ui.ChatList.OneHandCard.part("bottom"))
    poll(what = "bottom toolbars: the toolbar new-chat button and no floating button") {
      appPrefs.oneHandUI.get() && storedSetting("OneHandUI") == "true" && exists(Ui.ChatList.NewChat) && !exists(Ui.ChatList.NewChatFab)
    }
    poll(what = "the card still shown") { exists(Ui.ChatList.OneHandCard) && !appPrefs.oneHandUICardShown.get() }
  }

  @Test fun oneHandToolbarCardDismissShowsAlert() = uiTest {
    app("alice")
    appPrefs.oneHandUICardShown.set(false)
    click(Ui.ChatList.OneHandCard.part("close"))
    poll(what = "the `Reachable app toolbars` alert") { seesText("Reachable app toolbars") && seesText("You can change it in Appearance settings.") }
    poll(what = "the card gone and OneHandUICardShown stored") { !exists(Ui.ChatList.OneHandCard) && storedSetting("OneHandUICardShown") == "true" }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the alert to close") { !seesText("You can change it in Appearance settings.") }
  }

  @Test fun oneHandCardAutoHiddenAfterThreeChats() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob)
    alice.chatList.row("bob").row("Private notes")
    quiesce()
    poll(what = "two listed chats: bob and the notes folder") { chatModel.chats.value.count { !it.chatInfo.contactCard && !it.chatInfo.chatDeleted } == 2 }
    appPrefs.oneHandUICardShown.set(false)
    poll(what = "the card shown with two chats") { exists(Ui.ChatList.OneHandCard) }
    settle()
    poll(what = "the card still shown") { exists(Ui.ChatList.OneHandCard) && !appPrefs.oneHandUICardShown.get() }
    connect(cath)
    alice.chatList.row("cath")
    poll(what = "the card gone with three chats and OneHandUICardShown stored") {
      !exists(Ui.ChatList.OneHandCard) && storedSetting("OneHandUICardShown") == "true"
    }
    poll(what = "no `Reachable app toolbars` alert") { !seesText("You can change it in Appearance settings.") }
  }

  @Test fun connectBannerCardOpensNewLink() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.row("bob")
    quiesce()
    appPrefs.addressCreationCardShown.set(false)
    val newLink = Ui.ChatList.ConnectBanner.part("newLink")
    scrollTo(newLink)
    poll(what = "the banner half `New 1-time link`") { "New 1-time link" in textsOf(newLink) }
    click(newLink)
    poll(what = "New chat on the `1-time link` tab with the link created") {
      seesText("New chat") && seesText("1-time link") && seesText("Share this 1-time invite link") && !seesText("Paste the link you received") &&
        chatModel.showingInvitation.value != null && !editableText(Ui.Link.Text).isNullOrBlank()
    }
  }

  @Test fun connectBannerCardOpensPasteLink() = uiTest {
    assumeCamera()
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.row("bob")
    quiesce()
    appPrefs.addressCreationCardShown.set(false)
    val pasteLink = Ui.ChatList.ConnectBanner.part("pasteLink")
    val label = if (appPlatform.isAndroid) "Paste link / Scan" else "Paste link"
    scrollTo(pasteLink)
    poll(what = "the banner half `$label`") { label in textsOf(pasteLink) }
    click(pasteLink)
    poll(what = "New chat on the `Connect via link` tab") {
      seesText("New chat") && seesText("Connect via link") && seesText("Paste the link you received") && seesText("Tap to paste link") &&
        !seesText("Share this 1-time invite link") && chatModel.showingInvitation.value == null &&
        seesText("Or scan QR code") == appPlatform.isAndroid
    }
  }
}
