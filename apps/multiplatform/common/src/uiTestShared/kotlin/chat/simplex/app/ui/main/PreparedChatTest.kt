package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.becomeBot
import chat.simplex.app.ui.harness.createAddress
import chat.simplex.app.ui.harness.createOneTimeLink
import chat.simplex.app.ui.harness.openNewChatFromSearch
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotEquals
import org.junit.Test

class PreparedChatTest : UiTest() {
  @Test fun preparedContactInvitationConnectButton() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val link = bob.createOneTimeLink()
    bob.eventually("and ask them to connect: /c <invitation_link_above>")
    alice.chatList.row("Private notes")
    check(chatModel.chats.value.none { it.chatInfo.chatViewName == "bob" }) { "a chat named bob exists before the link is opened" }
    val chat = openNewChatFromSearch(link, "bob")
    poll(what = "the banner `Tap Connect to chat` and a `Connect` button with no message field") {
      seesText("Tap Connect to chat") && exists(Ui.Chat.Connect) && "Connect" in textsOf(Ui.Chat.Connect) && !exists(Ui.Chat.Message)
    }
    backToChatList()
    poll(what = "`Open to connect` on bob's row") { "Open to connect" in alice.chatList.rowTexts("bob") }
    alice.chatList.open("bob")
    click(Ui.Chat.Connect)
    assertEquals("alice: contact is connected", bob.nextLine(CONNECT_TIMEOUT_MS))
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the message field in place of `Connect` with bob ready") {
      exists(Ui.Chat.Message) && !exists(Ui.Chat.Connect) && !seesText("Tap Connect to chat") &&
        (alice.chatList.chat("bob").chatInfo as? ChatInfo.Direct)?.contact?.sndReady == true
    }
    chat.send("hi")
    bob.expectsMessage("alice> hi", CONNECT_TIMEOUT_MS)
    backToChatList()
    poll(what = "`Open to connect` gone from bob's row") { "Open to connect" !in alice.chatList.rowTexts("bob") }
  }

  @Test fun preparedContactAddressSendRequestWithMessage() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = bob.createAddress(autoAccept = false)
    alice.chatList.row("Private notes")
    val chat = openNewChatFromSearch(address, "bob")
    poll(what = "the banner `Tap Connect to send request`, a message field and `Connect`") {
      seesText("Tap Connect to send request") && exists(Ui.Chat.Message) && exists(Ui.Chat.Connect) && "Connect" in textsOf(Ui.Chat.Connect)
    }
    click(Ui.Chat.Connect)
    poll(what = "`Send contact request?` with `Send request without message` and `Add message`") {
      seesText("Send contact request?") && exists(Ui.Alert.Confirm(Ui.AlertSource.Shared)) &&
        "Send request without message" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) && "Add message" in textsOf(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "the alert to close with `Connect` still shown") { !seesText("Send contact request?") && exists(Ui.Chat.Connect) }
    typeInto(Ui.Chat.Message, "hello")
    poll(what = "`Connect` hidden and the send button enabled once there is text") { !exists(Ui.Chat.Connect) && isEnabled(Ui.Chat.Send) }
    pressEnter(Ui.Chat.Message)
    hideKeyboard()
    poll(what = "`Send contact request?` with `Send request` and `Cancel`") {
      seesText("Send contact request?") && exists(Ui.Alert.Confirm(Ui.AlertSource.Shared)) &&
        "Send request" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) && "Cancel" in textsOf(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    bob.eventuallyAll(
      listOf("alice wants to connect to you!", "to accept: /ac alice", "to reject: /rc alice (the sender will NOT be notified)"),
      messages = listOf("alice> hello"), timeoutMs = CONNECT_TIMEOUT_MS
    )
    poll(what = "the message field cleared and `contact should accept…` shown") {
      editableText(Ui.Chat.Message).isNullOrEmpty() && seesText("contact should accept…")
    }
    chat.itemId("hello")
    bob sends "/ac alice"
    bob.nextLine().let { check(it.startsWith("alice: accepting contact request")) { "bob's next line after /ac is `$it`, not the acceptance" } }
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`contact should accept…` gone with bob ready") {
      !seesText("contact should accept…") && (alice.chatList.chat("bob").chatInfo as? ChatInfo.Direct)?.contact?.sndReady == true
    }
    chat.send("hi")
    bob.expectsMessage("alice> hi", CONNECT_TIMEOUT_MS)
  }

  @Test fun preparedBotConnect() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    bob.becomeBot()
    val address = bob.createAddress()
    alice.chatList.row("Private notes")
    openNewChatFromSearch(address, "bob")
    poll(what = "the banner `Tap Connect to use bot` and `Connect` with no message field") {
      seesText("Tap Connect to use bot") && exists(Ui.Chat.Connect) && "Connect" in textsOf(Ui.Chat.Connect) &&
        !exists(Ui.Chat.Message) && !seesText("Tap Connect to send request")
    }
    backToChatList()
    poll(what = "`Open to use bot` on bob's row") { alice.chatList.rowTexts("bob").let { "Open to use bot" in it && "Open to connect" !in it } }
    val chat = alice.chatList.open("bob")
    click(Ui.Chat.Connect)
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the message field and the `Bot` banner after connecting") {
      exists(Ui.Chat.Message) && !exists(Ui.Chat.Connect) && seesText("Bot") && !seesText("Tap Connect to use bot")
    }
    chat.send("hi")
    bob.expectsMessage("alice> hi", CONNECT_TIMEOUT_MS)
  }

  @Test fun preparedContactIncognitoBeforeConnect() = uiTest {
    app("alice"); val bob = cli("bob")
    val link = bob.createOneTimeLink()
    bob.eventually("and ask them to connect: /c <invitation_link_above>")
    val aliceId = chatModel.currentUser.value!!.userId
    openNewChatFromSearch(link, "bob")
    poll(what = "`Your profile` with alice selected and no `Incognito` option") {
      seesText("Your profile") && exists(Ui.Chat.PickerUser(aliceId)) && !exists(Ui.Chat.PickerIncognito)
    }
    click(Ui.Chat.PickerUser(aliceId))
    poll(what = "the profile list with `Incognito`") { exists(Ui.Chat.PickerIncognito) }
    click(Ui.Chat.PickerIncognito)
    poll(what = "the list collapsed to `Incognito`") { exists(Ui.Chat.PickerIncognito) && !exists(Ui.Chat.PickerUser(aliceId)) }
    click(Ui.Chat.Connect)
    val shared = bob.eventuallyMatching(Regex("(\\S+): contact is connected"), CONNECT_TIMEOUT_MS).groupValues[1]
    assertNotEquals("alice", shared)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the profile picker gone and the message field shown") {
      !seesText("Your profile") && !exists(Ui.Chat.PickerIncognito) && exists(Ui.Chat.Message)
    }
  }

  @Test fun preparedContactChangeProfileBeforeConnect() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    alice.userPicker().profiles().apply { add("alice2"); close() }
    alice.userPicker().switchTo("alice")
    val link = bob.createOneTimeLink()
    bob.eventually("and ask them to connect: /c <invitation_link_above>")
    val aliceId = chatModel.users.first { it.user.displayName == "alice" }.user.userId
    val alice2Id = chatModel.users.first { it.user.displayName == "alice2" }.user.userId
    openNewChatFromSearch(link, "bob")
    val bobChatId = chatModel.chatId.value
    poll(what = "the picker with alice selected") { exists(Ui.Chat.PickerUser(aliceId)) && !exists(Ui.Chat.PickerUser(alice2Id)) }
    click(Ui.Chat.PickerUser(aliceId))
    poll(what = "alice2 in the profile list") { exists(Ui.Chat.PickerUser(alice2Id)) }
    click(Ui.Chat.PickerUser(alice2Id))
    poll(what = "alice2 active with bob's prepared chat still open and selected in the picker") {
      chatModel.currentUser.value?.userId == alice2Id && chatModel.chatId.value == bobChatId &&
        exists(Ui.Chat.PickerUser(alice2Id)) && !exists(Ui.Chat.PickerUser(aliceId))
    }
    click(Ui.Chat.Connect)
    assertEquals("alice2: contact is connected", bob.nextLine(CONNECT_TIMEOUT_MS))
  }
}
