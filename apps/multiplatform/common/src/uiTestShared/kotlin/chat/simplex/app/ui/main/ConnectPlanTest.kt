package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.PeerFlags
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.chatLinkItemId
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.createAddress
import chat.simplex.app.ui.harness.createFullAddress
import chat.simplex.app.ui.harness.createFullOneTimeLink
import chat.simplex.app.ui.harness.enterLinkInSearch
import chat.simplex.app.ui.harness.joinLink
import chat.simplex.app.ui.harness.keepOneTimeLink
import chat.simplex.app.ui.harness.openPasteLink
import chat.simplex.app.ui.harness.planConfirm
import chat.simplex.app.ui.harness.planDismiss
import chat.simplex.app.ui.harness.planProfileName
import chat.simplex.app.ui.harness.planOption
import chat.simplex.app.ui.harness.quiesce
import chat.simplex.app.ui.harness.searchShowsLink
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.MsgContent
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertNotEquals
import org.junit.Test

class ConnectPlanTest : UiTest() {
  @Test fun pasteLinkInvalidTextAlert() = uiTest {
    assumeCamera()
    val alice = app("alice")
    val address = alice.userPicker().createAddress()
    openPasteLink(alice)
    setClipboardText(null)
    click(Ui.Link.Paste)
    settle()
    check(exists(Ui.Link.Paste) && "Tap to paste link" in textsOf(Ui.Link.Paste)) { "the paste row is gone after a tap with an empty clipboard" }
    check(!seesText("Invalid link!") && !exists(Ui.Alert.Confirm(Ui.AlertSource.Shared)) && !exists(planConfirm)) { "an alert opened for an empty clipboard" }
    setClipboardText("https://example.com/invite")
    click(Ui.Link.Paste)
    poll(what = "the `Invalid link!` alert for a web link") { seesText("Invalid link!") && seesText("The text you pasted is not a SimpleX link.") }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the alert to close with the paste row still offered") { !seesText("Invalid link!") && exists(Ui.Link.Paste) }
    setClipboardText(address)
    click(Ui.Link.Paste)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `Connect to yourself?` alert with the pasted link in place of the row") {
      seesText("Connect to yourself?") && !exists(Ui.Link.Paste)
    }
    click(planOption("cancel"))
    poll(what = "the alert to close and the paste row to return") { !seesText("Connect to yourself?") && exists(Ui.Link.Paste) }
  }

  @Test fun planOwnInvitationConnectToYourself() = uiTest {
    val alice = app("alice")
    val address = alice.userPicker().createAddress()
    alice.chatList.row("Private notes")
    check(chatModel.chats.value.none { it.chatInfo.chatViewName == "alice" }) { "a chat named alice exists before alice connects to herself" }
    enterLinkInSearch(address)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `Connect to yourself?` alert for alice's own address with three buttons") {
      seesText("Connect to yourself?") && seesText("This is your own SimpleX address!", substring = true) &&
        exists(planOption("useCurrentProfile")) && exists(planOption("useIncognito")) && exists(planOption("cancel"))
    }
    click(planOption("useCurrentProfile"))
    poll(what = "`Connection request sent!` for an address") {
      seesText("Connection request sent!") && seesText("You will be connected when your connection request is accepted, please wait or check later!")
    }
    click(planConfirm)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice's own contact request in the model") { chatModel.chats.value.any { it.chatInfo.chatViewName == "alice" } }
    alice.chatList.request("alice")
    poll(what = "the search field cleared and editable") { editableText(Ui.ChatList.Search).isNullOrEmpty() && isEnabled(Ui.ChatList.Search) }

    val pendingIds = { chatModel.chats.value.filter { it.chatInfo is ChatInfo.ContactConnection }.map { it.id }.toSet() }
    val contactsNamedAlice = {
      chatModel.chats.value.filter { (it.chatInfo as? ChatInfo.Direct)?.contact?.let { c -> c.profile.displayName == "alice" && c.sndReady } == true }
    }
    val pendingBefore = pendingIds()
    val link = keepOneTimeLink(alice)
    check(contactsNamedAlice().isEmpty()) { "a connected contact named alice exists before alice uses her own 1-time link" }
    enterLinkInSearch(link)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `Connect to yourself?` alert for alice's own 1-time link") {
      seesText("Connect to yourself?") && seesText("This is your own one-time link!", substring = true)
    }
    click(planOption("useCurrentProfile"))
    poll(what = "`Connection request sent!` for a 1-time link") {
      seesText("Connection request sent!") && seesText("You will be connected when your contact's device is online, please wait or check later!")
    }
    click(planConfirm)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "two contacts named alice connected to each other and the 1-time link no longer pending") {
      contactsNamedAlice().let { c -> c.size == 2 && c.all { exists(Ui.ChatList.Row(it.id)) } } && pendingIds() == pendingBefore
    }
  }

  @Test fun planInvitationAlreadyConnectingFullLink() = uiTest {
    val alice = app("alice"); val bob = cli("bob", PeerFlags(fullLinks = true))
    val full = bob.createFullOneTimeLink()
    bob.stop()
    alice.chatList.row("Private notes")
    val pending = { chatModel.chats.value.count { it.chatInfo is ChatInfo.ContactConnection } }
    enterLinkInSearch(full)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `Connect via one-time link?` alert for the full link") {
      searchShowsLink("SimpleX one-time invitation") && seesText("Connect via one-time link?") &&
        seesText("Your profile will be sent to the contact that you received this link from.", substring = true)
    }
    click(planOption("cancel"))
    poll(what = "the alert to close and the search to clear") {
      !seesText("Connect via one-time link?") && editableText(Ui.ChatList.Search).isNullOrEmpty() && isEnabled(Ui.ChatList.Search)
    }
    check(pending() == 0) { "Cancel created a pending connection" }
    enterLinkInSearch(full)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `Connect via one-time link?` alert again") { seesText("Connect via one-time link?") }
    click(planOption("useCurrentProfile"))
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`Connection request sent!` for a 1-time link") {
      seesText("Connection request sent!") && seesText("You will be connected when your contact's device is online, please wait or check later!")
    }
    click(planConfirm)
    poll(what = "one pending connection and the search cleared") {
      pending() == 1 && editableText(Ui.ChatList.Search).isNullOrEmpty() && isEnabled(Ui.ChatList.Search)
    }
    enterLinkInSearch(full)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`Already connecting!` for the same link while bob is offline") {
      seesText("Already connecting!") && seesText("You are already connecting via this one-time link!", substring = true) && !seesText("Connect via one-time link?")
    }
    click(planConfirm)
    poll(what = "the alert to close with still one pending connection") { !seesText("Already connecting!") && pending() == 1 }
    bob.start()
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob connected and no pending connection left") {
      chatModel.chats.value.any { (it.chatInfo as? ChatInfo.Direct)?.contact?.let { c -> c.displayName == "bob" && c.sndReady } == true } && pending() == 0
    }
    alice.chatList.row("bob")
  }

  @Test fun planAddressRepeatConnectionRequest() = uiTest {
    val alice = app("alice"); val bob = cli("bob", PeerFlags(fullLinks = true))
    val full = bob.createFullAddress()
    alice.chatList.row("Private notes")
    enterLinkInSearch(full)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `Connect via contact address?` alert for the full address") {
      searchShowsLink("SimpleX contact address") && seesText("Connect via contact address?") &&
        seesText("Your profile will be sent to the contact that you received this link from.", substring = true)
    }
    click(planOption("useIncognito"))
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`Connection request sent!` for an address") {
      seesText("Connection request sent!") && seesText("You will be connected when your connection request is accepted, please wait or check later!")
    }
    click(planConfirm)
    val rand = bob.eventuallyMatching(Regex("(\\S+) wants to connect to you!"), CONNECT_TIMEOUT_MS).groupValues[1]
    assertNotEquals("alice", rand)
    bob expects "to accept: /ac $rand"
    bob expects "to reject: /rc $rand (the sender will NOT be notified)"
    poll(what = "the search field cleared and editable") { editableText(Ui.ChatList.Search).isNullOrEmpty() && isEnabled(Ui.ChatList.Search) }
    enterLinkInSearch(full)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`Repeat connection request?` for the pending request") {
      seesText("Repeat connection request?") && seesText("You have already requested connection via this address!", substring = true) &&
        !seesText("Connect via contact address?")
    }
    click(planOption("cancel"))
    poll(what = "the alert to close and the search to clear") {
      !seesText("Repeat connection request?") && editableText(Ui.ChatList.Search).isNullOrEmpty() && isEnabled(Ui.ChatList.Search)
    }
    bob sends "/ac $rand"
    bob.nextLine().let { check(it.startsWith("$rand: accepting contact request")) { "bob's next line after /ac is `$it`, not the acceptance" } }
    bob.eventually("$rand: contact is connected", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "an incognito contact bob ready to send") {
      chatModel.chats.value.any { (it.chatInfo as? ChatInfo.Direct)?.contact?.let { c -> c.displayName == "bob" && c.contactConnIncognito && c.sndReady } == true }
    }
  }

  @Test fun planViaChatItemLinkCard() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(cath)
    val bobAddress = bob.createAddress()
    joinLink(bobAddress)
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    val cathAddress = cath.createAddress()
    cath sends "/share address @alice"
    cath.eventuallyMessage("@alice contact address of @cath (signed):")
    cath expects cathAddress
    bob sends "/share address @alice"
    bob.eventuallyMessage("@alice contact address of @bob (signed):")
    bob expects bobAddress
    val bobId = alice.chatList.chatId("bob"); val cathId = alice.chatList.chatId("cath")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "both address cards received") {
      listOf(bobId, cathId).all { id -> chatModel.chats.value.firstOrNull { it.id == id }?.chatItems?.lastOrNull()?.content?.msgContent is MsgContent.MCChat }
    }
    quiesce()

    enterLinkInSearch(cathAddress)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the plan alert for cath's pasted address with `Open new chat`") {
      exists(planConfirm) && "Open new chat" in textsOf(planConfirm) && textOf(planProfileName) == "cath"
    }
    check(!seesText("Link signature verified.")) { "a pasted address with no signature shows `Link signature verified.`" }
    click(planDismiss)
    poll(what = "the alert to close and the search to clear") { !exists(planConfirm) && editableText(Ui.ChatList.Search).isNullOrEmpty() }

    backToChatList()
    click(Ui.ChatList.Row(cathId).part("link"))
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the plan alert for cath's chat-list card with `Link signature verified.`") {
      textOf(planProfileName) == "cath" && seesText("Link signature verified.") && exists(planConfirm) && "Open new chat" in textsOf(planConfirm)
    }
    click(planDismiss)
    poll(what = "the alert to close") { !exists(planConfirm) }

    val bobChat = alice.chatList.open("bob")
    poll(what = "bob's chat open") { chatModel.chatId.value == bobId }
    val bobCard = chatLinkItemId()
    poll(what = "bob's card with his name, `Contact address (from owner)` and `Tap to open`") {
      textsOf(Ui.Chat.Item(bobCard)).containsAll(listOf("bob", "Contact address (from owner)", "Tap to open"))
    }
    click(Ui.Chat.Item(bobCard).part("chatLink"))
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the known-contact alert for bob with `Open chat`") {
      exists(planConfirm) && "Open chat" in textsOf(planConfirm) && textOf(planProfileName) == "bob" && !seesText("Link signature verified.")
    }
    click(planConfirm)
    poll(what = "bob's chat still open and no second chat named bob") {
      !exists(planConfirm) && chatModel.chatId.value == bobId && chatModel.chats.value.count { it.chatInfo.chatViewName == "bob" } == 1
    }
    bobChat.send("still here")
    bob.expectsMessage("alice> still here", CONNECT_TIMEOUT_MS)

    backToChatList()
    alice.chatList.open("cath")
    poll(what = "cath's chat open") { chatModel.chatId.value == cathId }
    val cathCard = chatLinkItemId()
    poll(what = "cath's card with her name, `Contact address (from owner)` and `Tap to open`, and no link text") {
      textsOf(Ui.Chat.Item(cathCard)).let { t -> t.containsAll(listOf("cath", "Contact address (from owner)", "Tap to open")) && cathAddress !in t }
    }
    click(Ui.Chat.Item(cathCard).part("chatLink"))
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the plan alert for cath's card with `Link signature verified.` and `Open new chat`") {
      seesText("Link signature verified.") && exists(planConfirm) && "Open new chat" in textsOf(planConfirm)
    }
    click(planConfirm)
    poll(what = "a new prepared chat with cath open") {
      chatModel.chatId.value != cathId && seesText("Tap Connect to send request") && chatModel.chats.value.count { it.chatInfo.chatViewName == "cath" } == 2
    }
  }
}
