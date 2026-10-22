package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitRowText
import chat.simplex.app.ui.harness.archiveContact
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.contactOf
import chat.simplex.app.ui.harness.createAddress
import chat.simplex.app.ui.harness.enterLinkInSearch
import chat.simplex.app.ui.harness.openArchivedContact
import chat.simplex.app.ui.harness.openNewChatFromSearch
import chat.simplex.app.ui.harness.planConfirm
import chat.simplex.app.ui.harness.planDismiss
import chat.simplex.app.ui.harness.presetFilter
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.chatlist.PresetTagKind
import chat.simplex.common.views.helpers.ModalManager
import org.junit.Test

class DeleteContactTest : UiTest() {
  @Test fun deleteContactOnlyConversation() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    bob sends "@alice hello"
    val bobId = alice.chatList.chatId("bob")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's `hello` on his row") { "hello" in alice.chatList.rowTexts("bob") }
    alice.chatList.row("cath")
    appPrefs.showDeleteConversationNotice.set(true)
    poll(what = "the conversation notice stored as enabled") { storedSetting(CONVERSATION_NOTICE) == "true" }
    alice.chatList.rowMenu("bob", "delete")
    poll(what = "`Delete contact?` with `Only delete conversation`, `Delete contact` and `Cancel`") {
      seesText("Delete contact?") && "Only delete conversation" in textsOf(ONLY_CONVERSATION) &&
        "Delete contact" in textsOf(DELETE_CONTACT) && "Cancel" in textsOf(CANCEL)
    }
    click(CANCEL)
    poll(what = "the alert closed with bob's row and `hello` kept") {
      !exists(ONLY_CONVERSATION) && "hello" in alice.chatList.rowTexts("bob") && !contactOf("bob").chatDeleted
    }
    alice.chatList.rowMenu("bob", "delete")
    click(ONLY_CONVERSATION)
    poll(what = "the `Conversation deleted!` notice with `Don't show again`") {
      seesText("Conversation deleted!") && seesText("You can send messages to bob from Archived contacts.") &&
        "Don't show again" in textsOf(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "the notice closed and turned off on disk") {
      !seesText("Conversation deleted!") && !appPrefs.showDeleteConversationNotice.get() && storedSetting(CONVERSATION_NOTICE) == "false"
    }
    poll(what = "bob's row hidden with his contact archived, and cath still listed") {
      contactOf("bob").chatDeleted && !exists(Ui.ChatList.Row(bobId)) && alice.chatList.shown("cath")
    }
    presetFilter(PresetTagKind.CONTACTS, "Contacts")
    poll(what = "only cath under `Contacts`") {
      alice.chatList.shown("cath") && !exists(Ui.ChatList.Row(bobId)) && !alice.chatList.shown("Private notes")
    }
    bob sends "@alice hi"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob back under `Contacts` with `hi` and without `hello`") {
      !contactOf("bob").chatDeleted && exists(Ui.ChatList.Row(bobId)) && alice.chatList.rowTexts("bob").let { "hi" in it && "hello" !in it }
    }
    val chat = alice.chatList.open("bob")
    chat.itemId("hi")
    check("hello" !in chat.itemTexts()) { "the deleted conversation came back with bob's new message" }
    chat.send("back")
    bob.expectsMessage("alice> back", CONNECT_TIMEOUT_MS)
  }

  @Test fun deleteContactWithoutNotification() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    alice.chatList.row("cath")
    val bobId = alice.chatList.chatId("bob")
    alice.chatList.open("bob").info()
    click(Ui.Info.DeleteContact)
    click(DELETE_CONTACT)
    poll(what = "the second `Delete contact?` alert with `Keep conversation` and both delete options") {
      seesText("bob\n\nContact will be deleted - this cannot be undone!") && exists(KEEP_CONVERSATION) &&
        "Delete without notification" in textsOf(WITHOUT_NOTIFICATION) && "Delete and notify contact" in textsOf(AND_NOTIFY)
    }
    click(WITHOUT_NOTIFICATION)
    poll(what = "contact info and bob's chat closed") { chatModel.chatId.value == null && !ModalManager.end.hasModalsOpen() && !exists(Ui.Info.DeleteContact) }
    alice.chatList.gone("bob")
    check(!exists(Ui.ChatList.Row(bobId)) && alice.chatList.shown("cath")) { "bob's row is listed or cath's row is gone after deleting bob" }
    alice.chatList.rowMenu("cath", "delete")
    click(DELETE_CONTACT)
    click(AND_NOTIFY)
    cath.eventually("alice deleted contact with you", CONNECT_TIMEOUT_MS)
    alice.chatList.gone("cath")
    bob sends "/u"
    bob expects "user profile: bob"
  }

  @Test fun deleteContactKeepConversation() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice hello"
    val bobId = alice.chatList.chatId("bob")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's `hello` on his row") { "hello" in alice.chatList.rowTexts("bob") }
    appPrefs.showDeleteContactNotice.set(true)
    alice.chatList.rowMenu("bob", "delete")
    click(DELETE_CONTACT)
    click(KEEP_CONVERSATION)
    click(AND_NOTIFY)
    poll(what = "the `Contact deleted!` notice with `OK` and `Don't show again`") {
      seesText("Contact deleted!") && seesText("You can still view conversation with bob in the list of chats.") &&
        "OK" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) && "Don't show again" in textsOf(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    }
    bob.eventually("alice deleted contact with you", CONNECT_TIMEOUT_MS)
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the notice closed and still enabled on disk") {
      !seesText("Contact deleted!") && appPrefs.showDeleteContactNotice.get() && storedSetting(CONTACT_NOTICE) == "true"
    }
    poll(what = "bob's row kept with `hello` and his contact deleted") {
      !contactOf("bob").active && exists(Ui.ChatList.Row(bobId)) && "hello" in alice.chatList.rowTexts("bob")
    }
    val chat = alice.chatList.open("bob")
    chat.itemId("hello")
    poll(what = "`contact deleted` in place of the message field") { seesText("contact deleted") }
  }

  @Test fun contactDeletedByPeerShowsDeleted() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice hello"
    awaitRowText(alice, "bob", "hello")
    val chat = alice.chatList.open("bob")
    chat.itemId("hello")
    check(contactOf("bob").active && !seesText("contact deleted")) { "bob's contact is inactive before he deletes it" }
    bob sends "/d alice"
    bob expects "alice: contact is deleted"
    chat.itemId("bob's `deleted contact` event") { it.content.text == "deleted contact" }
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's contact inactive and `contact deleted` in place of the message field") {
      !contactOf("bob").active && seesText("contact deleted")
    }
  }

  @Test fun deleteArchivedContactConfirm() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    archiveContact(alice, "bob")
    archiveContact(alice, "cath")
    openArchivedContact(alice, "cath").contactInfo()
    click(Ui.Info.DeleteContact)
    awaitConfirmArchivedDeletion("cath")
    click(CANCEL)
    poll(what = "the alert closed with cath still archived") { !seesText(CONFIRM_DELETION) && contactOf("cath").chatDeleted }
    click(Ui.Info.DeleteContact)
    awaitConfirmArchivedDeletion("cath")
    click(WITHOUT_NOTIFICATION)
    alice.chatList.gone("cath")
    backToChatList()
    openArchivedContact(alice, "bob").contactInfo()
    click(Ui.Info.DeleteContact)
    awaitConfirmArchivedDeletion("bob")
    click(AND_NOTIFY)
    bob.eventually("alice deleted contact with you", CONNECT_TIMEOUT_MS)
    alice.chatList.gone("bob")
    cath sends "/u"
    cath expects "user profile: cath"
  }

  @Test fun deleteNotReadyContactConfirm() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = bob.createAddress(autoAccept = false)
    alice.chatList.row("Private notes")
    val chat = openNewChatFromSearch(address, "bob")
    click(Ui.Chat.Connect)
    poll(what = "`Send contact request?` with `Send request without message`") {
      seesText("Send contact request?") && "Send request without message" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    bob.eventuallyAll(
      listOf("alice wants to connect to you!", "to accept: /ac alice", "to reject: /rc alice (the sender will NOT be notified)"),
      timeoutMs = CONNECT_TIMEOUT_MS
    )
    poll(what = "bob's contact connecting and not ready") { contactOf("bob").let { it.activeConn != null && !it.ready } }
    val bobId = alice.chatList.chatId("bob")
    hideKeyboard()
    chat.info()
    click(Ui.Info.DeleteContact)
    awaitConfirmNotReadyDeletion()
    click(CANCEL)
    poll(what = "the alert closed with bob kept") { !seesText(CONFIRM_DELETION) && chatModel.chats.value.any { it.id == bobId } }
    click(Ui.Info.DeleteContact)
    awaitConfirmNotReadyDeletion()
    click(CONFIRM)
    alice.chatList.gone("bob")
    poll(what = "contact info and bob's chat closed") { chatModel.chatId.value == null && !ModalManager.end.hasModalsOpen() }
    backToChatList()
    enterLinkInSearch(address)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the plan for bob's address offering `Open new chat` again, as the contact is gone from the core") {
      exists(planConfirm) && "Open new chat" in textsOf(planConfirm) && seesText("bob")
    }
    click(planDismiss)
  }

  private fun UiScope.awaitConfirmArchivedDeletion(name: String) =
    poll(what = "`Confirm contact deletion?` for the archived $name with both delete options and `Cancel`") {
      seesText(CONFIRM_DELETION) && seesText("$name\n\n$CANNOT_UNDO") && "Delete and notify contact" in textsOf(AND_NOTIFY) &&
        "Delete without notification" in textsOf(WITHOUT_NOTIFICATION) && "Cancel" in textsOf(CANCEL) && !seesText("Delete contact?")
    }

  private fun UiScope.awaitConfirmNotReadyDeletion() =
    poll(what = "`Confirm contact deletion?` for the not-ready bob with only `Confirm` and `Cancel`") {
      seesText(CONFIRM_DELETION) && seesText("bob\n\n$CANNOT_UNDO") && "Confirm" in textsOf(CONFIRM) && "Cancel" in textsOf(CANCEL) &&
        !exists(AND_NOTIFY) && !exists(WITHOUT_NOTIFICATION)
    }

  private companion object {
    val ONLY_CONVERSATION = Ui.Alert.Option(Ui.AlertSource.Shared, "onlyConversation")
    val DELETE_CONTACT = Ui.Alert.Option(Ui.AlertSource.Shared, "deleteContact")
    val CANCEL = Ui.Alert.Option(Ui.AlertSource.Shared, "cancel")
    val KEEP_CONVERSATION = Ui.Alert.Option(Ui.AlertSource.Shared, "keepConversation")
    val WITHOUT_NOTIFICATION = Ui.Alert.Option(Ui.AlertSource.Shared, "deleteWithoutNotification")
    val AND_NOTIFY = Ui.Alert.Option(Ui.AlertSource.Shared, "deleteAndNotify")
    val CONFIRM = Ui.Alert.Option(Ui.AlertSource.Shared, "confirm")
    const val CONFIRM_DELETION = "Confirm contact deletion?"
    const val CANNOT_UNDO = "Contact will be deleted - this cannot be undone!"
    const val CONVERSATION_NOTICE = "showDeleteConversationNotice"
    const val CONTACT_NOTICE = "showDeleteContactNotice"
  }
}
