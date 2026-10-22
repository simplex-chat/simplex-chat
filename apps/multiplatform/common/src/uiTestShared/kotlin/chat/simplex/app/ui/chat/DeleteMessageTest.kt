package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.common.model.ChatFeature
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.model.ContactFeatureAllowed
import chat.simplex.common.model.contactFeaturesAllowedToPrefs
import chat.simplex.common.model.contactUserPrefsToFeaturesAllowed
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertFalse
import org.junit.Test

class DeleteMessageTest : UiTest() {
  @Test fun delete_received_onlyForMe() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice hello"
    val chat = alice.chatList.open("bob")
    val id = chat.itemId("hello")
    chat.menu("hello", "delete")
    waitFor(Ui.Alert.Option(Ui.AlertSource.Shared, "forMe"))
    assertFalse(exists(Ui.Alert.Option(Ui.AlertSource.Shared, "forEveryone")))
    click(Ui.Alert.Option(Ui.AlertSource.Shared, "forMe"))
    poll(what = "the item removed") { chat.item(id) == null && !exists(Ui.Chat.Item(id)) }
    chat.send("ping")
    bob expectsMessage "alice> ping"
  }

  @Test fun delete_forEveryone_fullDeletion_peerSeesDeleted() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.send("one")
    bob expectsMessage "alice> one"
    val one = chat.itemId("one")
    chat.menu("one", "delete")
    poll(what = "`Delete message?` saying it will be marked for deletion, with both buttons") {
      seesText("Delete message?") && seesText(MARK_ONE) && exists(FOR_ME) && exists(FOR_EVERYONE)
    }
    click(FOR_ME)
    poll(what = "`one` removed from alice's chat") { chat.item(one) == null && !exists(Ui.Chat.Item(one)) }
    bob sends "/set delete @alice always"
    bob expects "you updated preferences for alice:"
    bob expects "Full deletion: enabled for contact (you allow: always, contact allows: no)"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's full-deletion consent reaching alice") { alice.chatList.chat("bob").chatInfo.featureEnabled(ChatFeature.FullDelete) }
    chat.send("two")
    bob expectsMessage "alice> two"
    val two = chat.itemId("two")
    chat.menu("two", "delete")
    poll(what = "`Delete message?` saying it cannot be undone") { seesText(FULL_ONE) && !seesText(MARK_ONE) && exists(FOR_EVERYONE) }
    click(FOR_EVERYONE)
    bob expectsMessage "alice> [deleted] two"
    poll(what = "`two` removed, not marked deleted") { chat.item(two) == null && !exists(Ui.Chat.Item(two)) }
    chat.send("three")
    bob expectsMessage "alice> three"
    chat.send("four")
    bob expectsMessage "alice> four"
    val three = chat.itemId("three"); val four = chat.itemId("four")
    hideKeyboard()
    chat.select("three")
    chat.toggle(four)
    poll(what = "Selected 2") { chat.selectionTitle() == "Selected 2" }
    click(SEL_DELETE)
    poll(what = "`Delete 2 messages?` saying they cannot be undone, with For everyone") { seesText("Delete 2 messages?") && seesText(FULL_MANY) && exists(FOR_EVERYONE) }
    click(FOR_EVERYONE)
    bob expectsMessage "alice> [deleted] three"
    bob expectsMessage "alice> [deleted] four"
    poll(what = "both removed and selection ended") { chat.item(three) == null && chat.item(four) == null && !exists(SEL_TITLE) }
  }

  @Test fun markedDeleted_received_revealHide() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice keep"
    bob sends "@alice one"
    bob sends "@alice two"
    val chat = alice.chatList.open("bob")
    val keep = chat.itemId("keep"); val one = chat.itemId("one"); val two = chat.itemId("two")
    check(textsOf(Ui.Chat.Item(one)).let { t -> t.any { "one" in it } && "marked deleted" !in t }) { "`one` is not shown as sent before bob deletes it" }
    bob sends "\\@alice one"
    bob expects "message marked deleted"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`one` shown as `marked deleted` without its text") {
      textsOf(Ui.Chat.Item(one)).let { t -> "marked deleted" in t && t.none { "one" in it } }
    }
    bob sends "\\@alice two"
    bob expects "message marked deleted"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`2 messages marked deleted` on one row") {
      "2 messages marked deleted" in textsOf(Ui.Chat.Item(two)) && !exists(Ui.Chat.Item(one))
    }
    check(textsOf(Ui.Chat.Item(keep)).any { "keep" in it }) { "`keep` changed after bob's deletions" }
    chat.menu("two", "reveal")
    poll(what = "both texts revealed") { textsOf(Ui.Chat.Item(one)).any { "one" in it } && textsOf(Ui.Chat.Item(two)).any { "two" in it } }
    chat.menu("two", "hide")
    poll(what = "collapsed again") { "2 messages marked deleted" in textsOf(Ui.Chat.Item(two)) && !exists(Ui.Chat.Item(one)) }
    chat.menu("two", "select")
    poll(what = "the collapsed range selected as 2") { chat.selectionTitle() == "Selected 2" }
    escape()
    poll(what = "selection mode to end") { !exists(SEL_TITLE) }
    allowFullDeletionViaApi("bob")
    bob expects "alice updated preferences for you:"
    bob expects "Full deletion: enabled for you (you allow: default (no), contact allows: always)"
    bob sends "@alice three"
    val three = chat.itemId("three")
    bob sends "\\@alice three"
    bob expects "message deleted"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`three` removed from alice's chat") { chat.item(three) == null && !exists(Ui.Chat.Item(three)) }
    check(exists(Ui.Chat.Item(keep)) && "2 messages marked deleted" in textsOf(Ui.Chat.Item(two))) { "`keep` or the collapsed row changed after the full deletion" }
  }

  private fun allowFullDeletionViaApi(contactName: String) {
    val contact = (chatModel.chats.value.first { it.chatInfo.chatViewName == contactName }.chatInfo as ChatInfo.Direct).contact
    val prefs = contactFeaturesAllowedToPrefs(contactUserPrefsToFeaturesAllowed(contact.mergedPreferences).copy(fullDelete = ContactFeatureAllowed.Always))
    runBlocking { chatController.apiSetContactPrefs(null, contact.contactId, prefs) } ?: error("apiSetContactPrefs($contactName) failed")
  }

  private companion object {
    val FOR_ME = Ui.Alert.Option(Ui.AlertSource.Shared, "forMe")
    val FOR_EVERYONE = Ui.Alert.Option(Ui.AlertSource.Shared, "forEveryone")
    val SEL_TITLE = Ui.Chat.Selection.part("title")
    val SEL_DELETE = Ui.Chat.Selection.part("delete")
    const val MARK_ONE = "Message will be marked for deletion. The recipient(s) will be able to reveal this message."
    const val FULL_ONE = "Message will be deleted - this cannot be undone!"
    const val FULL_MANY = "Messages will be deleted - this cannot be undone!"
  }
}
