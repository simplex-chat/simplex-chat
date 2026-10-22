package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.described
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertFalse
import org.junit.Test

class MultiSelectTest : UiTest() {
  @Test fun select_toggle_updatesCounter() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice one"
    bob sends "@alice two"
    val chat = alice.chatList.open("bob")
    chat.send("mine")
    bob expectsMessage "alice> mine"
    val one = chat.itemId("one"); val two = chat.itemId("two"); val mine = chat.itemId("mine")
    waitFor(Ui.Chat.Bar.part("title"))
    assertFalse("selection title before Select", exists(SEL_TITLE))
    assertFalse("selection Delete before Select", exists(SEL_DELETE))
    hideKeyboard()
    chat.menu("one", "select")
    poll(what = "`Selected 1` instead of bob's chat bar, Delete enabled") { chat.selectionTitle() == "Selected 1" && !exists(Ui.Chat.Bar.part("title")) && isEnabled(SEL_DELETE) }
    chat.toggle(two)
    poll(what = "Selected 2") { chat.selectionTitle() == "Selected 2" }
    chat.toggle(mine)
    poll(what = "Selected 3") { chat.selectionTitle() == "Selected 3" }
    chat.toggle(one)
    poll(what = "Selected 2 after unselecting `one`") { chat.selectionTitle() == "Selected 2" }
    chat.toggle(two)
    poll(what = "Selected 1") { chat.selectionTitle() == "Selected 1" }
    chat.toggle(mine)
    poll(what = "`Nothing selected` with Delete disabled") { chat.selectionTitle() == "Nothing selected" && exists(SEL_DELETE) && !isEnabled(SEL_DELETE) }
    click(described("Back"))
    poll(what = "selection ended with bob's chat bar back and all three messages kept") {
      !exists(SEL_TITLE) && !exists(SEL_DELETE) && exists(Ui.Chat.Bar.part("title")) && listOf(one, two, mine).all { chat.item(it) != null && exists(Ui.Chat.Item(it)) }
    }
  }

  @Test fun selectDelete_forEveryone_peerSeesBoth() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.send("hello")
    bob expectsMessage "alice> hello"
    bob sends "@alice hey"
    chat.send("hi")
    bob expectsMessage "alice> hi"
    chat.send("bye")
    bob expectsMessage "alice> bye"
    val hello = chat.itemId("hello"); val hey = chat.itemId("hey"); val hi = chat.itemId("hi"); val bye = chat.itemId("bye")
    hideKeyboard()
    chat.select("hello")
    chat.toggle(hey)
    poll(what = "Selected 2") { chat.selectionTitle() == "Selected 2" }
    click(SEL_DELETE)
    poll(what = "`Delete 2 messages?` saying they cannot be undone, with Delete for me") { seesText("Delete 2 messages?") && seesText(FULL_MANY) && exists(FOR_ME) }
    assertFalse("For everyone offered for a selection with bob's message", exists(FOR_EVERYONE))
    click(FOR_ME)
    poll(what = "`hello` and `hey` removed, selection ended, `hi` kept") {
      chat.item(hello) == null && chat.item(hey) == null && !exists(Ui.Chat.Item(hello)) && !exists(Ui.Chat.Item(hey)) && !exists(SEL_TITLE) && exists(Ui.Chat.Item(hi))
    }
    chat.select("hi")
    chat.toggle(bye)
    poll(what = "Selected 2") { chat.selectionTitle() == "Selected 2" }
    click(SEL_DELETE)
    poll(what = "`Delete 2 messages?` saying they will be marked for deletion, with For everyone") { seesText(MARK_MANY) && exists(FOR_EVERYONE) }
    click(FOR_EVERYONE)
    bob expectsMessage "alice> [marked deleted] hi"
    bob expectsMessage "alice> [marked deleted] bye"
    poll(what = "alice's two messages collapsed into `2 messages marked deleted`") { "2 messages marked deleted" in textsOf(Ui.Chat.Item(bye)) && !exists(Ui.Chat.Item(hi)) }
  }

  @KnownAppIssue("15.5 item 22", "Forward disabled and Delete enabled after bob deleted it")
  @Test fun select_itemDeletedByPeer_recheck() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice one"
    val chat = alice.chatList.open("bob")
    chat.send("mine")
    bob expectsMessage "alice> mine"
    val one = chat.itemId("one"); val mine = chat.itemId("mine")
    val forward = Ui.Chat.Selection.part("forward")
    hideKeyboard()
    chat.select("one")
    chat.toggle(mine)
    poll(what = "`Selected 2` with Forward and Delete enabled") { chat.selectionTitle() == "Selected 2" && isEnabled(forward) && isEnabled(SEL_DELETE) }
    bob sends "\\@alice one"
    bob.eventually("message marked deleted")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's `one` marked deleted") { chat.item(one)?.meta?.itemDeleted != null }
    poll(what = "`one` still selected, Forward disabled and Delete enabled after bob deleted it") {
      chat.selectionTitle() == "Selected 2" && exists(forward) && !isEnabled(forward) && isEnabled(SEL_DELETE)
    }
    chat.toggle(one)
    poll(what = "`Selected 1` with Forward enabled again once the deleted message is unselected") {
      chat.selectionTitle() == "Selected 1" && isEnabled(forward) && isEnabled(SEL_DELETE)
    }
  }

  private companion object {
    val FOR_ME = Ui.Alert.Option(Ui.AlertSource.Shared, "forMe")
    val FOR_EVERYONE = Ui.Alert.Option(Ui.AlertSource.Shared, "forEveryone")
    val SEL_TITLE = Ui.Chat.Selection.part("title")
    val SEL_DELETE = Ui.Chat.Selection.part("delete")
    const val FULL_MANY = "Messages will be deleted - this cannot be undone!"
    const val MARK_MANY = "Messages will be marked for deletion. The recipient(s) will be able to reveal these messages."
  }
}
