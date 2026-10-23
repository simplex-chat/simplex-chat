package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitRowText
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.app.ui.harness.confirmAlert
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.described
import chat.simplex.app.ui.harness.emptyGroup
import chat.simplex.app.ui.harness.openItemInfo
import chat.simplex.app.ui.harness.openTimedMenu
import chat.simplex.app.ui.harness.ownGroup
import chat.simplex.common.model.CIDeleted
import chat.simplex.common.model.CIForwardedFrom
import chat.simplex.common.model.CIStatus
import chat.simplex.common.model.ChatItem
import chat.simplex.common.model.SndCIStatusProgress
import chat.simplex.common.model.localTimestamp
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertFalse
import org.junit.Test

class ItemInfoTest : UiTest() {
  @Test fun info_sent_showsSentAt() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.send("mine")
    bob expectsMessage "alice> mine"
    bob sends "@alice theirs"
    val mine = chat.itemId("mine"); val theirs = chat.itemId("theirs")
    chat.itemInfo("mine").run {
      val sentAt = localTimestamp(chat.item(mine)!!.meta.itemTs)
      check(row("Sent at") == sentAt) { "`Sent at` of alice's message: `${row("Sent at")}`, expected `$sentAt`" }
      check(!seesText("Received message") && !seesText("Received at") && !seesText("Deleted at")) { "received or deleted rows in the info of alice's message" }
      close()
    }
    chat.receivedItemInfo("theirs").run {
      val meta = chat.item(theirs)!!.meta
      check(row("Sent at") == localTimestamp(meta.itemTs) && row("Received at") == localTimestamp(meta.createdAt)) {
        "`Sent at` / `Received at` of bob's message: `${row("Sent at")}` / `${row("Received at")}`"
      }
      check(!seesText("Sent message") && !seesText("Deleted at")) { "sent or deleted rows in the info of bob's message" }
      close()
    }
    bob sends "\\@alice theirs"
    bob expects "message marked deleted"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's message marked deleted") {
      chat.item(theirs)?.meta?.itemDeleted is CIDeleted.Deleted && "marked deleted" in textsOf(Ui.Chat.Item(theirs))
    }
    chat.receivedItemInfo("theirs").run {
      val deletedAt = (chat.item(theirs)!!.meta.itemDeleted as CIDeleted.Deleted).deletedTs!!.let(::localTimestamp)
      check(row("Deleted at") == deletedAt) { "`Deleted at` of bob's deleted message: `${row("Deleted at")}`, expected `$deletedAt`" }
      close()
    }
  }

  @Test fun info_delivery_listsGroupMembers() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    val direct = alice.chatList.open("bob")
    direct.send("just you")
    bob expectsMessage "alice> just you"
    direct.itemInfo("just you").run {
      assertFalse("the info of a direct message has a Delivery tab", seesText("Delivery"))
      close()
    }
    backToChatList()
    val chat = ownGroup(alice, "team", bob, cath)
    chat.send("hello team")
    bob.eventuallyMessage("#team alice> hello team", CONNECT_TIMEOUT_MS)
    cath.eventuallyMessage("#team alice> hello team", CONNECT_TIMEOUT_MS)
    chat.itemInfo("hello team").run {
      val bobId = memberId("bob"); val cathId = memberId("cath")
      poll(what = "the Delivery tab listing bob and cath") {
        seesText("Delivery") && listed(bobId, "bob") && listed(cathId, "cath")
      }
      close()
    }
  }

  @Test fun info_delivery_liveUpdatesOnReceipt() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    val chat = ownGroup(alice, "team", bob, cath)
    cath.eventually("#team: member bob is connected", CONNECT_TIMEOUT_MS)
    cath.stop()
    chat.send("hi")
    bob.eventuallyMessage("#team alice> hi", CONNECT_TIMEOUT_MS)
    val hi = chat.itemId("hi")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`hi` received by bob only") {
      (chat.item(hi)?.meta?.itemStatus as? CIStatus.SndRcvd)?.sndProgress == SndCIStatusProgress.Partial
    }
    chat.itemInfo("hi").run {
      val bobRow = Ui.ItemInfo.Member(memberId("bob")); val cathRow = Ui.ItemInfo.Member(memberId("cath"))
      poll(what = "the Delivery tab with bob `$RECEIVED` and cath `$SENT`") {
        descriptionsOf(bobRow).let { RECEIVED in it && SENT !in it } && descriptionsOf(cathRow).let { SENT in it && RECEIVED !in it }
      }
      cath.start()
      cath.eventuallyMessage("#team alice> hi", CONNECT_TIMEOUT_MS)
      poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "cath `$RECEIVED` in the open Delivery tab") {
        descriptionsOf(cathRow).let { RECEIVED in it && SENT !in it } && RECEIVED in descriptionsOf(bobRow)
      }
      close()
    }
  }

  @Test fun info_quoteTab_showsQuoted() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice question"
    awaitRowText(alice, "bob", "question")
    val chat = alice.chatList.open("bob")
    chat.itemId("question")
    chat.send("plain")
    bob expectsMessage "alice> plain"
    chat.itemInfo("plain").run {
      check(!exists(QUOTE_TAB) && !exists(Ui.ItemInfo.Quote)) { "the info of a message without a quote has an `In reply to` tab" }
      close()
    }
    hideKeyboard()
    chat.menu("question", "reply")
    chat.send("answer")
    bob expectsMessage "alice> > question"
    bob expects "      answer"
    chat.itemInfo("answer").run {
      poll(what = "the `In reply to` tab, with the history shown first") { exists(QUOTE_TAB) && !exists(Ui.ItemInfo.Quote) }
      click(QUOTE_TAB)
      poll(what = "the `In reply to` tab showing bob's `question`") {
        exists(Ui.ItemInfo.Quote) && textsOf(Ui.ItemInfo.Quote).any { "question" in it } && seesText("In reply to")
      }
      close()
    }
  }

  @KnownAppIssue("15.5 item 30", "Compose-Espresso link", intermittent = true)
  @Test fun info_forwardedTab_showsSource() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    emptyGroup(alice, "team")
    backToChatList()
    bob sends "@alice hi"
    awaitRowText(alice, "bob", "hi")
    val chat = alice.chatList.open("bob")
    chat.itemId("hi")
    hideKeyboard()
    chat.forwardTo("hi", "team")
    val teamId = alice.chatList.chatId("team")
    poll(what = "team open with the forwarded `hi`") { chatModel.chatId.value == teamId && chat.itemTexts().contains("hi") }
    hideKeyboard()
    chat.itemInfo("hi").run {
      poll(what = "the `Forwarded` tab, with the history shown first") { exists(FORWARDED_TAB) && !exists(Ui.ItemInfo.ForwardedFrom) }
      click(FORWARDED_TAB)
      poll(what = "`Forwarded from` bob, hidden from the recipients") {
        exists(Ui.ItemInfo.ForwardedFrom) && "bob" in textsOf(Ui.ItemInfo.ForwardedFrom) && seesText("Forwarded from") && seesText(SOURCE_HIDDEN)
      }
    }
    click(Ui.ItemInfo.ForwardedFrom)
    val bobId = alice.chatList.chatId("bob")
    poll(what = "bob's chat opened from the source row and the item info closed") { chatModel.chatId.value == bobId && !exists(Ui.ItemInfo.ForwardedFrom) }
    hideKeyboard()
    chat.forwardTo("hi", "Private notes")
    val notesId = alice.chatList.chatId("Private notes")
    val savedFromBob = { ci: ChatItem -> ci.content.text == "hi" && (ci.meta.itemForwarded as? CIForwardedFrom.Contact)?.chatName == "bob" }
    poll(what = "the notes open with the saved `hi`") { chatModel.chatId.value == notesId && chatModel.chatsContext.chatItems.value.any(savedFromBob) }
    hideKeyboard()
    openItemInfo(chat.itemId("the `hi` saved from bob in the notes", savedFromBob), "Saved message").run {
      poll(what = "the `Saved` tab, with the history shown first") { exists(SAVED_TAB) && !exists(Ui.ItemInfo.ForwardedFrom) }
      click(SAVED_TAB)
      poll(what = "`Saved from` bob, without the recipients line") {
        exists(Ui.ItemInfo.ForwardedFrom) && "bob" in textsOf(Ui.ItemInfo.ForwardedFrom) && seesText("Saved from") &&
          !seesText("Forwarded from") && !seesText(SOURCE_HIDDEN)
      }
      close()
    }
  }

  @Test fun info_timed_showsDisappearsAt() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.send("plain")
    bob expectsMessage "alice> plain"
    chat.itemInfo("plain").run {
      check(!seesText("Disappears at")) { "the info of a message without a timer shows `Disappears at`" }
      close()
    }
    typeInto(Ui.Chat.Message, "timed")
    openTimedMenu()
    click(Ui.Chat.SendTimed.option(FIVE_MINUTES))
    bob.expectsMessage("alice> timed", CONNECT_TIMEOUT_MS)
    val id = chat.itemId("timed")
    var disappearsAt: String? = null
    poll(what = "alice's `timed` with a 5-minute timer") {
      disappearsAt = chat.item(id)?.meta?.itemTimed?.takeIf { it.ttl == FIVE_MINUTES }?.deleteAt?.let(::localTimestamp)
      disappearsAt != null
    }
    chat.itemInfo("timed").run {
      check(row("Disappears at") == disappearsAt) { "`Disappears at` of the timed message: `${row("Disappears at")}`, expected `$disappearsAt`" }
      close()
    }
  }

  @Test fun info_moderated_showsModeratedAt() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = ownGroup(alice, "team", bob)
    bob sends "#team spam"
    val id = chat.itemId("spam")
    chat.receivedItemInfo("spam").run {
      check(!seesText("Moderated at") && !seesText("Deleted at")) { "the info of bob's message shows `Moderated at` or `Deleted at` before moderation" }
      close()
    }
    hideKeyboard()
    chat.menu("spam", "moderate")
    confirmAlert("Delete member message?", "The message will be marked as moderated for all members.", "Delete")
    bob.eventuallyMessage("#team bob> [marked deleted by alice] spam", CONNECT_TIMEOUT_MS)
    var moderatedAt: String? = null
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's `spam` moderated by alice") {
      moderatedAt = (chat.item(id)?.meta?.itemDeleted as? CIDeleted.Moderated)?.deletedTs?.let(::localTimestamp)
      moderatedAt != null && "moderated by alice" in textsOf(Ui.Chat.Item(id))
    }
    openItemInfo(id, "Received message").run {
      check(row("Moderated at") == moderatedAt && !seesText("Deleted at")) { "`Moderated at` of the moderated message: `${row("Moderated at")}`, expected `$moderatedAt`" }
      close()
    }
  }

  private fun UiScope.listed(groupMemberId: Long, name: String): Boolean =
    exists(Ui.ItemInfo.Member(groupMemberId)) && name in textsOf(Ui.ItemInfo.Member(groupMemberId))

  private fun UiScope.memberId(name: String): Long {
    var id: Long? = null
    poll(what = "member `$name` in the model") {
      id = chatModel.groupMembers.value.firstOrNull { it.memberProfile.displayName == name }?.groupMemberId
      id != null
    }
    return id!!
  }

  private companion object {
    const val SENT = "sent"
    const val RECEIVED = "received"
    const val FIVE_MINUTES = 300
    const val SOURCE_HIDDEN = "Recipient(s) can't see who this message is from."
    val QUOTE_TAB = described("In reply to")
    val FORWARDED_TAB = described("Forwarded")
    val SAVED_TAB = described("Saved")
  }
}
