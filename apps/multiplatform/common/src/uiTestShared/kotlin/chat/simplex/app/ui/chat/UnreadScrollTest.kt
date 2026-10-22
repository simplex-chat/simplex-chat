package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitRowText
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.contactOf
import chat.simplex.app.ui.harness.loadedItemId
import chat.simplex.app.ui.harness.loadedText
import chat.simplex.app.ui.harness.loadedNumbered
import chat.simplex.app.ui.harness.ownGroup
import chat.simplex.app.ui.harness.replyViaApi
import chat.simplex.app.ui.harness.sendTextsViaApi
import chat.simplex.app.ui.harness.shownText
import chat.simplex.app.ui.harness.switchAwayFromChat
import chat.simplex.common.model.ChatPagination
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

private const val UNREAD = 60
private const val FAR = 120
private const val NOTES = "Private notes"
private const val OLDER = 300
private const val AROUND_AFTER = ChatPagination.PRELOAD_COUNT * 2
private const val NEWER = 360
private const val MAX_PAGES = 150
private const val BANNER = "Your contact"

class UnreadScrollTest : UiTest() {
  @Test fun unread_openChat_scrollsToFirstUnread() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    for (i in 1..UNREAD) bob sends "@alice m$i"
    awaitRowText(alice, "bob", "$UNREAD")
    val chat = alice.chatList.open("bob")
    val first = chat.itemId("m1")
    var below = 0
    poll(what = "`m1` read, and the row and the scroll-down button both counting the unread messages below the view") {
      below = alice.chatList.chat("bob").chatStats.unreadCount
      chat.item(first)?.isRcvNew == false && below in 1 until UNREAD &&
        exists(Ui.Chat.ScrollToBottom) && "$below" in textsOf(Ui.Chat.ScrollToBottom) && "$below" in alice.chatList.rowTexts("bob")
    }
    val newest = chatModel.chatsContext.chatItems.value.lastOrNull { it.content.text == "m$UNREAD" }
    check(newest == null || (newest.isRcvNew && !exists(Ui.Chat.Item(newest.id)))) { "`m$UNREAD` was shown or read before the chat was scrolled to it" }
    click(Ui.Chat.ScrollToBottom)
    val last = chat.itemId("`m$UNREAD` in view after the scroll-down button") { it.content.text == "m$UNREAD" }
    var above = 0
    poll(what = "`m$UNREAD` read, no scroll-down button, and the row and the scroll-up button both counting the skipped unread messages") {
      above = alice.chatList.chat("bob").chatStats.unreadCount
      chat.item(last)?.isRcvNew == false && !exists(Ui.Chat.ScrollToBottom) && above in 1 until below &&
        exists(Ui.Chat.UnreadAbove) && "$above" in textsOf(Ui.Chat.UnreadAbove) && "$above" in alice.chatList.rowTexts("bob")
    }
    rightClick(Ui.Chat.UnreadAbove)
    click(Ui.Chat.UnreadAbove.part("markRead"))
    poll(what = "every message read: no scroll-up button and no unread count on the row") {
      alice.chatList.chat("bob").chatStats.unreadCount == 0 && !exists(Ui.Chat.UnreadAbove) &&
        alice.chatList.rowTexts("bob").none { it.isNotEmpty() && it.all(Char::isDigit) }
    }
  }

  @Test fun unreadChatFlag_clearedOnOpen() = uiTest {
    val alice = app("alice")
    alice.chatList.row(NOTES)
    val badges = alice.chatList.rowTexts(NOTES).count { it.isEmpty() }
    alice.chatList.rowMenu(NOTES, "markUnread")
    poll(what = "the unread mark on the notes row") {
      alice.chatList.chat(NOTES).chatStats.unreadChat && alice.chatList.rowTexts(NOTES).count { it.isEmpty() } > badges
    }
    alice.chatList.open(NOTES)
    poll(what = "the unread mark cleared by opening the notes") {
      !alice.chatList.chat(NOTES).chatStats.unreadChat && alice.chatList.rowTexts(NOTES).count { it.isEmpty() } == badges
    }
    val restarted = restart()
    restarted.chatList.row(NOTES)
    check(!restarted.chatList.chat(NOTES).chatStats.unreadChat && restarted.chatList.rowTexts(NOTES).count { it.isEmpty() } == badges) {
      "the unread mark is back after a restart"
    }
  }

  @Test fun pagination_quoteOutsidePage_jumpLoadsOriginal() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.send("the original")
    bob expectsMessage "alice> the original"
    val original = chat.itemId("the original")
    switchAwayFromChat(alice)
    for (i in 1..FAR) bob sends "@alice f$i"
    bob types ">@alice (the original) far reply"
    bob expectsMessage "@alice > the original"
    bob expects "      far reply"
    awaitRowText(alice, "bob", "far reply")
    alice.chatList.rowMenu("bob", "markRead")
    poll(what = "bob's chat marked read") { alice.chatList.chat("bob").chatStats.unreadCount == 0 }
    alice.chatList.open("bob")
    val reply = chat.itemId("far reply")
    check(chat.item(original) == null && !exists(Ui.Chat.Item(original))) { "`the original` was loaded with the newest page" }
    click(Ui.Chat.Item(reply).part("quote"))
    poll(what = "`the original` loaded and in view, with the scroll-down arrow and no count") {
      exists(Ui.Chat.Item(original)) && exists(Ui.Chat.ScrollToBottom) && textsOf(Ui.Chat.ScrollToBottom).isEmpty()
    }
    bob sends "@alice new one"
    val newOne = loadedItemId("`new one` received in alice's open chat") { it.content.text == "new one" }
    poll(what = "`the original` still in view, `new one` below it and the scroll-down button counting 1") {
      exists(Ui.Chat.Item(original)) && !exists(Ui.Chat.Item(newOne)) && exists(Ui.Chat.ScrollToBottom) && "1" in textsOf(Ui.Chat.ScrollToBottom)
    }
    click(Ui.Chat.ScrollToBottom)
    poll(what = "`new one` in view and read, with no scroll-down button") {
      exists(Ui.Chat.Item(newOne)) && chat.item(newOne)?.isRcvNew == false && !exists(Ui.Chat.ScrollToBottom)
    }
    bob sends "@alice latest"
    val latest = loadedItemId("`latest` received in alice's open chat") { it.content.text == "latest" }
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the chat scrolled down to `latest` by itself") { exists(Ui.Chat.Item(latest)) }
  }

  @Test fun pagination_scrollUp_loadsOlder() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's contact ready to send to") { alice.chatList.shown("bob") && contactOf("bob").sndReady }
    val texts = (1..OLDER).map { "p$it" }
    sendTextsViaApi("bob", texts)
    val chat = alice.chatList.open("bob")
    chat.itemId("p$OLDER")
    val initial = loadedNumbered("p")
    check(initial == texts.takeLast(ChatPagination.INITIAL_COUNT)) { "the chat opened with ${initial.size} messages ${initial.firstOrNull()}..${initial.lastOrNull()}, not the newest ${ChatPagination.INITIAL_COUNT}" }
    check(!seesText(BANNER)) { "the chat banner is shown before the oldest message is loaded" }
    var pages = 0
    while (!seesText(BANNER)) {
      if (++pages > MAX_PAGES) throw AssertionError("the chat banner not shown after $MAX_PAGES pages up; oldest loaded: ${loadedNumbered("p").firstOrNull()}")
      val loaded = chatModel.chatsContext.chatItems.value.size
      if (!scrollChatTowardOlder()) {
        poll(what = "older messages loaded at the top of the chat, or the chat banner; oldest loaded: ${loadedNumbered("p").firstOrNull()}") {
          chatModel.chatsContext.chatItems.value.size > loaded || seesText(BANNER)
        }
      }
    }
    check(loadedNumbered("p") == texts) { "the messages loaded by scrolling up are not `p1`..`p$OLDER` in order: ${loadedNumbered("p").size} loaded" }
  }

  @Test fun pagination_afterAround_scrollDownLoadsNewer() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.send("the original")
    bob expectsMessage "alice> the original"
    val original = chat.itemId("the original")
    switchAwayFromChat(alice)
    val texts = (1..NEWER).map { "n$it" }
    sendTextsViaApi("bob", texts)
    replyViaApi("bob", original, "far reply")
    alice.chatList.open("bob")
    val reply = chat.itemId("far reply")
    check(chat.item(original) == null) { "`the original` was loaded with the newest page" }
    click(Ui.Chat.Item(reply).part("quote"))
    poll(what = "`the original` loaded around and in view") { exists(Ui.Chat.Item(original)) }
    val aroundNewest = "n$AROUND_AFTER"
    val firstGap = "n${AROUND_AFTER + 1}"
    check(loadedText(aroundNewest) && !loadedText(firstGap) && loadedText("n$NEWER")) { "the jump did not leave a gap after `$aroundNewest`: ${loadedNumbered("n").size} loaded" }
    var pages = 0
    while (!loadedText(firstGap)) {
      if (shownText(aroundNewest)) throw AssertionError("`$aroundNewest`, the newest message loaded around `the original`, scrolled into view before newer messages were loaded below it")
      if (++pages > MAX_PAGES) throw AssertionError("`$firstGap` not loaded after $MAX_PAGES pages down")
      if (!scrollChatTowardNewer()) throw AssertionError("the chat stopped scrolling down before `$firstGap` was loaded")
    }
    check(!shownText(aroundNewest)) { "`$firstGap` was loaded only after `$aroundNewest` scrolled into view" }
    while (!exists(Ui.Chat.Item(reply))) {
      if (++pages > MAX_PAGES) throw AssertionError("`far reply` not in view after $MAX_PAGES pages down; ${loadedNumbered("n").size} of $NEWER loaded")
      if (!scrollChatTowardNewer()) throw AssertionError("the chat stopped scrolling down before `far reply`; ${loadedNumbered("n").size} of $NEWER loaded")
    }
    check(loadedNumbered("n") == texts) { "the messages loaded by scrolling down are not `n1`..`n$NEWER` in order: ${loadedNumbered("n").size} loaded" }
  }

  @Test fun groupRcv_avatarClick_opensMemberInfo() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    val chat = ownGroup(alice, "team", bob, cath)
    chat.send("start")
    bob.eventuallyMessage("#team alice> start", CONNECT_TIMEOUT_MS)
    cath.eventuallyMessage("#team alice> start", CONNECT_TIMEOUT_MS)
    bob sends "#team from bob"
    val fromBob = chat.itemId("from bob")
    cath sends "#team from cath"
    val fromCath = chat.itemId("from cath")
    poll(what = "the avatars of bob and cath on their messages") {
      exists(Ui.Chat.Item(fromBob).part("avatar")) && exists(Ui.Chat.Item(fromCath).part("avatar"))
    }
    check(!exists(Ui.MemberInfo.BlockForAll)) { "member info is open before an avatar is clicked" }
    hideKeyboard()
    click(Ui.Chat.Item(fromCath).part("avatar"))
    poll(what = "member info with `Change role` and `Block for all`") { exists(Ui.MemberInfo.Role) && exists(Ui.MemberInfo.BlockForAll) }
    click(Ui.MemberInfo.BlockForAll)
    poll(what = "the `Block member for all?` alert naming cath, not bob") {
      seesText("Block member for all?") && seesText("All new messages from cath will be hidden!") && !seesText("All new messages from bob will be hidden!")
    }
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "the alert closed on cath's member info") { !seesText("Block member for all?") && exists(Ui.MemberInfo.BlockForAll) }
  }
}
