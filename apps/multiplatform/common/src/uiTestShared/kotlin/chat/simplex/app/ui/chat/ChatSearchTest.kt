package chat.simplex.app.ui.chat

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitRowText
import chat.simplex.app.ui.harness.closeChatSearch
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.contentFilter
import chat.simplex.app.ui.harness.loadedItemId
import chat.simplex.app.ui.harness.loadedItemIds
import chat.simplex.app.ui.harness.ownGroup
import chat.simplex.app.ui.harness.pickContentFilter
import chat.simplex.app.ui.harness.switchAwayFromChat
import chat.simplex.common.model.MsgContent
import chat.simplex.common.ui.Ui
import org.junit.Test

private const val FIXTURE_JPG = "tests/fixtures/test.jpg"
private const val FILLERS = 40

class ChatSearchTest : UiTest() {
  @Test fun search_direct_filtersResults() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.send("hello there 🙂")
    bob expectsMessage "alice> hello there 🙂"
    bob sends "@alice hello there"
    bob sends "@alice how are you?"
    val matches = listOf(chat.itemId("hello there 🙂"), chat.itemId("hello there"))
    val other = chat.itemId("how are you?")
    chat.search("ello ther")
    poll(what = "only the two matching items") {
      chat.itemTexts().toSet() == setOf("hello there 🙂", "hello there") &&
        matches.all { exists(Ui.Chat.Item(it)) } && !exists(Ui.Chat.Item(other))
    }
  }

  @Test fun search_group_filtersResults() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = ownGroup(alice, "team", bob)
    bob sends "#team apples and pears"
    val bobs = chat.itemId("bob's `apples and pears`") { it.content.text == "apples and pears" }
    bob sends "#team plums"
    val plums = chat.itemId("bob's `plums`") { it.content.text == "plums" }
    chat.send("pears for bob")
    val mine = chat.itemId("pears for bob")
    bob.eventuallyMessage("#team alice> pears for bob", CONNECT_TIMEOUT_MS)
    chat.search("pears")
    poll(what = "bob's and alice's messages with `pears`, without bob's `plums`") {
      chat.itemTexts().toSet() == setOf("apples and pears", "pears for bob") &&
        exists(Ui.Chat.Item(bobs)) && exists(Ui.Chat.Item(mine)) && !exists(Ui.Chat.Item(plums))
    }
    replaceText(Ui.Chat.Bar.part("search"), "apples")
    poll(what = "only bob's `apples and pears`") {
      chat.itemTexts() == listOf("apples and pears") && exists(Ui.Chat.Item(bobs)) && !exists(Ui.Chat.Item(mine)) && !exists(Ui.Chat.Item(plums))
    }
  }

  @KnownAppIssue("15.5 item 30", "Compose-Espresso link", intermittent = true)
  @Test fun search_goToResult_opensAroundItem() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice alpha"
    bob sends "@alice the needle"
    for (i in 1..FILLERS) bob sends "@alice f$i"
    awaitRowText(alice, "bob", "f$FILLERS")
    alice.chatList.rowMenu("bob", "markRead")
    poll(what = "bob's chat marked read") { alice.chatList.chat("bob").chatStats.unreadCount == 0 }
    val chat = alice.chatList.open("bob")
    val newest = chat.itemId("f$FILLERS")
    val alpha = loadedItemId("`alpha` loaded") { it.content.text == "alpha" }
    val needle = loadedItemId("`the needle` loaded") { it.content.text == "the needle" }
    val goTo = Ui.Chat.Item(needle).part("goTo")
    check(!exists(Ui.Chat.Item(newest).part("goTo"))) { "`f$FILLERS` has a go-to button outside search" }
    check(!exists(Ui.Chat.Item(needle))) { "`the needle` is in view with the newest messages" }
    chat.search("needle")
    poll(what = "only `the needle`, with its go-to button") {
      chat.itemTexts() == listOf("the needle") && exists(goTo) && !exists(Ui.Chat.Item(alpha)) && !exists(Ui.Chat.Item(newest))
    }
    hideKeyboard()
    click(goTo)
    poll(what = "the search closed and `the needle` in view, above `f$FILLERS` with the scroll-down button") {
      !exists(Ui.Chat.Bar.part("search")) && !exists(goTo) &&
        exists(Ui.Chat.Item(needle)) && !exists(Ui.Chat.Item(newest)) && exists(Ui.Chat.ScrollToBottom)
    }
  }

  @Test fun search_notes_filtersResults() = uiTest {
    val alice = app("alice")
    val notes = alice.chatList.open("Private notes")
    notes.send("keep in mind")
    notes.send("ahoy!")
    val keep = notes.itemId("keep in mind"); val ahoy = notes.itemId("ahoy!")
    check(!exists(Ui.Chat.Bar.part("search"))) { "the search field is open before the search button is clicked" }
    hideKeyboard()
    click(Ui.Chat.SearchButton)
    typeInto(Ui.Chat.Bar.part("search"), "keep")
    poll(what = "only `keep in mind`") {
      notes.itemTexts() == listOf("keep in mind") && exists(Ui.Chat.Item(keep)) && !exists(Ui.Chat.Item(ahoy))
    }
    closeChatSearch()
    poll(what = "both notes shown again after the search closed") {
      notes.itemTexts().containsAll(listOf("keep in mind", "ahoy!")) && exists(Ui.Chat.Item(keep)) && exists(Ui.Chat.Item(ahoy))
    }
  }

  @Test fun contentFilter_types_filterAndReset() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    ownGroup(alice, "team", bob)
    click(Ui.Chat.ContentFilter)
    poll(what = "only `Images` and `Links` in the content filter menu of the empty group") {
      exists(contentFilter("Images")) && exists(contentFilter("Links")) && !exists(contentFilter("Files"))
    }
    click(contentFilter("Images"))
    poll(what = "the `Images` filter on in the empty group") { exists(Ui.Chat.Bar.part("search")) }
    pickContentFilter("all")
    poll(what = "the filter off in the empty group") { !exists(Ui.Chat.Bar.part("search")) }
    switchAwayFromChat(alice)
    bob sends "#team no links here"
    bob sends "#team check out https://example.com"
    bob sends "#team see https://simplex.chat"
    bob sends "/f #team ${bob.createFile("list.txt", "apples")}"
    bob expects "use /fc 1 to cancel sending"
    bob.eventually("completed uploading file 1 (list.txt) for #team", CONNECT_TIMEOUT_MS)
    bob sends "/img #team $FIXTURE_JPG"
    bob expectsMessage "/f #team $FIXTURE_JPG"
    bob expects "use /fc 2 to cancel sending"
    bob.eventually("completed uploading file 2 (test.jpg) for #team", CONNECT_TIMEOUT_MS)
    bob sends "#team last words"
    awaitRowText(alice, "team", "bob: last words")
    alice.chatList.open("team")
    val plain = loadedItemId("`no links here`") { it.content.text == "no links here" }
    val example = loadedItemId("`check out https://example.com`") { it.content.text == "check out https://example.com" }
    val simplex = loadedItemId("`see https://simplex.chat`") { it.content.text == "see https://simplex.chat" }
    val file = loadedItemId("the list.txt file") { it.file?.fileName == "list.txt" }
    val image = loadedItemId("the image") { it.content.msgContent is MsgContent.MCImage }
    val last = loadedItemId("`last words`") { it.content.text == "last words" }
    val all = setOf(plain, example, simplex, file, image, last)
    click(Ui.Chat.ContentFilter)
    poll(what = "`Images`, `Files` and `Links` in the content filter menu, without `Videos`, `Voice messages` or `All messages`") {
      listOf("Images", "Files", "Links").all { exists(contentFilter(it)) } && listOf("Videos", "Voice", "all").none { exists(contentFilter(it)) }
    }
    click(contentFilter("Files"))
    showsOnly("only the list.txt file", setOf(file), all)
    click(Ui.Chat.ContentFilter)
    poll(what = "`All messages` in the menu while a filter is on") { exists(contentFilter("all")) }
    click(contentFilter("Images"))
    showsOnly("only the image", setOf(image), all)
    pickContentFilter("Links")
    showsOnly("only the two messages with links", setOf(example, simplex), all)
    typeInto(Ui.Chat.Bar.part("search"), "example")
    showsOnly("only the link message with `example`", setOf(example), all)
    hideKeyboard()
    pickContentFilter("all")
    poll(what = "the filter off: no search field and every message loaded again, `last words` in view") {
      !exists(Ui.Chat.Bar.part("search")) && loadedItemIds().containsAll(all) && exists(Ui.Chat.Item(last))
    }
  }

  private fun UiScope.showsOnly(what: String, shown: Set<Long>, all: Set<Long>) = poll(what = what) {
    loadedItemIds() == shown && shown.all { exists(Ui.Chat.Item(it)) } && (all - shown).none { exists(Ui.Chat.Item(it)) }
  }
}
