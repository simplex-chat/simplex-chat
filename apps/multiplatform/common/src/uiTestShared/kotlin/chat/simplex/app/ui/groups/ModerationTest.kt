package chat.simplex.app.ui.groups

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.ownGroupWithTwo
import chat.simplex.common.ui.Ui
import org.junit.Test

class ModerationTest : UiTest() {
  @Test fun moderate_multipleSelected() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    val chat = ownGroupWithTwo(alice, bob, cath)
    bob sends "#team spam one"
    cath.eventuallyMessage("#team bob> spam one", CONNECT_TIMEOUT_MS)
    cath sends "#team spam two"
    bob.eventuallyMessage("#team cath> spam two", CONNECT_TIMEOUT_MS)
    val one = chat.itemId("bob's `spam one`") { it.content.text == "spam one" }
    val two = chat.itemId("cath's `spam two`") { it.content.text == "spam two" }
    chat.select("spam one")
    chat.toggle(two)
    poll(what = "`Selected 2` in the selection toolbar") { chat.selectionTitle() == "Selected 2" }
    click(Ui.Chat.Selection.part("moderate"))
    poll(what = "the `Delete 2 messages of members?` alert saying they will be marked as moderated, with `Delete`") {
      seesText("Delete 2 messages of members?") && seesText("The messages will be marked as moderated for all members.") &&
        "Delete" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    bob.expectsMessage("#team bob> [marked deleted by alice] spam one", CONNECT_TIMEOUT_MS)
    bob.expectsMessage("#team cath> [marked deleted by alice] spam two", CONNECT_TIMEOUT_MS)
    cath.expectsMessage("#team bob> [marked deleted by alice] spam one", CONNECT_TIMEOUT_MS)
    cath.expectsMessage("#team cath> [marked deleted by alice] spam two", CONNECT_TIMEOUT_MS)
    poll(what = "both messages collapsed into `2 messages moderated by alice` and selection mode ended") {
      "2 messages moderated by alice" in textsOf(Ui.Chat.Item(two)) && !exists(Ui.Chat.Item(one)) && !exists(Ui.Chat.Selection.part("title"))
    }
  }
}
