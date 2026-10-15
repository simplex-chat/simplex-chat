package chat.simplex.app.ui.desktop

import androidx.compose.ui.test.performTextInput
import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.createOneTimeLink
import chat.simplex.app.ui.harness.openRowMenu
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.ui.Ui
import org.junit.Test

private val planConfirm = Ui.Alert.Confirm(Ui.AlertSource.PrivacySensitive)
private val notOffered = listOf("markRead", "markUnread", "favorite", "unfavorite", "ntfs.None", "ntfs.All", "clear")

class ChatListMenusPendingTest : UiTest() {
  @Test fun contactMenuForNotReadyContactOnlyDelete() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val link = bob.createOneTimeLink()
    alice.chatList.row("Private notes")
    enterLinkInSearch(link)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `Open new chat` alert for bob") {
      exists(planConfirm) && "Open new chat" in textsOf(planConfirm) && seesText("bob")
    }
    click(planConfirm)
    poll(what = "the prepared contact bob without a connection") {
      (alice.chatList.chat("bob").chatInfo as? ChatInfo.Direct)?.contact?.let { it.nextConnectPrepared && it.activeConn == null } == true
    }
    val id = openRowMenu("bob")
    poll(what = "a row menu with only `Delete`") {
      exists(Ui.ChatList.RowMenu(id, "delete")) && notOffered.none { exists(Ui.ChatList.RowMenu(id, it)) } && !seesText("Add to list")
    }
    click(Ui.ChatList.RowMenu(id, "delete"))
    poll(what = "the `Confirm contact deletion?` alert") { seesText("Confirm contact deletion?") }
  }
}

private fun UiScope.enterLinkInSearch(link: String) {
  waitFor(Ui.ChatList.Search)
  val node = ui.onNode(matcher(Ui.ChatList.Search))
  ui.runOnUiThread { node.performTextInput(link) }
}
