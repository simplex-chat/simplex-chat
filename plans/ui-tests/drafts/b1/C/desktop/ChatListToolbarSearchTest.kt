package chat.simplex.app.ui.desktop

import androidx.compose.ui.test.performTextInput
import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.createAddress
import chat.simplex.app.ui.harness.createGroupLink
import chat.simplex.app.ui.harness.joinLink
import chat.simplex.app.ui.harness.quiesce
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

private val planConfirm = Ui.Alert.Confirm(Ui.AlertSource.PrivacySensitive)

class ChatListToolbarSearchTest : UiTest() {
  @Test fun searchPasteLinkTriggersConnectPlan() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = bob.createAddress()
    alice.chatList.row("Private notes")
    enterLinkInSearch(address)
    poll(what = "the address description in the read-only search field") { searchShowsLink("SimpleX contact address") }
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the plan alert for bob with `Open new chat`") {
      exists(planConfirm) && "Open new chat" in textsOf(planConfirm) && seesText("bob")
    }
    click(Ui.Alert.Dismiss(Ui.AlertSource.PrivacySensitive))
    poll(what = "the search field cleared and editable") { editableText(Ui.ChatList.Search).isNullOrEmpty() && isEnabled(Ui.ChatList.Search) }
  }

  @Test fun searchPasteKnownContactLinkFiltersToContact() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = bob.createAddress()
    joinLink(address)
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    alice.chatList.row("bob").row("Private notes")
    quiesce()
    enterLinkInSearch(address)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "only bob listed for his address") {
      searchShowsLink("SimpleX contact address") && alice.chatList.shown("bob") && !alice.chatList.shown("Private notes")
    }
    settle()
    poll(what = "no plan alert for a known contact") { !exists(planConfirm) }
  }

  @Test fun searchPasteKnownGroupLinkFiltersToGroup() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val link = bob.createGroupLink("team")
    joinLink(link)
    bob.eventually("#team: alice joined the group", CONNECT_TIMEOUT_MS)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice an active member of team") {
      chatModel.chats.value.any { (it.chatInfo as? ChatInfo.Group)?.takeIf { g -> g.chatViewName == "team" }?.groupInfo?.membership?.memberActive == true }
    }
    alice.chatList.row("team").row("Private notes")
    quiesce()
    enterLinkInSearch(link)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "only team listed for its link") {
      searchShowsLink("SimpleX group link") && alice.chatList.shown("team") && !alice.chatList.shown("Private notes")
    }
    settle()
    poll(what = "no plan alert for a known group") { !exists(planConfirm) }
  }
}

private fun UiScope.enterLinkInSearch(link: String) {
  waitFor(Ui.ChatList.Search)
  val node = ui.onNode(matcher(Ui.ChatList.Search))
  ui.runOnUiThread { node.performTextInput(link) }
}

private fun UiScope.searchShowsLink(description: String): Boolean =
  !isEnabled(Ui.ChatList.Search) && editableText(Ui.ChatList.Search).orEmpty().startsWith("$description (via ")
