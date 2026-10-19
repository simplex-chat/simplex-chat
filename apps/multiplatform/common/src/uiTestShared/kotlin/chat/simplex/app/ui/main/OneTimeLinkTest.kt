package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotEquals
import org.junit.Test

class OneTimeLinkTest : UiTest() {
  @Test fun oneTimeLinkSwitchToIncognito() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val link = alice.newChat().createOneTimeLink()
    assertFalse(seesText("A new random profile will be shared."))
    click(Ui.Link.ShareProfile)
    click(Ui.Link.ProfileIncognito)
    poll(what = "the incognito footer") { seesText("A new random profile will be shared.") }
    bob sends "/c $link"; bob expects "confirmation sent!"
    val shared = bob.eventuallyMatching(Regex("(\\S+): contact is connected"), CONNECT_TIMEOUT_MS).groupValues[1]
    assertNotEquals("alice", shared)
    poll(what = "an incognito contact bob") {
      chatModel.chats.value.any { (it.chatInfo as? ChatInfo.Direct)?.contact?.let { c -> c.displayName == "bob" && c.contactConnIncognito } == true }
    }
  }
}
