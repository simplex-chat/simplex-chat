package chat.simplex.app.ui.harness

import chat.simplex.common.platform.chatController
import kotlinx.coroutines.runBlocking
import org.junit.Test

class HarnessCliPeerTest : UiTest() {
  @Test fun cliConnectsToGuiInvitation() = uiTest {
    val alice = app("alice")
    val bob = cli("bob")
    val (link, _) = runBlocking { chatController.apiAddContact(null, incognito = false) }.first ?: error("apiAddContact failed")
    bob sends "/c ${link.simplexChatUri(short = true)}"
    bob expects "confirmation sent!"
    bob.eventually("alice: contact is connected")
    alice.chatList.row("bob")
  }
}
