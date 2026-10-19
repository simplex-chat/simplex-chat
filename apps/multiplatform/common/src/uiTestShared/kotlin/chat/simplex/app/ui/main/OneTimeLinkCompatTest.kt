package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import org.junit.Test

class OneTimeLinkCompatTest : UiTest() {
  @Test fun oneTimeLinkCliConnectsViaShortLink() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val link = alice.newChat().createOneTimeLink()
    bob sends "/c $link"; bob expects "confirmation sent!"
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    alice.chatList.row("bob")
  }
}
