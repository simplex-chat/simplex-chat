package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.UiTest
import org.junit.Test

class OneTimeLinkCompatTest : UiTest() {
  @Test fun oneTimeLinkCliConnectsViaShortLink() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val link = alice.newChat().createOneTimeLink()
    bob sends "/c $link"; bob expects "confirmation sent!"
    bob expects "alice: contact is connected"
    alice.chatList.row("bob")
  }
}
