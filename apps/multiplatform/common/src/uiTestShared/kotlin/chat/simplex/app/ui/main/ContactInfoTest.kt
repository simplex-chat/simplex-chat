package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.common.model.ChatInfo
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertEquals
import org.junit.Test

class ContactInfoTest : UiTest() {
  @Test fun contactInfoVerifyCodeMatchesCli() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val shown = alice.chatList.open("bob").contactInfo().securityCode()
    bob sends "/code alice"
    bob.eventually("alice security code:")
    assertEquals(bob.nextLine().filterNot(Char::isWhitespace), shown)
  }

  @Test fun contactInfoMarkVerified() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val info = alice.chatList.open("bob").contactInfo()
    info.markVerified()
    poll(what = "bob verified in the model") { (alice.chatList.chat("bob").chatInfo as ChatInfo.Direct).contact.verified }
    poll(what = "the row `View security code`") { info.verifyRowText() == "View security code" }
    click(Ui.Info.VerifyCode)
    poll(what = "`bob is verified`") { seesText("bob is verified") }
  }
}
