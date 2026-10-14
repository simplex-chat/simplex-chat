package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.UiTest
import org.junit.Test

class AddressCompatTest : UiTest() {
  @Test fun addressCliConnectsAndRequestAppears() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val address = alice.userPicker().address().create()
    bob sends "/c $address"; bob expects "connection request sent!"
    alice.chatList.request("bob")
  }
}
