package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.common.platform.chatModel
import org.junit.Test

class ChatProfilesTest : UiTest() {
  @Test fun profilesAddProfile() = uiTest {
    val alice = app("alice")
    alice.userPicker().profiles().add("work")
    poll(what = "alice and work listed") { chatModel.users.map { it.user.displayName }.containsAll(listOf("alice", "work")) }
  }
}
