package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.common.model.GroupProfile
import chat.simplex.common.platform.chatController
import kotlinx.coroutines.runBlocking
import org.junit.Test

class UserPickerTest : UiTest() {
  @Test fun userPickerSwitchProfile() = uiTest {
    val alice = app("alice")
    runBlocking { chatController.apiNewGroup(null, incognito = false, GroupProfile("team", "", null)) } ?: error("apiNewGroup failed")
    alice.userPicker().profiles().apply { add("work"); close() }
    alice.userPicker().switchTo("alice")
    alice.chatList.row("team")
  }
}
