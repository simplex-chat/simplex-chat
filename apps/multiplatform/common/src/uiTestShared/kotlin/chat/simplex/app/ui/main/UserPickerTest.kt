package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.common.model.GroupProfile
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import kotlinx.coroutines.runBlocking
import org.junit.Test

class UserPickerTest : UiTest() {
  @Test fun userPickerSwitchProfile() = uiTest {
    val alice = app("alice")
    runBlocking { chatController.apiNewGroup(null, incognito = false, GroupProfile("team", "", null)) } ?: error("apiNewGroup failed")
    val aliceNotes = alice.chatList.row("Private notes").chatId("Private notes")
    alice.userPicker().profiles().apply { add("work"); close() }
    poll(what = "work's chat list without alice's chats") {
      val notes = chatModel.chats.value.firstOrNull { it.chatInfo.chatViewName == "Private notes" }?.id
      notes != null && notes != aliceNotes && exists(Ui.ChatList.Row(notes)) && !exists(Ui.ChatList.Row(aliceNotes))
    }
    alice.userPicker().switchTo("alice")
    alice.chatList.row("team")
  }
}
