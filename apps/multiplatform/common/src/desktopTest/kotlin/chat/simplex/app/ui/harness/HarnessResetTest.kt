package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.ModalManager
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.FixMethodOrder
import org.junit.Test
import org.junit.runners.MethodSorters

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
class HarnessResetTest : UiTest() {
  @Test fun a_leavesStateBehind() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.chatList.row("bob")
    appPrefs.densityScale.set(1.7f)
    click(Ui.ChatList.NewChat)
    poll(what = "the new chat sheet") { ModalManager.start.hasModalsOpen() }
  }

  @Test fun b_startsClean() = uiTest {
    app("alice")
    assertEquals(1f, appPrefs.densityScale.get())
    assertNull(storedSetting("DensityScale"))
    assertFalse(ModalManager.start.hasModalsOpen())
    assertFalse(chatModel.chats.value.any { it.chatInfo.chatViewName == "bob" })
    poll(what = "the chat list") { seesText("Private notes") }
    assertFalse(seesText("bob"))
  }
}
