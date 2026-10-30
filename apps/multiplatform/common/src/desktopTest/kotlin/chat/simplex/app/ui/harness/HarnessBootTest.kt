package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertEquals
import org.junit.Test

class HarnessBootTest : UiTest() {
  @Test fun bootShowsChatListForNewUser() = uiTest {
    app("alice")
    waitFor(Ui.ChatList.NewChat)
    assertEquals(0, appPrefs.appearanceBarsBlurRadius.get())
  }
}
