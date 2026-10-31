package chat.simplex.app.ui

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.common.ui.Ui
import org.junit.Test

class SmokeTest : UiTest() {
  @Test fun bootCreatesUserAndShowsChatList() = uiTest {
    app("alice")
    waitFor(Ui.ChatList.NewChat, 60_000)
  }
}
