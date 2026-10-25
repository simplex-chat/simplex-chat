package chat.simplex.app.ui.android

import androidx.compose.ui.state.ToggleableState
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.backToChatList
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiKey
import chat.simplex.common.views.helpers.ModalManager
import org.junit.Test

class AppearanceAndroidTest : UiTest() {
  @Test fun appearanceAndroid_interfaceSectionToolbars() = uiTest {
    val alice = app("alice")
    waitFor(Ui.ChatList.Search)
    check(storedSetting(ONE_HAND) == null && storedSetting(CHAT_BOTTOM) == null) { "the toolbar prefs are not at their defaults: ${storedSetting(ONE_HAND)}, ${storedSetting(CHAT_BOTTOM)}" }
    poll(what = "the chat list search field and toolbar at the bottom of the screen") { lowerHalf(Ui.ChatList.Search) == true && lowerHalf(CHAT_LIST_BAR) == true }
    alice.chatList.open(NOTES)
    poll(what = "the notes toolbar at the bottom of the screen") { lowerHalf(CHAT_TITLE) == true }
    backToChatList()

    alice.appearance()
    check(toggleState(Ui.Appearance.OneHandUI) == ToggleableState.On && toggleState(Ui.Appearance.ChatBottomBar) == ToggleableState.On) {
      "Reachable app toolbars ${toggleState(Ui.Appearance.OneHandUI)}, Reachable chat toolbar ${toggleState(Ui.Appearance.ChatBottomBar)}"
    }
    click(Ui.Appearance.ChatBottomBar)
    poll(what = "Reachable chat toolbar off") { storedSetting(CHAT_BOTTOM) == "false" && toggleState(Ui.Appearance.ChatBottomBar) == ToggleableState.Off }
    closeAppearance()
    poll(what = "the chat list search field and toolbar still at the bottom with Reachable chat toolbar off") { lowerHalf(Ui.ChatList.Search) == true && lowerHalf(CHAT_LIST_BAR) == true }
    alice.chatList.open(NOTES)
    poll(what = "the notes toolbar at the top of the screen with Reachable chat toolbar off") { lowerHalf(CHAT_TITLE) == false }
    backToChatList()
    alice.appearance()
    click(Ui.Appearance.OneHandUI)
    poll(what = "Reachable app toolbars off, hiding Reachable chat toolbar") {
      storedSetting(ONE_HAND) == "false" && toggleState(Ui.Appearance.OneHandUI) == ToggleableState.Off && !exists(Ui.Appearance.ChatBottomBar)
    }
    closeAppearance()
    poll(what = "the chat list search field and toolbar at the top of the screen") { lowerHalf(Ui.ChatList.Search) == false && lowerHalf(CHAT_LIST_BAR) == false }

    alice.appearance()
    click(Ui.Appearance.OneHandUI)
    poll(what = "Reachable app toolbars on, showing Reachable chat toolbar turned on again") {
      storedSetting(ONE_HAND) == "true" && exists(Ui.Appearance.ChatBottomBar) && toggleState(Ui.Appearance.ChatBottomBar) == ToggleableState.On && storedSetting(CHAT_BOTTOM) == "true"
    }
    click(Ui.Appearance.OneHandUI)
    poll(what = "Reachable app toolbars off with Reachable chat toolbar still stored on") { storedSetting(ONE_HAND) == "false" && storedSetting(CHAT_BOTTOM) == "true" }
    closeAppearance()
    alice.chatList.open(NOTES)
    poll(what = "the notes toolbar at the top of the screen with Reachable app toolbars off") { lowerHalf(CHAT_TITLE) == false }
  }

  private fun UiScope.lowerHalf(key: UiKey): Boolean? =
    rule.onAllNodes(matcher(key)).fetchSemanticsNodes().firstOrNull()?.boundsInWindow?.let { it.top >= windowFrame().height / 2f }

  private fun UiScope.closeAppearance() {
    escape()
    poll(what = "Your settings again") { exists(Ui.Settings.Appearance) && !exists(Ui.Appearance.OneHandUI) }
    escape()
    poll(what = "the chat list with nothing open") { !ModalManager.start.hasModalsOpen() && !exists(Ui.Settings.Appearance) && !exists(Ui.UserPicker.Sheet) && exists(Ui.ChatList.Search) }
  }

  private companion object {
    const val ONE_HAND = "OneHandUI"
    const val CHAT_BOTTOM = "ChatBottomBar"
    const val NOTES = "Private notes"
    val CHAT_TITLE = Ui.Chat.Bar.part("title")
    val CHAT_LIST_BAR = Ui.ChatList.Bar.part("title")
  }
}
