package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.theme.CurrentColors
import chat.simplex.common.ui.theme.DefaultTheme
import chat.simplex.common.ui.theme.ThemeManager
import chat.simplex.common.views.chat.chatViewScrollState
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.ModalManager
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Assume.assumeTrue
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
    onUiThread { ThemeManager.applyTheme(DefaultTheme.DARK.themeName) }
    check(storedSetting("CurrentTheme") == DefaultTheme.DARK.themeName && CurrentColors.value.base == DefaultTheme.DARK) { "the dark theme was not applied" }
    click(Ui.ChatList.NewChat)
    poll(what = "the new chat sheet") { ModalManager.start.hasModalsOpen() }
    onUiThread { AlertManager.shared.showAlertMsg(LEFTOVER_ALERT) }
    poll(what = "the leftover alert") { seesText(LEFTOVER_ALERT) }
    CommonFields.selectedOperatorIds.field.set(null, setOf(1L))
    chatViewScrollState.value = true
    setClipboardText(LEFTOVER_CLIPBOARD)
    poll(what = "the leftover text on the clipboard") { clipboardText() == LEFTOVER_CLIPBOARD }
    stateLeftBehind = true
  }

  @Test fun b_startsClean() = uiTest {
    assumeTrue("a_leavesStateBehind did not leave its state in this process", stateLeftBehind)
    app("alice")
    assertEquals(1f, appPrefs.densityScale.get())
    assertNull(storedSetting("DensityScale"))
    assertEquals(DefaultTheme.SYSTEM_THEME_NAME, appPrefs.currentTheme.get())
    assertEquals(DefaultTheme.SYSTEM_THEME_NAME, CurrentColors.value.name)
    assertFalse(storedSetting("CurrentTheme") == DefaultTheme.DARK.themeName)
    assertEquals(emptySet<Long>(), CommonFields.selectedOperatorIds.field.get(null))
    assertFalse(chatViewScrollState.value)
    assertFalse(ModalManager.start.hasModalsOpen())
    assertFalse(AlertManager.shared.hasAlertsShown())
    assertFalse(chatModel.chats.value.any { it.chatInfo.chatViewName == "bob" })
    poll(what = "the chat list") { seesText("Private notes") }
    assertFalse(seesText("bob"))
    assertFalse(seesText(LEFTOVER_ALERT))
    assertNull(clipboardText())
  }

  private companion object {
    const val LEFTOVER_ALERT = "Alert left by the previous test"
    const val LEFTOVER_CLIPBOARD = "Clipboard text left by the previous test"
    var stateLeftBehind = false
  }
}
