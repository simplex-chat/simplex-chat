package chat.simplex.app

import androidx.compose.foundation.layout.Column
import androidx.compose.runtime.MutableState
import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.semantics.SemanticsProperties
import androidx.compose.ui.test.*
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.window.WindowState
import chat.simplex.common.simplexWindowState
import chat.simplex.common.views.usersettings.*
import org.junit.Rule
import kotlin.test.Test
import kotlin.test.assertEquals

class AddressAutoReplyTest {
  @get:Rule
  val rule = createComposeRule()

  private fun setContent(state: MutableState<AddressSettingsState>, saved: MutableList<AddressSettingsState>) {
    simplexWindowState.windowState = WindowState()
    rule.setContent {
      Column {
        AutoAcceptToggle(state) { saved.add(it) }
        AutoReplyEditor(state)
      }
    }
  }

  private fun fieldText(): String =
    rule.onNode(hasSetTextAction()).fetchSemanticsNode().config[SemanticsProperties.EditableText].text

  @Test
  fun turningAutoAcceptOffClearsWelcomeMessageField() {
    val state = mutableStateOf(AddressSettingsState(autoAccept = true, autoReply = "Hello from Alice"))
    val saved = mutableListOf<AddressSettingsState>()
    setContent(state, saved)

    rule.onNode(isToggleable()).performClick()
    rule.waitForIdle()

    assertEquals("", saved.last().autoReply)
    assertEquals("", fieldText())
  }

  @Test
  fun welcomeMessageTypedWithAutoAcceptOffIsSavedWhenTurnedOn() {
    val state = mutableStateOf(AddressSettingsState(autoAccept = false))
    val saved = mutableListOf<AddressSettingsState>()
    setContent(state, saved)

    rule.onNode(hasSetTextAction()).performTextInput("Typed while off")
    rule.onNode(isToggleable()).performClick()
    rule.waitForIdle()

    assertEquals("Typed while off", saved.last().autoReply)
    assertEquals("Typed while off", fieldText())
  }
}
