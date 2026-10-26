package chat.simplex.app

import chat.simplex.app.ui.harness.CommonFields
import chat.simplex.app.ui.harness.DesktopFields
import kotlin.test.Test
import kotlin.test.assertTrue

class HarnessReflectionTest {
  @Test
  fun everyPrivateFieldTheUiTestResetWritesExists() {
    val missing = (CommonFields.all + DesktopFields.all).mapNotNull { ref -> runCatching { ref.field }.exceptionOrNull()?.let { "$ref ($it)" } } +
      listOfNotNull(runCatching { DesktopFields.skikoClipboardValue() }.exceptionOrNull()?.let { "${DesktopFields.skikoClipboard} lazy _value ($it)" })
    assertTrue(missing.isEmpty(), "the UI test harness resets these private fields by reflection, and they no longer exist; update harness/CommonReset.kt or harness/Reset.kt:\n" + missing.joinToString("\n"))
  }
}
