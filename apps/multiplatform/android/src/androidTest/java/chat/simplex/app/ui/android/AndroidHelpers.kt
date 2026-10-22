package chat.simplex.app.ui.android

import android.app.Notification
import android.app.NotificationManager
import android.content.ComponentName
import android.os.ParcelFileDescriptor
import android.os.PowerManager
import android.text.InputType
import android.view.View
import android.view.inputmethod.EditorInfo
import android.widget.EditText
import androidx.compose.ui.geometry.Rect
import androidx.compose.ui.semantics.SemanticsProperties
import androidx.compose.ui.state.ToggleableState
import androidx.core.view.ViewCompat
import androidx.core.view.WindowInsetsCompat
import androidx.test.espresso.Espresso
import androidx.test.espresso.UiController
import androidx.test.espresso.ViewAction
import androidx.test.espresso.ViewInteraction
import androidx.test.espresso.matcher.ViewMatchers
import androidx.test.platform.app.InstrumentationRegistry
import androidx.test.runner.lifecycle.ActivityLifecycleMonitorRegistry
import androidx.test.runner.lifecycle.Stage
import androidx.work.WorkInfo
import chat.simplex.app.SimplexService
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTestApp
import chat.simplex.common.model.NotificationsMode
import chat.simplex.common.platform.androidAppContext
import chat.simplex.common.platform.chatModel
import chat.simplex.common.platform.getWorkManagerInstance
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiKey
import chat.simplex.common.views.usersettings.changeNotificationsMode
import org.hamcrest.CoreMatchers
import org.hamcrest.Matcher

internal fun shell(cmd: String): String =
  ParcelFileDescriptor.AutoCloseInputStream(InstrumentationRegistry.getInstrumentation().uiAutomation.executeShellCommand(cmd)).bufferedReader().use { it.readText() }

internal fun serviceNotification(): Notification? =
  androidAppContext.getSystemService(NotificationManager::class.java).activeNotifications.firstOrNull { it.id == SimplexService.SIMPLEX_SERVICE_ID }?.notification

internal fun receiverState(receiver: Class<*>): Int =
  androidAppContext.packageManager.getComponentEnabledSetting(ComponentName(androidAppContext.packageName, receiver.name))

internal fun uniqueWorkActive(name: String): Boolean = uniqueWorkInfos(name).any { !it.state.isFinished }

internal fun workStates(name: String): List<WorkInfo.State> = uniqueWorkInfos(name).map { it.state }

@Suppress("UNCHECKED_CAST")
private fun uniqueWorkInfos(name: String): List<WorkInfo> {
  val wm = androidAppContext.getWorkManagerInstance()
  val future = wm.javaClass.getMethod("getWorkInfosForUniqueWork", String::class.java).invoke(wm, name) as java.util.concurrent.Future<*>
  return future.get() as List<WorkInfo>
}

internal fun UiScope.withBackgroundServiceAllowed(body: () -> Unit) {
  val pkg = androidAppContext.packageName
  val allowListed = androidAppContext.getSystemService(PowerManager::class.java).isIgnoringBatteryOptimizations(pkg)
  if (!allowListed) shell("dumpsys deviceidle whitelist +$pkg")
  val result = runCatching {
    poll(what = "the app allowed to run in background (battery allow-list, no background restriction)") { SimplexService.isBackgroundAllowed() }
    body()
  }
  val stopped = runCatching {
    UiTestApp.onMain { changeNotificationsMode(NotificationsMode.OFF, chatModel) }
    poll(what = "the SimpleX Chat service to stop and its notification to go") { !SimplexService.isServiceStarted && !SimplexService.isServiceStarting && serviceNotification() == null }
  }
  if (!allowListed) shell("dumpsys deviceidle whitelist -$pkg")
  result.getOrThrow()
  stopped.getOrThrow()
}

internal class WindowFrame(val height: Int, val keyboardTop: Int)

internal fun windowFrame(): WindowFrame {
  var frame: WindowFrame? = null
  UiTestApp.onMain {
    val decor = ActivityLifecycleMonitorRegistry.getInstance().getActivitiesInStage(Stage.RESUMED).firstOrNull()?.window?.decorView
    val insets = decor?.let { ViewCompat.getRootWindowInsets(it) }
    if (decor != null && insets != null) frame = WindowFrame(decor.height, decor.height - insets.getInsets(WindowInsetsCompat.Type.ime()).bottom)
  }
  return frame ?: error("no resumed activity window to measure")
}

internal fun UiScope.settledKeyboardTop(): Int {
  var last = -1
  poll(what = "the soft keyboard to finish opening") {
    val f = windowFrame()
    (keyboardShown() && f.keyboardTop < f.height && f.keyboardTop == last).also { last = f.keyboardTop }
  }
  settle()
  return windowFrame().keyboardTop
}

internal fun UiScope.boundsInWindow(key: UiKey): Rect {
  waitFor(key)
  return rule.onNode(matcher(key)).fetchSemanticsNode().boundsInWindow
}

internal fun UiScope.toggleState(key: UiKey): ToggleableState? {
  waitFor(key)
  return rule.onNode(matcher(key)).fetchSemanticsNode().config.getOrElseNullable(SemanticsProperties.ToggleableState) { null }
}

internal fun messageEditText(): ViewInteraction =
  Espresso.onView(CoreMatchers.allOf(ViewMatchers.isAssignableFrom(EditText::class.java), ViewMatchers.withTagValue(CoreMatchers.equalTo<Any>(Ui.Chat.Message.tag))))

internal fun messageLineCount(): Int {
  var lines = -1
  messageEditText().check { v, e -> lines = (v as? EditText)?.lineCount ?: throw (e ?: AssertionError("the message field is not an EditText")) }
  return lines
}

internal fun pressImeEnter() {
  messageEditText().perform(ImeEnterKey())
}

private class ImeEnterKey : ViewAction {
  override fun getConstraints(): Matcher<View> = ViewMatchers.isAssignableFrom(EditText::class.java)
  override fun getDescription() = "press Enter on the soft keyboard: a line break committed through the input connection, unless the field asks for an Enter action"
  override fun perform(uiController: UiController, view: View) {
    val info = EditorInfo()
    val ic = (view as EditText).onCreateInputConnection(info) ?: error("the message field has no input connection")
    check((info.inputType and InputType.TYPE_TEXT_FLAG_MULTI_LINE) != 0 && (info.imeOptions and EditorInfo.IME_FLAG_NO_ENTER_ACTION) != 0) {
      "the soft keyboard's Enter is an editor action for the message field: inputType 0x${info.inputType.toString(16)}, imeOptions 0x${info.imeOptions.toString(16)}"
    }
    ic.commitText("\n", 1)
    uiController.loopMainThreadUntilIdle()
  }
}
