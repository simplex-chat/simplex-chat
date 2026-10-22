package chat.simplex.app.ui.android

import android.app.Notification
import android.app.NotificationManager
import android.content.ComponentName
import android.os.ParcelFileDescriptor
import android.os.PowerManager
import android.view.KeyEvent
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

internal fun uniqueWorkActive(name: String): Boolean =
  androidAppContext.getWorkManagerInstance().getWorkInfosForUniqueWork(name).get().any { !it.state.isFinished }

internal fun workStates(name: String): List<WorkInfo.State> =
  androidAppContext.getWorkManagerInstance().getWorkInfosForUniqueWork(name).get().map { it.state }

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
  override fun getDescription() = "send the Enter key through the input connection, as the soft keyboard does"
  override fun perform(uiController: UiController, view: View) {
    val ic = (view as EditText).onCreateInputConnection(EditorInfo()) ?: error("the message field has no input connection")
    ic.sendKeyEvent(KeyEvent(KeyEvent.ACTION_DOWN, KeyEvent.KEYCODE_ENTER))
    ic.sendKeyEvent(KeyEvent(KeyEvent.ACTION_UP, KeyEvent.KEYCODE_ENTER))
    uiController.loopMainThreadUntilIdle()
  }
}
