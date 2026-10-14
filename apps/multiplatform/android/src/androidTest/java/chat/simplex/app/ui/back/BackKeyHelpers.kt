package chat.simplex.app.ui.back

import androidx.core.view.ViewCompat
import androidx.core.view.WindowInsetsCompat
import androidx.test.core.app.ActivityScenario
import androidx.test.espresso.Espresso
import androidx.test.runner.lifecycle.ActivityLifecycleMonitorRegistry
import androidx.test.runner.lifecycle.Stage
import chat.simplex.app.MainActivity
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTestApp

private fun liveMainActivities(): List<MainActivity> {
  var live = emptyList<MainActivity>()
  UiTestApp.onMain {
    val monitor = ActivityLifecycleMonitorRegistry.getInstance()
    live = Stage.values().filter { it != Stage.DESTROYED }.flatMap { monitor.getActivitiesInStage(it) }.filterIsInstance<MainActivity>()
  }
  return live
}

internal fun mainActivityFinished(): Boolean = liveMainActivities().all { it.isFinishing }

internal fun UiScope.backFromRoot() {
  Espresso.pressBackUnconditionally()
  poll(what = "MainActivity to finish") { mainActivityFinished() }
}

internal fun UiScope.relaunch(body: () -> Unit) {
  poll(what = "the finished MainActivity to be destroyed") { liveMainActivities().isEmpty() }
  ActivityScenario.launch(MainActivity::class.java).use {
    rule.waitForIdle()
    body()
  }
}

internal fun keyboardShown(): Boolean {
  var shown = false
  UiTestApp.onMain {
    val decor = ActivityLifecycleMonitorRegistry.getInstance().getActivitiesInStage(Stage.RESUMED).firstOrNull()?.window?.decorView
    shown = decor != null && ViewCompat.getRootWindowInsets(decor)?.isVisible(WindowInsetsCompat.Type.ime()) == true
  }
  return shown
}

internal fun UiScope.hideKeyboard() {
  runCatching { poll(timeoutMs = 2_000, what = "the soft keyboard to show", screenshot = false) { keyboardShown() } }
  Espresso.closeSoftKeyboard()
  poll(what = "the soft keyboard to hide") { !keyboardShown() }
}
