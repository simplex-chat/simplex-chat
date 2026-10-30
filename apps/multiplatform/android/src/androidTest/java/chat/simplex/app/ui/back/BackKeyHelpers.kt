package chat.simplex.app.ui.back

import androidx.compose.ui.test.isDisplayed
import androidx.test.core.app.ActivityScenario
import androidx.test.espresso.Espresso
import androidx.test.runner.lifecycle.ActivityLifecycleMonitorRegistry
import androidx.test.runner.lifecycle.Stage
import chat.simplex.app.MainActivity
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTestApp
import chat.simplex.common.ui.UiKey

private fun liveMainActivities(): List<MainActivity> {
  var live = emptyList<MainActivity>()
  UiTestApp.onMain {
    val monitor = ActivityLifecycleMonitorRegistry.getInstance()
    live = Stage.values().filter { it != Stage.DESTROYED }.flatMap { monitor.getActivitiesInStage(it) }.filterIsInstance<MainActivity>()
  }
  return live
}

internal fun UiScope.displayed(key: UiKey): Boolean = rule.onNode(matcher(key)).isDisplayed()

internal fun mainActivityFinished(): Boolean = liveMainActivities().all { it.isFinishing }

internal fun UiScope.backFromRoot() {
  check(liveMainActivities().any { !it.isFinishing }) { "no running MainActivity before Back" }
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


