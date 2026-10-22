package chat.simplex.app.ui.android

import android.Manifest
import android.app.Notification
import android.content.pm.PackageManager
import chat.simplex.app.SimplexService
import chat.simplex.app.ui.harness.App
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.described
import chat.simplex.common.model.NotificationsMode
import chat.simplex.common.platform.androidAppContext
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.AlertManager
import org.junit.Test

class NotificationModesAndroidTest : UiTest() {
  @Test fun ntfMode_switchToService_startsForegroundService() = uiTest {
    val alice = app("alice")
    requireNotificationPermission()
    withBackgroundServiceAllowed {
      openNotificationSettings(alice)
      check(OFF_TITLE in textsOf(Ui.Notifications.Mode)) { "Notification service row: ${textsOf(Ui.Notifications.Mode)}" }
      openModePicker()
      check(MODES.all { (mode, title) -> title in textsOf(option(mode)) }) { "mode options: ${MODES.map { textsOf(option(it.first)) }}" }
      check(checkedTitles() == listOf(OFF_TITLE) && seesText(OFF_DESC)) { "picker before the change: checked ${checkedTitles()}" }
      check(serviceNotification() == null && receiverState(START_RECEIVER) != PackageManager.COMPONENT_ENABLED_STATE_ENABLED && !uniqueWorkActive(RESTART_WORK)) {
        "before Always on: service notification ${serviceNotification() != null}, StartReceiver ${receiverState(START_RECEIVER)}, restart work ${workStates(RESTART_WORK)}"
      }

      click(option(NotificationsMode.SERVICE))
      poll(what = "Always on checked and stored, with its description") {
        storedSetting(MODE_KEY) == NotificationsMode.SERVICE.name && checkedTitles() == listOf(SERVICE_TITLE) && seesText(SERVICE_DESC)
      }
      poll(what = "the SimpleX Chat service notification with Hide") {
        serviceNotification()?.let { n ->
          n.extras.getCharSequence(Notification.EXTRA_TITLE)?.toString() == "SimpleX Chat service" &&
            n.extras.getCharSequence(Notification.EXTRA_TEXT)?.toString() == "Receiving messages…" &&
            n.channelId == SimplexService.NOTIFICATION_CHANNEL_ID && n.actions.orEmpty().any { it.title?.toString() == "Hide" }
        } == true && SimplexService.isServiceStarted
      }
      poll(what = "the boot and update receivers enabled and the restart worker scheduled") {
        receiverState(START_RECEIVER) == PackageManager.COMPONENT_ENABLED_STATE_ENABLED &&
          receiverState(UPDATE_RECEIVER) == PackageManager.COMPONENT_ENABLED_STATE_ENABLED && uniqueWorkActive(RESTART_WORK)
      }
      check(!AlertManager.shared.hasAlertsShown()) { "switching to Always on while allowed in background showed an alert: ${texts().take(20)}" }
      escape()
      poll(what = "Notifications showing Always on") { exists(Ui.Notifications.Mode) && SERVICE_TITLE in textsOf(Ui.Notifications.Mode) }

      openModePicker()
      click(option(NotificationsMode.OFF))
      poll(what = "Runs when app is open checked and stored") { storedSetting(MODE_KEY) == NotificationsMode.OFF.name && checkedTitles() == listOf(OFF_TITLE) }
      poll(what = "the service stopped, its notification gone, the receivers disabled and the restart worker cancelled") {
        serviceNotification() == null && !SimplexService.isServiceStarted &&
          receiverState(START_RECEIVER) == PackageManager.COMPONENT_ENABLED_STATE_DISABLED &&
          receiverState(UPDATE_RECEIVER) == PackageManager.COMPONENT_ENABLED_STATE_DISABLED && !uniqueWorkActive(RESTART_WORK)
      }
      check(!AlertManager.shared.hasAlertsShown()) { "switching to Runs when app is open showed an alert: ${texts().take(20)}" }
    }
  }

  @Test fun ntfMode_switchToPeriodic_schedulesFetcher() = uiTest {
    val alice = app("alice")
    requireNotificationPermission()
    withBackgroundServiceAllowed {
      val fetcher = "${androidAppContext.packageName}.UNIQUE_MESSAGES_FETCHER"
      openNotificationSettings(alice)
      openModePicker()
      check(checkedTitles() == listOf(OFF_TITLE) && !uniqueWorkActive(fetcher)) { "before Starts periodically: checked ${checkedTitles()}, fetcher ${workStates(fetcher)}" }

      click(option(NotificationsMode.PERIODIC))
      poll(what = "Starts periodically checked and stored, with its description") {
        storedSetting(MODE_KEY) == NotificationsMode.PERIODIC.name && checkedTitles() == listOf(PERIODIC_TITLE) && seesText(PERIODIC_DESC)
      }
      poll(what = "the periodic message fetcher scheduled") { uniqueWorkActive(fetcher) }
      check(serviceNotification() == null && !SimplexService.isServiceStarted && receiverState(START_RECEIVER) == PackageManager.COMPONENT_ENABLED_STATE_DISABLED) {
        "Starts periodically: service notification ${serviceNotification() != null}, service started ${SimplexService.isServiceStarted}, StartReceiver ${receiverState(START_RECEIVER)}"
      }
      check(!uniqueWorkActive(RESTART_WORK) && !AlertManager.shared.hasAlertsShown()) { "Starts periodically: restart work ${workStates(RESTART_WORK)}, alert ${AlertManager.shared.hasAlertsShown()}" }
      escape()
      poll(what = "Notifications showing Starts periodically") { exists(Ui.Notifications.Mode) && PERIODIC_TITLE in textsOf(Ui.Notifications.Mode) }

      openModePicker()
      click(option(NotificationsMode.OFF))
      poll(what = "the periodic message fetcher cancelled with Runs when app is open stored") {
        storedSetting(MODE_KEY) == NotificationsMode.OFF.name && !uniqueWorkActive(fetcher)
      }
    }
  }

  private fun requireNotificationPermission() =
    check(androidAppContext.checkSelfPermission(Manifest.permission.POST_NOTIFICATIONS) == PackageManager.PERMISSION_GRANTED) {
      "the runner must grant POST_NOTIFICATIONS (setup item 2): without it the service does not start"
    }

  private fun UiScope.openNotificationSettings(app: App) {
    app.settings()
    click(described("Notifications"))
    waitFor(Ui.Notifications.Mode)
  }

  private fun UiScope.openModePicker() {
    click(Ui.Notifications.Mode)
    waitFor(option(NotificationsMode.SERVICE))
  }

  private fun UiScope.checkedTitles(): List<String> = MODES.map { it.second }.filter { seesDescription(it) }

  private fun option(mode: NotificationsMode) = Ui.Notifications.Mode.option(mode)

  private companion object {
    const val MODE_KEY = "NotificationsMode"
    const val OFF_TITLE = "Runs when app is open"
    const val PERIODIC_TITLE = "Starts periodically"
    const val SERVICE_TITLE = "Always on"
    const val OFF_DESC = "App can receive notifications only when it's running, no background service will be started"
    const val PERIODIC_DESC = "Checks new messages every 10 minutes for up to 1 minute"
    const val SERVICE_DESC = "Background service is always running – notifications will be shown as soon as the messages are available."
    const val RESTART_WORK = SimplexService.SERVICE_START_WORKER_WORK_NAME_PERIODIC
    val MODES = listOf(NotificationsMode.OFF to OFF_TITLE, NotificationsMode.PERIODIC to PERIODIC_TITLE, NotificationsMode.SERVICE to SERVICE_TITLE)
    val START_RECEIVER = SimplexService.StartReceiver::class.java
    val UPDATE_RECEIVER = SimplexService.AppUpdateReceiver::class.java
  }
}
