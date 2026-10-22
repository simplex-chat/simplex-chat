package chat.simplex.app.ui.android

import android.app.Notification
import android.app.NotificationManager
import android.content.Intent
import android.net.Uri
import androidx.core.content.FileProvider
import androidx.test.platform.app.InstrumentationRegistry
import androidx.test.runner.lifecycle.ActivityLifecycleMonitorRegistry
import androidx.test.runner.lifecycle.Stage
import chat.simplex.app.MainActivity
import chat.simplex.app.model.NtfManager
import chat.simplex.app.ui.harness.App
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTestApp
import chat.simplex.app.ui.harness.chatData
import chat.simplex.app.ui.harness.chatRunningInCore
import chat.simplex.common.helpers.APPLICATION_ID
import chat.simplex.common.platform.androidAppContext
import chat.simplex.common.platform.chatModel
import chat.simplex.common.platform.isAppOnForeground
import chat.simplex.common.ui.Ui
import java.io.File

internal fun UiScope.deliverToApp(intent: Intent) {
  InstrumentationRegistry.getInstrumentation().targetContext.startActivity(
    Intent(intent).setClass(androidAppContext, MainActivity::class.java).addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
  )
  rule.waitForIdle()
}

internal fun contentUri(file: File): Uri = FileProvider.getUriForFile(androidAppContext, "$APPLICATION_ID.provider", file)

internal fun shareTextIntent(text: String): Intent =
  Intent(Intent.ACTION_SEND).setType("text/plain").putExtra(Intent.EXTRA_TEXT, text)

internal fun shareStreamIntent(type: String, uri: Uri, text: String? = null): Intent =
  Intent(Intent.ACTION_SEND).setType(type).putExtra(Intent.EXTRA_STREAM, uri).apply { if (text != null) putExtra(Intent.EXTRA_TEXT, text) }

internal fun shareStreamsIntent(type: String, uris: List<Uri>): Intent =
  Intent(Intent.ACTION_SEND_MULTIPLE).setType(type).putParcelableArrayListExtra(Intent.EXTRA_STREAM, ArrayList(uris))

internal fun viewIntent(link: String?): Intent =
  Intent(Intent.ACTION_VIEW).apply { if (link != null) data = Uri.parse(link) }

internal fun UiScope.shareListTitle(): String? =
  Ui.Share.Bar.part("title").let { if (exists(it)) textsOf(it).firstOrNull() else null }

internal fun UiScope.awaitShareList(title: String) =
  poll(what = "the share list titled `$title` in place of the chat list") { shareListTitle() == title && !exists(Ui.ChatList.Search) }

internal fun UiScope.stopChatFromChatData(app: App) {
  chatData(app).management()
  click(Ui.Database.RunChat)
  poll(what = "the Stop chat? alert with Stop") { seesText("Stop chat?") && "Stop" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
  click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
  poll(what = "the chat stopped on screen and in the core") { seesText("Chat is stopped") && chatModel.chatRunning.value == false && !chatRunningInCore() }
}

internal class PostedNotification(val id: Int, val notification: Notification) {
  val title: String? get() = notification.extras.getCharSequence(Notification.EXTRA_TITLE)?.toString()
  val text: String? get() = notification.extras.getCharSequence(Notification.EXTRA_TEXT)?.toString()
  override fun toString() = "#$id `$title`: `$text`"
}

private val notificationManager: NotificationManager get() = androidAppContext.getSystemService(NotificationManager::class.java)

internal fun messageNotifications(): List<PostedNotification> =
  notificationManager.activeNotifications
    .filter { it.notification.channelId == NtfManager.MessageChannel && (it.notification.flags and Notification.FLAG_GROUP_SUMMARY) == 0 }
    .map { PostedNotification(it.id, it.notification) }

internal fun messageSummaryShown(): Boolean =
  notificationManager.activeNotifications.any {
    it.notification.channelId == NtfManager.MessageChannel && it.notification.group == NtfManager.MessageGroup && (it.notification.flags and Notification.FLAG_GROUP_SUMMARY) != 0
  }

internal fun clearNotificationsViaApi() = notificationManager.cancelAll()

internal fun UiScope.awaitForeground() = poll(what = "the app in the foreground") { isAppOnForeground }

internal fun runningMainActivities(): List<MainActivity> {
  var live = emptyList<MainActivity>()
  UiTestApp.onMain {
    val monitor = ActivityLifecycleMonitorRegistry.getInstance()
    live = Stage.values().filter { it != Stage.DESTROYED }.flatMap { monitor.getActivitiesInStage(it) }.filterIsInstance<MainActivity>()
  }
  return live
}

internal fun UiScope.finishMainActivities() {
  val live = runningMainActivities()
  UiTestApp.onMain { live.forEach { it.finish() } }
  poll(what = "every MainActivity to be destroyed") { runningMainActivities().isEmpty() }
}
