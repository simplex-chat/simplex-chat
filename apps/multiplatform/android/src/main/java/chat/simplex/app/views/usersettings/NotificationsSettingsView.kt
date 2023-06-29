package chat.simplex.app.views.usersettings

import SectionBottomSpacer
import SectionView
import SectionViewSelectable
import android.os.Build
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.capitalize
import androidx.compose.ui.text.intl.Locale
import androidx.compose.ui.text.style.TextOverflow
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import kotlinx.coroutines.*
import kotlin.collections.ArrayList

enum class NotificationsMode(private val requiresIgnoringBatterySinceSdk: Int) {
  OFF(Int.MAX_VALUE), PERIODIC(Build.VERSION_CODES.M), SERVICE(Build.VERSION_CODES.S), /*INSTANT(Int.MAX_VALUE) - for Firebase notifications */;

  val requiresIgnoringBattery
    get() = requiresIgnoringBatterySinceSdk <= Build.VERSION.SDK_INT

  companion object {
    val default: NotificationsMode = SERVICE
  }
}

enum class NotificationPreviewMode {
  MESSAGE, CONTACT, HIDDEN;

  companion object {
    val default: NotificationPreviewMode = MESSAGE
  }
}

@Composable
fun NotificationsSettingsView(
  chatModel: ChatModel,
) {
  val onNotificationPreviewModeSelected = { mode: NotificationPreviewMode ->
    chatModel.controller.appPrefs.notificationPreviewMode.set(mode.name)
    chatModel.notificationPreviewMode.value = mode
  }

  NotificationsSettingsLayout(
    notificationsMode = chatModel.notificationsMode,
    notificationPreviewMode = chatModel.notificationPreviewMode,
    showPage = { page ->
      ModalManager.shared.showModalCloseable(true) {
          when (page) {
            CurrentPage.NOTIFICATIONS_MODE -> NotificationsModeView(chatModel.notificationsMode) { changeNotificationsMode(it, chatModel) }
            CurrentPage.NOTIFICATION_PREVIEW_MODE -> NotificationPreviewView(chatModel.notificationPreviewMode, onNotificationPreviewModeSelected)
          }
      }
    },
  )
}

enum class CurrentPage {
  NOTIFICATIONS_MODE, NOTIFICATION_PREVIEW_MODE
}

@Composable
fun NotificationsSettingsLayout(
  notificationsMode: State<NotificationsMode>,
  notificationPreviewMode: State<NotificationPreviewMode>,
  showPage: (CurrentPage) -> Unit,
) {
  val modes = remember { notificationModes() }
  val previewModes = remember { notificationPreviewModes() }

  Column(
    Modifier.fillMaxWidth().verticalScroll(rememberScrollState()),
  ) {
    AppBarTitle(stringResource(R.string.notifications))
    SectionView(null) {
      SettingsActionItemWithContent(null, stringResource(R.string.settings_notifications_mode_title), { showPage(CurrentPage.NOTIFICATIONS_MODE) }) {
        Text(
          modes.first { it.value == notificationsMode.value }.title,
          maxLines = 1,
          overflow = TextOverflow.Ellipsis,
          color = MaterialTheme.colors.secondary
        )
      }
      SettingsActionItemWithContent(null, stringResource(R.string.settings_notification_preview_mode_title), { showPage(CurrentPage.NOTIFICATION_PREVIEW_MODE) }) {
        Text(
          previewModes.first { it.value == notificationPreviewMode.value }.title,
          maxLines = 1,
          overflow = TextOverflow.Ellipsis,
          color = MaterialTheme.colors.secondary
        )
      }
    }
    SectionBottomSpacer()
  }
}

@Composable
fun NotificationsModeView(
  notificationsMode: State<NotificationsMode>,
  onNotificationsModeSelected: (NotificationsMode) -> Unit,
) {
  val modes = remember { notificationModes() }
  Column(
    Modifier.fillMaxWidth(),
  ) {
    AppBarTitle(stringResource(R.string.settings_notifications_mode_title).lowercase().capitalize(Locale.current))
    SectionViewSelectable(null, notificationsMode, modes, onNotificationsModeSelected)
  }
}

@Composable
fun NotificationPreviewView(
  notificationPreviewMode: State<NotificationPreviewMode>,
  onNotificationPreviewModeSelected: (NotificationPreviewMode) -> Unit,
) {
  val previewModes = remember { notificationPreviewModes() }
  Column(
    Modifier.fillMaxWidth(),
  ) {
    AppBarTitle(stringResource(R.string.settings_notification_preview_title))
    SectionViewSelectable(null, notificationPreviewMode, previewModes, onNotificationPreviewModeSelected)
  }
}

// mode, name, description
private fun notificationModes(): List<ValueTitleDesc<NotificationsMode>> {
  val res = ArrayList<ValueTitleDesc<NotificationsMode>>()
  res.add(
    ValueTitleDesc(
      NotificationsMode.OFF,
      generalGetString(R.string.notifications_mode_off),
      generalGetString(R.string.notifications_mode_off_desc),
    )
  )
  res.add(
    ValueTitleDesc(
      NotificationsMode.PERIODIC,
      generalGetString(R.string.notifications_mode_periodic),
      generalGetString(R.string.notifications_mode_periodic_desc),
    )
  )
  res.add(
    ValueTitleDesc(
      NotificationsMode.SERVICE,
      generalGetString(R.string.notifications_mode_service),
      generalGetString(R.string.notifications_mode_service_desc),
    )
  )
  return res
}

// preview mode, name, description
fun notificationPreviewModes(): List<ValueTitleDesc<NotificationPreviewMode>> {
  val res = ArrayList<ValueTitleDesc<NotificationPreviewMode>>()
  res.add(
    ValueTitleDesc(
      NotificationPreviewMode.MESSAGE,
      generalGetString(R.string.notification_preview_mode_message),
      generalGetString(R.string.notification_preview_mode_message_desc),
    )
  )
  res.add(
    ValueTitleDesc(
      NotificationPreviewMode.CONTACT,
      generalGetString(R.string.notification_preview_mode_contact),
      generalGetString(R.string.notification_preview_mode_contact_desc),
    )
  )
  res.add(
    ValueTitleDesc(
      NotificationPreviewMode.HIDDEN,
      generalGetString(R.string.notification_preview_mode_hidden),
      generalGetString(R.string.notification_display_mode_hidden_desc),
    )
  )
  return res
}

fun changeNotificationsMode(mode: NotificationsMode, chatModel: ChatModel) {
  chatModel.controller.appPrefs.notificationsMode.set(mode.name)
  if (mode.requiresIgnoringBattery && !chatModel.controller.isIgnoringBatteryOptimizations(chatModel.controller.appContext)) {
    chatModel.controller.appPrefs.backgroundServiceNoticeShown.set(false)
  }
  chatModel.notificationsMode.value = mode
  SimplexService.StartReceiver.toggleReceiver(mode == NotificationsMode.SERVICE)
  CoroutineScope(Dispatchers.Default).launch {
    if (mode == NotificationsMode.SERVICE)
      SimplexService.start(SimplexApp.context)
    else
      SimplexService.safeStopService(SimplexApp.context)
  }

  if (mode != NotificationsMode.PERIODIC) {
    MessagesFetcherWorker.cancelAll()
  }
  chatModel.controller.showBackgroundServiceNoticeIfNeeded()
}
