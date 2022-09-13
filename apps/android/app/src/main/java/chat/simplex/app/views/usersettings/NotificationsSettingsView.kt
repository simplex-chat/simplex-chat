package chat.simplex.app.views.usersettings

import SectionItemViewSpaceBetween
import SectionTextFooter
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Check
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.capitalize
import androidx.compose.ui.text.intl.Locale
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import kotlinx.coroutines.*
import kotlin.collections.ArrayList

enum class NotificationsMode(val requiresIgnoringBattery: Boolean) {
  OFF(false), PERIODIC(false), SERVICE(true), /*INSTANT(false) - for Firebase notifications */;

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
  showCustomModal: (@Composable (ChatModel, () -> Unit) -> Unit) -> (() -> Unit),
) {
  val onNotificationsModeSelected = { mode: NotificationsMode ->
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
        SimplexService.stop(SimplexApp.context)
    }

    if (mode != NotificationsMode.PERIODIC) {
      MessagesFetcherWorker.cancelAll()
    }
    chatModel.controller.showBackgroundServiceNoticeIfNeeded()
  }
  val onNotificationPreviewModeSelected = { mode: NotificationPreviewMode ->
    chatModel.controller.appPrefs.notificationPreviewMode.set(mode.name)
    chatModel.notificationPreviewMode.value = mode
  }

  NotificationsSettingsLayout(
    notificationsMode = chatModel.notificationsMode,
    notificationPreviewMode = chatModel.notificationPreviewMode,
    showPage = { page ->
      showCustomModal { _, close ->
        ModalView(
          close = close, modifier = Modifier,
          background = if (isInDarkTheme()) MaterialTheme.colors.background else SettingsBackgroundLight
        ) {
          when (page) {
            CurrentPage.NOTIFICATIONS_MODE -> NotificationsModeView(chatModel.notificationsMode, onNotificationsModeSelected)
            CurrentPage.NOTIFICATION_PREVIEW_MODE -> NotificationPreviewView(chatModel.notificationPreviewMode, onNotificationPreviewModeSelected)
          }
        }
      }()
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
    Modifier.fillMaxWidth(),
    horizontalAlignment = Alignment.Start,
  ) {
    Text(
      stringResource(R.string.notifications),
      Modifier.padding(start = 16.dp, end = 16.dp, bottom = 24.dp),
      style = MaterialTheme.typography.h1
    )
    SectionView(null) {
      Column(
        Modifier.padding(horizontal = 8.dp)
      ) {
        SectionItemViewSpaceBetween({ showPage(CurrentPage.NOTIFICATIONS_MODE) }, padding = PaddingValues()) {
          Text(stringResource(R.string.settings_notifications_mode_title))
          Spacer(Modifier.padding(horizontal = 10.dp))
          Text(
            modes.first { it.first == notificationsMode.value }.second,
            maxLines = 1,
            overflow = TextOverflow.Ellipsis,
            color = HighOrLowlight
          )
        }
        Spacer(Modifier.padding(horizontal = 4.dp))
        SectionItemViewSpaceBetween({ showPage(CurrentPage.NOTIFICATION_PREVIEW_MODE) }, padding = PaddingValues()) {
          Text(stringResource(R.string.settings_notification_preview_mode_title))
          Spacer(Modifier.padding(horizontal = 10.dp))
          Text(
            previewModes.first { it.first == notificationPreviewMode.value }.second,
            maxLines = 1,
            overflow = TextOverflow.Ellipsis,
            color = HighOrLowlight
          )
        }
      }
    }
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
    horizontalAlignment = Alignment.Start,
  ) {
    Text(
      stringResource(R.string.settings_notifications_mode_title).lowercase().capitalize(Locale.current),
      Modifier.padding(start = 16.dp, end = 16.dp, bottom = 24.dp),
      style = MaterialTheme.typography.h1
    )
    SectionView(null) {
      LazyColumn(
        Modifier.padding(horizontal = 8.dp)
      ) {
        items(modes.size) { index ->
          val item = modes[index]
          val onClick = {
            onNotificationsModeSelected(item.first)
          }
          SectionItemViewSpaceBetween(onClick, padding = PaddingValues()) {
            Text(item.second)
            if (notificationsMode.value == item.first) {
              Icon(Icons.Outlined.Check, item.second, tint = HighOrLowlight)
            }
          }
          Spacer(Modifier.padding(horizontal = 4.dp))
        }
      }
    }
    SectionTextFooter(modes.first { it.first == notificationsMode.value }.third)
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
    horizontalAlignment = Alignment.Start,
  ) {
    Text(
      stringResource(R.string.settings_notification_preview_title),
      Modifier.padding(start = 16.dp, end = 16.dp, bottom = 24.dp),
      style = MaterialTheme.typography.h1
    )

    SectionView(null) {
      LazyColumn(
        Modifier.padding(horizontal = 8.dp)
      ) {
        items(previewModes.size) { index ->
          val item = previewModes[index]
          val onClick = {
            onNotificationPreviewModeSelected(item.first)
          }
          SectionItemViewSpaceBetween(onClick, padding = PaddingValues()) {
            Text(item.second)
            if (notificationPreviewMode.value == item.first) {
              Icon(Icons.Outlined.Check, item.second, tint = HighOrLowlight)
            }
          }
          Spacer(Modifier.padding(horizontal = 4.dp))
        }
      }
    }
    SectionTextFooter(previewModes.first { it.first == notificationPreviewMode.value }.third)
  }
}

// mode, name, description
fun notificationModes(): List<Triple<NotificationsMode, String, String>> {
  val res = ArrayList<Triple<NotificationsMode, String, String>>()
  res.add(
    Triple(
      NotificationsMode.OFF,
      generalGetString(R.string.notifications_mode_off),
      generalGetString(R.string.notifications_mode_off_desc),
    )
  )
  res.add(
    Triple(
      NotificationsMode.PERIODIC,
      generalGetString(R.string.notifications_mode_periodic),
      generalGetString(R.string.notifications_mode_periodic_desc),
    )
  )
  res.add(
    Triple(
      NotificationsMode.SERVICE,
      generalGetString(R.string.notifications_mode_service),
      generalGetString(R.string.notifications_mode_service_desc),
    )
  )
  return res
}

// preview mode, name, description
fun notificationPreviewModes(): List<Triple<NotificationPreviewMode, String, String>> {
  val res = ArrayList<Triple<NotificationPreviewMode, String, String>>()
  res.add(
    Triple(
      NotificationPreviewMode.MESSAGE,
      generalGetString(R.string.notification_preview_mode_message),
      generalGetString(R.string.notification_preview_mode_message_desc),
    )
  )
  res.add(
    Triple(
      NotificationPreviewMode.CONTACT,
      generalGetString(R.string.notification_preview_mode_contact),
      generalGetString(R.string.notification_preview_mode_contact_desc),
    )
  )
  res.add(
    Triple(
      NotificationPreviewMode.HIDDEN,
      generalGetString(R.string.notification_preview_mode_hidden),
      generalGetString(R.string.notification_display_mode_hidden_desc),
    )
  )
  return res
}
