package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionView
import SectionViewSelectable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.AnnotatedString
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.capitalize
import androidx.compose.ui.text.intl.Locale
import androidx.compose.ui.text.style.TextOverflow
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import kotlin.collections.ArrayList

@Composable
fun NotificationsSettingsView(
  chatModel: ChatModel,
) {
  val onNotificationPreviewModeSelected = { mode: NotificationPreviewMode ->
    chatModel.controller.appPrefs.notificationPreviewMode.set(mode.name)
    chatModel.notificationPreviewMode.value = mode
  }

  NotificationsSettingsLayout(
    notificationsMode = remember { chatModel.controller.appPrefs.notificationsMode.state },
    notificationPreviewMode = chatModel.notificationPreviewMode,
    showPage = { page ->
      ModalManager.start.showModalCloseable(true) {
        when (page) {
          CurrentPage.NOTIFICATIONS_MODE -> NotificationsModeView(chatModel.controller.appPrefs.notificationsMode.state) { changeNotificationsMode(it, chatModel) }
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

  ColumnWithScrollBar(
    Modifier.fillMaxWidth(),
  ) {
    AppBarTitle(stringResource(MR.strings.notifications))
    SectionView(null) {
      if (appPlatform == AppPlatform.ANDROID) {
        SettingsActionItemWithContent(null, stringResource(MR.strings.settings_notifications_mode_title), { showPage(CurrentPage.NOTIFICATIONS_MODE) }) {
          Text(
            modes.first { it.value == notificationsMode.value }.title,
            maxLines = 1,
            overflow = TextOverflow.Ellipsis,
            color = MaterialTheme.colors.secondary
          )
        }
      }
      SettingsActionItemWithContent(null, stringResource(MR.strings.settings_notification_preview_mode_title), { showPage(CurrentPage.NOTIFICATION_PREVIEW_MODE) }) {
        Text(
          previewModes.first { it.value == notificationPreviewMode.value }.title,
          maxLines = 1,
          overflow = TextOverflow.Ellipsis,
          color = MaterialTheme.colors.secondary
        )
      }
      SettingsPreferenceItem(null, stringResource(MR.strings.settings_notification_led_title), chatModel.controller.appPrefs.notificationLEDEnabled)
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
  ColumnWithScrollBar(
    Modifier.fillMaxWidth(),
  ) {
    AppBarTitle(stringResource(MR.strings.settings_notifications_mode_title).lowercase().capitalize(Locale.current))
    SectionViewSelectable(null, notificationsMode, modes, onNotificationsModeSelected)
  }
}

@Composable
fun NotificationPreviewView(
  notificationPreviewMode: State<NotificationPreviewMode>,
  onNotificationPreviewModeSelected: (NotificationPreviewMode) -> Unit,
) {
  val previewModes = remember { notificationPreviewModes() }
  ColumnWithScrollBar(
    Modifier.fillMaxWidth(),
  ) {
    AppBarTitle(stringResource(MR.strings.settings_notification_preview_title))
    SectionViewSelectable(null, notificationPreviewMode, previewModes, onNotificationPreviewModeSelected)
  }
}

// mode, name, description
private fun notificationModes(): List<ValueTitleDesc<NotificationsMode>> {
  val res = ArrayList<ValueTitleDesc<NotificationsMode>>()
  res.add(
    ValueTitleDesc(
      NotificationsMode.OFF,
      generalGetString(MR.strings.notifications_mode_off),
      AnnotatedString(generalGetString(MR.strings.notifications_mode_off_desc)),
    )
  )
  res.add(
    ValueTitleDesc(
      NotificationsMode.PERIODIC,
      generalGetString(MR.strings.notifications_mode_periodic),
      AnnotatedString(generalGetString(MR.strings.notifications_mode_periodic_desc)),
    )
  )
  res.add(
    ValueTitleDesc(
      NotificationsMode.SERVICE,
      generalGetString(MR.strings.notifications_mode_service),
      AnnotatedString(generalGetString(MR.strings.notifications_mode_service_desc)),
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
      generalGetString(MR.strings.notification_preview_mode_message),
      AnnotatedString(generalGetString(MR.strings.notification_preview_mode_message_desc)),
    )
  )
  res.add(
    ValueTitleDesc(
      NotificationPreviewMode.CONTACT,
      generalGetString(MR.strings.notification_preview_mode_contact),
      AnnotatedString(generalGetString(MR.strings.notification_preview_mode_contact_desc)),
    )
  )
  res.add(
    ValueTitleDesc(
      NotificationPreviewMode.HIDDEN,
      generalGetString(MR.strings.notification_preview_mode_hidden),
      AnnotatedString(generalGetString(MR.strings.notification_display_mode_hidden_desc)),
    )
  )
  return res
}

fun changeNotificationsMode(mode: NotificationsMode, chatModel: ChatModel) {
  chatModel.controller.appPrefs.notificationsMode.set(mode)
  platform.androidNotificationsModeChanged(mode)
}
