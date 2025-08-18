package chat.simplex.common.views.usersettings

import SectionDividerSpaced
import SectionTextFooter
import SectionView
import androidx.compose.runtime.Composable
import androidx.compose.runtime.State
import chat.simplex.common.model.NotificationsMode
import chat.simplex.common.platform.AppPlatform
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.platform.chatModel
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
actual fun ServiceSaveBatterySection(notificationsMode: State<NotificationsMode>) {
  if (appPlatform == AppPlatform.ANDROID && notificationsMode.value == NotificationsMode.SERVICE && chatModel.controller.appPrefs.developerTools.get()) {
    SectionDividerSpaced()
    SectionView("BATTERY") {
      SettingsPreferenceItem(painterResource(MR.images.ic_battery_3_bar), "Save battery (BETA)", chatModel.controller.appPrefs.backgroundServiceSaveBattery)
      SettingsActionItem(painterResource(MR.images.ic_restart_alt), stringResource(MR.strings.settings_restart_app), ::restartApp)
      SectionTextFooter("This option may delay notifications. Restart the app after changing this option.")
    }
  }
}
