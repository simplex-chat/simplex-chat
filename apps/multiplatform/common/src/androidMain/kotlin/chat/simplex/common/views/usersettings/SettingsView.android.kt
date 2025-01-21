package chat.simplex.common.views.usersettings

import SectionView
import androidx.compose.runtime.Composable
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import com.jakewharton.processphoenix.ProcessPhoenix
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
actual fun SettingsSectionApp(
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showVersion: () -> Unit,
  withAuth: (title: String, desc: String, block: () -> Unit) -> Unit
) {
  SectionView(stringResource(MR.strings.settings_section_title_app)) {
    SettingsActionItem(painterResource(MR.images.ic_restart_alt), stringResource(MR.strings.settings_restart_app), ::restartApp)
    SettingsActionItem(painterResource(MR.images.ic_power_settings_new), stringResource(MR.strings.settings_shutdown), { shutdownAppAlert(::shutdownApp) })
    SettingsActionItem(painterResource(MR.images.ic_code), stringResource(MR.strings.settings_developer_tools), showSettingsModal { DeveloperView(withAuth) })
    AppVersionItem(showVersion)
  }
}


fun restartApp() {
  ProcessPhoenix.triggerRebirth(androidAppContext)
  shutdownApp()
}

private fun shutdownApp() {
  androidAppContext.getWorkManagerInstance().cancelAllWork()
  platform.androidServiceSafeStop()
  Runtime.getRuntime().exit(0)
}

private fun shutdownAppAlert(onConfirm: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.shutdown_alert_question),
    text = generalGetString(MR.strings.shutdown_alert_desc),
    destructive = true,
    onConfirm = onConfirm
  )
}
