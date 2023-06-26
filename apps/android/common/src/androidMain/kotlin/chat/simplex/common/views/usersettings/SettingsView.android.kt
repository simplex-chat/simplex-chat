package chat.simplex.common.views.usersettings

import SectionView
import androidx.compose.runtime.Composable
import com.icerockdev.library.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import com.jakewharton.processphoenix.ProcessPhoenix
import androidx.work.WorkManager
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.androidAppContext
import chat.simplex.common.platform.serviceSafeStop
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.generalGetString

@Composable
actual fun SettingsSectionApp(
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showCustomModal: (@Composable (ChatModel, () -> Unit) -> Unit) -> (() -> Unit),
  showVersion: () -> Unit,
  withAuth: (title: String, desc: String, block: () -> Unit) -> Unit
) {
  SectionView(stringResource(MR.strings.settings_section_title_app)) {
    SettingsActionItem(painterResource(MR.images.ic_restart_alt), stringResource(MR.strings.settings_restart_app), ::restartApp, extraPadding = true)
    SettingsActionItem(painterResource(MR.images.ic_power_settings_new), stringResource(MR.strings.settings_shutdown), { shutdownAppAlert(::shutdownApp) }, extraPadding = true)
    SettingsActionItem(painterResource(MR.images.ic_code), stringResource(MR.strings.settings_developer_tools), showSettingsModal { DeveloperView(it, showCustomModal, withAuth) }, extraPadding = true)
    AppVersionItem(showVersion)
  }
}


private fun restartApp() {
  ProcessPhoenix.triggerRebirth(androidAppContext)
  shutdownApp()
}

private fun shutdownApp() {
  WorkManager.getInstance(androidAppContext).cancelAllWork()
  serviceSafeStop()
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
