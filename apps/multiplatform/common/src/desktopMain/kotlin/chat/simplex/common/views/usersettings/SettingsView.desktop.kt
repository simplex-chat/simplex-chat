package chat.simplex.common.views.usersettings

import SectionView
import androidx.compose.runtime.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
actual fun AdvancedSettingsAppSection(
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  withAuth: (title: String, desc: String, block: () -> Unit) -> Unit,
) {
  SectionView {
    SettingsActionItem(painterResource(MR.images.ic_code), stringResource(MR.strings.settings_developer_tools), showSettingsModal { DeveloperView(withAuth) })
    val selectedChannel = remember { appPrefs.appUpdateChannel.state }
    val allowInternalBuilds = remember { appPrefs.allowInternalBuilds.state }.value
    val values = AppUpdatesChannel.entries.filter { it != AppUpdatesChannel.INTERNAL || allowInternalBuilds }.map { it to it.text }
    ExposedDropDownSettingRow(stringResource(MR.strings.app_check_for_updates), values, selectedChannel) {
      appPrefs.appUpdateChannel.set(it)
      setupUpdateChecker()
    }
  }
}

@Composable
actual fun AppShutdownItem() {}

@Composable
actual fun AllowInternalBuildsItem() {
  SettingsPreferenceItem(painterResource(MR.images.ic_download), stringResource(MR.strings.allow_internal_builds), appPrefs.allowInternalBuilds) { allowed ->
    if (!allowed && appPrefs.appUpdateChannel.get() == AppUpdatesChannel.INTERNAL) {
      appPrefs.appUpdateChannel.set(AppUpdatesChannel.DISABLED)
      setupUpdateChecker()
    }
  }
}
