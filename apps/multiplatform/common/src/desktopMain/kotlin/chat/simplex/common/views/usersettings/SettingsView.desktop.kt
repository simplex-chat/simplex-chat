package chat.simplex.common.views.usersettings

import SectionView
import androidx.compose.runtime.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
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
    val values = AppUpdatesChannel.entries.map { it to it.text }
    ExposedDropDownSettingRow(stringResource(MR.strings.app_check_for_updates), values, selectedChannel) {
      appPrefs.appUpdateChannel.set(it)
      setupUpdateChecker()
    }
  }
}

@Composable
actual fun AppShutdownItem() {}
