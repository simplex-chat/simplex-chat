package chat.simplex.common.views.usersettings

import SectionView
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.padding
import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.Modifier
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.AppUpdatesChannel
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
actual fun SettingsSectionApp(
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showCustomModal: (@Composable ModalData.(ChatModel, () -> Unit) -> Unit) -> (() -> Unit),
  showVersion: () -> Unit,
  withAuth: (title: String, desc: String, block: () -> Unit) -> Unit
) {
  SectionView(stringResource(MR.strings.settings_section_title_app)) {
    SettingsActionItem(painterResource(MR.images.ic_code), stringResource(MR.strings.settings_developer_tools), showSettingsModal { DeveloperView(it, showCustomModal, withAuth) }, extraPadding = true)
    val selectedChannel = remember { appPrefs.appUpdateChannel.state }
    val values = AppUpdatesChannel.entries.map { it to it.text }
    Box(Modifier.padding(start = DEFAULT_PADDING_HALF * 1.4f)) {
      ExposedDropDownSettingRow(stringResource(MR.strings.app_check_for_updates), values, selectedChannel) {
        appPrefs.appUpdateChannel.set(it)
        setupUpdateChecker()
      }
    }
    AppVersionItem(showVersion)
  }
}
