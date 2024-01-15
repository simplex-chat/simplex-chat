package chat.simplex.common.views.usersettings

import SectionView
import androidx.compose.runtime.Composable
import chat.simplex.common.model.ChatModel
import chat.simplex.common.views.helpers.ModalData
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
    AppVersionItem(showVersion)
  }
}
