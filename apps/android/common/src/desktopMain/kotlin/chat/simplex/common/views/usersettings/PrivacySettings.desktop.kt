package chat.simplex.common.views.usersettings

import SectionView
import androidx.compose.runtime.Composable
import chat.simplex.common.model.ChatModel
import com.icerockdev.library.MR
import dev.icerock.moko.resources.compose.stringResource

@Composable
actual fun PrivacyDeviceSection(
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  setPerformLA: (Boolean) -> Unit,
) {
  SectionView(stringResource(MR.strings.settings_section_title_device)) {
    ChatLockItem(showSettingsModal, setPerformLA)
  }
}