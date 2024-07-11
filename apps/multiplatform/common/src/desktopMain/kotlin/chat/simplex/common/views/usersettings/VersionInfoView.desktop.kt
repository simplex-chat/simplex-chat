package chat.simplex.common.views.usersettings

import SectionView
import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.AppUpdatesChannel
import chat.simplex.common.views.helpers.ExposedDropDownSettingRow
import chat.simplex.common.views.helpers.setupUpdateChecker
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource

@Composable
actual fun AppUpdateSection() {
  SectionView(stringResource(MR.strings.app_updates).uppercase()) {
    val selectedChannel = remember { appPrefs.appUpdateChannel.state }
    val values = AppUpdatesChannel.entries.map { it to it.text }
    ExposedDropDownSettingRow(stringResource(MR.strings.app_check_for_updates), values, selectedChannel) {
      appPrefs.appUpdateChannel.set(it)
      setupUpdateChecker()
    }
  }
}
