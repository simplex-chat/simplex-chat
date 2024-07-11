package chat.simplex.common.views.usersettings

import SectionDividerSpaced
import SectionSpacer
import SectionView
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.padding
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.CoreVersionInfo
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource

@Composable
fun VersionInfoView(info: CoreVersionInfo) {
  Column {
    AppBarTitle(stringResource(MR.strings.app_version_title))
    Column(
      Modifier.padding(horizontal = DEFAULT_PADDING),
    ) {
      if (appPlatform.isAndroid) {
        Text(String.format(stringResource(MR.strings.app_version_name), BuildConfigCommon.ANDROID_VERSION_NAME))
        Text(String.format(stringResource(MR.strings.app_version_code), BuildConfigCommon.ANDROID_VERSION_CODE))
      } else {
        Text(String.format(stringResource(MR.strings.app_version_name), BuildConfigCommon.DESKTOP_VERSION_NAME))
        Text(String.format(stringResource(MR.strings.app_version_code), BuildConfigCommon.DESKTOP_VERSION_CODE))
      }
      Text(String.format(stringResource(MR.strings.core_version), info.version))
      val simplexmqCommit = if (info.simplexmqCommit.length >= 7) info.simplexmqCommit.substring(startIndex = 0, endIndex = 7) else info.simplexmqCommit
      Text(String.format(stringResource(MR.strings.core_simplexmq_version), info.simplexmqVersion, simplexmqCommit))
    }
    SectionDividerSpaced(maxBottomPadding = false)
    SectionView(stringResource(MR.strings.app_updates).uppercase()) {
      val selectedChannel = remember { appPrefs.appUpdateChannel.state }
      val values = AppUpdatesChannel.entries.map { it to it.text }
      ExposedDropDownSettingRow(stringResource(MR.strings.app_check_for_updates), values, selectedChannel) {
        appPrefs.appUpdateChannel.set(it)
        setupUpdateChecker()
      }
    }
  }
}
