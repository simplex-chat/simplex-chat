package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionView
import itemHPadding
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.padding
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.CoreVersionInfo
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.common.views.helpers.AppBarTitle
import chat.simplex.res.MR

@Composable
fun VersionInfoView(
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  withAuth: (title: String, desc: String, block: () -> Unit) -> Unit,
) {
  val versionInfo = remember { mutableStateOf<CoreVersionInfo?>(null) }
  LaunchedEffect(Unit) {
    versionInfo.value = chatModel.controller.apiGetVersion()
  }
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.app_version_title))
    SectionView {
      Column(Modifier.padding(horizontal = itemHPadding, vertical = DEFAULT_PADDING_HALF)) {
        if (appPlatform.isAndroid) {
          Text(String.format(stringResource(MR.strings.app_version_name), BuildConfigCommon.ANDROID_VERSION_NAME))
          Text(String.format(stringResource(MR.strings.app_version_code), BuildConfigCommon.ANDROID_VERSION_CODE))
        } else {
          Text(String.format(stringResource(MR.strings.app_version_name), BuildConfigCommon.DESKTOP_VERSION_NAME))
          Text(String.format(stringResource(MR.strings.app_version_code), BuildConfigCommon.DESKTOP_VERSION_CODE))
        }
        versionInfo.value?.let { info ->
          Text(String.format(stringResource(MR.strings.core_version), info.version))
          val simplexmqCommit = if (info.simplexmqCommit.length >= 7) info.simplexmqCommit.substring(startIndex = 0, endIndex = 7) else info.simplexmqCommit
          Text(String.format(stringResource(MR.strings.core_simplexmq_version), info.simplexmqVersion, simplexmqCommit))
        }
      }
    }
    SectionDividerSpaced()

    AdvancedSettingsAppSection(showSettingsModal, withAuth)
    SectionBottomSpacer()
  }
}
