package chat.simplex.common.views.usersettings

import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.padding
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.model.CoreVersionInfo
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.helpers.AppBarTitle
import chat.simplex.res.MR

@Composable
fun VersionInfoView(info: CoreVersionInfo) {
  Column(
    Modifier.padding(horizontal = DEFAULT_PADDING),
  ) {
    AppBarTitle(stringResource(MR.strings.app_version_title), withPadding = false)
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
}
