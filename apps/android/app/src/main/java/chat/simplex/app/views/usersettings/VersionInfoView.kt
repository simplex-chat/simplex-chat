package chat.simplex.common.views.usersettings

import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.padding
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.stringResource
import com.icerockdev.library.MR
import chat.simplex.common.model.CoreVersionInfo
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.helpers.AppBarTitle

@Composable
fun VersionInfoView(info: CoreVersionInfo) {
  Column(
    Modifier.padding(horizontal = DEFAULT_PADDING),
  ) {
    AppBarTitle(stringResource(MR.strings.app_version_title), false)
    Text(String.format(stringResource(MR.strings.app_version_name), appVersionInfo.first))
    if (appPlatform == AppPlatform.ANDROID) {
      Text(String.format(stringResource(MR.strings.app_version_code), appVersionInfo.second))
    }
    Text(String.format(stringResource(MR.strings.core_version), info.version))
    val simplexmqCommit = if (info.simplexmqCommit.length >= 7) info.simplexmqCommit.substring(startIndex = 0, endIndex = 7) else info.simplexmqCommit
    Text(String.format(stringResource(MR.strings.core_simplexmq_version), info.simplexmqVersion, simplexmqCommit))
  }
}
