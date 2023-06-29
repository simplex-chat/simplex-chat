package chat.simplex.app.views.usersettings

import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.padding
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import chat.simplex.app.BuildConfig
import chat.simplex.app.R
import chat.simplex.app.model.CoreVersionInfo
import chat.simplex.app.ui.theme.DEFAULT_PADDING
import chat.simplex.app.views.helpers.AppBarTitle

@Composable
fun VersionInfoView(info: CoreVersionInfo) {
  Column(
    Modifier.padding(horizontal = DEFAULT_PADDING),
  ) {
    AppBarTitle(stringResource(R.string.app_version_title), false)
    Text(String.format(stringResource(R.string.app_version_name), BuildConfig.VERSION_NAME))
    Text(String.format(stringResource(R.string.app_version_code), BuildConfig.VERSION_CODE))
    Text(String.format(stringResource(R.string.core_version), info.version))
    val simplexmqCommit = if (info.simplexmqCommit.length >= 7) info.simplexmqCommit.substring(startIndex = 0, endIndex = 7) else info.simplexmqCommit
    Text(String.format(stringResource(R.string.core_simplexmq_version), info.simplexmqVersion, simplexmqCommit))
  }
}
