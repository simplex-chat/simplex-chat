package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionView
import itemHPadding
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.padding
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.text.LinkAnnotation
import androidx.compose.ui.text.SpanStyle
import androidx.compose.ui.text.buildAnnotatedString
import androidx.compose.ui.text.withLink
import androidx.compose.ui.text.withStyle
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.CoreVersionInfo
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.common.views.helpers.AppBarTitle
import chat.simplex.common.views.helpers.openExternalLink
import chat.simplex.res.*

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
          if (info.simplexChatCommit.isNotEmpty()) {
            CommitText(stringResource(MR.strings.core_simplex_chat_commit), "simplex-chat", info.simplexChatCommit)
          }
          if (info.simplexmqCommit.isNotEmpty()) {
            CommitText(String.format(stringResource(MR.strings.core_simplexmq_version), info.simplexmqVersion, "%s"), "simplexmq", info.simplexmqCommit)
          } else {
            Text("simplexmq: v${info.simplexmqVersion}")
          }
        }
      }
    }
    SectionDividerSpaced()

    AdvancedSettingsAppSection(showSettingsModal, withAuth)
    SectionBottomSpacer()
  }
}

// the commit's first 7 characters link to it on GitHub, at the %s of format, or after the text of a translation without %s
@Composable
private fun CommitText(format: String, repository: String, commit: String) {
  val uriHandler = LocalUriHandler.current
  val url = "https://github.com/simplex-chat/$repository/commit/$commit"
  val parts = format.split("%s", limit = 2)
  val (before, after) = if (parts.size == 2) parts[0] to parts[1] else "${parts[0]} " to ""
  Text(buildAnnotatedString {
    append(before)
    withLink(LinkAnnotation.Url(url) { uriHandler.openExternalLink(url) }) {
      withStyle(SpanStyle(color = MaterialTheme.colors.primary)) {
        append(commit.take(7))
      }
    }
    append(after)
  })
}
