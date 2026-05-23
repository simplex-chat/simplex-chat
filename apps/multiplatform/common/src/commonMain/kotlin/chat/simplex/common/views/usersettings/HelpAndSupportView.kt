package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionView
import TextIconSpaced
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.platform.UriHandler
import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.SimpleXInfo
import chat.simplex.common.views.onboarding.WhatsNewView
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
fun HelpAndSupportView(
  chatModel: ChatModel,
  userDisplayName: String?,
  showModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showCustomModal: (@Composable ModalData.(ChatModel, () -> Unit) -> Unit) -> (() -> Unit),
  showVersion: () -> Unit,
) {
  val uriHandler = LocalUriHandler.current
  val stopped = chatModel.chatRunning.value == false
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.help_and_support))

    SectionView(stringResource(MR.strings.settings_section_title_help)) {
      SettingsActionItem(painterResource(MR.images.ic_help), stringResource(MR.strings.how_to_use_simplex_chat), showModal { HelpView(userDisplayName ?: "") }, disabled = stopped)
      SettingsActionItem(painterResource(MR.images.ic_add), stringResource(MR.strings.whats_new), showCustomModal { _, close -> WhatsNewView(viaSettings = true, close = close) }, disabled = stopped)
    }
    SectionDividerSpaced()

    SectionView(stringResource(MR.strings.settings_section_title_about)) {
      SettingsActionItem(painterResource(MR.images.ic_info), stringResource(MR.strings.about_simplex_chat), showModal { SimpleXInfo(it, onboarding = false) })
      AppVersionItem(showVersion)
    }
    SectionDividerSpaced()

    SectionView(stringResource(MR.strings.settings_section_title_contact)) {
      if (!chatModel.desktopNoUserNoRemote) {
        SettingsActionItem(painterResource(MR.images.ic_tag), stringResource(MR.strings.chat_with_the_founder), { uriHandler.openVerifiedSimplexUri(simplexTeamUri) }, textColor = MaterialTheme.colors.primary, disabled = stopped)
      }
      SettingsActionItem(painterResource(MR.images.ic_mail), stringResource(MR.strings.send_us_an_email), { uriHandler.openUriCatching("mailto:chat@simplex.chat") }, textColor = MaterialTheme.colors.primary)
    }
    SectionDividerSpaced()

    SectionView(stringResource(MR.strings.settings_section_title_support_project)) {
      if (!BuildConfigCommon.ANDROID_BUNDLE) {
        ContributeItem(uriHandler)
      }
      RateAppItem(uriHandler)
      StarOnGithubItem(uriHandler)
    }
    SectionBottomSpacer()
  }
}

@Composable private fun ContributeItem(uriHandler: UriHandler) {
  SectionItemView({ uriHandler.openExternalLink("https://github.com/simplex-chat/simplex-chat#contribute") }) {
    Icon(
      painterResource(MR.images.ic_keyboard),
      contentDescription = "GitHub",
      tint = MaterialTheme.colors.secondary,
    )
    TextIconSpaced()
    Text(generalGetString(MR.strings.contribute), color = MaterialTheme.colors.primary)
  }
}

@Composable private fun RateAppItem(uriHandler: UriHandler) {
  SectionItemView({
    runCatching { uriHandler.openUriCatching("market://details?id=chat.simplex.app") }
      .onFailure { uriHandler.openUriCatching("https://play.google.com/store/apps/details?id=chat.simplex.app") }
  }
  ) {
    Icon(
      painterResource(MR.images.ic_star),
      contentDescription = "Google Play",
      tint = MaterialTheme.colors.secondary,
    )
    TextIconSpaced()
    Text(generalGetString(MR.strings.rate_the_app), color = MaterialTheme.colors.primary)
  }
}

@Composable private fun StarOnGithubItem(uriHandler: UriHandler) {
  SectionItemView({ uriHandler.openExternalLink("https://github.com/simplex-chat/simplex-chat") }) {
    Icon(
      painter = painterResource(MR.images.ic_github),
      contentDescription = "GitHub",
      tint = MaterialTheme.colors.secondary,
    )
    TextIconSpaced()
    Text(generalGetString(MR.strings.star_on_github), color = MaterialTheme.colors.primary)
  }
}
