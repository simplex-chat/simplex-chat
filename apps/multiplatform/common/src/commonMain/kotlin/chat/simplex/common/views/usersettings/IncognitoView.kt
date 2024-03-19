package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.helpers.AppBarTitle
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.common.views.onboarding.ReadableTextWithLink
import chat.simplex.res.MR

@Composable
fun IncognitoView() {
  IncognitoLayout()
}

@Composable
fun IncognitoLayout() {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.settings_section_title_incognito))
    Column(
      Modifier
        .padding(horizontal = DEFAULT_PADDING),
      verticalArrangement = Arrangement.spacedBy(20.dp)
    ) {
      Text(generalGetString(MR.strings.incognito_info_protects))
      Text(generalGetString(MR.strings.incognito_info_allows))
      Text(generalGetString(MR.strings.incognito_info_share))
      ReadableTextWithLink(MR.strings.read_more_in_user_guide_with_link, "https://simplex.chat/docs/guide/chat-profiles.html#incognito-mode")
      SectionBottomSpacer()
    }
  }
}
