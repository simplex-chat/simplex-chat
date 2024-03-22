package chat.simplex.common.views.usersettings

import androidx.compose.foundation.layout.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.ReadableText
import chat.simplex.common.views.onboarding.ReadableTextWithLink
import chat.simplex.res.MR

@Composable
fun UserAddressLearnMore() {
  ColumnWithScrollBar(Modifier
    .fillMaxHeight()
    .padding(horizontal = DEFAULT_PADDING)
  ) {
    AppBarTitle(stringResource(MR.strings.simplex_address))
    ReadableText(MR.strings.you_can_share_your_address)
    ReadableText(MR.strings.you_wont_lose_your_contacts_if_delete_address)
    ReadableText(MR.strings.you_can_accept_or_reject_connection)
    ReadableTextWithLink(MR.strings.read_more_in_user_guide_with_link, "https://simplex.chat/docs/guide/app-settings.html#your-simplex-contact-address")
  }
}
