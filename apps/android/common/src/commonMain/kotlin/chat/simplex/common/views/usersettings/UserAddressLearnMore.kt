package chat.simplex.common.views.usersettings

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.stringResource
import com.icerockdev.library.MR
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.ReadableText
import chat.simplex.common.views.onboarding.ReadableTextWithLink

@Composable
fun UserAddressLearnMore() {
  Column(
    Modifier.verticalScroll(rememberScrollState()),
  ) {
    AppBarTitle(stringResource(MR.strings.simplex_address))
    ReadableText(MR.strings.you_can_share_your_address)
    ReadableText(MR.strings.you_wont_lose_your_contacts_if_delete_address)
    ReadableText(MR.strings.you_can_accept_or_reject_connection)
    ReadableTextWithLink(MR.strings.read_more_in_user_guide_with_link, "https://simplex.chat/docs/guide/app-settings.html#your-simplex-contact-address")
  }
}
