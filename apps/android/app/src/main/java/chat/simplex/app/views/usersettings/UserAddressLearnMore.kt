package chat.simplex.app.views.usersettings

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import chat.simplex.app.R
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.onboarding.ReadableText
import chat.simplex.app.views.onboarding.ReadableTextWithLink

@Composable
fun UserAddressLearnMore() {
  Column(
    Modifier.verticalScroll(rememberScrollState()),
  ) {
    AppBarTitle(stringResource(R.string.simplex_address))
    ReadableText(R.string.you_can_share_your_address)
    ReadableText(R.string.you_wont_lose_your_contacts_if_delete_address)
    ReadableText(R.string.you_can_accept_or_reject_connection)
    ReadableTextWithLink(R.string.read_more_in_user_guide_with_link, "https://simplex.chat/docs/guide/app-settings.html#your-simplex-contact-address")
  }
}
