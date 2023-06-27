package chat.simplex.common.views.newchat

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.Column
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.stringResource
import com.icerockdev.library.MR
import chat.simplex.common.views.helpers.AppBarTitle
import chat.simplex.common.views.onboarding.ReadableText
import chat.simplex.common.views.onboarding.ReadableTextWithLink

@Composable
fun AddContactLearnMore() {
  Column(
    Modifier.verticalScroll(rememberScrollState()),
  ) {
    AppBarTitle(stringResource(R.string.one_time_link))
    ReadableText(R.string.scan_qr_to_connect_to_contact)
    ReadableText(R.string.if_you_cant_meet_in_person)
    ReadableTextWithLink(R.string.read_more_in_user_guide_with_link, "https://simplex.chat/docs/guide/readme.html#connect-to-friends")
  }
}
