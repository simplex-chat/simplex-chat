package chat.simplex.common.views.newchat

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.Column
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import chat.simplex.common.platform.chatModel
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.views.helpers.AppBarTitle
import chat.simplex.common.views.helpers.KeyChangeEffect
import chat.simplex.common.views.onboarding.ReadableText
import chat.simplex.common.views.onboarding.ReadableTextWithLink
import chat.simplex.res.MR

@Composable
fun AddContactLearnMore(close: () -> Unit) {
  Column(
    Modifier.verticalScroll(rememberScrollState()),
  ) {
    AppBarTitle(stringResource(MR.strings.one_time_link))
    ReadableText(MR.strings.scan_qr_to_connect_to_contact)
    ReadableText(MR.strings.if_you_cant_meet_in_person)
    ReadableTextWithLink(MR.strings.read_more_in_user_guide_with_link, "https://simplex.chat/docs/guide/readme.html#connect-to-friends")
  }
  KeyChangeEffect(chatModel.chatId.value) {
    close()
  }
}
