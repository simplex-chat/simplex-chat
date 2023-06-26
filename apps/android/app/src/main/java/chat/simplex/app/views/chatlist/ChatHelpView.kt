package chat.simplex.common.views.chatlist

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import dev.icerock.moko.resources.compose.painterResource
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.SpanStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.icerockdev.library.MR
import chat.simplex.common.ui.theme.SimpleXTheme
import chat.simplex.common.views.helpers.annotatedStringResource
import chat.simplex.common.views.onboarding.ReadableTextWithLink
import chat.simplex.common.views.usersettings.MarkdownHelpView
import chat.simplex.common.views.usersettings.simplexTeamUri

val bold = SpanStyle(fontWeight = FontWeight.Bold)

@Composable
fun ChatHelpView(addContact: (() -> Unit)? = null) {
  Column(
    verticalArrangement = Arrangement.spacedBy(10.dp)
  ) {
    Text(stringResource(MR.strings.thank_you_for_installing_simplex), lineHeight = 22.sp)
    ReadableTextWithLink(MR.strings.you_can_connect_to_simplex_chat_founder, simplexTeamUri)
    Column(
      Modifier.padding(top = 24.dp),
      verticalArrangement = Arrangement.spacedBy(10.dp)
    ) {
      Text(
        stringResource(MR.strings.to_start_a_new_chat_help_header),
        style = MaterialTheme.typography.h2,
        lineHeight = 22.sp
      )
      Row(
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.spacedBy(8.dp)
      ) {
        Text(stringResource(MR.strings.chat_help_tap_button))
        Icon(
          painterResource(MR.images.ic_person_add),
          stringResource(MR.strings.add_contact),
          modifier = if (addContact != null) Modifier.clickable(onClick = addContact) else Modifier,
        )
        Text(stringResource(MR.strings.above_then_preposition_continuation))
      }
      Text(annotatedStringResource(MR.strings.add_new_contact_to_create_one_time_QR_code), lineHeight = 22.sp)
      Text(annotatedStringResource(MR.strings.scan_QR_code_to_connect_to_contact_who_shows_QR_code), lineHeight = 22.sp)
    }

    Column(
      Modifier.padding(top = 24.dp),
      verticalArrangement = Arrangement.spacedBy(10.dp)
    ) {
      Text(stringResource(MR.strings.to_connect_via_link_title), style = MaterialTheme.typography.h2)
      Text(stringResource(MR.strings.if_you_received_simplex_invitation_link_you_can_open_in_browser), lineHeight = 22.sp)
      Text(annotatedStringResource(MR.strings.desktop_scan_QR_code_from_app_via_scan_QR_code), lineHeight = 22.sp)
      Text(annotatedStringResource(MR.strings.mobile_tap_open_in_mobile_app_then_tap_connect_in_app), lineHeight = 22.sp)
    }

    Column(
      Modifier.padding(vertical = 24.dp),
      verticalArrangement = Arrangement.spacedBy(10.dp)
    ) {
      Text(stringResource(MR.strings.markdown_in_messages), style = MaterialTheme.typography.h2)
      MarkdownHelpView()
    }
  }
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
fun PreviewChatHelpLayout() {
  SimpleXTheme {
    ChatHelpView {}
  }
}
