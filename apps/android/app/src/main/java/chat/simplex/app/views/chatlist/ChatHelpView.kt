package chat.simplex.app.views.chatlist

import android.content.res.Configuration
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.PersonAdd
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.text.SpanStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.annotatedStringResource
import chat.simplex.app.views.helpers.generalGetString
import chat.simplex.app.views.usersettings.simplexTeamUri

val bold = SpanStyle(fontWeight = FontWeight.Bold)

@Composable
fun ChatHelpView(addContact: (() -> Unit)? = null) {
  Column(
    horizontalAlignment = Alignment.Start,
    verticalArrangement = Arrangement.spacedBy(10.dp)
  ) {
    val uriHandler = LocalUriHandler.current

    Text(generalGetString(R.string.thank_you_for_installing_simplex))
    Text(
      annotatedStringResource(R.string.you_can_connect_to_simplex_chat_founder),
      modifier = Modifier.clickable(onClick = {
        uriHandler.openUri(simplexTeamUri)
      })
    )

    Column(
      Modifier.padding(top = 24.dp),
      horizontalAlignment = Alignment.Start,
      verticalArrangement = Arrangement.spacedBy(10.dp)
    ) {
      Text(
        generalGetString(R.string.to_start_a_new_chat_help_header),
        style = MaterialTheme.typography.h2
      )
      Row(
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.spacedBy(8.dp)
      ) {
        Text(generalGetString(R.string.tap_button))
        Icon(
          Icons.Outlined.PersonAdd,
          generalGetString(R.string.add_contact),
          modifier = if (addContact != null) Modifier.clickable(onClick = addContact) else Modifier,
        )
        Text(generalGetString(R.string.above_then_preposition_continuation))
      }
      Text(annotatedStringResource(R.string.add_new_contact_to_create_one_time_QR_code))
      Text(annotatedStringResource(R.string.scan_QR_code_to_connect_to_contact_who_shows_QR_code))
    }

    Column(
      Modifier.padding(top = 24.dp),
      horizontalAlignment = Alignment.Start,
      verticalArrangement = Arrangement.spacedBy(10.dp)
    ) {
      Text(generalGetString(R.string.to_connect_via_link_title), style = MaterialTheme.typography.h2)
      Text(generalGetString(R.string.if_you_received_simplex_invitation_link_you_can_open_in_browser))
      Text(annotatedStringResource(R.string.desktop_scan_QR_code_from_app_via_scan_QR_code))
      Text(annotatedStringResource(R.string.mobile_tap_open_in_mobile_app_then_tap_connect_in_app))
    }
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewChatHelpLayout() {
  SimpleXTheme {
    ChatHelpView {}
  }
}
