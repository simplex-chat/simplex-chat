package chat.simplex.app.views.chat

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
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.usersettings.simplexTeamUri

val bold = SpanStyle(fontWeight = FontWeight.Bold)

@Composable
fun ChatHelpView(addContact: (() -> Unit)? = null) {
  Column(
    horizontalAlignment = Alignment.Start,
    verticalArrangement = Arrangement.spacedBy(10.dp)
  ) {
    val uriHandler = LocalUriHandler.current

    Text("Thank you for installing SimpleX Chat!")
    Text(
      buildAnnotatedString {
        append("You can ")
        withStyle(SpanStyle(color = MaterialTheme.colors.primary)) {
          append("connect to SimpleX team")
        }
        append(".")
      },
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
        "To start a new chat",
        style = MaterialTheme.typography.h2
      )
      Row(
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.spacedBy(8.dp)
      ) {
        Text("Tap button")
        Icon(
          Icons.Outlined.PersonAdd,
          "Add Contact",
          modifier = if (addContact != null) Modifier.clickable(onClick = addContact) else Modifier,
        )
        Text("above, then:")
      }
      Text(
        buildAnnotatedString {
          withStyle(bold) { append("Add new contact") }
          append(": to create your one-time QR Code for your contact.")
        }
      )
      Text(
        buildAnnotatedString {
          withStyle(bold) { append("Scan QR code") }
          append(": to connect to your contact who shows QR code to you.")
        }
      )
    }

    Column(
      Modifier.padding(top = 24.dp),
      horizontalAlignment = Alignment.Start,
      verticalArrangement = Arrangement.spacedBy(10.dp)
    ) {
      Text("To connect via link", style = MaterialTheme.typography.h2)
      Text("If you received SimpleX Chat invitation link you can open it in your browser:")
      Text(
        buildAnnotatedString {
          append("\uD83D\uDCBB desktop: scan displayed QR code from the app, via ")
          withStyle(bold) { append("Scan QR code") }
          append(".")
        }
      )
      Text(
        buildAnnotatedString {
          append("\uD83D\uDCF1 mobile: tap ")
          withStyle(bold) { append("Open in mobile app") }
          append(", then tap ")
          withStyle(bold) { append("Connect") }
          append(" in the app.")
        }
      )
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
    ChatHelpView({})
  }
}
