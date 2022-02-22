package chat.simplex.app.views.chat

import android.content.res.Configuration
import androidx.compose.foundation.background
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

@Composable
fun ChatHelpView(addContact: () -> Unit, doAddContact: Boolean) {
  Column(
    Modifier
      .fillMaxWidth()
      .background(MaterialTheme.colors.background),
    horizontalAlignment = Alignment.Start,
    verticalArrangement = Arrangement.spacedBy(10.dp)
  ) {
    val uriHandler = LocalUriHandler.current

    Text(
      "Thank you for installing SimpleX Chat!",
      color = MaterialTheme.colors.onBackground
    )
    Text(
      buildAnnotatedString {
        withStyle(SpanStyle(color = MaterialTheme.colors.onBackground)) {
          append("You can ")
        }
        withStyle(SpanStyle(color = MaterialTheme.colors.primary)) {
          append("connect to SimpleX team")
        }
        withStyle(SpanStyle(color = MaterialTheme.colors.onBackground)) {
          append(".")
        }
      },
      modifier = Modifier
        .clickable(onClick = { uriHandler.openUri(simplexTeamUri) })
    )

    Column(
      Modifier.padding(top = 24.dp),
      horizontalAlignment = Alignment.Start,
      verticalArrangement = Arrangement.spacedBy(10.dp)
    ) {
      Text(
        "To start a new chat",
        color = MaterialTheme.colors.onBackground,
        style = MaterialTheme.typography.h2
      )
      Row(
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.spacedBy(8.dp)
      ) {
        Text(
          "Tap button",
          color = MaterialTheme.colors.onBackground
        )
        Icon(
          Icons.Outlined.PersonAdd,
          "Add Contact",
          modifier = if (doAddContact) Modifier.clickable(onClick = addContact) else Modifier,
          tint = MaterialTheme.colors.onBackground,
        )
        Text(
          "above, then:",
          color = MaterialTheme.colors.onBackground
        )
      }
      Text(
        buildAnnotatedString {
          withStyle(SpanStyle(color = MaterialTheme.colors.onBackground, fontWeight = FontWeight.Bold)) {
            append("Add new contact")
          }
          withStyle(SpanStyle(color = MaterialTheme.colors.onBackground)) {
            append(": to create your one-time QR Code for your contact.")
          }
        }
      )
      Text(
        buildAnnotatedString {
          withStyle(SpanStyle(color = MaterialTheme.colors.onBackground, fontWeight = FontWeight.Bold)) {
            append("Scan QR code")
          }
          withStyle(SpanStyle(color = MaterialTheme.colors.onBackground)) {
            append(": to connect to your contact who shows QR code to you.")
          }
        }
      )
    }

    Column(
      Modifier.padding(top = 24.dp),
      horizontalAlignment = Alignment.Start,
      verticalArrangement = Arrangement.spacedBy(10.dp)
    ) {
      Text(
        "To connect via link",
        color = MaterialTheme.colors.onBackground,
        style = MaterialTheme.typography.h2
      )
      Text(
        "If you received SimpleX Chat invitation link you can open it in your browser:",
        color = MaterialTheme.colors.onBackground
      )
      Text(
        buildAnnotatedString {
          withStyle(SpanStyle(color = MaterialTheme.colors.onBackground)) {
            append("\uD83D\uDCBB desktop: scan displayed QR code from the app, via ")
          }
          withStyle(
            SpanStyle(color = MaterialTheme.colors.onBackground, fontWeight = FontWeight.Bold)
          ) {
            append("Scan QR code")
          }
          withStyle(SpanStyle(color = MaterialTheme.colors.onBackground)) {
            append(".")
          }
        }
      )
      Text(
        buildAnnotatedString {
          withStyle(SpanStyle(color = MaterialTheme.colors.onBackground)) {
            append("\uD83D\uDCF1 mobile: tap ")
          }
          withStyle(
            SpanStyle(color = MaterialTheme.colors.onBackground, fontWeight = FontWeight.Bold)
          ) {
            append("Open in mobile app")
          }
          withStyle(SpanStyle(color = MaterialTheme.colors.onBackground)) {
            append(", then tap ")
          }
          withStyle(
            SpanStyle(color = MaterialTheme.colors.onBackground, fontWeight = FontWeight.Bold)
          ) {
            append("Connect")
          }
          withStyle(SpanStyle(color = MaterialTheme.colors.onBackground)) {
            append(" in the app.")
          }
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
    ChatHelpView({}, false)
  }
}
