package chat.simplex.app.views.usersettings

import android.content.res.Configuration
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.Profile
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.TerminalView
import chat.simplex.app.views.newchat.ModalManager

@Composable
fun SettingsView(chatModel: ChatModel) {
  val user = chatModel.currentUser.value
  if (user != null) {
    SettingsLayout(
      chatModel = chatModel,
      profile = user.profile
    )
  }
}

val simplexTeamUri =
  "simplex:/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D"

@Composable
fun SettingsLayout(chatModel: ChatModel, profile: Profile) {
  val uriHandler = LocalUriHandler.current
  Surface(
    Modifier
      .background(MaterialTheme.colors.background)
      .fillMaxSize()
  ) {
    Column(
      Modifier
        .fillMaxSize()
        .background(MaterialTheme.colors.background)
        .padding(8.dp)
        .padding(top = 16.dp)
    ) {
      Text(
        "Your Settings",
        style = MaterialTheme.typography.h1,
      )
      Spacer(Modifier.height(30.dp))

      SettingsSectionView({ ModalManager.shared.showModal { UserProfileView(chatModel) } }, 60.dp) {
        Icon(
          Icons.Outlined.AccountCircle,
          contentDescription = "Avatar Placeholder",
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Column {
          Text(
            profile.displayName,
            style = MaterialTheme.typography.caption,
            fontWeight = FontWeight.Bold,
          )
          Text(profile.fullName)
        }
      }
      Divider(Modifier.padding(horizontal = 8.dp))
      SettingsSectionView({ ModalManager.shared.showModal { UserAddressView(chatModel) } }) {
        Icon(
          Icons.Outlined.QrCode,
          contentDescription = "Address",
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text("Your SimpleX contact address")
      }
      Spacer(Modifier.height(24.dp))

      SettingsSectionView({ ModalManager.shared.showModal { HelpView(chatModel) } }) {
        Icon(
          Icons.Outlined.HelpOutline,
          contentDescription = "Chat help",
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text("How to use SimpleX Chat")
      }
      SettingsSectionView({ ModalManager.shared.showModal { MarkdownHelpView() } }) {
        Icon(
          Icons.Outlined.TextFormat,
          contentDescription = "Markdown help",
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text("Markdown in messages")
      }
      Divider(Modifier.padding(horizontal = 8.dp))
      SettingsSectionView({ uriHandler.openUri(simplexTeamUri) }) {
        Icon(
          Icons.Outlined.Tag,
          contentDescription = "SimpleX Team",
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text(
          "Get help & advice via chat",
          color = MaterialTheme.colors.primary
        )
      }
      Divider(Modifier.padding(horizontal = 8.dp))
      SettingsSectionView({ uriHandler.openUri("mailto:chat@simplex.chat") }) {
        Icon(
          Icons.Outlined.Email,
          contentDescription = "Email",
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text(
          "Ask questions via email",
          color = MaterialTheme.colors.primary
        )
      }
      Spacer(Modifier.height(24.dp))

      SettingsSectionView({ ModalManager.shared.showCustomModal { close -> TerminalView(chatModel, close) } }) {
        Icon(
          painter = painterResource(id = R.drawable.ic_outline_terminal),
          contentDescription = "Chat console",
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text("Chat console")
      }
      Divider(Modifier.padding(horizontal = 8.dp))
      SettingsSectionView({ uriHandler.openUri("https://github.com/simplex-chat/simplex-chat") }) {
        Icon(
          painter = painterResource(id = R.drawable.ic_github),
          contentDescription = "GitHub",
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text(
          buildAnnotatedString {
            append("Install ")
            withStyle(SpanStyle(color = MaterialTheme.colors.primary)) {
              append("SimpleX Chat for terminal")
            }
          }
        )
      }
    }
  }
}

@Composable
fun SettingsSectionView(func: () -> Unit, height: Dp = 48.dp, content: (@Composable () -> Unit)) {
  Row(
    Modifier
      .padding(start = 8.dp)
      .fillMaxWidth()
      .clickable(onClick = func)
      .height(height),
    verticalAlignment = Alignment.CenterVertically
  ) {
    content.invoke()
  }
}

//@Preview(showBackground = true)
//@Preview(
//  uiMode = Configuration.UI_MODE_NIGHT_YES,
//  showBackground = true,
//  name = "Dark Mode"
//)
//@Composable
//fun PreviewSettingsLayout() {
//  SimpleXTheme {
//    SettingsLayout(
//      profile = Profile.sampleData,
//      navigate = {}
//    )
//  }
//}
