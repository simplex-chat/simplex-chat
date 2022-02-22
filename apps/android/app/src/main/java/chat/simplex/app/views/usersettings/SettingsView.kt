package chat.simplex.app.views.usersettings

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
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import chat.simplex.app.Pages
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.Profile
import chat.simplex.app.ui.theme.SimpleXTheme

@Composable
fun SettingsView(chatModel: ChatModel, nav: NavController) {
  val user = chatModel.currentUser.value
  if (user != null) {
    SettingsLayout(
      profile = user.profile,
      navigate = nav::navigate
    )
  }
}

val simplexTeamUri =
  "simplex:/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D"

@Composable
fun SettingsLayout(
  profile: Profile,
  navigate: (String) -> Unit
) {
  val uriHandler = LocalUriHandler.current
  Column(
    Modifier
      .fillMaxWidth()
      .padding(8.dp)
  ) {
    Text(
      "Your Settings",
      style = MaterialTheme.typography.h1,
    )
    Spacer(Modifier.height(24.dp))

    SettingsSectionView(
      content = {
        Icon(
          Icons.Outlined.AccountCircle,
          contentDescription = "Avatar Placeholder",
          tint = MaterialTheme.colors.onSurface,
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text(profile.displayName, fontWeight = FontWeight.Bold)
      },
      func = { navigate(Pages.UserProfile.route) }
    )
    Divider(Modifier.padding(horizontal = 8.dp))
    SettingsSectionView(
      content = {
        Icon(
          Icons.Outlined.QrCode,
          contentDescription = "Address",
          tint = MaterialTheme.colors.onSurface,
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text("Your SimpleX contact address")
      },
      func = { println("navigate to address") }
    )
    Spacer(Modifier.height(24.dp))

    SettingsSectionView(
      content = {
        Icon(
          Icons.Outlined.HelpOutline,
          contentDescription = "Help",
          tint = MaterialTheme.colors.onSurface,
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text("How to use SimpleX Chat")
      },
      func = { println("navigate to help") }
    )
    Divider(Modifier.padding(horizontal = 8.dp))
    SettingsSectionView(
      content = {
        Icon(
          Icons.Outlined.Tag,
          contentDescription = "SimpleX Team",
          tint = MaterialTheme.colors.onSurface,
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text(
          "Get help & advice via chat",
          color = MaterialTheme.colors.primary
        )
      },
      func = { uriHandler.openUri(simplexTeamUri) }
    )
    Divider(Modifier.padding(horizontal = 8.dp))
    SettingsSectionView(
      content = {
        Icon(
          Icons.Outlined.Email,
          contentDescription = "Email",
          tint = MaterialTheme.colors.onSurface,
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text(
          "Ask questions via email",
          color = MaterialTheme.colors.primary
        )
      },
      func = { uriHandler.openUri("mailto:chat@simplex.chat") }
    )
    Spacer(Modifier.height(24.dp))

    SettingsSectionView(
      content = {
        Icon(
          painter = painterResource(id = R.drawable.ic_outline_terminal),
          contentDescription = "Chat console",
          tint = MaterialTheme.colors.onSurface,
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text("Chat console")
      },
      func = { navigate(Pages.Terminal.route) }
    )
    Divider(Modifier.padding(horizontal = 8.dp))
    SettingsSectionView(
      content = {
        Icon(
          painter = painterResource(id = R.drawable.ic_github),
          contentDescription = "GitHub",
          tint = MaterialTheme.colors.onSurface,
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text("Install ")
        Text(
          "SimpleX Chat for terminal",
          color = MaterialTheme.colors.primary
        )
      },
      func = { uriHandler.openUri("https://github.com/simplex-chat/simplex-chat") }
    )
  }
}

@Composable
fun SettingsSectionView(content: (@Composable () -> Unit), func: () -> Unit) {
  Surface(
    modifier = Modifier
      .fillMaxWidth()
      .clickable(onClick = func)
      .height(48.dp),
  ) {
    Row(
      Modifier.padding(start = 8.dp),
      verticalAlignment = Alignment.CenterVertically
    ) {
      content.invoke()
    }
  }
}

@Preview(showBackground = true)
@Composable
fun PreviewSettingsLayout() {
  SimpleXTheme {
    SettingsLayout(
      profile = Profile.sampleData,
      navigate = {}
    )
  }
}
