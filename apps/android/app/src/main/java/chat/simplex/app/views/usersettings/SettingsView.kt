package chat.simplex.app.views.usersettings

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import chat.simplex.app.Pages
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.Profile
import chat.simplex.app.ui.theme.SimpleXTheme

@Composable
fun SettingsView(chatModel: ChatModel, nav: NavController) {
  val user = chatModel.currentUser.value
  if (user != null) {
    SettingsLayout(
      profile = user.profile,
      back = nav::popBackStack,
      navigate = nav::navigate
    )
  }
}

val simplexTeamUri = "simplex:/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D"

@Composable
fun SettingsLayout(
  profile: Profile,
  back: () -> Unit,
  navigate: (String) -> Unit
) {
  val uriHandler = LocalUriHandler.current
  Column() {
    Button(onClick = back) {
      Text("Back")
    }
    Text("Your Settings")
    Spacer(Modifier.height(4.dp))

    Text("YOU", style = MaterialTheme.typography.h4)
    Button(onClick = { navigate(Pages.UserProfile.route) }) {
      Text(profile.displayName)
    }

    Text("HELP", style = MaterialTheme.typography.h4)
    Button(onClick = { println("navigate to help") }) {
      Text("How to use SimpleX Chat")
    }
    Button(onClick = { uriHandler.openUri(simplexTeamUri) }) {
      Text("Get help & advice via chat")
    }
    Button(onClick = { uriHandler.openUri("mailto:chat@simplex.chat") }) {
      Text("Ask questions via email")
    }
    Spacer(Modifier.height(10.dp))

    Text("DEVELOP", style = MaterialTheme.typography.h4)
    Button(onClick = { navigate(Pages.Terminal.route) }) {
      Text("Chat console")
    }
    Button(onClick = { uriHandler.openUri("https://github.com/simplex-chat/simplex-chat") }) {
      Text("Install SimpleX for terminal")
    }
  }
}

@Preview(showBackground = true)
@Composable
fun PreviewSettingsLayout() {
  SimpleXTheme {
    SettingsLayout(
      profile = Profile.sampleData,
      back = {},
      navigate = {}
    )
  }
}
