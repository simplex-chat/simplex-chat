package chat.simplex.app.views.usersettings

import android.content.res.Configuration
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import chat.simplex.app.BuildConfig
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.Profile
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.TerminalView
import chat.simplex.app.views.helpers.ProfileImage
import chat.simplex.app.views.newchat.ModalManager

@Composable
fun SettingsView(chatModel: ChatModel) {
  val user = chatModel.currentUser.value
  if (user != null) {
    SettingsLayout(
      profile = user.profile,
      runServiceInBackground = chatModel.runServiceInBackground,
      setRunServiceInBackground = { on ->
        chatModel.controller.setRunServiceInBackground(on)
        chatModel.runServiceInBackground.value = on
      },
      showModal = { modalView -> { ModalManager.shared.showModal { modalView(chatModel) } } },
      showCustomModal = { modalView -> { ModalManager.shared.showCustomModal { close -> modalView(chatModel, close) } } },
      showTerminal = { ModalManager.shared.showCustomModal { close -> TerminalView(chatModel, close) } }
    )
  }
}

val simplexTeamUri =
  "simplex:/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D"

@Composable
fun SettingsLayout(
  profile: Profile,
  runServiceInBackground: MutableState<Boolean>,
  setRunServiceInBackground: (Boolean) -> Unit,
  showModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showCustomModal: (@Composable (ChatModel, () -> Unit) -> Unit) -> (() -> Unit),
  showTerminal: () -> Unit
) {
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
        modifier = Modifier.padding(start = 8.dp)
      )
      Spacer(Modifier.height(30.dp))

      SettingsSectionView(showCustomModal { chatModel, close -> UserProfileView(chatModel, close) }, 80.dp) {
        ProfileImage(size = 60.dp, profile.image)
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
      SettingsSectionView(showModal { UserAddressView(it) }) {
        Icon(
          Icons.Outlined.QrCode,
          contentDescription = "Address",
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text("Your SimpleX contact address")
      }
      Spacer(Modifier.height(24.dp))

      SettingsSectionView(showModal { HelpView(it) }) {
        Icon(
          Icons.Outlined.HelpOutline,
          contentDescription = "Chat help",
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text("How to use SimpleX Chat")
      }
      Divider(Modifier.padding(horizontal = 8.dp))
      SettingsSectionView(showModal { MarkdownHelpView() }) {
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
          "Chat with the founder",
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
          "Send us email",
          color = MaterialTheme.colors.primary
        )
      }
      Spacer(Modifier.height(24.dp))

      SettingsSectionView(showModal { SMPServersView(it) }) {
        Icon(
          Icons.Outlined.Dns,
          contentDescription = "SMP servers",
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text("SMP servers")
      }
      SettingsSectionView() {
        Icon(
          Icons.Outlined.Bolt,
          contentDescription = "Instant notifications",
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text("Instant notifications", Modifier
          .padding(end = 24.dp)
          .fillMaxWidth()
          .weight(1F))
        Switch(
          checked = runServiceInBackground.value,
          onCheckedChange = { setRunServiceInBackground(it) },
          colors = SwitchDefaults.colors(
            checkedThumbColor = MaterialTheme.colors.primary,
            uncheckedThumbColor = HighOrLowlight
          ),
          modifier = Modifier.padding(end = 8.dp)
        )
      }
      Divider(Modifier.padding(horizontal = 8.dp))
      SettingsSectionView(showTerminal) {
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
      Divider(Modifier.padding(horizontal = 8.dp))
      SettingsSectionView() {
        Text("v${BuildConfig.VERSION_NAME} (${BuildConfig.VERSION_CODE})")
      }
    }
  }
}

@Composable
fun SettingsSectionView(click: (() -> Unit)? = null, height: Dp = 48.dp, content: (@Composable () -> Unit)) {
  val modifier = Modifier
    .padding(start = 8.dp)
    .fillMaxWidth()
    .height(height)
  Row(
    if (click == null) modifier else modifier.clickable(onClick = click),
    verticalAlignment = Alignment.CenterVertically
  ) {
    content()
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewSettingsLayout() {
  SimpleXTheme {
    SettingsLayout(
      profile = Profile.sampleData,
      runServiceInBackground = remember { mutableStateOf(true) },
      setRunServiceInBackground = {},
      showModal = {{}},
      showCustomModal = {{}},
      showTerminal = {}
    )
  }
}
