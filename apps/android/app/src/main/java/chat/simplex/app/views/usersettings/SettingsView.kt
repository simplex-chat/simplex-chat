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
import androidx.compose.ui.res.stringResource
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
import chat.simplex.app.views.call.VideoCallView
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.onboarding.SimpleXInfo

@Composable
fun SettingsView(chatModel: ChatModel) {
  val user = chatModel.currentUser.value
  if (user != null) {
    SettingsLayout(
      profile = user.profile,
      runServiceInBackground = chatModel.runServiceInBackground,
      setRunServiceInBackground = { on ->
        chatModel.controller.setRunServiceInBackground(on)
        if (on && !chatModel.controller.isIgnoringBatteryOptimizations(chatModel.controller.appContext)) {
          chatModel.controller.setBackgroundServiceNoticeShown(false)
        }
        chatModel.controller.showBackgroundServiceNoticeIfNeeded()
        chatModel.runServiceInBackground.value = on
      },
      showModal = { modalView -> { ModalManager.shared.showModal { modalView(chatModel) } } },
      showCustomModal = { modalView -> { ModalManager.shared.showCustomModal { close -> modalView(chatModel, close) } } },
      showTerminal = { ModalManager.shared.showCustomModal { close -> TerminalView(chatModel, close) } },
      showVideoChatPrototype = { ModalManager.shared.showCustomModal { close -> VideoCallView(close) } },
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
  showTerminal: () -> Unit,
  showVideoChatPrototype: () -> Unit
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
        stringResource(R.string.your_settings),
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
          contentDescription = stringResource(R.string.icon_descr_address),
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text(stringResource(R.string.your_simplex_contact_address))
      }
      Spacer(Modifier.height(24.dp))

      SettingsSectionView(showModal { HelpView(it) }) {
        Icon(
          Icons.Outlined.HelpOutline,
          contentDescription = stringResource(R.string.icon_descr_help),
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text(stringResource(R.string.how_to_use_simplex_chat))
      }
      Divider(Modifier.padding(horizontal = 8.dp))
      SettingsSectionView(showModal { SimpleXInfo(it, onboarding = false) }) {
        Icon(
          Icons.Outlined.Info,
          contentDescription = stringResource(R.string.icon_descr_help),
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text(stringResource(R.string.about_simplex_chat))
      }
      Divider(Modifier.padding(horizontal = 8.dp))
      SettingsSectionView(showModal { MarkdownHelpView() }) {
        Icon(
          Icons.Outlined.TextFormat,
          contentDescription = stringResource(R.string.markdown_help),
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text(stringResource(R.string.markdown_in_messages))
      }
      Divider(Modifier.padding(horizontal = 8.dp))
      SettingsSectionView({ uriHandler.openUri(simplexTeamUri) }) {
        Icon(
          Icons.Outlined.Tag,
          contentDescription = stringResource(R.string.icon_descr_simplex_team),
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text(
          stringResource(R.string.chat_with_the_founder),
          color = MaterialTheme.colors.primary
        )
      }
      Divider(Modifier.padding(horizontal = 8.dp))
      SettingsSectionView({ uriHandler.openUri("mailto:chat@simplex.chat") }) {
        Icon(
          Icons.Outlined.Email,
          contentDescription = stringResource(R.string.icon_descr_email),
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text(
          stringResource(R.string.send_us_an_email),
          color = MaterialTheme.colors.primary
        )
      }
      Spacer(Modifier.height(24.dp))

      SettingsSectionView(showModal { SMPServersView(it) }) {
        Icon(
          Icons.Outlined.Dns,
          contentDescription = stringResource(R.string.smp_servers),
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text(stringResource(R.string.smp_servers))
      }
      Divider(Modifier.padding(horizontal = 8.dp))
      SettingsSectionView() {
        Icon(
          Icons.Outlined.Bolt,
          contentDescription = stringResource(R.string.private_notifications),
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text(
          stringResource(R.string.private_notifications), Modifier
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
          contentDescription = stringResource(R.string.chat_console),
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text(stringResource(R.string.chat_console))
      }
      Divider(Modifier.padding(horizontal = 8.dp))
      SettingsSectionView({ uriHandler.openUri("https://github.com/simplex-chat/simplex-chat") }) {
        Icon(
          painter = painterResource(id = R.drawable.ic_github),
          contentDescription = "GitHub",
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text(annotatedStringResource(R.string.install_simplex_chat_for_terminal))
      }
      Divider(Modifier.padding(horizontal = 8.dp))
      SettingsSectionView(showVideoChatPrototype) {
//      SettingsSectionView() {
        Text("v${BuildConfig.VERSION_NAME} (${BuildConfig.VERSION_CODE})")
      }
    }
  }
}

@Composable
fun SettingsSectionView(click: (() -> Unit)? = null, height: Dp = 46.dp, content: (@Composable () -> Unit)) {
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
      showTerminal = {},
      showVideoChatPrototype = {}
    )
  }
}
