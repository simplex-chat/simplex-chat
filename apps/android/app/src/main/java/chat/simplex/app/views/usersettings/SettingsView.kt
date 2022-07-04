package chat.simplex.app.views.usersettings

import android.content.res.Configuration
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.platform.UriHandler
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.*
import chat.simplex.app.BuildConfig
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.TerminalView
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.onboarding.SimpleXInfo

@Composable
fun SettingsView(chatModel: ChatModel, setPerformLA: (Boolean) -> Unit) {
  val user = chatModel.currentUser.value

  fun setRunServiceInBackground(on: Boolean) {
    chatModel.controller.appPrefs.runServiceInBackground.set(on)
    if (on && !chatModel.controller.isIgnoringBatteryOptimizations(chatModel.controller.appContext)) {
      chatModel.controller.appPrefs.backgroundServiceNoticeShown.set(false)
    }
    chatModel.controller.showBackgroundServiceNoticeIfNeeded()
    chatModel.runServiceInBackground.value = on
  }

  if (user != null) {
    SettingsLayout(
      profile = user.profile,
      runServiceInBackground = chatModel.runServiceInBackground,
      setRunServiceInBackground = ::setRunServiceInBackground,
      setPerformLA = setPerformLA,
      showModal = { modalView -> { ModalManager.shared.showModal { modalView(chatModel) } } },
      showSettingsModal = { modalView -> { ModalManager.shared.showCustomModal { close ->
        ModalView(close = close, modifier = Modifier,
          background = if (isSystemInDarkTheme()) MaterialTheme.colors.background else SettingsBackgroundLight) {
          modalView(chatModel)
        }
      } } },
      showCustomModal = { modalView -> { ModalManager.shared.showCustomModal { close -> modalView(chatModel, close) } } },
      showTerminal = { ModalManager.shared.showCustomModal { close -> TerminalView(chatModel, close) } }
//      showVideoChatPrototype = { ModalManager.shared.showCustomModal { close -> CallViewDebug(close) } },
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
  setPerformLA: (Boolean) -> Unit,
  showModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showCustomModal: (@Composable (ChatModel, () -> Unit) -> Unit) -> (() -> Unit),
  showTerminal: () -> Unit,
//  showVideoChatPrototype: () -> Unit
) {
  val uriHandler = LocalUriHandler.current
  Surface(Modifier.fillMaxSize().verticalScroll(rememberScrollState())) {
    Column(
      Modifier
        .fillMaxSize()
        .background(if (isSystemInDarkTheme()) MaterialTheme.colors.background else SettingsBackgroundLight)
        .padding(top = 16.dp)
    ) {
      @Composable fun divider() = Divider(Modifier.padding(horizontal = 8.dp))
      @Composable fun spacer() = Spacer(Modifier.height(30.dp))
      Text(
        stringResource(R.string.your_settings),
        style = MaterialTheme.typography.h1,
        modifier = Modifier.padding(start = 16.dp)
      )
      Spacer(Modifier.height(30.dp))

      SettingsSectionView(stringResource(R.string.settings_section_title_you)) {
        SettingsItemView(showCustomModal { chatModel, close -> UserProfileView(chatModel, close) }, 80.dp) {
          ProfilePreview(profile)
        }
        divider()
        SettingsActionItem(Icons.Outlined.QrCode, stringResource(R.string.your_simplex_contact_address), showModal { UserAddressView(it) })
      }
      spacer()

      SettingsSectionView(stringResource(R.string.settings_section_title_settings)) {
        SettingsActionItem(Icons.Outlined.Videocam, stringResource(R.string.settings_audio_video_calls), showSettingsModal { CallSettingsView(it) })
        divider()
        SettingsActionItem(Icons.Outlined.Lock, stringResource(R.string.privacy_and_security), showSettingsModal { PrivacySettingsView(it, setPerformLA) })
        divider()
        PrivateNotificationsItem(runServiceInBackground, setRunServiceInBackground)
        divider()
        SettingsActionItem(Icons.Outlined.Dns, stringResource(R.string.smp_servers), showModal { SMPServersView(it) })
      }
      spacer()

      SettingsSectionView(stringResource(R.string.settings_section_title_help)) {
        SettingsActionItem(Icons.Outlined.HelpOutline, stringResource(R.string.how_to_use_simplex_chat), showModal { HelpView(it) })
        divider()
        SettingsActionItem(Icons.Outlined.Info, stringResource(R.string.about_simplex_chat), showModal { SimpleXInfo(it, onboarding = false) })
        divider()
        SettingsActionItem(Icons.Outlined.TextFormat, stringResource(R.string.markdown_in_messages), showModal { MarkdownHelpView() })
        divider()
        SettingsActionItem(Icons.Outlined.Tag, stringResource(R.string.chat_with_the_founder), { uriHandler.openUri(simplexTeamUri) }, textColor = MaterialTheme.colors.primary)
        divider()
        SettingsActionItem(Icons.Outlined.Email, stringResource(R.string.send_us_an_email), { uriHandler.openUri("mailto:chat@simplex.chat") }, textColor = MaterialTheme.colors.primary)
      }
      spacer()

      SettingsSectionView(stringResource(R.string.settings_section_title_develop)) {
        ChatConsoleItem(showTerminal)
        divider()
        InstallTerminalAppItem(uriHandler)
        divider()
//        SettingsActionItem(Icons.Outlined.Science, stringResource(R.string.settings_experimental_features), showSettingsModal { ExperimentalFeaturesView(it, enableCalls) })
//        divider()
        AppVersionItem()
      }
    }
  }
}

@Composable fun SettingsSectionView(title: String, content: (@Composable () -> Unit)) {
  Column {
    Text(title, color = HighOrLowlight, style = MaterialTheme.typography.body2,
      modifier = Modifier.padding(start = 16.dp, bottom = 5.dp), fontSize = 12.sp)
    Surface(color = if (isSystemInDarkTheme()) GroupDark else MaterialTheme.colors.background) {
      Column(Modifier.padding(horizontal = 6.dp)) { content() }
    }
  }
}

@Composable private fun PrivateNotificationsItem(
  runServiceInBackground: MutableState<Boolean>,
  setRunServiceInBackground: (Boolean) -> Unit
) {
  SettingsItemView() {
    Row(verticalAlignment = Alignment.CenterVertically) {
      Icon(
        Icons.Outlined.Bolt,
        contentDescription = stringResource(R.string.private_notifications),
        tint = HighOrLowlight,
      )
      Spacer(Modifier.padding(horizontal = 4.dp))
      Text(
        stringResource(R.string.private_notifications),
        Modifier
          .padding(end = 24.dp)
          .fillMaxWidth()
          .weight(1f)
      )
      Switch(
        checked = runServiceInBackground.value,
        onCheckedChange = { setRunServiceInBackground(it) },
        colors = SwitchDefaults.colors(
          checkedThumbColor = MaterialTheme.colors.primary,
          uncheckedThumbColor = HighOrLowlight
        ),
        modifier = Modifier.padding(end = 6.dp)
      )
    }
  }
}

@Composable fun ChatLockItem(performLA: MutableState<Boolean>, setPerformLA: (Boolean) -> Unit) {
  SettingsItemView() {
    Row(verticalAlignment = Alignment.CenterVertically) {
      Icon(
        Icons.Outlined.Lock,
        contentDescription = stringResource(R.string.chat_lock),
        tint = HighOrLowlight,
      )
      Spacer(Modifier.padding(horizontal = 4.dp))
      Text(
        stringResource(R.string.chat_lock), Modifier
          .padding(end = 24.dp)
          .fillMaxWidth()
          .weight(1F)
      )
      Switch(
        checked = performLA.value,
        onCheckedChange = { setPerformLA(it) },
        colors = SwitchDefaults.colors(
          checkedThumbColor = MaterialTheme.colors.primary,
          uncheckedThumbColor = HighOrLowlight
        ),
        modifier = Modifier.padding(end = 6.dp)
      )
    }
  }
}

@Composable private fun ChatConsoleItem(showTerminal: () -> Unit) {
  SettingsItemView(showTerminal) {
    Icon(
      painter = painterResource(id = R.drawable.ic_outline_terminal),
      contentDescription = stringResource(R.string.chat_console),
      tint = HighOrLowlight,
    )
    Spacer(Modifier.padding(horizontal = 4.dp))
    Text(stringResource(R.string.chat_console))
  }
}

@Composable private fun InstallTerminalAppItem(uriHandler: UriHandler) {
  SettingsItemView({ uriHandler.openUri("https://github.com/simplex-chat/simplex-chat") }) {
    Icon(
      painter = painterResource(id = R.drawable.ic_github),
      contentDescription = "GitHub",
      tint = HighOrLowlight,
    )
    Spacer(Modifier.padding(horizontal = 4.dp))
    Text(annotatedStringResource(R.string.install_simplex_chat_for_terminal))
  }
}

@Composable private fun AppVersionItem() {
  SettingsItemView() {
    Text("v${BuildConfig.VERSION_NAME} (${BuildConfig.VERSION_CODE})")
  }
}

@Composable fun ProfilePreview(profileOf: NamedChat, size: Dp = 60.dp, color: Color = MaterialTheme.colors.secondary) {
  ProfileImage(size = size, image = profileOf.image, color = color)
  Spacer(Modifier.padding(horizontal = 4.dp))
  Column {
    Text(
      profileOf.displayName,
      style = MaterialTheme.typography.caption,
      fontWeight = FontWeight.Bold,
    )
    Text(profileOf.fullName)
  }
}

@Composable
fun SettingsItemView(click: (() -> Unit)? = null, height: Dp = 46.dp, content: (@Composable () -> Unit)) {
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

@Composable
fun SettingsActionItem(icon: ImageVector, text: String, click: (() -> Unit)? = null, textColor: Color = Color.Unspecified) {
  SettingsItemView(click) {
    Icon(icon, text, tint = HighOrLowlight)
    Spacer(Modifier.padding(horizontal = 4.dp))
    Text(text, color = textColor)
  }
}

@Composable
fun SettingsPreferenceItem(icon: ImageVector, text: String, pref: Preference<Boolean>, prefState: MutableState<Boolean>? = null) {
  SettingsItemView() {
    Row(verticalAlignment = Alignment.CenterVertically) {
      Icon(icon, text, tint = HighOrLowlight)
      Spacer(Modifier.padding(horizontal = 4.dp))
      SharedPreferenceToggle(text, pref, prefState)
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
fun PreviewSettingsLayout() {
  SimpleXTheme {
    SettingsLayout(
      profile = Profile.sampleData,
      runServiceInBackground = remember { mutableStateOf(true) },
      setRunServiceInBackground = {},
      setPerformLA = {},
      showModal = { {} },
      showSettingsModal = { {} },
      showCustomModal = { {} },
      showTerminal = {},
//      showVideoChatPrototype = {}
    )
  }
}
