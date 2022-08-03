package chat.simplex.app.views.usersettings

import SectionDivider
import SectionItemView
import SectionSpacer
import SectionView
import android.content.res.Configuration
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Report
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
import chat.simplex.app.views.database.DatabaseView
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.onboarding.SimpleXInfo

@Composable
fun SettingsView(chatModel: ChatModel, setPerformLA: (Boolean) -> Unit) {
  val user = chatModel.currentUser.value
  val stopped = chatModel.chatRunning.value == false

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
      stopped,
      runServiceInBackground = chatModel.runServiceInBackground,
      developerTools = chatModel.controller.appPrefs.developerTools,
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
      showTerminal = { ModalManager.shared.showCustomModal { close -> TerminalView(chatModel, close) } },
      showNetworkSettings = {
        withApi {
          val cfg = chatModel.controller.getNetworkConfig()
          if (cfg != null) {
            ModalManager.shared.showCustomModal { close ->
              ModalView(close = close, modifier = Modifier,
                background = if (isSystemInDarkTheme()) MaterialTheme.colors.background else SettingsBackgroundLight) {
                NetworkSettingsView(chatModel, cfg)
              }
            }
          }
        }
      }
//      showVideoChatPrototype = { ModalManager.shared.showCustomModal { close -> CallViewDebug(close) } },
    )
  }
}

val simplexTeamUri =
  "simplex:/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D"

// TODO pass close
//fun showSectionedModal(chatModel: ChatModel, modalView: (@Composable (ChatModel) -> Unit)) {
//  ModalManager.shared.showCustomModal { close ->
//    ModalView(close = close, modifier = Modifier,
//      background = if (isSystemInDarkTheme()) MaterialTheme.colors.background else SettingsBackgroundLight) {
//      modalView(chatModel)
//    }
//  }
//}

@Composable
fun SettingsLayout(
  profile: Profile,
  stopped: Boolean,
  runServiceInBackground: MutableState<Boolean>,
  developerTools: Preference<Boolean>,
  setRunServiceInBackground: (Boolean) -> Unit,
  setPerformLA: (Boolean) -> Unit,
  showModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showCustomModal: (@Composable (ChatModel, () -> Unit) -> Unit) -> (() -> Unit),
  showTerminal: () -> Unit,
  showNetworkSettings: () -> Unit
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
      Text(
        stringResource(R.string.your_settings),
        style = MaterialTheme.typography.h1,
        modifier = Modifier.padding(start = 16.dp)
      )
      SectionSpacer()

      SectionView(stringResource(R.string.settings_section_title_you)) {
        SectionItemView(showCustomModal { chatModel, close -> UserProfileView(chatModel, close) }, 80.dp, disabled = stopped) {
          ProfilePreview(profile, stopped = stopped)
        }
        SectionDivider()
        SettingsActionItem(Icons.Outlined.QrCode, stringResource(R.string.your_simplex_contact_address), showModal { UserAddressView(it) }, disabled = stopped)
        SectionDivider()
        DatabaseItem(showSettingsModal { DatabaseView(it, showSettingsModal) }, stopped)
      }
      SectionSpacer()

      SectionView(stringResource(R.string.settings_section_title_settings)) {
        SettingsActionItem(Icons.Outlined.Videocam, stringResource(R.string.settings_audio_video_calls), showSettingsModal { CallSettingsView(it) }, disabled = stopped)
        SectionDivider()
        SettingsActionItem(Icons.Outlined.Lock, stringResource(R.string.privacy_and_security), showSettingsModal { PrivacySettingsView(it, setPerformLA) }, disabled = stopped)
        SectionDivider()
        PrivateNotificationsItem(runServiceInBackground, setRunServiceInBackground, stopped)
        SectionDivider()
        SettingsActionItem(Icons.Outlined.Dns, stringResource(R.string.smp_servers), showModal { SMPServersView(it) }, disabled = stopped)
        SectionDivider()
        SettingsActionItem(Icons.Outlined.SettingsEthernet, stringResource(R.string.network_settings), showNetworkSettings, disabled = stopped)
      }
      SectionSpacer()

      SectionView(stringResource(R.string.settings_section_title_help)) {
        SettingsActionItem(Icons.Outlined.HelpOutline, stringResource(R.string.how_to_use_simplex_chat), showModal { HelpView(it) }, disabled = stopped)
        SectionDivider()
        SettingsActionItem(Icons.Outlined.Info, stringResource(R.string.about_simplex_chat), showModal { SimpleXInfo(it, onboarding = false) })
        SectionDivider()
        SettingsActionItem(Icons.Outlined.TextFormat, stringResource(R.string.markdown_in_messages), showModal { MarkdownHelpView() })
        SectionDivider()
        SettingsActionItem(Icons.Outlined.Tag, stringResource(R.string.chat_with_the_founder), { uriHandler.openUri(simplexTeamUri) }, textColor = MaterialTheme.colors.primary, disabled = stopped)
        SectionDivider()
        SettingsActionItem(Icons.Outlined.Email, stringResource(R.string.send_us_an_email), { uriHandler.openUri("mailto:chat@simplex.chat") }, textColor = MaterialTheme.colors.primary)
      }
      SectionSpacer()

      SectionView(stringResource(R.string.settings_section_title_develop)) {
        ChatConsoleItem(showTerminal, stopped)
        SectionDivider()
        SettingsPreferenceItem(Icons.Outlined.Construction, stringResource(R.string.settings_developer_tools), developerTools)
        SectionDivider()
        InstallTerminalAppItem(uriHandler)
        SectionDivider()
//        SettingsActionItem(Icons.Outlined.Science, stringResource(R.string.settings_experimental_features), showSettingsModal { ExperimentalFeaturesView(it, enableCalls) })
//        SectionDivider()
        AppVersionItem()
      }
    }
  }
}

@Composable private fun DatabaseItem(openDatabaseView: () -> Unit, stopped: Boolean) {
  SectionItemView(openDatabaseView) {
    Row(
      Modifier.fillMaxWidth(),
      horizontalArrangement = Arrangement.SpaceBetween
    ) {
      Row {
        Icon(
          Icons.Outlined.Archive,
          contentDescription = stringResource(R.string.database_export_and_import),
          tint = HighOrLowlight,
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text(stringResource(R.string.database_export_and_import))
      }
      if (stopped) {
        Icon(
          Icons.Filled.Report,
          contentDescription = stringResource(R.string.chat_is_stopped),
          tint = Color.Red,
          modifier = Modifier.padding(end = 6.dp)
        )
      }
    }
  }
}

@Composable private fun PrivateNotificationsItem(
  runServiceInBackground: MutableState<Boolean>,
  setRunServiceInBackground: (Boolean) -> Unit,
  stopped: Boolean
) {
  SectionItemView(disabled = stopped) {
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
          .weight(1f),
        color = if (stopped) HighOrLowlight else Color.Unspecified
      )
      Switch(
        checked = runServiceInBackground.value,
        onCheckedChange = { setRunServiceInBackground(it) },
        colors = SwitchDefaults.colors(
          checkedThumbColor = MaterialTheme.colors.primary,
          uncheckedThumbColor = HighOrLowlight
        ),
        modifier = Modifier.padding(end = 6.dp),
        enabled = !stopped
      )
    }
  }
}

@Composable fun ChatLockItem(performLA: MutableState<Boolean>, setPerformLA: (Boolean) -> Unit) {
  SectionItemView() {
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
        )
      )
    }
  }
}

@Composable private fun ChatConsoleItem(showTerminal: () -> Unit, stopped: Boolean) {
  SectionItemView(showTerminal, disabled = stopped) {
    Icon(
      painter = painterResource(id = R.drawable.ic_outline_terminal),
      contentDescription = stringResource(R.string.chat_console),
      tint = HighOrLowlight,
    )
    Spacer(Modifier.padding(horizontal = 4.dp))
    Text(
      stringResource(R.string.chat_console),
      color = if (stopped) HighOrLowlight else Color.Unspecified
    )
  }
}

@Composable private fun InstallTerminalAppItem(uriHandler: UriHandler) {
  SectionItemView({ uriHandler.openUri("https://github.com/simplex-chat/simplex-chat") }) {
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
  SectionItemView() {
    Text("v${BuildConfig.VERSION_NAME} (${BuildConfig.VERSION_CODE})")
  }
}

@Composable fun ProfilePreview(profileOf: NamedChat, size: Dp = 60.dp, color: Color = MaterialTheme.colors.secondary, stopped: Boolean = false) {
  ProfileImage(size = size, image = profileOf.image, color = color)
  Spacer(Modifier.padding(horizontal = 4.dp))
  Column {
    Text(
      profileOf.displayName,
      style = MaterialTheme.typography.caption,
      fontWeight = FontWeight.Bold,
      color = if (stopped) HighOrLowlight else Color.Unspecified
    )
    Text(
      profileOf.fullName,
      color = if (stopped) HighOrLowlight else Color.Unspecified
    )
  }
}

@Composable
fun SettingsActionItem(icon: ImageVector, text: String, click: (() -> Unit)? = null, textColor: Color = Color.Unspecified, disabled: Boolean = false) {
  SectionItemView(click, disabled = disabled) {
    Icon(icon, text, tint = HighOrLowlight)
    Spacer(Modifier.padding(horizontal = 4.dp))
    Text(text, color = if (disabled) HighOrLowlight else textColor)
  }
}

@Composable
fun SettingsPreferenceItem(icon: ImageVector, text: String, pref: Preference<Boolean>, prefState: MutableState<Boolean>? = null) {
  SectionItemView() {
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
      stopped = false,
      runServiceInBackground = remember { mutableStateOf(true) },
      developerTools = Preference({ false }, {}),
      setRunServiceInBackground = {},
      setPerformLA = {},
      showModal = { {} },
      showSettingsModal = { {} },
      showCustomModal = { {} },
      showTerminal = {},
      showNetworkSettings = {}
//      showVideoChatPrototype = {}
    )
  }
}
