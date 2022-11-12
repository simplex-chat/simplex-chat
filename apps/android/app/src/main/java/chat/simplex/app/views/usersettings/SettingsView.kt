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
import androidx.compose.material.icons.filled.*
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
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.*
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.TerminalView
import chat.simplex.app.views.database.DatabaseView
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.newchat.CreateLinkTab
import chat.simplex.app.views.newchat.CreateLinkView
import chat.simplex.app.views.onboarding.SimpleXInfo

@Composable
fun SettingsView(chatModel: ChatModel, setPerformLA: (Boolean) -> Unit) {
  val user = chatModel.currentUser.value
  val stopped = chatModel.chatRunning.value == false

  MaintainIncognitoState(chatModel)

  if (user != null) {
    SettingsLayout(
      profile = user.profile,
      stopped,
      chatModel.chatDbEncrypted.value == true,
      chatModel.incognito,
      chatModel.controller.appPrefs.incognito,
      developerTools = chatModel.controller.appPrefs.developerTools,
      user.displayName,
      setPerformLA = setPerformLA,
      showModal = { modalView -> { ModalManager.shared.showModal { modalView(chatModel) } } },
      showSettingsModal = { modalView -> { ModalManager.shared.showModal(true) { modalView(chatModel) } } },
      showCustomModal = { modalView -> { ModalManager.shared.showCustomModal { close -> modalView(chatModel, close) } } },
      showTerminal = { ModalManager.shared.showCustomModal { close -> TerminalView(chatModel, close) } },
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
//      background = if (isInDarkTheme()) MaterialTheme.colors.background else SettingsBackgroundLight) {
//      modalView(chatModel)
//    }
//  }
//}

@Composable
fun SettingsLayout(
  profile: LocalProfile,
  stopped: Boolean,
  encrypted: Boolean,
  incognito: MutableState<Boolean>,
  incognitoPref: Preference<Boolean>,
  developerTools: Preference<Boolean>,
  userDisplayName: String,
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
        .background(if (isInDarkTheme()) MaterialTheme.colors.background else SettingsBackgroundLight)
        .padding(top = DEFAULT_PADDING)
    ) {
      Text(
        stringResource(R.string.your_settings),
        style = MaterialTheme.typography.h1,
        modifier = Modifier.padding(horizontal = DEFAULT_PADDING),
        overflow = TextOverflow.Ellipsis,
      )

      Spacer(Modifier.height(30.dp))

      SectionView(stringResource(R.string.settings_section_title_you)) {
        SectionItemView(showCustomModal { chatModel, close -> UserProfileView(chatModel, close) }, 80.dp, disabled = stopped) {
          ProfilePreview(profile, stopped = stopped)
        }
        SectionDivider()
        SettingsIncognitoActionItem(incognitoPref, incognito, stopped) { showModal { IncognitoView() }() }
        SectionDivider()
        SettingsActionItem(Icons.Outlined.QrCode, stringResource(R.string.your_simplex_contact_address), showModal { CreateLinkView(it, CreateLinkTab.LONG_TERM) }, disabled = stopped)
        SectionDivider()
        DatabaseItem(encrypted, showSettingsModal { DatabaseView(it, showSettingsModal) }, stopped)
      }
      SectionSpacer()

      SectionView(stringResource(R.string.settings_section_title_settings)) {
        SettingsActionItem(Icons.Outlined.Bolt, stringResource(R.string.notifications), showSettingsModal { NotificationsSettingsView(it) }, disabled = stopped)
        SectionDivider()
        SettingsActionItem(Icons.Outlined.Videocam, stringResource(R.string.settings_audio_video_calls), showSettingsModal { CallSettingsView(it, showModal) }, disabled = stopped)
        SectionDivider()
        SettingsActionItem(Icons.Outlined.Lock, stringResource(R.string.privacy_and_security), showSettingsModal { PrivacySettingsView(it, setPerformLA) }, disabled = stopped)
        SectionDivider()
        SettingsActionItem(Icons.Outlined.LightMode, stringResource(R.string.appearance_settings), showSettingsModal { AppearanceView() }, disabled = stopped)
        SectionDivider()
        SettingsActionItem(Icons.Outlined.WifiTethering, stringResource(R.string.network_and_servers), showSettingsModal { NetworkAndServersView(it, showModal, showSettingsModal) }, disabled = stopped)
      }
      SectionSpacer()

      SectionView(stringResource(R.string.settings_section_title_help)) {
        SettingsActionItem(Icons.Outlined.HelpOutline, stringResource(R.string.how_to_use_simplex_chat), showModal { HelpView(userDisplayName) }, disabled = stopped)
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

      SectionView(stringResource(R.string.settings_section_title_support)) {
        ContributeItem(uriHandler)
        SectionDivider()
        RateAppItem(uriHandler)
        SectionDivider()
        StarOnGithubItem(uriHandler)
      }
      SectionSpacer()

      SectionView(stringResource(R.string.settings_section_title_develop)) {
        val devTools = remember { mutableStateOf(developerTools.get()) }
        SettingsPreferenceItem(Icons.Outlined.Construction, stringResource(R.string.settings_developer_tools), developerTools, devTools)
        SectionDivider()
        if (devTools.value) {
          ChatConsoleItem(showTerminal)
          SectionDivider()
          InstallTerminalAppItem(uriHandler)
          SectionDivider()
        }
//        SettingsActionItem(Icons.Outlined.Science, stringResource(R.string.settings_experimental_features), showSettingsModal { ExperimentalFeaturesView(it, enableCalls) })
//        SectionDivider()
        AppVersionItem()
      }
    }
  }
}

@Composable
fun SettingsIncognitoActionItem(
  incognitoPref: Preference<Boolean>,
  incognito: MutableState<Boolean>,
  stopped: Boolean,
  onClickInfo: () -> Unit,
) {
  SettingsPreferenceItemWithInfo(
    if (incognito.value) Icons.Filled.TheaterComedy else Icons.Outlined.TheaterComedy,
    if (incognito.value) Indigo else HighOrLowlight,
    stringResource(R.string.incognito),
    stopped,
    onClickInfo,
    incognitoPref,
    incognito
  )
}

@Composable
fun MaintainIncognitoState(chatModel: ChatModel) {
  // Cache previous value and once it changes in background, update it via API
  var cachedIncognito by remember { mutableStateOf(chatModel.incognito.value) }
  LaunchedEffect(chatModel.incognito.value) {
    // Don't do anything if nothing changed
    if (cachedIncognito == chatModel.incognito.value) return@LaunchedEffect
    try {
      chatModel.controller.apiSetIncognito(chatModel.incognito.value)
    } catch (e: Exception) {
      // Rollback the state
      chatModel.controller.appPrefs.incognito.set(cachedIncognito)
      // Crash the app
      throw e
    }
    cachedIncognito = chatModel.incognito.value
  }
}

@Composable private fun DatabaseItem(encrypted: Boolean, openDatabaseView: () -> Unit, stopped: Boolean) {
  SectionItemView(openDatabaseView) {
    Row(
      Modifier.fillMaxWidth(),
      horizontalArrangement = Arrangement.SpaceBetween
    ) {
      Row {
        Icon(
          Icons.Outlined.FolderOpen,
          contentDescription = stringResource(R.string.database_passphrase_and_export),
          tint = if (encrypted) HighOrLowlight else WarningOrange,
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text(stringResource(R.string.database_passphrase_and_export))
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

@Composable private fun ContributeItem(uriHandler: UriHandler) {
  SectionItemView({ uriHandler.openUri("https://github.com/simplex-chat/simplex-chat#contribute") }) {
    Icon(
      Icons.Outlined.Keyboard,
      contentDescription = "GitHub",
      tint = HighOrLowlight,
    )
    Spacer(Modifier.padding(horizontal = 4.dp))
    Text(generalGetString(R.string.contribute), color = MaterialTheme.colors.primary)
  }
}

@Composable private fun RateAppItem(uriHandler: UriHandler) {
  SectionItemView({
    runCatching { uriHandler.openUri("market://details?id=chat.simplex.app") }
      .onFailure { uriHandler.openUri("https://play.google.com/store/apps/details?id=chat.simplex.app") }
  }
  ) {
    Icon(
      Icons.Outlined.StarOutline,
      contentDescription = "Google Play",
      tint = HighOrLowlight,
    )
    Spacer(Modifier.padding(horizontal = 4.dp))
    Text(generalGetString(R.string.rate_the_app), color = MaterialTheme.colors.primary)
  }
}

@Composable private fun StarOnGithubItem(uriHandler: UriHandler) {
  SectionItemView({ uriHandler.openUri("https://github.com/simplex-chat/simplex-chat") }) {
    Icon(
      painter = painterResource(id = R.drawable.ic_github),
      contentDescription = "GitHub",
      tint = HighOrLowlight,
    )
    Spacer(Modifier.padding(horizontal = 4.dp))
    Text(generalGetString(R.string.star_on_github), color = MaterialTheme.colors.primary)
  }
}

@Composable private fun ChatConsoleItem(showTerminal: () -> Unit) {
  SectionItemView(showTerminal) {
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
  SectionItemView({ uriHandler.openUri("https://github.com/simplex-chat/simplex-chat") }) {
    Icon(
      painter = painterResource(id = R.drawable.ic_github),
      contentDescription = "GitHub",
      tint = HighOrLowlight,
    )
    Spacer(Modifier.padding(horizontal = 4.dp))
    Text(generalGetString(R.string.install_simplex_chat_for_terminal), color = MaterialTheme.colors.primary)
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
      color = if (stopped) HighOrLowlight else Color.Unspecified,
      maxLines = 1,
      overflow = TextOverflow.Ellipsis
    )
    Text(
      profileOf.fullName,
      color = if (stopped) HighOrLowlight else Color.Unspecified,
      maxLines = 1,
      overflow = TextOverflow.Ellipsis
    )
  }
}

@Composable
fun SettingsActionItem(icon: ImageVector, text: String, click: (() -> Unit)? = null, textColor: Color = Color.Unspecified, iconColor: Color = HighOrLowlight, disabled: Boolean = false) {
  SectionItemView(click, disabled = disabled) {
    Icon(icon, text, tint = if (disabled) HighOrLowlight else iconColor)
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

@Composable
fun SettingsPreferenceItemWithInfo(
  icon: ImageVector,
  iconTint: Color,
  text: String,
  stopped: Boolean,
  onClickInfo: () -> Unit,
  pref: Preference<Boolean>,
  prefState: MutableState<Boolean>? = null
) {
  SectionItemView(onClickInfo) {
    Row(verticalAlignment = Alignment.CenterVertically) {
      Icon(icon, text, tint = if (stopped) HighOrLowlight else iconTint)
      Spacer(Modifier.padding(horizontal = 4.dp))
      SharedPreferenceToggleWithIcon(text, Icons.Outlined.Info, stopped, onClickInfo, pref, prefState)
    }
  }
}

@Composable
fun PreferenceToggleWithIcon(
  text: String,
  icon: ImageVector,
  iconColor: Color = HighOrLowlight,
  checked: Boolean,
  onChange: (Boolean) -> Unit = {},
) {
  Row(Modifier.fillMaxWidth(), verticalAlignment = Alignment.CenterVertically) {
    Icon(
      icon,
      null,
      tint = iconColor
    )
    Spacer(Modifier.padding(horizontal = 4.dp))
    Text(text)
    Spacer(Modifier.fillMaxWidth().weight(1f))
    Switch(
      checked = checked,
      onCheckedChange = {
        onChange(it)
      },
      colors = SwitchDefaults.colors(
        checkedThumbColor = MaterialTheme.colors.primary,
        uncheckedThumbColor = HighOrLowlight
      )
    )
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
      profile = LocalProfile.sampleData,
      stopped = false,
      encrypted = false,
      incognito = remember { mutableStateOf(false) },
      incognitoPref = Preference({ false }, {}),
      developerTools = Preference({ false }, {}),
      userDisplayName = "Alice",
      setPerformLA = {},
      showModal = { {} },
      showSettingsModal = { {} },
      showCustomModal = { {}},
      showTerminal = {},
//      showVideoChatPrototype = {}
    )
  }
}
