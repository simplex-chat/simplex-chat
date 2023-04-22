package chat.simplex.app.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionItemViewWithIcon
import SectionView
import TextIconSpaced
import android.content.Context
import android.content.res.Configuration
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.platform.*
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.*
import androidx.fragment.app.FragmentActivity
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.database.DatabaseView
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.newchat.CreateLinkTab
import chat.simplex.app.views.newchat.CreateLinkView
import chat.simplex.app.views.onboarding.SimpleXInfo
import chat.simplex.app.views.onboarding.WhatsNewView

@Composable
fun SettingsView(chatModel: ChatModel, setPerformLA: (Boolean, FragmentActivity) -> Unit) {
  val user = chatModel.currentUser.value
  val stopped = chatModel.chatRunning.value == false

  MaintainIncognitoState(chatModel)

  if (user != null) {
    val requireAuth = remember { chatModel.controller.appPrefs.performLA.state }
    val context = LocalContext.current
    SettingsLayout(
      profile = user.profile,
      stopped,
      chatModel.chatDbEncrypted.value == true,
      chatModel.incognito,
      chatModel.controller.appPrefs.incognito,
      user.displayName,
      setPerformLA = setPerformLA,
      showModal = { modalView -> { ModalManager.shared.showModal { modalView(chatModel) } } },
      showSettingsModal = { modalView -> { ModalManager.shared.showModal(true) { modalView(chatModel) } } },
      showSettingsModalWithSearch = { modalView ->
        ModalManager.shared.showCustomModal { close ->
          val search = rememberSaveable { mutableStateOf("") }
          ModalView(
            { close() },
            endButtons = {
              SearchTextField(Modifier.fillMaxWidth(), stringResource(android.R.string.search_go), alwaysVisible = true) { search.value = it }
            },
            content = { modalView(chatModel, search) })
        }
      },
      showCustomModal = { modalView -> { ModalManager.shared.showCustomModal { close -> modalView(chatModel, close) } } },
      showVersion = {
        withApi {
          val info = chatModel.controller.apiGetVersion()
          if (info != null) {
            ModalManager.shared.showModal { VersionInfoView(info) }
          }
        }
      },
      withAuth = { block ->
        if (!requireAuth.value) {
          block()
        } else {
          ModalManager.shared.showModalCloseable { close ->
            val onFinishAuth = { success: Boolean ->
              if (success) {
                close()
                block()
              }
            }
            LaunchedEffect(Unit) {
              runAuth(context, onFinishAuth)
            }
            Box(
              Modifier.fillMaxSize(),
              contentAlignment = Alignment.Center
            ) {
              SimpleButton(
                stringResource(R.string.auth_unlock),
                icon = Icons.Outlined.Lock,
                click = {
                  runAuth(context, onFinishAuth)
                }
              )
            }
          }
        }
      },
    )
  }
}

val simplexTeamUri =
  "simplex:/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D"

@Composable
fun SettingsLayout(
  profile: LocalProfile,
  stopped: Boolean,
  encrypted: Boolean,
  incognito: MutableState<Boolean>,
  incognitoPref: SharedPreference<Boolean>,
  userDisplayName: String,
  setPerformLA: (Boolean, FragmentActivity) -> Unit,
  showModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showSettingsModalWithSearch: (@Composable (ChatModel, MutableState<String>) -> Unit) -> Unit,
  showCustomModal: (@Composable (ChatModel, () -> Unit) -> Unit) -> (() -> Unit),
  showVersion: () -> Unit,
  withAuth: (block: () -> Unit) -> Unit
) {
  val uriHandler = LocalUriHandler.current
  Box(Modifier.fillMaxSize().verticalScroll(rememberScrollState()).background(MaterialTheme.colors.background)) {
    Column(
      Modifier
        .fillMaxSize()
        .padding(top = DEFAULT_PADDING)
    ) {
      AppBarTitle(stringResource(R.string.your_settings))

      SectionView(stringResource(R.string.settings_section_title_you)) {
        SectionItemView(showCustomModal { chatModel, close -> UserProfileView(chatModel, close) }, 80.dp, padding = PaddingValues(start = 16.dp, end = DEFAULT_PADDING), disabled = stopped) {
          ProfilePreview(profile, stopped = stopped)
        }
        val profileHidden = rememberSaveable { mutableStateOf(false) }
        SettingsActionItem(Icons.Outlined.ManageAccounts, stringResource(R.string.your_chat_profiles), { withAuth { showSettingsModalWithSearch { it, search -> UserProfilesView(it, search, profileHidden) } } }, disabled = stopped, extraPadding = true)
        SettingsIncognitoActionItem(incognitoPref, incognito, stopped) { showModal { IncognitoView() }() }
        SettingsActionItem(Icons.Outlined.QrCode, stringResource(R.string.your_simplex_contact_address), showModal { CreateLinkView(it, CreateLinkTab.LONG_TERM) }, disabled = stopped, extraPadding = true)
        ChatPreferencesItem(showCustomModal, stopped = stopped)
      }
      SectionDividerSpaced()

      SectionView(stringResource(R.string.settings_section_title_settings)) {
        SettingsActionItem(Icons.Outlined.Bolt, stringResource(R.string.notifications), showSettingsModal { NotificationsSettingsView(it) }, disabled = stopped, extraPadding = true)
        SettingsActionItem(Icons.Outlined.WifiTethering, stringResource(R.string.network_and_servers), showSettingsModal { NetworkAndServersView(it, showModal, showSettingsModal, showCustomModal) }, disabled = stopped, extraPadding = true)
        SettingsActionItem(Icons.Outlined.Videocam, stringResource(R.string.settings_audio_video_calls), showSettingsModal { CallSettingsView(it, showModal) }, disabled = stopped, extraPadding = true)
        SettingsActionItem(Icons.Outlined.Lock, stringResource(R.string.privacy_and_security), showSettingsModal { PrivacySettingsView(it, showSettingsModal, setPerformLA) }, disabled = stopped, extraPadding = true)
        SettingsActionItem(Icons.Outlined.LightMode, stringResource(R.string.appearance_settings), showSettingsModal { AppearanceView(it) }, disabled = stopped, extraPadding = true)
        DatabaseItem(encrypted, showSettingsModal { DatabaseView(it, showSettingsModal) }, stopped)
      }
      SectionDividerSpaced()

      SectionView(stringResource(R.string.settings_section_title_help)) {
        SettingsActionItem(Icons.Outlined.HelpOutline, stringResource(R.string.how_to_use_simplex_chat), showModal { HelpView(userDisplayName) }, disabled = stopped, extraPadding = true)
        SettingsActionItem(Icons.Outlined.Add, stringResource(R.string.whats_new), showCustomModal { _, close -> WhatsNewView(viaSettings = true, close) }, disabled = stopped, extraPadding = true)
        SettingsActionItem(Icons.Outlined.Info, stringResource(R.string.about_simplex_chat), showModal { SimpleXInfo(it, onboarding = false) }, extraPadding = true)
        SettingsActionItem(Icons.Outlined.Tag, stringResource(R.string.chat_with_the_founder), { uriHandler.openUriCatching(simplexTeamUri) }, textColor = MaterialTheme.colors.primary, disabled = stopped, extraPadding = true)
        SettingsActionItem(Icons.Outlined.Email, stringResource(R.string.send_us_an_email), { uriHandler.openUriCatching("mailto:chat@simplex.chat") }, textColor = MaterialTheme.colors.primary, extraPadding = true)
      }
      SectionDividerSpaced()

      SectionView(stringResource(R.string.settings_section_title_support)) {
        ContributeItem(uriHandler)
        RateAppItem(uriHandler)
        StarOnGithubItem(uriHandler)
      }
      SectionDividerSpaced()

      SectionView(stringResource(R.string.settings_section_title_develop)) {
        SettingsActionItem(Icons.Outlined.Code, stringResource(R.string.settings_developer_tools), showSettingsModal { DeveloperView(it, showCustomModal, withAuth) }, extraPadding = true)
        AppVersionItem(showVersion)
      }
      SectionBottomSpacer()
    }
  }
}

@Composable
fun SettingsIncognitoActionItem(
  incognitoPref: SharedPreference<Boolean>,
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
  SectionItemViewWithIcon(openDatabaseView) {
    Row(
      Modifier.fillMaxWidth(),
      horizontalArrangement = Arrangement.SpaceBetween
    ) {
      Row(Modifier.weight(1f)) {
        Icon(
          Icons.Outlined.FolderOpen,
          contentDescription = stringResource(R.string.database_passphrase_and_export),
          tint = if (encrypted) HighOrLowlight else WarningOrange,
        )
        TextIconSpaced(true)
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

@Composable fun ChatPreferencesItem(showCustomModal: ((@Composable (ChatModel, () -> Unit) -> Unit) -> (() -> Unit)), stopped: Boolean) {
  SettingsActionItem(
    Icons.Outlined.ToggleOn,
    stringResource(R.string.chat_preferences),
    click = if (stopped) null else ({
      withApi {
        showCustomModal { m, close ->
          PreferencesView(m, m.currentUser.value ?: return@showCustomModal, close)
        }()
      }
    }),
    disabled = stopped,
    extraPadding = true
  )
}

@Composable
fun ChatLockItem(
  chatModel: ChatModel,
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  setPerformLA: (Boolean, FragmentActivity) -> Unit
) {
  val performLA = remember { chatModel.performLA }
  val currentLAMode = remember { chatModel.controller.appPrefs.laMode }
  SettingsActionItemWithContent(
    click = showSettingsModal { SimplexLockView(chatModel, currentLAMode, setPerformLA) },
    icon = if (performLA.value) Icons.Filled.Lock else Icons.Outlined.Lock,
    text = stringResource(R.string.chat_lock),
    iconColor = if (performLA.value) SimplexGreen else HighOrLowlight,
    extraPadding = false,
  ) {
    Text(if (performLA.value) remember { currentLAMode.state }.value.text else generalGetString(androidx.compose.ui.R.string.off), color = HighOrLowlight)
  }
}

@Composable private fun ContributeItem(uriHandler: UriHandler) {
  SectionItemViewWithIcon({ uriHandler.openUriCatching("https://github.com/simplex-chat/simplex-chat#contribute") }) {
    Icon(
      Icons.Outlined.Keyboard,
      contentDescription = "GitHub",
      tint = HighOrLowlight,
    )
    TextIconSpaced(extraPadding = true)
    Text(generalGetString(R.string.contribute), color = MaterialTheme.colors.primary)
  }
}

@Composable private fun RateAppItem(uriHandler: UriHandler) {
  SectionItemViewWithIcon({
    runCatching { uriHandler.openUriCatching("market://details?id=chat.simplex.app") }
      .onFailure { uriHandler.openUriCatching("https://play.google.com/store/apps/details?id=chat.simplex.app") }
  }
  ) {
    Icon(
      Icons.Outlined.StarOutline,
      contentDescription = "Google Play",
      tint = HighOrLowlight,
    )
    TextIconSpaced(extraPadding = true)
    Text(generalGetString(R.string.rate_the_app), color = MaterialTheme.colors.primary)
  }
}

@Composable private fun StarOnGithubItem(uriHandler: UriHandler) {
  SectionItemViewWithIcon({ uriHandler.openUriCatching("https://github.com/simplex-chat/simplex-chat") }) {
    Icon(
      painter = painterResource(id = R.drawable.ic_github),
      contentDescription = "GitHub",
      tint = HighOrLowlight,
    )
    TextIconSpaced(extraPadding = true)
    Text(generalGetString(R.string.star_on_github), color = MaterialTheme.colors.primary)
  }
}

@Composable fun ChatConsoleItem(showTerminal: () -> Unit) {
  SectionItemView(showTerminal) {
    Icon(
      painter = painterResource(id = R.drawable.ic_outline_terminal),
      contentDescription = stringResource(R.string.chat_console),
      tint = HighOrLowlight,
    )
    TextIconSpaced()
    Text(stringResource(R.string.chat_console))
  }
}

@Composable fun InstallTerminalAppItem(uriHandler: UriHandler) {
  SectionItemView({ uriHandler.openUriCatching("https://github.com/simplex-chat/simplex-chat") }) {
    Icon(
      painter = painterResource(id = R.drawable.ic_github),
      contentDescription = "GitHub",
      tint = HighOrLowlight,
    )
    TextIconSpaced()
    Text(generalGetString(R.string.install_simplex_chat_for_terminal), color = MaterialTheme.colors.primary)
  }
}

@Composable private fun AppVersionItem(showVersion: () -> Unit) {
  SectionItemViewWithIcon(showVersion) { AppVersionText() }
}

@Composable fun AppVersionText() {
  Text("v${BuildConfig.VERSION_NAME} (${BuildConfig.VERSION_CODE})")
}

@Composable fun ProfilePreview(profileOf: NamedChat, size: Dp = 60.dp, color: Color = MaterialTheme.colors.secondary, stopped: Boolean = false) {
  ProfileImage(size = size, image = profileOf.image, color = color)
  Spacer(Modifier.padding(horizontal = 8.dp))
  Column(Modifier.height(size), verticalArrangement = Arrangement.Center) {
    Text(
      profileOf.displayName,
      style = MaterialTheme.typography.caption,
      fontWeight = FontWeight.Bold,
      color = if (stopped) HighOrLowlight else Color.Unspecified,
      maxLines = 1,
      overflow = TextOverflow.Ellipsis
    )
    if (profileOf.fullName.isNotEmpty()) {
      Text(
        profileOf.fullName,
        Modifier.padding(vertical = 5.dp),
        color = if (stopped) HighOrLowlight else Color.Unspecified,
        maxLines = 1,
        overflow = TextOverflow.Ellipsis
      )
    }
  }
}

@Composable
fun SettingsActionItem(icon: ImageVector, text: String, click: (() -> Unit)? = null, textColor: Color = Color.Unspecified, iconColor: Color = HighOrLowlight, disabled: Boolean = false, extraPadding: Boolean = false) {
  SectionItemView(click, disabled = disabled, extraPadding = extraPadding) {
    Icon(icon, text, tint = if (disabled) HighOrLowlight else iconColor)
    TextIconSpaced(extraPadding)
    Text(text, color = if (disabled) HighOrLowlight else textColor)
  }
}

@Composable
fun SettingsActionItemWithContent(icon: ImageVector?, text: String? = null, click: (() -> Unit)? = null, iconColor: Color = HighOrLowlight, disabled: Boolean = false, extraPadding: Boolean = false, content: @Composable RowScope.() -> Unit) {
  SectionItemView(
    click,
    extraPadding = extraPadding,
    padding = if (extraPadding && icon != null)
      PaddingValues(start = DEFAULT_PADDING * 1.7f, end = DEFAULT_PADDING)
    else
      PaddingValues(horizontal = DEFAULT_PADDING),
    disabled = disabled
  ) {
    if (icon != null) {
      Icon(icon, text, tint = if (disabled) HighOrLowlight else iconColor)
      TextIconSpaced(extraPadding)
    }
    if (text != null) {
      Text(text, Modifier.weight(1f), color = if (disabled) HighOrLowlight else MaterialTheme.colors.onBackground)
      Spacer(Modifier.width(DEFAULT_PADDING))
    }
    content()
  }
}

@Composable
fun SettingsPreferenceItem(
  icon: ImageVector?,
  text: String,
  pref: SharedPreference<Boolean>,
  iconColor: Color = HighOrLowlight,
  enabled: Boolean = true,
  onChange: ((Boolean) -> Unit)? = null,
) {
  SettingsActionItemWithContent(icon, text, iconColor = iconColor,) {
    SharedPreferenceToggle(pref, enabled, onChange)
  }
}

@Composable
fun SettingsPreferenceItemWithInfo(
  icon: ImageVector,
  iconTint: Color,
  text: String,
  stopped: Boolean,
  onClickInfo: () -> Unit,
  pref: SharedPreference<Boolean>,
  prefState: MutableState<Boolean>? = null
) {
  SettingsActionItemWithContent(icon, null, click = if (stopped) null else onClickInfo, iconColor = iconTint, extraPadding = true,) {
    SharedPreferenceToggleWithIcon(text, Icons.Outlined.Info, stopped, onClickInfo, pref, prefState)
  }
}

@Composable
fun PreferenceToggle(
  text: String,
  checked: Boolean,
  onChange: (Boolean) -> Unit = {},
) {
  SettingsActionItemWithContent(null, text, extraPadding = true,) {
    Switch(
      checked = checked,
      onCheckedChange = onChange,
      colors = SwitchDefaults.colors(
        checkedThumbColor = MaterialTheme.colors.primary,
        uncheckedThumbColor = HighOrLowlight
      )
    )
  }
}

@Composable
fun PreferenceToggleWithIcon(
  text: String,
  icon: ImageVector? = null,
  iconColor: Color? = HighOrLowlight,
  checked: Boolean,
  extraPadding: Boolean = false,
  onChange: (Boolean) -> Unit = {},
) {
  SettingsActionItemWithContent(icon, text, iconColor = iconColor ?: HighOrLowlight, extraPadding = extraPadding) {
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

private fun runAuth(context: Context, onFinish: (success: Boolean) -> Unit) {
  authenticate(
    generalGetString(R.string.auth_open_chat_console),
    generalGetString(R.string.auth_log_in_using_credential),
    context as FragmentActivity,
    completed = { laResult ->
      onFinish(laResult == LAResult.Success || laResult is LAResult.Unavailable)
    }
  )
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
      incognitoPref = SharedPreference({ false }, {}),
      userDisplayName = "Alice",
      setPerformLA = { _, _ -> },
      showModal = { {} },
      showSettingsModal = { {} },
      showSettingsModalWithSearch = { },
      showCustomModal = { {} },
      showVersion = {},
      withAuth = {},
    )
  }
}
