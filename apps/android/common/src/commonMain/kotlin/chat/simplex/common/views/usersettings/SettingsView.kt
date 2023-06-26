package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionItemViewWithIcon
import SectionView
import TextIconSpaced
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.platform.*
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import com.icerockdev.library.MR
import chat.simplex.common.model.*
import chat.simplex.common.platform.appVersionInfo
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.database.DatabaseView
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.SimpleXInfo
import chat.simplex.common.views.onboarding.WhatsNewView

@Composable
fun SettingsView(chatModel: ChatModel, setPerformLA: (Boolean) -> Unit) {
  val user = chatModel.currentUser.value
  val stopped = chatModel.chatRunning.value == false

  MaintainIncognitoState(chatModel)

  if (user != null) {
    val requireAuth = remember { chatModel.controller.appPrefs.performLA.state }
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
              SearchTextField(Modifier.fillMaxWidth(), stringResource(MR.strings.search), alwaysVisible = true) { search.value = it }
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
      withAuth = { title, desc, block ->
        if (!requireAuth.value) {
          block()
        } else {
          var autoShow = true
          ModalManager.shared.showModalCloseable { close ->
            val onFinishAuth = { success: Boolean ->
              if (success) {
                close()
                block()
              }
            }

            LaunchedEffect(Unit) {
              if (autoShow) {
                autoShow = false
                runAuth(title, desc, onFinishAuth)
              }
            }
            Box(
              Modifier.fillMaxSize().background(MaterialTheme.colors.background),
              contentAlignment = Alignment.Center
            ) {
              SimpleButton(
                stringResource(MR.strings.auth_unlock),
                icon = painterResource(MR.images.ic_lock),
                click = {
                  runAuth(title, desc, onFinishAuth)
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
  setPerformLA: (Boolean) -> Unit,
  showModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showSettingsModalWithSearch: (@Composable (ChatModel, MutableState<String>) -> Unit) -> Unit,
  showCustomModal: (@Composable (ChatModel, () -> Unit) -> Unit) -> (() -> Unit),
  showVersion: () -> Unit,
  withAuth: (title: String, desc: String, block: () -> Unit) -> Unit
) {
  val theme = CurrentColors.collectAsState()
  val uriHandler = LocalUriHandler.current
  Box(Modifier.fillMaxSize().verticalScroll(rememberScrollState()).themedBackground(theme.value.base)) {
    Column(
      Modifier
        .fillMaxSize()
        .padding(top = DEFAULT_PADDING)
    ) {
      AppBarTitle(stringResource(MR.strings.your_settings))

      SectionView(stringResource(MR.strings.settings_section_title_you)) {
        SectionItemView(showCustomModal { chatModel, close -> UserProfileView(chatModel, close) }, 80.dp, padding = PaddingValues(start = 16.dp, end = DEFAULT_PADDING), disabled = stopped) {
          ProfilePreview(profile, stopped = stopped)
        }
        val profileHidden = rememberSaveable { mutableStateOf(false) }
        SettingsActionItem(painterResource(MR.images.ic_manage_accounts), stringResource(MR.strings.your_chat_profiles), { withAuth(generalGetString(MR.strings.auth_open_chat_profiles), generalGetString(MR.strings.auth_log_in_using_credential)) { showSettingsModalWithSearch { it, search -> UserProfilesView(it, search, profileHidden) } } }, disabled = stopped, extraPadding = true)
        SettingsIncognitoActionItem(incognitoPref, incognito, stopped) { showModal { IncognitoView() }() }
        SettingsActionItem(painterResource(MR.images.ic_qr_code), stringResource(MR.strings.your_simplex_contact_address), showCustomModal { it, close -> UserAddressView(it, shareViaProfile = it.currentUser.value!!.addressShared, close = close) }, disabled = stopped, extraPadding = true)
        ChatPreferencesItem(showCustomModal, stopped = stopped)
      }
      SectionDividerSpaced()

      SectionView(stringResource(MR.strings.settings_section_title_settings)) {
        SettingsActionItem(painterResource(MR.images.ic_bolt), stringResource(MR.strings.notifications), showSettingsModal { NotificationsSettingsView(it) }, disabled = stopped, extraPadding = true)
        SettingsActionItem(painterResource(MR.images.ic_wifi_tethering), stringResource(MR.strings.network_and_servers), showSettingsModal { NetworkAndServersView(it, showModal, showSettingsModal, showCustomModal) }, disabled = stopped, extraPadding = true)
        SettingsActionItem(painterResource(MR.images.ic_videocam), stringResource(MR.strings.settings_audio_video_calls), showSettingsModal { CallSettingsView(it, showModal) }, disabled = stopped, extraPadding = true)
        SettingsActionItem(painterResource(MR.images.ic_lock), stringResource(MR.strings.privacy_and_security), showSettingsModal { PrivacySettingsView(it, showSettingsModal, setPerformLA) }, disabled = stopped, extraPadding = true)
        SettingsActionItem(painterResource(MR.images.ic_light_mode), stringResource(MR.strings.appearance_settings), showSettingsModal { AppearanceView(it, showSettingsModal) }, extraPadding = true)
        DatabaseItem(encrypted, showSettingsModal { DatabaseView(it, showSettingsModal) }, stopped)
      }
      SectionDividerSpaced()

      SectionView(stringResource(MR.strings.settings_section_title_help)) {
        SettingsActionItem(painterResource(MR.images.ic_help), stringResource(MR.strings.how_to_use_simplex_chat), showModal { HelpView(userDisplayName) }, disabled = stopped, extraPadding = true)
        SettingsActionItem(painterResource(MR.images.ic_add), stringResource(MR.strings.whats_new), showCustomModal { _, close -> WhatsNewView(viaSettings = true, close) }, disabled = stopped, extraPadding = true)
        SettingsActionItem(painterResource(MR.images.ic_info), stringResource(MR.strings.about_simplex_chat), showModal { SimpleXInfo(it, onboarding = false) }, extraPadding = true)
        SettingsActionItem(painterResource(MR.images.ic_tag), stringResource(MR.strings.chat_with_the_founder), { uriHandler.openUriCatching(simplexTeamUri) }, textColor = MaterialTheme.colors.primary, disabled = stopped, extraPadding = true)
        SettingsActionItem(painterResource(MR.images.ic_mail), stringResource(MR.strings.send_us_an_email), { uriHandler.openUriCatching("mailto:chat@simplex.chat") }, textColor = MaterialTheme.colors.primary, extraPadding = true)
      }
      SectionDividerSpaced()

      SectionView(stringResource(MR.strings.settings_section_title_support)) {
        ContributeItem(uriHandler)
        RateAppItem(uriHandler)
        StarOnGithubItem(uriHandler)
      }
      SectionDividerSpaced()

      SectionView(stringResource(MR.strings.settings_section_title_develop)) {
        SettingsActionItem(painterResource(MR.images.ic_code), stringResource(MR.strings.settings_developer_tools), showSettingsModal { DeveloperView(it, showCustomModal, withAuth) }, extraPadding = true)
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
    if (incognito.value) painterResource(MR.images.ic_theater_comedy_filled) else painterResource(MR.images.ic_theater_comedy),
    if (incognito.value) Indigo else MaterialTheme.colors.secondary,
    stringResource(MR.strings.incognito),
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
          painterResource(MR.images.ic_database),
          contentDescription = stringResource(MR.strings.database_passphrase_and_export),
          tint = if (encrypted) MaterialTheme.colors.secondary else WarningOrange,
        )
        TextIconSpaced(true)
        Text(stringResource(MR.strings.database_passphrase_and_export))
      }
      if (stopped) {
        Icon(
          painterResource(MR.images.ic_report_filled),
          contentDescription = stringResource(MR.strings.chat_is_stopped),
          tint = Color.Red,
          modifier = Modifier.padding(end = 6.dp)
        )
      }
    }
  }
}

@Composable fun ChatPreferencesItem(showCustomModal: ((@Composable (ChatModel, () -> Unit) -> Unit) -> (() -> Unit)), stopped: Boolean) {
  SettingsActionItem(
    painterResource(MR.images.ic_toggle_on),
    stringResource(MR.strings.chat_preferences),
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
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  setPerformLA: (Boolean) -> Unit
) {
  val performLA = remember { ChatModel.performLA }
  val currentLAMode = remember { ChatModel.controller.appPrefs.laMode }
  SettingsActionItemWithContent(
    click = showSettingsModal { SimplexLockView(ChatModel, currentLAMode, setPerformLA) },
    icon = if (performLA.value) painterResource(MR.images.ic_lock_filled) else painterResource(MR.images.ic_lock),
    text = stringResource(MR.strings.chat_lock),
    iconColor = if (performLA.value) SimplexGreen else MaterialTheme.colors.secondary,
    extraPadding = false,
  ) {
    Text(if (performLA.value) remember { currentLAMode.state }.value.text else generalGetString(MR.strings.mode_off), color = MaterialTheme.colors.secondary)
  }
}

@Composable private fun ContributeItem(uriHandler: UriHandler) {
  SectionItemViewWithIcon({ uriHandler.openUriCatching("https://github.com/simplex-chat/simplex-chat#contribute") }) {
    Icon(
      painterResource(MR.images.ic_keyboard),
      contentDescription = "GitHub",
      tint = MaterialTheme.colors.secondary,
    )
    TextIconSpaced(extraPadding = true)
    Text(generalGetString(MR.strings.contribute), color = MaterialTheme.colors.primary)
  }
}

@Composable private fun RateAppItem(uriHandler: UriHandler) {
  SectionItemViewWithIcon({
    runCatching { uriHandler.openUriCatching("market://details?id=chat.simplex.app") }
      .onFailure { uriHandler.openUriCatching("https://play.google.com/store/apps/details?id=chat.simplex.app") }
  }
  ) {
    Icon(
      painterResource(MR.images.ic_star),
      contentDescription = "Google Play",
      tint = MaterialTheme.colors.secondary,
    )
    TextIconSpaced(extraPadding = true)
    Text(generalGetString(MR.strings.rate_the_app), color = MaterialTheme.colors.primary)
  }
}

@Composable private fun StarOnGithubItem(uriHandler: UriHandler) {
  SectionItemViewWithIcon({ uriHandler.openUriCatching("https://github.com/simplex-chat/simplex-chat") }) {
    Icon(
      painter = painterResource(MR.images.ic_github),
      contentDescription = "GitHub",
      tint = MaterialTheme.colors.secondary,
    )
    TextIconSpaced(extraPadding = true)
    Text(generalGetString(MR.strings.star_on_github), color = MaterialTheme.colors.primary)
  }
}

@Composable fun ChatConsoleItem(showTerminal: () -> Unit) {
  SectionItemView(showTerminal) {
    Icon(
      painter = painterResource(MR.images.ic_outline_terminal),
      contentDescription = stringResource(MR.strings.chat_console),
      tint = MaterialTheme.colors.secondary,
    )
    TextIconSpaced()
    Text(stringResource(MR.strings.chat_console))
  }
}

@Composable fun InstallTerminalAppItem(uriHandler: UriHandler) {
  SectionItemView({ uriHandler.openUriCatching("https://github.com/simplex-chat/simplex-chat") }) {
    Icon(
      painter = painterResource(MR.images.ic_github),
      contentDescription = "GitHub",
      tint = MaterialTheme.colors.secondary,
    )
    TextIconSpaced()
    Text(generalGetString(MR.strings.install_simplex_chat_for_terminal), color = MaterialTheme.colors.primary)
  }
}

@Composable private fun AppVersionItem(showVersion: () -> Unit) {
  SectionItemViewWithIcon(showVersion) { AppVersionText() }
}

@Composable fun AppVersionText() {
  Text(appVersionInfo.first + (if (appVersionInfo.second != null) " (" + appVersionInfo.second + ")" else ""))
}

@Composable fun ProfilePreview(profileOf: NamedChat, size: Dp = 60.dp, iconColor: Color = MaterialTheme.colors.secondaryVariant, textColor: Color = MaterialTheme.colors.onBackground, stopped: Boolean = false) {
  ProfileImage(size = size, image = profileOf.image, color = iconColor)
  Spacer(Modifier.padding(horizontal = 8.dp))
  Column(Modifier.height(size), verticalArrangement = Arrangement.Center) {
    Text(
      profileOf.displayName,
      style = MaterialTheme.typography.caption,
      fontWeight = FontWeight.Bold,
      color = if (stopped) MaterialTheme.colors.secondary else textColor,
      maxLines = 1,
      overflow = TextOverflow.Ellipsis
    )
    if (profileOf.fullName.isNotEmpty()) {
      Text(
        profileOf.fullName,
        Modifier.padding(vertical = 5.dp),
        color = if (stopped) MaterialTheme.colors.secondary else textColor,
        maxLines = 1,
        overflow = TextOverflow.Ellipsis
      )
    }
  }
}

@Composable
fun SettingsActionItem(icon: Painter, text: String, click: (() -> Unit)? = null, textColor: Color = Color.Unspecified, iconColor: Color = MaterialTheme.colors.secondary, disabled: Boolean = false, extraPadding: Boolean = false) {
  SectionItemView(click, disabled = disabled, extraPadding = extraPadding) {
    Icon(icon, text, tint = if (disabled) MaterialTheme.colors.secondary else iconColor)
    TextIconSpaced(extraPadding)
    Text(text, color = if (disabled) MaterialTheme.colors.secondary else textColor)
  }
}

@Composable
fun SettingsActionItemWithContent(icon: Painter?, text: String? = null, click: (() -> Unit)? = null, iconColor: Color = MaterialTheme.colors.secondary, disabled: Boolean = false, extraPadding: Boolean = false, content: @Composable RowScope.() -> Unit) {
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
      Icon(icon, text, Modifier, tint = if (disabled) MaterialTheme.colors.secondary else iconColor)
      TextIconSpaced(extraPadding)
    }
    if (text != null) {
      Text(text, Modifier.weight(1f), color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.onBackground)
      Spacer(Modifier.width(DEFAULT_PADDING))
    }
    content()
  }
}

@Composable
fun SettingsPreferenceItem(
  icon: Painter?,
  text: String,
  pref: SharedPreference<Boolean>,
  iconColor: Color = MaterialTheme.colors.secondary,
  enabled: Boolean = true,
  onChange: ((Boolean) -> Unit)? = null,
) {
  SettingsActionItemWithContent(icon, text, iconColor = iconColor,) {
    SharedPreferenceToggle(pref, enabled, onChange)
  }
}

@Composable
fun SettingsPreferenceItemWithInfo(
  icon: Painter,
  iconTint: Color,
  text: String,
  stopped: Boolean,
  onClickInfo: () -> Unit,
  pref: SharedPreference<Boolean>,
  prefState: MutableState<Boolean>? = null
) {
  SettingsActionItemWithContent(icon, null, click = if (stopped) null else onClickInfo, iconColor = iconTint, extraPadding = true,) {
    SharedPreferenceToggleWithIcon(text, painterResource(MR.images.ic_info), stopped, onClickInfo, pref, prefState)
  }
}

@Composable
fun PreferenceToggle(
  text: String,
  checked: Boolean,
  onChange: (Boolean) -> Unit = {},
) {
  SettingsActionItemWithContent(null, text, extraPadding = true,) {
    DefaultSwitch(
      checked = checked,
      onCheckedChange = onChange,
    )
  }
}

@Composable
fun PreferenceToggleWithIcon(
  text: String,
  icon: Painter? = null,
  iconColor: Color? = MaterialTheme.colors.secondary,
  checked: Boolean,
  extraPadding: Boolean = false,
  onChange: (Boolean) -> Unit = {},
) {
  SettingsActionItemWithContent(icon, text, iconColor = iconColor ?: MaterialTheme.colors.secondary, extraPadding = extraPadding) {
    DefaultSwitch(
      checked = checked,
      onCheckedChange = {
        onChange(it)
      },
    )
  }
}

private fun runAuth(title: String, desc: String, onFinish: (success: Boolean) -> Unit) {
  authenticate(
    title,
    desc,
    completed = { laResult ->
      onFinish(laResult == LAResult.Success || laResult is LAResult.Unavailable)
    }
  )
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
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
      setPerformLA = { _ -> },
      showModal = { {} },
      showSettingsModal = { {} },
      showSettingsModalWithSearch = { },
      showCustomModal = { {} },
      showVersion = {},
      withAuth = { _, _, _ -> },
    )
  }
}
