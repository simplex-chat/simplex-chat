package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
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
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.CreateProfile
import chat.simplex.common.views.database.DatabaseView
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.migration.MigrateFromDeviceView
import chat.simplex.common.views.onboarding.SimpleXInfo
import chat.simplex.common.views.onboarding.WhatsNewView
import chat.simplex.res.MR
import kotlinx.coroutines.*

@Composable
fun SettingsView(chatModel: ChatModel, setPerformLA: (Boolean) -> Unit, close: () -> Unit) {
  val user = chatModel.currentUser.value
  val stopped = chatModel.chatRunning.value == false
  SettingsLayout(
    profile = user?.profile,
    stopped,
    chatModel.chatDbEncrypted.value == true,
    remember { chatModel.controller.appPrefs.storeDBPassphrase.state }.value,
    remember { chatModel.controller.appPrefs.notificationsMode.state },
    user?.displayName,
    setPerformLA = setPerformLA,
    showModal = { modalView -> { ModalManager.start.showModal { modalView(chatModel) } } },
    showSettingsModal = { modalView -> { ModalManager.start.showModal(true) { modalView(chatModel) } } },
    showSettingsModalWithSearch = { modalView ->
      ModalManager.start.showCustomModal { close ->
        val search = rememberSaveable { mutableStateOf("") }
        ModalView(
          { close() },
          endButtons = {
            SearchTextField(Modifier.fillMaxWidth(), placeholder = stringResource(MR.strings.search_verb), alwaysVisible = true) { search.value = it }
          },
          content = { modalView(chatModel, search) })
      }
    },
    showCustomModal = { modalView -> { ModalManager.start.showCustomModal { close -> modalView(chatModel, close) } } },
    showVersion = {
      withBGApi {
        val info = chatModel.controller.apiGetVersion()
        if (info != null) {
          ModalManager.start.showModal { VersionInfoView(info) }
        }
      }
    },
    withAuth = ::doWithAuth,
  )
  KeyChangeEffect(chatModel.updatingProgress.value != null) {
    close()
  }
}

val simplexTeamUri =
  "simplex:/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D"

@Composable
fun SettingsLayout(
  profile: LocalProfile?,
  stopped: Boolean,
  encrypted: Boolean,
  passphraseSaved: Boolean,
  notificationsMode: State<NotificationsMode>,
  userDisplayName: String?,
  setPerformLA: (Boolean) -> Unit,
  showModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showSettingsModalWithSearch: (@Composable (ChatModel, MutableState<String>) -> Unit) -> Unit,
  showCustomModal: (@Composable ModalData.(ChatModel, () -> Unit) -> Unit) -> (() -> Unit),
  showVersion: () -> Unit,
  withAuth: (title: String, desc: String, block: () -> Unit) -> Unit,
) {
  val scope = rememberCoroutineScope()
  val view = LocalMultiplatformView()
  LaunchedEffect(Unit) {
    hideKeyboard(view)
  }
  val theme = CurrentColors.collectAsState()
  val uriHandler = LocalUriHandler.current
  ColumnWithScrollBar(
    Modifier
      .fillMaxSize()
      .themedBackground(theme.value.base)
  ) {
    AppBarTitle(stringResource(MR.strings.your_settings))

    SectionView(stringResource(MR.strings.settings_section_title_settings)) {
      SettingsActionItem(painterResource(if (notificationsMode.value == NotificationsMode.OFF) MR.images.ic_bolt_off else MR.images.ic_bolt), stringResource(MR.strings.notifications), showSettingsModal { NotificationsSettingsView(it) }, disabled = stopped)
      SettingsActionItem(painterResource(MR.images.ic_wifi_tethering), stringResource(MR.strings.network_and_servers), showSettingsModal { NetworkAndServersView() }, disabled = stopped)
      SettingsActionItem(painterResource(MR.images.ic_videocam), stringResource(MR.strings.settings_audio_video_calls), showSettingsModal { CallSettingsView(it, showModal) }, disabled = stopped)
      SettingsActionItem(painterResource(MR.images.ic_lock), stringResource(MR.strings.privacy_and_security), showSettingsModal { PrivacySettingsView(it, showSettingsModal, setPerformLA) }, disabled = stopped)
      SettingsActionItem(painterResource(MR.images.ic_light_mode), stringResource(MR.strings.appearance_settings), showSettingsModal { AppearanceView(it) })
    }
    SectionDividerSpaced()

    SectionView(stringResource(MR.strings.settings_section_title_chat_database)) {
      DatabaseItem(encrypted, passphraseSaved, showSettingsModal { DatabaseView(it, showSettingsModal) }, stopped)
      SettingsActionItem(painterResource(MR.images.ic_ios_share), stringResource(MR.strings.migrate_from_device_to_another_device), { withAuth(generalGetString(MR.strings.auth_open_migration_to_another_device), generalGetString(MR.strings.auth_log_in_using_credential)) { ModalManager.fullscreen.showCustomModal { close -> MigrateFromDeviceView(close) } } }, disabled = stopped)
    }

    SectionDividerSpaced()

    SectionView(stringResource(MR.strings.settings_section_title_help)) {
      SettingsActionItem(painterResource(MR.images.ic_help), stringResource(MR.strings.how_to_use_simplex_chat), showModal { HelpView(userDisplayName ?: "") }, disabled = stopped)
      SettingsActionItem(painterResource(MR.images.ic_add), stringResource(MR.strings.whats_new), showCustomModal { _, close -> WhatsNewView(viaSettings = true, close) }, disabled = stopped)
      SettingsActionItem(painterResource(MR.images.ic_info), stringResource(MR.strings.about_simplex_chat), showModal { SimpleXInfo(it, onboarding = false) })
      if (!chatModel.desktopNoUserNoRemote) {
        SettingsActionItem(painterResource(MR.images.ic_tag), stringResource(MR.strings.chat_with_the_founder), { uriHandler.openVerifiedSimplexUri(simplexTeamUri) }, textColor = MaterialTheme.colors.primary, disabled = stopped)
      }
      SettingsActionItem(painterResource(MR.images.ic_mail), stringResource(MR.strings.send_us_an_email), { uriHandler.openUriCatching("mailto:chat@simplex.chat") }, textColor = MaterialTheme.colors.primary)
    }
    SectionDividerSpaced()

    SectionView(stringResource(MR.strings.settings_section_title_support)) {
      ContributeItem(uriHandler)
      RateAppItem(uriHandler)
      StarOnGithubItem(uriHandler)
    }
    SectionDividerSpaced()

    SettingsSectionApp(showSettingsModal, showCustomModal, showVersion, withAuth)
    SectionBottomSpacer()
  }
}

@Composable
expect fun SettingsSectionApp(
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showCustomModal: (@Composable ModalData.(ChatModel, () -> Unit) -> Unit) -> (() -> Unit),
  showVersion: () -> Unit,
  withAuth: (title: String, desc: String, block: () -> Unit) -> Unit
)

@Composable private fun DatabaseItem(encrypted: Boolean, saved: Boolean, openDatabaseView: () -> Unit, stopped: Boolean) {
  SectionItemView(openDatabaseView) {
    Row(
      Modifier.fillMaxWidth(),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.SpaceBetween
    ) {
      Row(Modifier.weight(1f), verticalAlignment = Alignment.CenterVertically) {
        Icon(
          painterResource(MR.images.ic_database),
          contentDescription = stringResource(MR.strings.database_passphrase_and_export),
          tint = if (encrypted && (appPlatform.isAndroid || !saved)) MaterialTheme.colors.secondary else WarningOrange,
        )
        TextIconSpaced(false)
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

@Composable fun ChatPreferencesItem(showCustomModal: ((@Composable ModalData.(ChatModel, () -> Unit) -> Unit) -> (() -> Unit)), stopped: Boolean) {
  SettingsActionItem(
    painterResource(MR.images.ic_toggle_on),
    stringResource(MR.strings.chat_preferences),
    click = if (stopped) null else ({
      showCustomModal { m, close ->
        PreferencesView(m, m.currentUser.value ?: return@showCustomModal, close)
      }()
    }),
    disabled = stopped
  )
}

@Composable
fun ChatLockItem(
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  setPerformLA: (Boolean) -> Unit
) {
  val performLA = remember { appPrefs.performLA.state }
  val currentLAMode = remember { ChatModel.controller.appPrefs.laMode }
  SettingsActionItemWithContent(
    click = showSettingsModal { SimplexLockView(ChatModel, currentLAMode, setPerformLA) },
    icon = if (performLA.value) painterResource(MR.images.ic_lock_filled) else painterResource(MR.images.ic_lock),
    text = stringResource(MR.strings.chat_lock),
    iconColor = if (performLA.value) SimplexGreen else MaterialTheme.colors.secondary
  ) {
    Text(if (performLA.value) remember { currentLAMode.state }.value.text else generalGetString(MR.strings.la_mode_off), color = MaterialTheme.colors.secondary)
  }
}

@Composable private fun ContributeItem(uriHandler: UriHandler) {
  SectionItemView({ uriHandler.openUriCatching("https://github.com/simplex-chat/simplex-chat#contribute") }) {
    Icon(
      painterResource(MR.images.ic_keyboard),
      contentDescription = "GitHub",
      tint = MaterialTheme.colors.secondary,
    )
    TextIconSpaced()
    Text(generalGetString(MR.strings.contribute), color = MaterialTheme.colors.primary)
  }
}

@Composable private fun RateAppItem(uriHandler: UriHandler) {
  SectionItemView({
    runCatching { uriHandler.openUriCatching("market://details?id=chat.simplex.app") }
      .onFailure { uriHandler.openUriCatching("https://play.google.com/store/apps/details?id=chat.simplex.app") }
  }
  ) {
    Icon(
      painterResource(MR.images.ic_star),
      contentDescription = "Google Play",
      tint = MaterialTheme.colors.secondary,
    )
    TextIconSpaced()
    Text(generalGetString(MR.strings.rate_the_app), color = MaterialTheme.colors.primary)
  }
}

@Composable private fun StarOnGithubItem(uriHandler: UriHandler) {
  SectionItemView({ uriHandler.openUriCatching("https://github.com/simplex-chat/simplex-chat") }) {
    Icon(
      painter = painterResource(MR.images.ic_github),
      contentDescription = "GitHub",
      tint = MaterialTheme.colors.secondary,
    )
    TextIconSpaced()
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

@Composable fun TerminalAlwaysVisibleItem(pref: SharedPreference<Boolean>, onChange: (Boolean) -> Unit) {
  SettingsActionItemWithContent(painterResource(MR.images.ic_engineering), stringResource(MR.strings.terminal_always_visible)) {
    DefaultSwitch(
      checked = remember { pref.state }.value,
      onCheckedChange = onChange,
    )
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

@Composable fun ResetHintsItem(unchangedHints: MutableState<Boolean>) {
  SectionItemView({
    resetHintPreferences()
    unchangedHints.value = true
  }, disabled = unchangedHints.value) {
    Icon(
      painter = painterResource(MR.images.ic_lightbulb),
      contentDescription = "Lightbulb",
      tint = MaterialTheme.colors.secondary,
    )
    TextIconSpaced()
    Text(generalGetString(MR.strings.reset_all_hints), color = if (unchangedHints.value) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
  }
}

private fun resetHintPreferences() {
  for ((pref, def) in appPreferences.hintPreferences) {
    pref.set(def)
  }
}

fun unchangedHintPreferences(): Boolean = appPreferences.hintPreferences.all { (pref, def) ->
  pref.state.value == def
}

@Composable
fun AppVersionItem(showVersion: () -> Unit) {
  SectionItemView(showVersion) { AppVersionText() }
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
    if (profileOf.fullName.isNotEmpty() && profileOf.fullName != profileOf.displayName) {
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
fun SettingsActionItemWithContent(icon: Painter?, text: String? = null, click: (() -> Unit)? = null, iconColor: Color = MaterialTheme.colors.secondary, textColor: Color = MaterialTheme.colors.onBackground, disabled: Boolean = false, extraPadding: Boolean = false, content: @Composable RowScope.() -> Unit) {
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
      val padding = with(LocalDensity.current) { 6.sp.toDp() }
      Text(text, Modifier.weight(1f).padding(vertical = padding), color = if (disabled) MaterialTheme.colors.secondary else textColor)
      Spacer(Modifier.width(DEFAULT_PADDING))
      Row(Modifier.widthIn(max = (windowWidth() - DEFAULT_PADDING * 2) / 2)) {
        content()
      }
    } else {
      Row {
        content()
      }
    }
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
fun PreferenceToggle(
  text: String,
  disabled: Boolean = false,
  checked: Boolean,
  onChange: (Boolean) -> Unit = {},
) {
  SettingsActionItemWithContent(null, text, disabled = disabled) {
    DefaultSwitch(
      checked = checked,
      onCheckedChange = onChange,
      enabled = !disabled
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

fun doWithAuth(title: String, desc: String, block: () -> Unit) {
  val requireAuth = chatModel.controller.appPrefs.performLA.get()
  if (!requireAuth) {
    block()
  } else {
    var autoShow = true
    ModalManager.fullscreen.showModalCloseable { close ->
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
}

private fun runAuth(title: String, desc: String, onFinish: (success: Boolean) -> Unit) {
  authenticate(
    title,
    desc,
    oneTime = true,
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
      passphraseSaved = false,
      notificationsMode = remember { mutableStateOf(NotificationsMode.OFF) },
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
