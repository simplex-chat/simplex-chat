package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import itemHPadding
import SectionItemView
import SectionDividerSpaced
import SectionTextFooter
import SectionView
import TextIconSpaced
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.lerp
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
import chat.simplex.common.views.badges.BadgesView
import chat.simplex.common.views.badges.currentShownBadge
import chat.simplex.common.views.database.DatabaseView
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.migration.MigrateFromDeviceView
import chat.simplex.common.views.onboarding.GetStakeView
import chat.simplex.common.views.onboarding.SimpleXInfo
import chat.simplex.common.views.onboarding.WhatsNewView
import chat.simplex.common.views.onboarding.crowdfundingAvailable
import chat.simplex.common.views.usersettings.networkAndServers.NetworkAndServersView
import chat.simplex.res.*
import java.io.File
import chat.simplex.common.platform.getAppFilePath // Добавлен импорт для кэша

@Composable
fun SettingsView(chatModel: ChatModel, setPerformLA: (Boolean) -> Unit, close: () -> Unit) {
  val stopped = chatModel.chatRunning.value == false
  val showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit) = { modalView -> { ModalManager.start.showModal(settings = true, cardScreen = true) { modalView(chatModel) } } }
  SettingsLayout(
    stopped,
    chatModel.chatDbEncrypted.value == true,
    remember { chatModel.controller.appPrefs.storeDBPassphrase.state }.value,
    setPerformLA = setPerformLA,
    showModal = { modalView -> { ModalManager.start.showModal { modalView(chatModel) } } },
    showSettingsModal = showSettingsModal,
    showSettingsModalWithSearch = { modalView ->
      ModalManager.start.showCustomModal { close ->
        val search = rememberSaveable { mutableStateOf("") }
        ModalView(
          { close() },
          cardScreen = true,
          showSearch = true,
          searchAlwaysVisible = true,
          onSearchValueChanged = { search.value = it },
          content = { modalView(chatModel, search) })
      }
    },
    showCustomModal = { modalView -> { ModalManager.start.showCustomModal { close -> modalView(chatModel, close) } } },
    showVersion = {
      ModalManager.start.showModal(cardScreen = true) { VersionInfoView(showSettingsModal, ::doWithAuth) }
    },
    withAuth = ::doWithAuth,
  )
  KeyChangeEffect(chatModel.updatingProgress.value != null) {
    close()
  }
}

val simplexTeamUri =
  "simplex:/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw?h=smp6.simplex.im"

val simplexNewsUri =
  "simplex:/c#grcfG3ulVI4Sh6ow33qBsmSk7uEy3gRSl2KkJ5ER6tA?h=smp18.simplex.im"

@Composable
fun SettingsLayout(
  stopped: Boolean,
  encrypted: Boolean,
  passphraseSaved: Boolean,
  setPerformLA: (Boolean) -> Unit,
  showModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showSettingsModalWithSearch: (@Composable (ChatModel, MutableState<String>) -> Unit) -> Unit,
  showCustomModal: (@Composable ModalData.(ChatModel, () -> Unit) -> Unit) -> (() -> Unit),
  showVersion: () -> Unit,
  withAuth: (title: String, desc: String, block: () -> Unit) -> Unit,
  versionInfo: Pair<String, Int?> = appVersionInfo,
) {
  val view = LocalMultiplatformView()
  LaunchedEffect(Unit) {
    hideKeyboard(view)
  }
  val notificationsMode = remember { chatModel.controller.appPrefs.notificationsMode.state }
  val bottomPadding = WindowInsets.navigationBars.asPaddingValues().calculateBottomPadding() + 90.dp

  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.your_settings))

    // Вход в FLEX Settings через нативный модальный стек
    FlexSettingsEntryTile(onClick = showSettingsModal { FlexSettingsView() })

    SectionView {
      SettingsActionItem(painterResource(MR.images.ic_light_mode), stringResource(MR.strings.appearance_settings), showSettingsModal { AppearanceView(it) })
      SettingsActionItem(painterResource(MR.images.ic_lock), stringResource(MR.strings.your_privacy), showSettingsModal { PrivacySettingsView(it, showSettingsModal, setPerformLA) }, disabled = stopped)
      SettingsActionItem(painterResource(MR.images.ic_help), stringResource(MR.strings.help_and_support), showSettingsModal { HelpAndSupportView(it, showModal, showCustomModal) })
      DatabaseItem(encrypted, passphraseSaved, showSettingsModal { DatabaseView() }, stopped)
      SettingsActionItem(painterResource(MR.images.ic_ios_share), stringResource(MR.strings.migrate_from_device_to_another_device), { withAuth(generalGetString(MR.strings.auth_open_migration_to_another_device), generalGetString(MR.strings.auth_log_in_using_credential)) { ModalManager.fullscreen.showCustomModal { close -> MigrateFromDeviceView(close) } } }, disabled = stopped)
    }
    SectionDividerSpaced()

    val (badgeLabel, badgeBenefits) = when (currentShownBadge()?.badgeType) {
      null -> MR.strings.badges_support_simplex_title to MR.strings.v7_1_supporter_badge_descr
      is BadgeType.Legend -> MR.strings.badges_your_legend_badge to MR.strings.badges_legend_benefits
      is BadgeType.Supporter, is BadgeType.Investor, is BadgeType.Unknown -> MR.strings.badges_your_badge to MR.strings.badges_supporter_benefits
    }
   SectionView {
      SectionItemView(click = {
        RichHaptics.perform(HapticPattern.CLICK)
        ModalManager.start.showCustomModal(id = ModalViewId.BADGES) { close -> BadgesView(ModalManager.start, close) }
      }) {
        val badgeType = chatModel.currentUser.value?.profile?.localBadge?.badge?.badgeType ?: BadgeType.Supporter
        Image(painterResource(badgeImage(badgeType)), stringResource(badgeLabel), Modifier.size(24.dp))
        TextIconSpaced()
        Text(stringResource(badgeLabel))
      }
    }
    SectionTextFooter(stringResource(badgeBenefits))
    SectionDividerSpaced()

    SectionView(stringResource(MR.strings.advanced_settings)) {
      SettingsActionItem(painterResource(MR.images.ic_wifi_tethering), stringResource(MR.strings.network_and_servers), showCustomModal { _, close -> NetworkAndServersView(close) }, disabled = stopped)
      if (appPlatform == AppPlatform.ANDROID) {
        SettingsActionItem(painterResource(if (notificationsMode.value == NotificationsMode.OFF) MR.images.ic_bolt_off else MR.images.ic_bolt), stringResource(MR.strings.notifications), showSettingsModal { NotificationsSettingsView(it) }, disabled = stopped)
      }
      SettingsActionItem(painterResource(MR.images.ic_videocam), stringResource(MR.strings.settings_audio_video_calls), showSettingsModal { CallSettingsView(it, showModal) }, disabled = stopped)
      AppShutdownItem()
      AppVersionItem(showVersion, versionInfo)
    }

    if (crowdfundingAvailable()) {
      SectionDividerSpaced()
      SectionView(stringResource(MR.strings.invest_settings_title)) {
        SettingsActionItem(
          painterResource(MR.images.ic_redeem),
          stringResource(MR.strings.v7_0_crowdfunding),
          { ModalManager.start.showModalCloseable(cardScreen = true) { close -> GetStakeView(showFirstImage = true, close = close) } }
        )
      }
    }
    Spacer(Modifier.height(bottomPadding))
  }
}

@Composable
fun HelpAndSupportView(
  chatModel: ChatModel,
  showModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  showCustomModal: (@Composable ModalData.(ChatModel, () -> Unit) -> Unit) -> (() -> Unit),
) {
  val uriHandler = LocalUriHandler.current
  val stopped = chatModel.chatRunning.value == false
  val userDisplayName = chatModel.currentUser.value?.displayName ?: ""
  val bottomPadding = WindowInsets.navigationBars.asPaddingValues().calculateBottomPadding() + 90.dp

  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.help_and_support))

    SectionView(stringResource(MR.strings.settings_section_title_help)) {
      SettingsActionItem(painterResource(MR.images.ic_help), stringResource(MR.strings.how_to_use_simplex_chat), showModal { HelpView(userDisplayName) }, disabled = stopped)
      SettingsActionItem(painterResource(MR.images.ic_add), stringResource(MR.strings.whats_new), showCustomModal { _, close -> WhatsNewView(viaSettings = true, close = close) }, disabled = stopped)
      SettingsActionItem(painterResource(MR.images.ic_info), stringResource(MR.strings.about_simplex_chat), showModal { SimpleXInfo(it, onboarding = false) })
    }
    SectionDividerSpaced()

    SectionView(stringResource(MR.strings.settings_section_title_contact)) {
      if (!chatModel.desktopNoUserNoRemote) {
        SettingsActionItem(painterResource(MR.images.ic_bigtop_updates), stringResource(MR.strings.follow_simplex_network_news), { uriHandler.openVerifiedSimplexUri(simplexNewsUri) }, textColor = MaterialTheme.colors.primary, disabled = stopped)
        SettingsActionItem(painterResource(MR.images.ic_tag), stringResource(MR.strings.chat_with_the_founder), { uriHandler.openVerifiedSimplexUri(simplexTeamUri) }, textColor = MaterialTheme.colors.primary, disabled = stopped)
      }
      SettingsActionItem(painterResource(MR.images.ic_mail), stringResource(MR.strings.send_us_an_email), { uriHandler.openUriCatching("mailto:chat@simplex.chat") }, textColor = MaterialTheme.colors.primary)
    }
    SectionDividerSpaced()

    SectionView(stringResource(MR.strings.settings_section_title_support_project)) {
      if (!platform.androidIsPlayStoreBuild) {
        ContributeItem(uriHandler)
      }
      if (appPlatform.isAndroid) {
        RateAppItem(uriHandler)
      }
      StarOnGithubItem(uriHandler)
    }
    Spacer(Modifier.height(bottomPadding))
  }
}

@Composable
expect fun AdvancedSettingsAppSection(
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  withAuth: (title: String, desc: String, block: () -> Unit) -> Unit,
)

@Composable
expect fun AppShutdownItem()

@Composable private fun DatabaseItem(encrypted: Boolean, saved: Boolean, openDatabaseView: () -> Unit, stopped: Boolean) {
  SectionItemView({
    RichHaptics.perform(HapticPattern.CLICK)
    openDatabaseView()
  }) {
    Row(
      Modifier.fillMaxWidth(),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.SpaceBetween
    ) {
      Row(Modifier.weight(1f), verticalAlignment = Alignment.CenterVertically) {
        Icon(
          painterResource(MR.images.ic_database),
          contentDescription = stringResource(MR.strings.chat_data),
          tint = if (encrypted && (appPlatform.isAndroid || !saved)) MaterialTheme.colors.primary.copy(alpha = 0.85f) else WarningOrange,
          modifier = Modifier.size(22.dp)
        )
        TextIconSpaced(false)
        Text(
          stringResource(MR.strings.chat_data),
          color = MaterialTheme.colors.onSurface,
          fontWeight = FontWeight.Medium,
          fontSize = 15.5.sp
        )
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
    iconColor = if (performLA.value) SimplexGreen else MaterialTheme.colors.primary.copy(alpha = 0.85f)
  ) {
    Text(
      if (performLA.value) remember { currentLAMode.state }.value.text else generalGetString(MR.strings.la_mode_off),
      color = MaterialTheme.colors.secondary,
      fontSize = 14.sp
    )
  }
}

@Composable fun ContributeItem(uriHandler: UriHandler) {
  SettingsActionItem(
    icon = painterResource(MR.images.ic_keyboard),
    text = generalGetString(MR.strings.contribute),
    click = { uriHandler.openExternalLink("https://github.com/simplex-chat/simplex-chat#contribute") },
    textColor = MaterialTheme.colors.primary
  )
}

@Composable fun RateAppItem(uriHandler: UriHandler) {
  SettingsActionItem(
    icon = painterResource(MR.images.ic_star),
    text = generalGetString(MR.strings.rate_the_app),
    click = {
      runCatching { uriHandler.openUriCatching("market://details?id=chat.simplex.app") }
        .onFailure { uriHandler.openUriCatching("https://play.google.com/store/apps/details?id=chat.simplex.app") }
    },
    textColor = MaterialTheme.colors.primary
  )
}

@Composable fun StarOnGithubItem(uriHandler: UriHandler) {
  SettingsActionItem(
    icon = painterResource(MR.images.ic_github),
    text = generalGetString(MR.strings.star_on_github),
    click = { uriHandler.openExternalLink("https://github.com/simplex-chat/simplex-chat") },
    textColor = MaterialTheme.colors.primary
  )
}

@Composable fun ChatConsoleItem(showTerminal: () -> Unit) {
  SettingsActionItem(
    icon = painterResource(MR.images.ic_outline_terminal),
    text = stringResource(MR.strings.chat_console),
    click = showTerminal
  )
}

@Composable fun TerminalAlwaysVisibleItem(pref: SharedPreference<Boolean>, onChange: (Boolean) -> Unit) {
  val state = remember { pref.state }
  SettingsActionItemWithContent(
    icon = painterResource(MR.images.ic_engineering),
    text = stringResource(MR.strings.terminal_always_visible),
    click = {
      val next = !state.value
      RichHaptics.perform(if (next) HapticPattern.TOGGLE_ON else HapticPattern.TOGGLE_OFF)
      onChange(next)
    }
  ) {
    DefaultSwitch(
      checked = state.value,
      onCheckedChange = onChange,
    )
  }
}

@Composable fun InstallTerminalAppItem(uriHandler: UriHandler) {
  SettingsActionItem(
    icon = painterResource(MR.images.ic_github),
    text = generalGetString(MR.strings.install_simplex_chat_for_terminal),
    click = { uriHandler.openExternalLink("https://github.com/simplex-chat/simplex-chat") },
    textColor = MaterialTheme.colors.primary
  )
}

@Composable fun ResetHintsItem(unchangedHints: MutableState<Boolean>) {
  SettingsActionItem(
    icon = painterResource(MR.images.ic_lightbulb),
    text = generalGetString(MR.strings.reset_all_hints),
    click = {
      RichHaptics.perform(HapticPattern.SUCCESS)
      resetHintPreferences()
      unchangedHints.value = true
    },
    disabled = unchangedHints.value,
    textColor = if (unchangedHints.value) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
  )
}

private fun resetHintPreferences() {
  appPreferences.hintPreferences.forEach { it.reset() }
}

fun unchangedHintPreferences(): Boolean = appPreferences.hintPreferences.all { it.isUnchanged() }

@Composable
fun AppVersionItem(showVersion: () -> Unit, versionInfo: Pair<String, Int?> = appVersionInfo) {
  SectionItemView({
    RichHaptics.perform(HapticPattern.CLICK)
    showVersion()
  }) {
    Row(
      Modifier.fillMaxWidth(),
      verticalAlignment = Alignment.CenterVertically
    ) {
      Icon(
        painterResource(MR.images.ic_info),
        contentDescription = null,
        tint = MaterialTheme.colors.primary.copy(alpha = 0.85f),
        modifier = Modifier.size(22.dp)
      )
      TextIconSpaced(false)
      AppVersionText(versionInfo)
    }
  }
}

@Composable fun AppVersionText(versionInfo: Pair<String, Int?> = appVersionInfo) {
  Text(
    versionInfo.first + (if (versionInfo.second != null) " (" + versionInfo.second + ")" else ""),
    color = MaterialTheme.colors.onSurface,
    fontWeight = FontWeight.Medium,
    fontSize = 15.5.sp
  )
}

@Composable fun ProfilePreview(profileOf: NamedChat, size: Dp = 60.dp, iconColor: Color = MaterialTheme.colors.secondaryVariant, textColor: Color = MaterialTheme.colors.onBackground, stopped: Boolean = false, badge: LocalBadge? = null) {
  ProfileImage(size = size, image = profileOf.image, color = iconColor)
  Spacer(Modifier.padding(horizontal = 8.dp))
  Column(Modifier.height(size), verticalArrangement = Arrangement.Center) {
    NameWithBadge(
      profileOf.displayName,
      badge,
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
fun SettingsActionItem(
  icon: Painter,
  text: String,
  click: (() -> Unit)? = null,
  textColor: Color = Color.Unspecified,
  iconColor: Color = MaterialTheme.colors.primary.copy(alpha = 0.85f),
  disabled: Boolean = false,
  extraPadding: Boolean = false
) {
  val onClick = if (click != null) {
    {
      RichHaptics.perform(HapticPattern.CLICK)
      click()
    }
  } else null

  SectionItemView(onClick, disabled = disabled, extraPadding = extraPadding) {
    Icon(
      icon,
      contentDescription = text,
      tint = if (disabled) MaterialTheme.colors.secondary else if (iconColor != MaterialTheme.colors.secondary) iconColor else MaterialTheme.colors.primary.copy(alpha = 0.85f),
      modifier = Modifier.size(22.dp)
    )
    TextIconSpaced(extraPadding)
    Text(
      text,
      color = if (disabled) MaterialTheme.colors.secondary else if (textColor != Color.Unspecified) textColor else MaterialTheme.colors.onSurface,
      fontWeight = FontWeight.Medium,
      fontSize = 15.5.sp
    )
  }
}

@Composable
fun SettingsActionItemWithContent(
  icon: Painter?,
  text: String? = null,
  click: (() -> Unit)? = null,
  iconColor: Color = MaterialTheme.colors.primary.copy(alpha = 0.85f),
  textColor: Color = MaterialTheme.colors.onSurface,
  disabled: Boolean = false,
  extraPadding: Boolean = false,
  content: @Composable RowScope.() -> Unit
) {
  val onClick = if (click != null) {
    {
      RichHaptics.perform(HapticPattern.CLICK)
      click()
    }
  } else null

  SectionItemView(
    onClick,
    extraPadding = extraPadding,
    padding = if (extraPadding && icon != null)
      PaddingValues(start = DEFAULT_PADDING * 1.7f, end = itemHPadding)
    else
      PaddingValues(horizontal = itemHPadding),
    disabled = disabled
  ) {
    if (icon != null) {
      Icon(
        icon,
        text,
        Modifier.size(22.dp),
        tint = if (disabled) MaterialTheme.colors.secondary else if (iconColor != MaterialTheme.colors.secondary) iconColor else MaterialTheme.colors.primary.copy(alpha = 0.85f)
      )
      TextIconSpaced(extraPadding)
    }
    if (text != null) {
      val padding = with(LocalDensity.current) { 6.sp.toDp() }
      Text(
        text,
        Modifier.weight(1f).padding(vertical = padding),
        color = if (disabled) MaterialTheme.colors.secondary else textColor,
        fontWeight = FontWeight.Medium,
        fontSize = 15.5.sp
      )
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
  iconColor: Color = MaterialTheme.colors.primary.copy(alpha = 0.85f),
  enabled: Boolean = true,
  onChange: ((Boolean) -> Unit)? = null,
) {
  SettingsActionItemWithContent(icon, text, iconColor = iconColor) {
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
  SettingsActionItemWithContent(
    icon = null,
    text = text,
    click = if (disabled) null else {
      {
        val next = !checked
        RichHaptics.perform(if (next) HapticPattern.TOGGLE_ON else HapticPattern.TOGGLE_OFF)
        onChange(next)
      }
    },
    disabled = disabled
  ) {
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
  iconColor: Color? = null,
  disabled: Boolean = false,
  checked: Boolean,
  extraPadding: Boolean = false,
  onChange: (Boolean) -> Unit = {},
) {
  SettingsActionItemWithContent(
    icon = icon,
    text = text,
    click = if (disabled) null else {
      {
        val next = !checked
        RichHaptics.perform(if (next) HapticPattern.TOGGLE_ON else HapticPattern.TOGGLE_OFF)
        onChange(next)
      }
    },
    iconColor = iconColor ?: MaterialTheme.colors.primary.copy(alpha = 0.85f),
    disabled = disabled,
    extraPadding = extraPadding
  ) {
    DefaultSwitch(
      checked = checked,
      onCheckedChange = onChange,
      enabled = !disabled
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
      Surface(color = MaterialTheme.colors.background.copy(1f), contentColor = LocalContentColor.current) {
        Box(
          Modifier.fillMaxSize(),
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

@Preview
@Composable
fun PreviewSettingsLayout() {
  SimpleXTheme {
    SettingsLayout(
      stopped = false,
      encrypted = false,
      passphraseSaved = false,
      setPerformLA = { _ -> },
      showModal = { {} },
      showSettingsModal = { {} },
      showSettingsModalWithSearch = { },
      showCustomModal = { {} },
      showVersion = {},
      withAuth = { _, _, _ -> },
      versionInfo = "7.1" to 172,
    )
  }
}

// 1. БЕЗОТКАЗНЫЙ КЭШ ДЛЯ USB PANIC
object FlexLocalPrefs {
    private var memoryCache = false
    private var isLoaded = false

    private fun getFile(): File {
        // Обязательно создаем папку перед тем как пытаться сохранить файл!
        val dir = File(getAppFilePath("flex_settings_dir"))
        if (!dir.exists()) dir.mkdirs() 
        return File(dir, "usb_panic_state.txt")
    }

    var usbPanic: Boolean
        get() {
            if (!isLoaded) {
                try { 
                    memoryCache = getFile().readText().trim() == "true" 
                } catch (e: Exception) { 
                    memoryCache = false 
                }
                isLoaded = true
            }
            return memoryCache
        }
        set(value) {
            memoryCache = value // Запоминаем в оперативной памяти на случай сбоев ФС
            try { getFile().writeText(value.toString()) } catch (e: Exception) {}
        }
}

@Composable
fun FlexSettingsView() {
    val usbPanicEnabled = rememberSaveable { mutableStateOf(chat.simplex.common.platform.UsbPanicDetector.isEnabled()) }
    val advancedHapticsEnabled = rememberSaveable { mutableStateOf(chat.simplex.common.platform.RichHaptics.isEnabled()) }

    fun toggleUsbPanic(newVal: Boolean) {
        usbPanicEnabled.value = newVal
        runCatching { chat.simplex.common.platform.UsbPanicDetector.setEnabled(newVal) }
    }

    fun toggleAdvancedHaptics(newVal: Boolean) {
        advancedHapticsEnabled.value = newVal
        runCatching { chat.simplex.common.platform.RichHaptics.setEnabled(newVal) }
    }

    ColumnWithScrollBar {
        AppBarTitle("FLEX Settings")

        SectionView(title = "SECURITY") {
            SettingsActionItemWithContent(
                icon = painterResource(MR.images.ic_security),
                text = "USB Panic",
                click = {
                    val next = !usbPanicEnabled.value
                    toggleUsbPanic(next)
                    runCatching { RichHaptics.perform(if (next) HapticPattern.TOGGLE_ON else HapticPattern.TOGGLE_OFF) }
                },
                iconColor = MaterialTheme.colors.primary.copy(alpha = 0.85f)
            ) {
                DefaultSwitch(
                    checked = usbPanicEnabled.value,
                    onCheckedChange = { newVal -> toggleUsbPanic(newVal) }
                )
            }
        }

        SectionDividerSpaced()

        SectionView(title = "INTERFACE & FEEDBACK") {
            SettingsActionItemWithContent(
                icon = painterResource(MR.images.ic_notifications),
                text = "Advanced Haptics",
                click = {
                    val next = !advancedHapticsEnabled.value
                    toggleAdvancedHaptics(next)
                    runCatching { RichHaptics.perform(if (next) HapticPattern.TOGGLE_ON else HapticPattern.TOGGLE_OFF) }
                },
                iconColor = WarningOrange
            ) {
                DefaultSwitch(
                    checked = advancedHapticsEnabled.value,
                    onCheckedChange = { newVal -> toggleAdvancedHaptics(newVal) }
                )
            }
        }

        SectionBottomSpacer()
    }
}
