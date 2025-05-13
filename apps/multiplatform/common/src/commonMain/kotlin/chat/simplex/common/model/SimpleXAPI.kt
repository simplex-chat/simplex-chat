package chat.simplex.common.model

import SectionItemView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import chat.simplex.common.views.helpers.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.Saver
import androidx.compose.runtime.saveable.listSaver
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.focus.focusRequester
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.ChatController.getNetCfg
import chat.simplex.common.model.ChatController.setNetCfg
import chat.simplex.common.model.ChatModel.changingActiveUserMutex
import chat.simplex.common.model.MsgContent.MCUnknown
import chat.simplex.common.model.SMPProxyFallback.AllowProtected
import chat.simplex.common.model.SMPProxyMode.Always
import dev.icerock.moko.resources.compose.painterResource
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.call.*
import chat.simplex.common.views.chat.item.showContentBlockedAlert
import chat.simplex.common.views.chat.item.showQuotedItemDoesNotExistAlert
import chat.simplex.common.views.chatlist.openGroupChat
import chat.simplex.common.views.migration.MigrationFileLinkData
import chat.simplex.common.views.onboarding.OnboardingStage
import chat.simplex.common.views.usersettings.*
import chat.simplex.common.views.usersettings.networkAndServers.serverHostname
import com.charleskorn.kaml.Yaml
import com.charleskorn.kaml.YamlConfiguration
import chat.simplex.res.MR
import com.russhwolf.settings.Settings
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.StringResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.*
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.sync.withLock
import kotlinx.datetime.Clock
import kotlinx.datetime.Instant
import kotlinx.serialization.*
import kotlinx.serialization.builtins.*
import kotlinx.serialization.descriptors.*
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.json.*
import java.util.Date

typealias ChatCtrl = Long

// version range that supports establishing direct connection with a group member (xGrpDirectInvVRange in core)
val CREATE_MEMBER_CONTACT_VERSION = 2

enum class CallOnLockScreen {
  DISABLE,
  SHOW,
  ACCEPT;

  companion object {
    val default = SHOW
  }
}

enum class SimplexLinkMode {
  DESCRIPTION,
  FULL,
  BROWSER;

  companion object {
    val default = DESCRIPTION
  }
}

class AppPreferences {
  // deprecated, remove in 2024
  private val runServiceInBackground = mkBoolPreference(SHARED_PREFS_RUN_SERVICE_IN_BACKGROUND, true)
  val notificationsMode = mkEnumPreference(
    SHARED_PREFS_NOTIFICATIONS_MODE,
    if (!runServiceInBackground.get()) NotificationsMode.OFF else NotificationsMode.default
  )  { NotificationsMode.values().firstOrNull { it.name == this } }
  val notificationPreviewMode = mkStrPreference(SHARED_PREFS_NOTIFICATION_PREVIEW_MODE, NotificationPreviewMode.default.name)
  val canAskToEnableNotifications = mkBoolPreference(SHARED_PREFS_CAN_ASK_TO_ENABLE_NOTIFICATIONS, true)
  val backgroundServiceNoticeShown = mkBoolPreference(SHARED_PREFS_SERVICE_NOTICE_SHOWN, false)
  val backgroundServiceBatteryNoticeShown = mkBoolPreference(SHARED_PREFS_SERVICE_BATTERY_NOTICE_SHOWN, false)
  val autoRestartWorkerVersion = mkIntPreference(SHARED_PREFS_AUTO_RESTART_WORKER_VERSION, 0)
  val webrtcPolicyRelay = mkBoolPreference(SHARED_PREFS_WEBRTC_POLICY_RELAY, true)
  val callOnLockScreen: SharedPreference<CallOnLockScreen> = mkSafeEnumPreference(SHARED_PREFS_WEBRTC_CALLS_ON_LOCK_SCREEN, CallOnLockScreen.default)
  val performLA = mkBoolPreference(SHARED_PREFS_PERFORM_LA, false)
  val laMode = mkEnumPreference(SHARED_PREFS_LA_MODE, LAMode.default) { LAMode.values().firstOrNull { it.name == this } }
  val laLockDelay = mkIntPreference(SHARED_PREFS_LA_LOCK_DELAY, 30)
  val laNoticeShown = mkBoolPreference(SHARED_PREFS_LA_NOTICE_SHOWN, false)
  val webrtcIceServers = mkStrPreference(SHARED_PREFS_WEBRTC_ICE_SERVERS, null)
  val privacyProtectScreen = mkBoolPreference(SHARED_PREFS_PRIVACY_PROTECT_SCREEN, true)
  val privacyAcceptImages = mkBoolPreference(SHARED_PREFS_PRIVACY_ACCEPT_IMAGES, true)
  val privacyLinkPreviews = mkBoolPreference(SHARED_PREFS_PRIVACY_LINK_PREVIEWS, true)
  val privacyChatListOpenLinks = mkEnumPreference(SHARED_PREFS_PRIVACY_CHAT_LIST_OPEN_LINKS, PrivacyChatListOpenLinksMode.ASK) { PrivacyChatListOpenLinksMode.values().firstOrNull { it.name == this } }
  val simplexLinkMode: SharedPreference<SimplexLinkMode> = mkSafeEnumPreference(SHARED_PREFS_PRIVACY_SIMPLEX_LINK_MODE, SimplexLinkMode.default)
  val privacyShowChatPreviews = mkBoolPreference(SHARED_PREFS_PRIVACY_SHOW_CHAT_PREVIEWS, true)
  val privacySaveLastDraft = mkBoolPreference(SHARED_PREFS_PRIVACY_SAVE_LAST_DRAFT, true)
  val privacyShortLinks = mkBoolPreference(SHARED_PREFS_PRIVACY_SHORT_LINKS, false)
  val privacyDeliveryReceiptsSet = mkBoolPreference(SHARED_PREFS_PRIVACY_DELIVERY_RECEIPTS_SET, false)
  val privacyEncryptLocalFiles = mkBoolPreference(SHARED_PREFS_PRIVACY_ENCRYPT_LOCAL_FILES, true)
  val privacyAskToApproveRelays = mkBoolPreference(SHARED_PREFS_PRIVACY_ASK_TO_APPROVE_RELAYS, true)
  val privacyMediaBlurRadius = mkIntPreference(SHARED_PREFS_PRIVACY_MEDIA_BLUR_RADIUS, 0)
  // Blur broken on Android 12, see https://github.com/chrisbanes/haze/issues/77. And not available before 12
  val deviceSupportsBlur = appPlatform.isDesktop || (platform.androidApiLevel ?: 0) >= 32
  val appearanceBarsBlurRadius = mkIntPreference(SHARED_PREFS_APPEARANCE_BARS_BLUR_RADIUS, if (deviceSupportsBlur) 50 else 0)
  val experimentalCalls = mkBoolPreference(SHARED_PREFS_EXPERIMENTAL_CALLS, false)
  val showUnreadAndFavorites = mkBoolPreference(SHARED_PREFS_SHOW_UNREAD_AND_FAVORITES, false)
  val chatArchiveName = mkStrPreference(SHARED_PREFS_CHAT_ARCHIVE_NAME, null)
  val chatArchiveTime = mkDatePreference(SHARED_PREFS_CHAT_ARCHIVE_TIME, null)
  val chatLastStart = mkDatePreference(SHARED_PREFS_CHAT_LAST_START, null)
  val chatStopped = mkBoolPreference(SHARED_PREFS_CHAT_STOPPED, false)
  val developerTools = mkBoolPreference(SHARED_PREFS_DEVELOPER_TOOLS, false)
  val logLevel = mkEnumPreference(SHARED_PREFS_LOG_LEVEL, LogLevel.WARNING) { LogLevel.entries.firstOrNull { it.name == this } }
  val showInternalErrors = mkBoolPreference(SHARED_PREFS_SHOW_INTERNAL_ERRORS, false)
  val showSlowApiCalls = mkBoolPreference(SHARED_PREFS_SHOW_SLOW_API_CALLS, false)
  val terminalAlwaysVisible = mkBoolPreference(SHARED_PREFS_TERMINAL_ALWAYS_VISIBLE, false)
  val networkUseSocksProxy = mkBoolPreference(SHARED_PREFS_NETWORK_USE_SOCKS_PROXY, false)
  val networkShowSubscriptionPercentage = mkBoolPreference(SHARED_PREFS_NETWORK_SHOW_SUBSCRIPTION_PERCENTAGE, false)
  private val _networkProxy = mkStrPreference(SHARED_PREFS_NETWORK_PROXY_HOST_PORT, json.encodeToString(NetworkProxy()))
  val networkProxy: SharedPreference<NetworkProxy> = SharedPreference(
    get = fun(): NetworkProxy {
      val value = _networkProxy.get() ?: return NetworkProxy()
      return try {
        if (value.startsWith("{")) {
          json.decodeFromString(value)
        } else {
          NetworkProxy(host = value.substringBefore(":").ifBlank { "localhost" }, port = value.substringAfter(":").toIntOrNull() ?: 9050)
        }
      } catch (e: Throwable) {
        NetworkProxy()
      }
    },
    set = fun(proxy: NetworkProxy) { _networkProxy.set(json.encodeToString(proxy)) }
  )
  val networkSessionMode: SharedPreference<TransportSessionMode> = mkSafeEnumPreference(SHARED_PREFS_NETWORK_SESSION_MODE, TransportSessionMode.default)
  val networkSMPProxyMode: SharedPreference<SMPProxyMode> = mkSafeEnumPreference(SHARED_PREFS_NETWORK_SMP_PROXY_MODE, SMPProxyMode.default)
  val networkSMPProxyFallback: SharedPreference<SMPProxyFallback> = mkSafeEnumPreference(SHARED_PREFS_NETWORK_SMP_PROXY_FALLBACK, SMPProxyFallback.default)
  val networkHostMode: SharedPreference<HostMode> = mkSafeEnumPreference(SHARED_PREFS_NETWORK_HOST_MODE, HostMode.default)
  val networkRequiredHostMode = mkBoolPreference(SHARED_PREFS_NETWORK_REQUIRED_HOST_MODE, false)
  val networkSMPWebPortServers: SharedPreference<SMPWebPortServers> = mkSafeEnumPreference(SHARED_PREFS_NETWORK_SMP_WEB_PORT_SERVERS, SMPWebPortServers.default)
  val networkTCPConnectTimeout = mkTimeoutPreference(SHARED_PREFS_NETWORK_TCP_CONNECT_TIMEOUT, NetCfg.defaults.tcpConnectTimeout, NetCfg.proxyDefaults.tcpConnectTimeout)
  val networkTCPTimeout = mkTimeoutPreference(SHARED_PREFS_NETWORK_TCP_TIMEOUT, NetCfg.defaults.tcpTimeout, NetCfg.proxyDefaults.tcpTimeout)
  val networkTCPTimeoutPerKb = mkTimeoutPreference(SHARED_PREFS_NETWORK_TCP_TIMEOUT_PER_KB, NetCfg.defaults.tcpTimeoutPerKb, NetCfg.proxyDefaults.tcpTimeoutPerKb)
  val networkRcvConcurrency = mkIntPreference(SHARED_PREFS_NETWORK_RCV_CONCURRENCY, NetCfg.defaults.rcvConcurrency)
  val networkSMPPingInterval = mkLongPreference(SHARED_PREFS_NETWORK_SMP_PING_INTERVAL, NetCfg.defaults.smpPingInterval)
  val networkSMPPingCount = mkIntPreference(SHARED_PREFS_NETWORK_SMP_PING_COUNT, NetCfg.defaults.smpPingCount)
  val networkEnableKeepAlive = mkBoolPreference(SHARED_PREFS_NETWORK_ENABLE_KEEP_ALIVE, NetCfg.defaults.enableKeepAlive)
  val networkTCPKeepIdle = mkIntPreference(SHARED_PREFS_NETWORK_TCP_KEEP_IDLE, KeepAliveOpts.defaults.keepIdle)
  val networkTCPKeepIntvl = mkIntPreference(SHARED_PREFS_NETWORK_TCP_KEEP_INTVL, KeepAliveOpts.defaults.keepIntvl)
  val networkTCPKeepCnt = mkIntPreference(SHARED_PREFS_NETWORK_TCP_KEEP_CNT, KeepAliveOpts.defaults.keepCnt)
  val incognito = mkBoolPreference(SHARED_PREFS_INCOGNITO, false)
  val liveMessageAlertShown = mkBoolPreference(SHARED_PREFS_LIVE_MESSAGE_ALERT_SHOWN, false)
  val showHiddenProfilesNotice = mkBoolPreference(SHARED_PREFS_SHOW_HIDDEN_PROFILES_NOTICE, true)
  val oneHandUICardShown = mkBoolPreference(SHARED_PREFS_ONE_HAND_UI_CARD_SHOWN, false)
  val addressCreationCardShown = mkBoolPreference(SHARED_PREFS_ADDRESS_CREATION_CARD_SHOWN, false)
  val showMuteProfileAlert = mkBoolPreference(SHARED_PREFS_SHOW_MUTE_PROFILE_ALERT, true)
  val appLanguage = mkStrPreference(SHARED_PREFS_APP_LANGUAGE, null)
  val appUpdateChannel = mkEnumPreference(SHARED_PREFS_APP_UPDATE_CHANNEL, AppUpdatesChannel.DISABLED) { AppUpdatesChannel.entries.firstOrNull { it.name == this } }
  val appSkippedUpdate = mkStrPreference(SHARED_PREFS_APP_SKIPPED_UPDATE, "")
  val appUpdateNoticeShown = mkBoolPreference(SHARED_PREFS_APP_UPDATE_NOTICE_SHOWN, false)

  val onboardingStage = mkEnumPreference(SHARED_PREFS_ONBOARDING_STAGE, OnboardingStage.OnboardingComplete) { OnboardingStage.values().firstOrNull { it.name == this } }
  val migrationToStage = mkStrPreference(SHARED_PREFS_MIGRATION_TO_STAGE, null)
  val migrationFromStage = mkStrPreference(SHARED_PREFS_MIGRATION_FROM_STAGE, null)
  val storeDBPassphrase = mkBoolPreference(SHARED_PREFS_STORE_DB_PASSPHRASE, true)
  val initialRandomDBPassphrase = mkBoolPreference(SHARED_PREFS_INITIAL_RANDOM_DB_PASSPHRASE, false)
  val encryptedDBPassphrase = mkStrPreference(SHARED_PREFS_ENCRYPTED_DB_PASSPHRASE, null)
  val initializationVectorDBPassphrase = mkStrPreference(SHARED_PREFS_INITIALIZATION_VECTOR_DB_PASSPHRASE, null)
  val encryptedAppPassphrase = mkStrPreference(SHARED_PREFS_ENCRYPTED_APP_PASSPHRASE, null)
  val initializationVectorAppPassphrase = mkStrPreference(SHARED_PREFS_INITIALIZATION_VECTOR_APP_PASSPHRASE, null)
  val encryptedSelfDestructPassphrase = mkStrPreference(SHARED_PREFS_ENCRYPTED_SELF_DESTRUCT_PASSPHRASE, null)
  val initializationVectorSelfDestructPassphrase = mkStrPreference(SHARED_PREFS_INITIALIZATION_VECTOR_SELF_DESTRUCT_PASSPHRASE, null)
  val encryptionStartedAt = mkDatePreference(SHARED_PREFS_ENCRYPTION_STARTED_AT, null)
  val confirmDBUpgrades = mkBoolPreference(SHARED_PREFS_CONFIRM_DB_UPGRADES, false)
  val selfDestruct = mkBoolPreference(SHARED_PREFS_SELF_DESTRUCT, false)
  val selfDestructDisplayName = mkStrPreference(SHARED_PREFS_SELF_DESTRUCT_DISPLAY_NAME, null)

  // This flag is set when database is first initialized and resets only when the database is removed.
  // This is needed for recover from incomplete initialization when only one database file is created.
  // If false - the app will clear database folder on missing file and re-initialize.
  // Note that this situation can only happen if passphrase for the first database is incorrect because, otherwise, backend will re-create second database automatically
  val newDatabaseInitialized = mkBoolPreference(SHARED_PREFS_NEW_DATABASE_INITIALIZED, false)

  /** after importing new database, this flag will be set and unset only after importing app settings in [initChatController] */
  val shouldImportAppSettings = mkBoolPreference(SHARED_PREFS_SHOULD_IMPORT_APP_SETTINGS, false)

  val currentTheme = mkStrPreference(SHARED_PREFS_CURRENT_THEME, DefaultTheme.SYSTEM_THEME_NAME)
  val systemDarkTheme = mkStrPreference(SHARED_PREFS_SYSTEM_DARK_THEME, DefaultTheme.SIMPLEX.themeName)
  val currentThemeIds = mkMapPreference(SHARED_PREFS_CURRENT_THEME_IDs, mapOf(), encode = {
    json.encodeToString(MapSerializer(String.serializer(), String.serializer()), it)
  }, decode = {
    json.decodeFromString(MapSerializer(String.serializer(), String.serializer()), it)
  })
  // Deprecated. Remove key from preferences in 2025
  val themeOverridesOld = mkMapPreference(SHARED_PREFS_THEMES_OLD, mapOf(), encode = {
    json.encodeToString(MapSerializer(String.serializer(), ThemeOverrides.serializer()), it)
  }, decode = {
    jsonCoerceInputValues.decodeFromString(MapSerializer(String.serializer(), ThemeOverrides.serializer()), it)
  }, settingsThemes)
  val themeOverrides = mkThemeOverridesPreference()
  val profileImageCornerRadius = mkFloatPreference(SHARED_PREFS_PROFILE_IMAGE_CORNER_RADIUS, 22.5f)
  val chatItemRoundness = mkFloatPreference(SHARED_PREFS_CHAT_ITEM_ROUNDNESS, 0.75f)
  val chatItemTail = mkBoolPreference(SHARED_PREFS_CHAT_ITEM_TAIL, true)
  val fontScale = mkFloatPreference(SHARED_PREFS_FONT_SCALE, 1f)
  val densityScale = mkFloatPreference(SHARED_PREFS_DENSITY_SCALE, 1f)
  val inAppBarsDefaultAlpha = if (deviceSupportsBlur) 0.875f else 0.975f
  val inAppBarsAlpha = mkFloatPreference(SHARED_PREFS_IN_APP_BARS_ALPHA, inAppBarsDefaultAlpha)

  val whatsNewVersion = mkStrPreference(SHARED_PREFS_WHATS_NEW_VERSION, null)
  val lastMigratedVersionCode = mkIntPreference(SHARED_PREFS_LAST_MIGRATED_VERSION_CODE, 0)
  val customDisappearingMessageTime = mkIntPreference(SHARED_PREFS_CUSTOM_DISAPPEARING_MESSAGE_TIME, 300)
  val deviceNameForRemoteAccess = mkStrPreference(SHARED_PREFS_DEVICE_NAME_FOR_REMOTE_ACCESS, deviceName)

  val confirmRemoteSessions = mkBoolPreference(SHARED_PREFS_CONFIRM_REMOTE_SESSIONS, false)
  val connectRemoteViaMulticast = mkBoolPreference(SHARED_PREFS_CONNECT_REMOTE_VIA_MULTICAST, false)
  val connectRemoteViaMulticastAuto = mkBoolPreference(SHARED_PREFS_CONNECT_REMOTE_VIA_MULTICAST_AUTO, true)
  val offerRemoteMulticast = mkBoolPreference(SHARED_PREFS_OFFER_REMOTE_MULTICAST, true)

  val desktopWindowState = mkStrPreference(SHARED_PREFS_DESKTOP_WINDOW_STATE, null)

  val showDeleteConversationNotice = mkBoolPreference(SHARED_PREFS_SHOW_DELETE_CONVERSATION_NOTICE, true)
  val showDeleteContactNotice = mkBoolPreference(SHARED_PREFS_SHOW_DELETE_CONTACT_NOTICE, true)
  val showSentViaProxy = mkBoolPreference(SHARED_PREFS_SHOW_SENT_VIA_RPOXY, false)


  val iosCallKitEnabled = mkBoolPreference(SHARED_PREFS_IOS_CALL_KIT_ENABLED, true)
  val iosCallKitCallsInRecents = mkBoolPreference(SHARED_PREFS_IOS_CALL_KIT_CALLS_IN_RECENTS, false)

  val oneHandUI = mkBoolPreference(SHARED_PREFS_ONE_HAND_UI, true)
  val chatBottomBar = mkBoolPreference(SHARED_PREFS_CHAT_BOTTOM_BAR, true)

  val hintPreferences: List<Pair<SharedPreference<Boolean>, Boolean>> = listOf(
    laNoticeShown to false,
    oneHandUICardShown to false,
    addressCreationCardShown to false,
    liveMessageAlertShown to false,
    showHiddenProfilesNotice to true,
    showMuteProfileAlert to true,
    showDeleteConversationNotice to true,
    showDeleteContactNotice to true,
  )

  private fun mkIntPreference(prefName: String, default: Int) =
    SharedPreference(
      get = fun() = settings.getInt(prefName, default),
      set = fun(value) = settings.putInt(prefName, value)
    )

  private fun mkLongPreference(prefName: String, default: Long) =
    SharedPreference(
      get = fun() = settings.getLong(prefName, default),
      set = fun(value) = settings.putLong(prefName, value)
    )

  private fun mkFloatPreference(prefName: String, default: Float) =
    SharedPreference(
      get = fun() = settings.getFloat(prefName, default),
      set = fun(value) = settings.putFloat(prefName, value)
    )

  private fun mkTimeoutPreference(prefName: String, default: Long, proxyDefault: Long): SharedPreference<Long> {
    val d = if (networkUseSocksProxy.get()) proxyDefault else default
    return SharedPreference(
      get = fun() = settings.getLong(prefName, d),
      set = fun(value) = settings.putLong(prefName, value)
    )
  }

  private fun mkBoolPreference(prefName: String, default: Boolean) =
    SharedPreference(
      get = fun() = settings.getBoolean(prefName, default),
      set = fun(value) = settings.putBoolean(prefName, value)
    )

  private fun mkStrPreference(prefName: String, default: String?): SharedPreference<String?> =
    SharedPreference(
      get = {
        val nullValue = "----------------------"
        val pref = settings.getString(prefName, default ?: nullValue)
        if (pref != nullValue) {
          pref
        } else {
          null
        }
      },
      set = fun(value) = if (value != null) settings.putString(prefName, value) else settings.remove(prefName)
    )

  private fun <T> mkEnumPreference(prefName: String, default: T, construct: String.() -> T?): SharedPreference<T> =
    SharedPreference(
      get = fun() = settings.getString(prefName, default.toString()).construct() ?: default,
      set = fun(value) = settings.putString(prefName, value.toString())
    )

  private inline fun <reified T : Enum<T>> mkSafeEnumPreference(key: String, default: T): SharedPreference<T> = SharedPreference(
    get = {
      val value = settings.getString(key, "")
      if (value == "") return@SharedPreference default
      try {
        enumValueOf<T>(value)
      } catch (e: IllegalArgumentException) {
        default
      }
    },
    set = { value -> settings.putString(key, value.name) }
  )

  private fun mkDatePreference(prefName: String, default: Instant?): SharedPreference<Instant?> =
    SharedPreference(
      get = {
        val nullValue = "----------------------"
        val pref = settings.getString(prefName, default?.toEpochMilliseconds()?.toString() ?: nullValue)
        if (pref != nullValue) {
          Instant.fromEpochMilliseconds(pref.toLong())
        } else {
          null
        }
      },
      set = fun(value) = if (value?.toEpochMilliseconds() != null) settings.putString(prefName, value.toEpochMilliseconds().toString()) else settings.remove(prefName)
    )

  private fun <K, V> mkMapPreference(prefName: String, default: Map<K, V>, encode: (Map<K, V>) -> String, decode: (String) -> Map<K, V>, prefs: Settings = settings): SharedPreference<Map<K,V>> =
    SharedPreference(
      get = fun() = decode(prefs.getString(prefName, encode(default))),
      set = fun(value) = prefs.putString(prefName, encode(value))
    )

  private fun mkThemeOverridesPreference(): SharedPreference<List<ThemeOverrides>> =
    SharedPreference(
      get = fun() = themeOverridesStore ?: (readThemeOverrides()).also { themeOverridesStore = it },
      set = fun(value) { if (writeThemeOverrides(value)) { themeOverridesStore = value } }
    )

  companion object {
    const val SHARED_PREFS_ID = "chat.simplex.app.SIMPLEX_APP_PREFS"
    internal const val SHARED_PREFS_THEMES_ID = "chat.simplex.app.THEMES"
    private const val SHARED_PREFS_AUTO_RESTART_WORKER_VERSION = "AutoRestartWorkerVersion"
    private const val SHARED_PREFS_RUN_SERVICE_IN_BACKGROUND = "RunServiceInBackground"
    private const val SHARED_PREFS_NOTIFICATIONS_MODE = "NotificationsMode"
    private const val SHARED_PREFS_NOTIFICATION_PREVIEW_MODE = "NotificationPreviewMode"
    private const val SHARED_PREFS_CAN_ASK_TO_ENABLE_NOTIFICATIONS = "CanAskToEnableNotifications"
    private const val SHARED_PREFS_SERVICE_NOTICE_SHOWN = "BackgroundServiceNoticeShown"
    private const val SHARED_PREFS_SERVICE_BATTERY_NOTICE_SHOWN = "BackgroundServiceBatteryNoticeShown"
    private const val SHARED_PREFS_WEBRTC_POLICY_RELAY = "WebrtcPolicyRelay"
    private const val SHARED_PREFS_WEBRTC_CALLS_ON_LOCK_SCREEN = "CallsOnLockScreen"
    private const val SHARED_PREFS_PERFORM_LA = "PerformLA"
    private const val SHARED_PREFS_LA_MODE = "LocalAuthenticationMode"
    private const val SHARED_PREFS_LA_LOCK_DELAY = "LocalAuthenticationLockDelay"
    private const val SHARED_PREFS_LA_NOTICE_SHOWN = "LANoticeShown"
    private const val SHARED_PREFS_WEBRTC_ICE_SERVERS = "WebrtcICEServers"
    private const val SHARED_PREFS_PRIVACY_PROTECT_SCREEN = "PrivacyProtectScreen"
    private const val SHARED_PREFS_PRIVACY_ACCEPT_IMAGES = "PrivacyAcceptImages"
    private const val SHARED_PREFS_PRIVACY_TRANSFER_IMAGES_INLINE = "PrivacyTransferImagesInline"
    private const val SHARED_PREFS_PRIVACY_LINK_PREVIEWS = "PrivacyLinkPreviews"
    private const val SHARED_PREFS_PRIVACY_CHAT_LIST_OPEN_LINKS = "ChatListOpenLinks"
    private const val SHARED_PREFS_PRIVACY_SIMPLEX_LINK_MODE = "PrivacySimplexLinkMode"
    private const val SHARED_PREFS_PRIVACY_SHOW_CHAT_PREVIEWS = "PrivacyShowChatPreviews"
    private const val SHARED_PREFS_PRIVACY_SAVE_LAST_DRAFT = "PrivacySaveLastDraft"
    private const val SHARED_PREFS_PRIVACY_SHORT_LINKS = "PrivacyShortLinks"
    private const val SHARED_PREFS_PRIVACY_DELIVERY_RECEIPTS_SET = "PrivacyDeliveryReceiptsSet"
    private const val SHARED_PREFS_PRIVACY_ENCRYPT_LOCAL_FILES = "PrivacyEncryptLocalFiles"
    private const val SHARED_PREFS_PRIVACY_ASK_TO_APPROVE_RELAYS = "PrivacyAskToApproveRelays"
    private const val SHARED_PREFS_PRIVACY_MEDIA_BLUR_RADIUS = "PrivacyMediaBlurRadius"
    private const val SHARED_PREFS_APPEARANCE_BARS_BLUR_RADIUS = "AppearanceBarsBlurRadius"
    const val SHARED_PREFS_PRIVACY_FULL_BACKUP = "FullBackup"
    private const val SHARED_PREFS_EXPERIMENTAL_CALLS = "ExperimentalCalls"
    private const val SHARED_PREFS_SHOW_UNREAD_AND_FAVORITES = "ShowUnreadAndFavorites"
    private const val SHARED_PREFS_CHAT_ARCHIVE_NAME = "ChatArchiveName"
    private const val SHARED_PREFS_CHAT_ARCHIVE_TIME = "ChatArchiveTime"
    private const val SHARED_PREFS_APP_LANGUAGE = "AppLanguage"
    private const val SHARED_PREFS_APP_UPDATE_CHANNEL = "AppUpdateChannel"
    private const val SHARED_PREFS_APP_SKIPPED_UPDATE = "AppSkippedUpdate"
    private const val SHARED_PREFS_APP_UPDATE_NOTICE_SHOWN = "AppUpdateNoticeShown"
    private const val SHARED_PREFS_ONBOARDING_STAGE = "OnboardingStage"
    const val SHARED_PREFS_MIGRATION_TO_STAGE = "MigrationToStage"
    const val SHARED_PREFS_MIGRATION_FROM_STAGE = "MigrationFromStage"
    private const val SHARED_PREFS_CHAT_LAST_START = "ChatLastStart"
    private const val SHARED_PREFS_CHAT_STOPPED = "ChatStopped"
    private const val SHARED_PREFS_DEVELOPER_TOOLS = "DeveloperTools"
    private const val SHARED_PREFS_LOG_LEVEL = "LogLevel"
    private const val SHARED_PREFS_SHOW_INTERNAL_ERRORS = "ShowInternalErrors"
    private const val SHARED_PREFS_SHOW_SLOW_API_CALLS = "ShowSlowApiCalls"
    private const val SHARED_PREFS_TERMINAL_ALWAYS_VISIBLE = "TerminalAlwaysVisible"
    private const val SHARED_PREFS_NETWORK_USE_SOCKS_PROXY = "NetworkUseSocksProxy"
    private const val SHARED_PREFS_NETWORK_SHOW_SUBSCRIPTION_PERCENTAGE = "ShowSubscriptionPercentage"
    private const val SHARED_PREFS_NETWORK_PROXY_HOST_PORT = "NetworkProxyHostPort"
    private const val SHARED_PREFS_NETWORK_SESSION_MODE = "NetworkSessionMode"
    private const val SHARED_PREFS_NETWORK_SMP_PROXY_MODE = "NetworkSMPProxyMode"
    private const val SHARED_PREFS_NETWORK_SMP_PROXY_FALLBACK = "NetworkSMPProxyFallback"
    private const val SHARED_PREFS_NETWORK_HOST_MODE = "NetworkHostMode"
    private const val SHARED_PREFS_NETWORK_REQUIRED_HOST_MODE = "NetworkRequiredHostMode"
    private const val SHARED_PREFS_NETWORK_SMP_WEB_PORT_SERVERS = "NetworkSMPWebPortServers"
    private const val SHARED_PREFS_NETWORK_TCP_CONNECT_TIMEOUT = "NetworkTCPConnectTimeout"
    private const val SHARED_PREFS_NETWORK_TCP_TIMEOUT = "NetworkTCPTimeout"
    private const val SHARED_PREFS_NETWORK_TCP_TIMEOUT_PER_KB = "networkTCPTimeoutPerKb"
    private const val SHARED_PREFS_NETWORK_RCV_CONCURRENCY = "networkRcvConcurrency"
    private const val SHARED_PREFS_NETWORK_SMP_PING_INTERVAL = "NetworkSMPPingInterval"
    private const val SHARED_PREFS_NETWORK_SMP_PING_COUNT = "NetworkSMPPingCount"
    private const val SHARED_PREFS_NETWORK_ENABLE_KEEP_ALIVE = "NetworkEnableKeepAlive"
    private const val SHARED_PREFS_NETWORK_TCP_KEEP_IDLE = "NetworkTCPKeepIdle"
    private const val SHARED_PREFS_NETWORK_TCP_KEEP_INTVL = "NetworkTCPKeepIntvl"
    private const val SHARED_PREFS_NETWORK_TCP_KEEP_CNT = "NetworkTCPKeepCnt"
    private const val SHARED_PREFS_INCOGNITO = "Incognito"
    private const val SHARED_PREFS_LIVE_MESSAGE_ALERT_SHOWN = "LiveMessageAlertShown"
    private const val SHARED_PREFS_SHOW_HIDDEN_PROFILES_NOTICE = "ShowHiddenProfilesNotice"
    private const val SHARED_PREFS_ONE_HAND_UI_CARD_SHOWN = "OneHandUICardShown"
    private const val SHARED_PREFS_ADDRESS_CREATION_CARD_SHOWN = "AddressCreationCardShown"
    private const val SHARED_PREFS_SHOW_MUTE_PROFILE_ALERT = "ShowMuteProfileAlert"
    private const val SHARED_PREFS_STORE_DB_PASSPHRASE = "StoreDBPassphrase"
    private const val SHARED_PREFS_INITIAL_RANDOM_DB_PASSPHRASE = "InitialRandomDBPassphrase"
    private const val SHARED_PREFS_ENCRYPTED_DB_PASSPHRASE = "EncryptedDBPassphrase"
    private const val SHARED_PREFS_INITIALIZATION_VECTOR_DB_PASSPHRASE = "InitializationVectorDBPassphrase"
    private const val SHARED_PREFS_ENCRYPTED_APP_PASSPHRASE = "EncryptedAppPassphrase"
    private const val SHARED_PREFS_INITIALIZATION_VECTOR_APP_PASSPHRASE = "InitializationVectorAppPassphrase"
    private const val SHARED_PREFS_ENCRYPTED_SELF_DESTRUCT_PASSPHRASE = "EncryptedSelfDestructPassphrase"
    private const val SHARED_PREFS_INITIALIZATION_VECTOR_SELF_DESTRUCT_PASSPHRASE = "InitializationVectorSelfDestructPassphrase"
    private const val SHARED_PREFS_ENCRYPTION_STARTED_AT = "EncryptionStartedAt"
    private const val SHARED_PREFS_NEW_DATABASE_INITIALIZED = "NewDatabaseInitialized"
    private const val SHARED_PREFS_SHOULD_IMPORT_APP_SETTINGS = "ShouldImportAppSettings"
    private const val SHARED_PREFS_CONFIRM_DB_UPGRADES = "ConfirmDBUpgrades"
    private const val SHARED_PREFS_ONE_HAND_UI = "OneHandUI"
    private const val SHARED_PREFS_CHAT_BOTTOM_BAR = "ChatBottomBar"
    private const val SHARED_PREFS_SELF_DESTRUCT = "LocalAuthenticationSelfDestruct"
    private const val SHARED_PREFS_SELF_DESTRUCT_DISPLAY_NAME = "LocalAuthenticationSelfDestructDisplayName"
    private const val SHARED_PREFS_PQ_EXPERIMENTAL_ENABLED = "PQExperimentalEnabled" // no longer used
    private const val SHARED_PREFS_CURRENT_THEME = "CurrentTheme"
    private const val SHARED_PREFS_CURRENT_THEME_IDs = "CurrentThemeIds"
    private const val SHARED_PREFS_SYSTEM_DARK_THEME = "SystemDarkTheme"
    private const val SHARED_PREFS_THEMES_OLD = "Themes"
    private const val SHARED_PREFS_PROFILE_IMAGE_CORNER_RADIUS = "ProfileImageCornerRadius"
    private const val SHARED_PREFS_CHAT_ITEM_ROUNDNESS = "ChatItemRoundness"
    private const val SHARED_PREFS_CHAT_ITEM_TAIL = "ChatItemTail"
    private const val SHARED_PREFS_FONT_SCALE = "FontScale"
    private const val SHARED_PREFS_DENSITY_SCALE = "DensityScale"
    private const val SHARED_PREFS_IN_APP_BARS_ALPHA = "InAppBarsAlpha"
    private const val SHARED_PREFS_WHATS_NEW_VERSION = "WhatsNewVersion"
    private const val SHARED_PREFS_LAST_MIGRATED_VERSION_CODE = "LastMigratedVersionCode"
    private const val SHARED_PREFS_CUSTOM_DISAPPEARING_MESSAGE_TIME = "CustomDisappearingMessageTime"
    private const val SHARED_PREFS_DEVICE_NAME_FOR_REMOTE_ACCESS = "DeviceNameForRemoteAccess"
    private const val SHARED_PREFS_CONFIRM_REMOTE_SESSIONS = "ConfirmRemoteSessions"
    private const val SHARED_PREFS_CONNECT_REMOTE_VIA_MULTICAST = "ConnectRemoteViaMulticast"
    private const val SHARED_PREFS_CONNECT_REMOTE_VIA_MULTICAST_AUTO = "ConnectRemoteViaMulticastAuto"
    private const val SHARED_PREFS_OFFER_REMOTE_MULTICAST = "OfferRemoteMulticast"
    private const val SHARED_PREFS_DESKTOP_WINDOW_STATE = "DesktopWindowState"
    private const val SHARED_PREFS_SHOW_DELETE_CONVERSATION_NOTICE = "showDeleteConversationNotice"
    private const val SHARED_PREFS_SHOW_DELETE_CONTACT_NOTICE = "showDeleteContactNotice"
    private const val SHARED_PREFS_SHOW_SENT_VIA_RPOXY = "showSentViaProxy"

    private const val SHARED_PREFS_IOS_CALL_KIT_ENABLED = "iOSCallKitEnabled"
    private const val SHARED_PREFS_IOS_CALL_KIT_CALLS_IN_RECENTS = "iOSCallKitCallsInRecents"

    private var themeOverridesStore: List<ThemeOverrides>? = null
  }
}

private const val MESSAGE_TIMEOUT: Int = 15_000_000

object ChatController {
  var ctrl: ChatCtrl? = -1
  val appPrefs: AppPreferences by lazy { AppPreferences() }

  val messagesChannel: Channel<API> = Channel()

  val chatModel = ChatModel
  private var receiverStarted = false
  var lastMsgReceivedTimestamp: Long = System.currentTimeMillis()
    private set

  fun hasChatCtrl() = ctrl != -1L && ctrl != null

  suspend fun getAgentSubsTotal(rh: Long?): Pair<SMPServerSubs, Boolean>? {
    val userId = currentUserId("getAgentSubsTotal")

    val r = sendCmd(rh, CC.GetAgentSubsTotal(userId), log = false)
    if (r is API.Result && r.res is CR.AgentSubsTotal) return r.res.subsTotal to r.res.hasSession
    Log.e(TAG, "getAgentSubsTotal bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun getAgentServersSummary(rh: Long?): PresentedServersSummary? {
    val userId = currentUserId("getAgentServersSummary")

    val r = sendCmd(rh, CC.GetAgentServersSummary(userId), log = false)
    if (r is API.Result && r.res is CR.AgentServersSummary) return r.res.serversSummary
    Log.e(TAG, "getAgentServersSummary bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun resetAgentServersStats(rh: Long?): Boolean = sendCommandOkResp(rh, CC.ResetAgentServersStats())

  private suspend fun currentUserId(funcName: String): Long = changingActiveUserMutex.withLock {
    val userId = chatModel.currentUser.value?.userId
    if (userId == null) {
      val error = "$funcName: no current user"
      Log.e(TAG, error)
      throw Exception(error)
    }
    userId
  }

  suspend fun startChat(user: User) {
    Log.d(TAG, "user: $user")
    try {
      apiSetNetworkConfig(getNetCfg())
      val chatRunning = apiCheckChatRunning()
      val users = listUsers(null)
      chatModel.users.clear()
      chatModel.users.addAll(users)
      if (!chatRunning) {
        chatModel.currentUser.value = user
        chatModel.localUserCreated.value = true
        getUserChatData(null)
        appPrefs.chatLastStart.set(Clock.System.now())
        chatModel.chatRunning.value = true
        startReceiver()
        setLocalDeviceName(appPrefs.deviceNameForRemoteAccess.get()!!)
        if (appPreferences.onboardingStage.get() == OnboardingStage.OnboardingComplete && !chatModel.controller.appPrefs.privacyDeliveryReceiptsSet.get()) {
          chatModel.setDeliveryReceipts.value = true
        }
        Log.d(TAG, "startChat: started")
      } else {
        withContext(Dispatchers.Main) {
          val chats = apiGetChats(null)
          chatModel.chatsContext.updateChats(chats)
        }
        Log.d(TAG, "startChat: running")
      }
      apiStartChat()
      appPrefs.chatStopped.set(false)
    } catch (e: Throwable) {
      Log.e(TAG, "failed starting chat $e")
      throw e
    }
  }

  suspend fun startChatWithoutUser() {
    Log.d(TAG, "user: null")
    try {
      if (chatModel.chatRunning.value == true) return
      chatModel.users.clear()
      chatModel.currentUser.value = null
      chatModel.localUserCreated.value = false
      appPrefs.chatLastStart.set(Clock.System.now())
      chatModel.chatRunning.value = true
      startReceiver()
      setLocalDeviceName(appPrefs.deviceNameForRemoteAccess.get()!!)
      Log.d(TAG, "startChat: started without user")
    } catch (e: Throwable) {
      Log.e(TAG, "failed starting chat without user $e")
      throw e
    }
  }

  suspend fun startChatWithTemporaryDatabase(ctrl: ChatCtrl, netCfg: NetCfg): User? {
    Log.d(TAG, "startChatWithTemporaryDatabase")
    val migrationActiveUser = apiGetActiveUser(null, ctrl) ?: apiCreateActiveUser(null, Profile(displayName = "Temp", fullName = ""), ctrl = ctrl)
    if (!apiSetNetworkConfig(netCfg, ctrl = ctrl)) {
      Log.e(TAG, "Error setting network config, stopping migration")
      return null
    }
    apiSetAppFilePaths(
      getMigrationTempFilesDirectory().absolutePath,
      getMigrationTempFilesDirectory().absolutePath,
      wallpapersDir.parentFile.absolutePath,
      remoteHostsDir.absolutePath,
      ctrl
    )
    apiStartChat(ctrl)
    return migrationActiveUser
  }

  suspend fun changeActiveUser(rhId: Long?, toUserId: Long, viewPwd: String?) {
    try {
      changeActiveUser_(rhId, toUserId, viewPwd)
    } catch (e: Exception) {
      Log.e(TAG, "Unable to set active user: ${e.stackTraceToString()}")
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.failed_to_active_user_title), e.stackTraceToString())
    }
  }

  suspend fun changeActiveUser_(rhId: Long?, toUserId: Long?, viewPwd: String?) {
    val prevActiveUser = chatModel.currentUser.value
    val currentUser = changingActiveUserMutex.withLock {
      (if (toUserId != null) apiSetActiveUser(rhId, toUserId, viewPwd) else apiGetActiveUser(rhId)).also {
        chatModel.currentUser.value = it
      }
    }
    if (prevActiveUser?.hidden == true) {
      ntfManager.cancelNotificationsForUser(prevActiveUser.userId)
    }
    val users = listUsers(rhId)
    chatModel.users.clear()
    chatModel.users.addAll(users)
    getUserChatData(rhId)
    val invitation = chatModel.callInvitations.values.firstOrNull { inv -> inv.user.userId == toUserId }
    if (invitation != null && currentUser != null) {
      chatModel.callManager.reportNewIncomingCall(invitation.copy(user = currentUser))
    }
  }

  suspend fun getUserChatData(rhId: Long?) {
    val hasUser = chatModel.currentUser.value != null
    chatModel.userAddress.value = if (hasUser) apiGetUserAddress(rhId) else null
    chatModel.chatItemTTL.value = if (hasUser) getChatItemTTL(rhId) else ChatItemTTL.None
    withContext(Dispatchers.Main) {
      val chats = apiGetChats(rhId)
      chatModel.chatsContext.updateChats(chats)
    }
    chatModel.userTags.value = apiGetChatTags(rhId).takeIf { hasUser } ?: emptyList()
    chatModel.activeChatTagFilter.value = null
    chatModel.updateChatTags(rhId)
  }

  private fun startReceiver() {
    Log.d(TAG, "ChatController startReceiver")
    if (receiverStarted) return
    receiverStarted = true
    CoroutineScope(Dispatchers.IO).launch {
      while (true) {
        /** Global [ctrl] can be null. It's needed for having the same [ChatModel] that already made in [ChatController] without the need
         * to change it everywhere in code after changing a database.
         * Since it can be changed in background thread, making this check to prevent NullPointerException */
        val ctrl = ctrl
        if (ctrl == null) {
          receiverStarted = false
          break
        }
        try {
          val msg = recvMsg(ctrl)
          if (msg != null) {
            val finishedWithoutTimeout = withTimeoutOrNull(60_000L) {
              processReceivedMsg(msg)
              messagesChannel.trySend(msg)
            }
            if (finishedWithoutTimeout == null) {
              Log.e(TAG, "Timeout reached while processing received message: " + msg.responseType)
              if (appPreferences.developerTools.get() && appPreferences.showSlowApiCalls.get()) {
                AlertManager.shared.showAlertMsg(
                  title = generalGetString(MR.strings.possible_slow_function_title),
                  text = generalGetString(MR.strings.possible_slow_function_desc).format(60, msg.responseType + "\n" + Exception().stackTraceToString()),
                  shareText = true
                )
              }
            }
          }
        } catch (e: Exception) {
          Log.e(TAG, "ChatController recvMsg/processReceivedMsg exception: " + e.stackTraceToString());
        } catch (e: Throwable) {
          Log.e(TAG, "ChatController recvMsg/processReceivedMsg throwable: " + e.stackTraceToString())
          AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error), e.stackTraceToString())
        }
      }
    }
  }

  suspend fun sendCmd(rhId: Long?, cmd: CC, otherCtrl: ChatCtrl? = null, log: Boolean = true): API {
    val ctrl = otherCtrl ?: ctrl ?: throw Exception("Controller is not initialized")

    return withContext(Dispatchers.IO) {
      val c = cmd.cmdString
      if (log) {
        chatModel.addTerminalItem(TerminalItem.cmd(rhId, cmd.obfuscated))
        Log.d(TAG, "sendCmd: ${cmd.cmdType}")
      }
      val rStr = if (rhId == null) chatSendCmd(ctrl, c) else chatSendRemoteCmd(ctrl, rhId.toInt(), c)
      // coroutine was cancelled already, no need to process response (helps with apiListMembers - very heavy query in large groups)
      interruptIfCancelled()
      val r = json.decodeFromString<API>(rStr)
      if (log) {
        Log.d(TAG, "sendCmd response type ${r.responseType}")
        if (r is API.Result && (r.res is CR.Response || r.res is CR.Invalid)) {
          Log.d(TAG, "sendCmd response json $rStr")
        }
        chatModel.addTerminalItem(TerminalItem.resp(rhId, r))
      }
      r
    }
  }

  fun recvMsg(ctrl: ChatCtrl): API? {
    val rStr = chatRecvMsgWait(ctrl, MESSAGE_TIMEOUT)
    return if (rStr == "") {
      null
    } else {
      val r = json.decodeFromString<API>(rStr)
      Log.d(TAG, "chatRecvMsg: ${r.responseType}")
      if (r is API.Result && (r.res is CR.Response || r.res is CR.Invalid)) Log.d(TAG, "chatRecvMsg json: $rStr")
      r
    }
  }

  suspend fun apiGetActiveUser(rh: Long?, ctrl: ChatCtrl? = null): User? {
    val r = sendCmd(rh, CC.ShowActiveUser(), ctrl)
    if (r is API.Result && r.res is CR.ActiveUser) return r.res.user.updateRemoteHostId(rh)
    Log.d(TAG, "apiGetActiveUser: ${r.responseType} ${r.details}")
    if (rh == null) {
      chatModel.localUserCreated.value = false
    }
    return null
  }

  suspend fun apiCreateActiveUser(rh: Long?, p: Profile?, pastTimestamp: Boolean = false, ctrl: ChatCtrl? = null): User? {
    val r = sendCmd(rh, CC.CreateActiveUser(p, pastTimestamp = pastTimestamp), ctrl)
    if (r is API.Result && r.res is CR.ActiveUser) return r.res.user.updateRemoteHostId(rh)
    val e = (r as? API.Error)?.err
    if (
      e is ChatError.ChatErrorStore && e.storeError is StoreError.DuplicateName ||
      e is ChatError.ChatErrorChat && e.errorType is ChatErrorType.UserExists
    ) {
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.failed_to_create_user_duplicate_title), generalGetString(MR.strings.failed_to_create_user_duplicate_desc))
    } else if (
      e is ChatError.ChatErrorChat && e.errorType is ChatErrorType.InvalidDisplayName
    ) {
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.failed_to_create_user_invalid_title), generalGetString(MR.strings.failed_to_create_user_invalid_desc))
    } else {
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.failed_to_create_user_title), r.details)
    }
    Log.d(TAG, "apiCreateActiveUser: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun listUsers(rh: Long?): List<UserInfo> {
    val r = sendCmd(rh, CC.ListUsers())
    if (r is API.Result && r.res is CR.UsersList) {
      val users = if (rh == null) r.res.users else r.res.users.map { it.copy(user = it.user.copy(remoteHostId = rh)) }
      return users.sortedBy { it.user.chatViewName }
    }
    Log.d(TAG, "listUsers: ${r.responseType} ${r.details}")
    throw Exception("failed to list users ${r.responseType} ${r.details}")
  }

  suspend fun apiSetActiveUser(rh: Long?, userId: Long, viewPwd: String?): User {
    val r = sendCmd(rh, CC.ApiSetActiveUser(userId, viewPwd))
    if (r is API.Result && r.res is CR.ActiveUser) return r.res.user.updateRemoteHostId(rh)
    Log.d(TAG, "apiSetActiveUser: ${r.responseType} ${r.details}")
    throw Exception("failed to set the user as active ${r.responseType} ${r.details}")
  }

  suspend fun apiSetAllContactReceipts(rh: Long?, enable: Boolean) {
    val r = sendCmd(rh, CC.SetAllContactReceipts(enable))
    if (r.result is CR.CmdOk) return
    throw Exception("failed to set receipts for all users ${r.responseType} ${r.details}")
  }

  suspend fun apiSetUserContactReceipts(u: User, userMsgReceiptSettings: UserMsgReceiptSettings) {
    val r = sendCmd(u.remoteHostId, CC.ApiSetUserContactReceipts(u.userId, userMsgReceiptSettings))
    if (r.result is CR.CmdOk) return
    throw Exception("failed to set receipts for user contacts ${r.responseType} ${r.details}")
  }

  suspend fun apiSetUserGroupReceipts(u: User, userMsgReceiptSettings: UserMsgReceiptSettings) {
    val r = sendCmd(u.remoteHostId, CC.ApiSetUserGroupReceipts(u.userId, userMsgReceiptSettings))
    if (r.result is CR.CmdOk) return
    throw Exception("failed to set receipts for user groups ${r.responseType} ${r.details}")
  }

  suspend fun apiHideUser(u: User, viewPwd: String): User =
    setUserPrivacy(u.remoteHostId, CC.ApiHideUser(u.userId, viewPwd))

  suspend fun apiUnhideUser(u: User, viewPwd: String): User =
    setUserPrivacy(u.remoteHostId, CC.ApiUnhideUser(u.userId, viewPwd))

  suspend fun apiMuteUser(u: User): User =
    setUserPrivacy(u.remoteHostId, CC.ApiMuteUser(u.userId))

  suspend fun apiUnmuteUser(u: User): User =
    setUserPrivacy(u.remoteHostId, CC.ApiUnmuteUser(u.userId))

  private suspend fun setUserPrivacy(rh: Long?, cmd: CC): User {
    val r = sendCmd(rh, cmd)
    if (r is API.Result && r.res is CR.UserPrivacy) return r.res.updatedUser.updateRemoteHostId(rh)
    else throw Exception("Failed to change user privacy: ${r.responseType} ${r.details}")
  }

  suspend fun apiDeleteUser(u: User, delSMPQueues: Boolean, viewPwd: String?) {
    val r = sendCmd(u.remoteHostId, CC.ApiDeleteUser(u.userId, delSMPQueues, viewPwd))
    if (r.result is CR.CmdOk) return
    Log.d(TAG, "apiDeleteUser: ${r.responseType} ${r.details}")
    throw Exception("failed to delete the user ${r.responseType} ${r.details}")
  }

  suspend fun apiStartChat(ctrl: ChatCtrl? = null): Boolean {
    val r = sendCmd(null, CC.StartChat(mainApp = true), ctrl)
    when (r.result) {
      is CR.ChatStarted -> return true
      is CR.ChatRunning -> return false
      else -> throw Exception("failed starting chat: ${r.responseType} ${r.details}")
    }
  }

  private suspend fun apiCheckChatRunning(): Boolean {
    val r = sendCmd(null, CC.CheckChatRunning())
    when (r.result) {
      is CR.ChatRunning -> return true
      is CR.ChatStopped -> return false
      else -> throw Exception("failed check chat running: ${r.responseType} ${r.details}")
    }
  }

  suspend fun apiStopChat(): Boolean {
    val r = sendCmd(null, CC.ApiStopChat())
    if (r.result is CR.ChatStopped) return true
    throw Exception("failed stopping chat: ${r.responseType} ${r.details}")
  }

  suspend fun apiSetAppFilePaths(filesFolder: String, tempFolder: String, assetsFolder: String, remoteHostsFolder: String, ctrl: ChatCtrl? = null) {
    val r = sendCmd(null, CC.ApiSetAppFilePaths(filesFolder, tempFolder, assetsFolder, remoteHostsFolder), ctrl)
    if (r.result is CR.CmdOk) return
    throw Exception("failed to set app file paths: ${r.responseType} ${r.details}")
  }

  suspend fun apiSetEncryptLocalFiles(enable: Boolean) = sendCommandOkResp(null, CC.ApiSetEncryptLocalFiles(enable))

  suspend fun apiSaveAppSettings(settings: AppSettings) {
    val r = sendCmd(null, CC.ApiSaveSettings(settings))
    if (r.result is CR.CmdOk) return
    throw Exception("failed to set app settings: ${r.responseType} ${r.details}")
  }

  suspend fun apiGetAppSettings(settings: AppSettings): AppSettings {
    val r = sendCmd(null, CC.ApiGetSettings(settings))
    if (r is API.Result && r.res is CR.AppSettingsR) return r.res.appSettings
    throw Exception("failed to get app settings: ${r.responseType} ${r.details}")
  }

  suspend fun apiExportArchive(config: ArchiveConfig): List<ArchiveError> {
    val r = sendCmd(null, CC.ApiExportArchive(config))
    if (r is API.Result && r.res is CR.ArchiveExported) return r.res.archiveErrors
    throw Exception("failed to export archive: ${r.responseType} ${r.details}")
  }

  suspend fun apiImportArchive(config: ArchiveConfig): List<ArchiveError> {
    val r = sendCmd(null, CC.ApiImportArchive(config))
    if (r is API.Result && r.res is CR.ArchiveImported) return r.res.archiveErrors
    throw Exception("failed to import archive: ${r.responseType} ${r.details}")
  }

  suspend fun apiDeleteStorage() {
    val r = sendCmd(null, CC.ApiDeleteStorage())
    if (r.result is CR.CmdOk) return
    throw Exception("failed to delete storage: ${r.responseType} ${r.details}")
  }

  suspend fun apiStorageEncryption(currentKey: String = "", newKey: String = ""): ChatError? {
    val r = sendCmd(null, CC.ApiStorageEncryption(DBEncryptionConfig(currentKey, newKey)))
    if (r.result is CR.CmdOk) return null
    else if (r is API.Error) return r.err
    throw Exception("failed to set storage encryption: ${r.responseType} ${r.details}")
  }

  suspend fun testStorageEncryption(key: String, ctrl: ChatCtrl? = null): ChatError? {
    val r = sendCmd(null, CC.TestStorageEncryption(key), ctrl)
    if (r.result is CR.CmdOk) return null
    else if (r is API.Error) return r.err
    throw Exception("failed to test storage encryption: ${r.responseType} ${r.details}")
  }

  suspend fun apiGetChats(rh: Long?): List<Chat> {
    val userId = kotlin.runCatching { currentUserId("apiGetChats") }.getOrElse { return emptyList() }
    val r = sendCmd(rh, CC.ApiGetChats(userId))
    if (r is API.Result && r.res is CR.ApiChats) return if (rh == null) r.res.chats else r.res.chats.map { it.copy(remoteHostId = rh) }
    Log.e(TAG, "failed getting the list of chats: ${r.responseType} ${r.details}")
    AlertManager.shared.showAlertMsg(generalGetString(MR.strings.failed_to_parse_chats_title), generalGetString(MR.strings.contact_developers))
    return emptyList()
  }

  private suspend fun apiGetChatTags(rh: Long?): List<ChatTag>?{
    val userId = currentUserId("apiGetChatTags")
    val r = sendCmd(rh, CC.ApiGetChatTags(userId))
    if (r is API.Result && r.res is CR.ChatTags) return r.res.userTags
    Log.e(TAG, "apiGetChatTags bad response: ${r.responseType} ${r.details}")
    AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_loading_chat_tags), "${r.responseType}: ${r.details}")
    return null
  }

  suspend fun apiGetChat(rh: Long?, type: ChatType, id: Long, contentTag: MsgContentTag? = null, pagination: ChatPagination, search: String = ""): Pair<Chat, NavigationInfo>? {
    val r = sendCmd(rh, CC.ApiGetChat(type, id, contentTag, pagination, search))
    if (r is API.Result && r.res is CR.ApiChat) return if (rh == null) r.res.chat to r.res.navInfo else r.res.chat.copy(remoteHostId = rh) to r.res.navInfo
    Log.e(TAG, "apiGetChat bad response: ${r.responseType} ${r.details}")
    val e = (r as? API.Error)?.err
    if (pagination is ChatPagination.Around && e is ChatError.ChatErrorStore && e.storeError is StoreError.ChatItemNotFound) {
      showQuotedItemDoesNotExistAlert()
    } else {
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.failed_to_parse_chat_title), generalGetString(MR.strings.contact_developers))
    }
    return null
  }

  suspend fun apiCreateChatTag(rh: Long?, tag: ChatTagData): List<ChatTag>? {
    val r = sendCmd(rh, CC.ApiCreateChatTag(tag))
    if (r is API.Result && r.res is CR.ChatTags) return r.res.userTags
    Log.e(TAG, "apiCreateChatTag bad response: ${r.responseType} ${r.details}")
    AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_creating_chat_tags), "${r.responseType}: ${r.details}")
    return null
  }

  suspend fun apiSetChatTags(rh: Long?, type: ChatType, id: Long, tagIds: List<Long>): Pair<List<ChatTag>, List<Long>>? {
    val r = sendCmd(rh, CC.ApiSetChatTags(type, id, tagIds))
    if (r is API.Result && r.res is CR.TagsUpdated) return r.res.userTags to r.res.chatTags
    Log.e(TAG, "apiSetChatTags bad response: ${r.responseType} ${r.details}")
    AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_updating_chat_tags), "${r.responseType}: ${r.details}")
    return null
  }

  suspend fun apiDeleteChatTag(rh: Long?, tagId: Long) = sendCommandOkResp(rh, CC.ApiDeleteChatTag(tagId))

  suspend fun apiUpdateChatTag(rh: Long?, tagId: Long, tag: ChatTagData) = sendCommandOkResp(rh, CC.ApiUpdateChatTag(tagId, tag))

  suspend fun apiReorderChatTags(rh: Long?, tagIds: List<Long>) = sendCommandOkResp(rh, CC.ApiReorderChatTags(tagIds))

  suspend fun apiSendMessages(rh: Long?, type: ChatType, id: Long, live: Boolean = false, ttl: Int? = null, composedMessages: List<ComposedMessage>): List<AChatItem>? {
    val cmd = CC.ApiSendMessages(type, id, live, ttl, composedMessages)
    return processSendMessageCmd(rh, cmd)
  }

  private suspend fun processSendMessageCmd(rh: Long?, cmd: CC): List<AChatItem>? {
    val r = sendCmd(rh, cmd)
    return when {
      r is API.Result && r.res is CR.NewChatItems -> r.res.chatItems
      r is API.Error && r.err is ChatError.ChatErrorStore && r.err.storeError is StoreError.LargeMsg && cmd is CC.ApiSendMessages -> {
        val mc = cmd.composedMessages.last().msgContent
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.maximum_message_size_title),
          if (mc is MsgContent.MCImage || mc is MsgContent.MCVideo || mc is MsgContent.MCLink) {
            generalGetString(MR.strings.maximum_message_size_reached_non_text)
          } else {
            generalGetString(MR.strings.maximum_message_size_reached_text)
          }
        )
        null
      }
      r is API.Error && r.err is ChatError.ChatErrorStore && r.err.storeError is StoreError.LargeMsg && cmd is CC.ApiForwardChatItems -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.maximum_message_size_title),
          generalGetString(MR.strings.maximum_message_size_reached_forwarding)
        )
        null
      }
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("processSendMessageCmd", generalGetString(MR.strings.error_sending_message), r)
        }
        null
      }
    }
  }

  suspend fun apiCreateChatItems(rh: Long?, noteFolderId: Long, composedMessages: List<ComposedMessage>): List<AChatItem>? {
    val cmd = CC.ApiCreateChatItems(noteFolderId, composedMessages)
    val r = sendCmd(rh, cmd)
    if (r is API.Result && r.res is CR.NewChatItems) return r.res.chatItems
    apiErrorAlert("apiCreateChatItems", generalGetString(MR.strings.error_creating_message), r)
    return null
  }

  suspend fun apiReportMessage(rh: Long?, groupId: Long, chatItemId: Long, reportReason: ReportReason, reportText: String): List<AChatItem>? {
    val r = sendCmd(rh, CC.ApiReportMessage(groupId, chatItemId, reportReason, reportText))
    if (r is API.Result && r.res is CR.NewChatItems) return r.res.chatItems
    apiErrorAlert("apiReportMessage", generalGetString(MR.strings.error_creating_report), r)
    return null
  }

  suspend fun apiGetChatItemInfo(rh: Long?, type: ChatType, id: Long, itemId: Long): ChatItemInfo? {
    val r = sendCmd(rh, CC.ApiGetChatItemInfo(type, id, itemId))
    if (r is API.Result && r.res is CR.ApiChatItemInfo) return r.res.chatItemInfo
    apiErrorAlert("apiGetChatItemInfo", generalGetString(MR.strings.error_loading_details), r)
    return null
  }

  suspend fun apiForwardChatItems(rh: Long?, toChatType: ChatType, toChatId: Long, fromChatType: ChatType, fromChatId: Long, itemIds: List<Long>, ttl: Int?): List<ChatItem>? {
    val cmd = CC.ApiForwardChatItems(toChatType, toChatId, fromChatType, fromChatId, itemIds, ttl)
    return processSendMessageCmd(rh, cmd)?.map { it.chatItem }
  }

  suspend fun apiPlanForwardChatItems(rh: Long?, fromChatType: ChatType, fromChatId: Long, chatItemIds: List<Long>): CR.ForwardPlan? {
    val r = sendCmd(rh, CC.ApiPlanForwardChatItems(fromChatType, fromChatId, chatItemIds))
    if (r is API.Result && r.res is CR.ForwardPlan) return r.res
    apiErrorAlert("apiPlanForwardChatItems", generalGetString(MR.strings.error_forwarding_messages), r)
    return null
  }

  suspend fun apiUpdateChatItem(rh: Long?, type: ChatType, id: Long, itemId: Long, updatedMessage: UpdatedMessage, live: Boolean = false): AChatItem? {
    val r = sendCmd(rh, CC.ApiUpdateChatItem(type, id, itemId, updatedMessage, live))
    when {
      r is API.Result && r.res is CR.ChatItemUpdated -> return r.res.chatItem
      r is API.Result && r.res is CR.ChatItemNotChanged -> return r.res.chatItem
      r is API.Error && r.err is ChatError.ChatErrorStore && r.err.storeError is StoreError.LargeMsg -> {
        val mc = updatedMessage.msgContent
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.maximum_message_size_title),
          if (mc is MsgContent.MCImage || mc is MsgContent.MCVideo || mc is MsgContent.MCLink) {
            generalGetString(MR.strings.maximum_message_size_reached_non_text)
          } else {
            generalGetString(MR.strings.maximum_message_size_reached_text)
          }
        )
        return null
      }
    }

    Log.e(TAG, "apiUpdateChatItem bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiChatItemReaction(rh: Long?, type: ChatType, id: Long, itemId: Long, add: Boolean, reaction: MsgReaction): ChatItem? {
    val r = sendCmd(rh, CC.ApiChatItemReaction(type, id, itemId, add, reaction))
    if (r is API.Result && r.res is CR.ChatItemReaction) return r.res.reaction.chatReaction.chatItem
    Log.e(TAG, "apiUpdateChatItem bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiGetReactionMembers(rh: Long?, groupId: Long, itemId: Long, reaction: MsgReaction): List<MemberReaction>? {
    val userId = currentUserId("apiGetReactionMembers")
    val r = sendCmd(rh, CC.ApiGetReactionMembers(userId, groupId, itemId, reaction))
    if (r is API.Result && r.res is CR.ReactionMembers) return r.res.memberReactions
    Log.e(TAG, "apiGetReactionMembers bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiDeleteChatItems(rh: Long?, type: ChatType, id: Long, itemIds: List<Long>, mode: CIDeleteMode): List<ChatItemDeletion>? {
    val r = sendCmd(rh, CC.ApiDeleteChatItem(type, id, itemIds, mode))
    if (r is API.Result && r.res is CR.ChatItemsDeleted) return r.res.chatItemDeletions
    Log.e(TAG, "apiDeleteChatItem bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiDeleteMemberChatItems(rh: Long?, groupId: Long, itemIds: List<Long>): List<ChatItemDeletion>? {
    val r = sendCmd(rh, CC.ApiDeleteMemberChatItem(groupId, itemIds))
    if (r is API.Result && r.res is CR.ChatItemsDeleted) return r.res.chatItemDeletions
    Log.e(TAG, "apiDeleteMemberChatItem bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiArchiveReceivedReports(rh: Long?, groupId: Long): CR.GroupChatItemsDeleted? {
    val r = sendCmd(rh, CC.ApiArchiveReceivedReports(groupId))
    if (r is API.Result && r.res is CR.GroupChatItemsDeleted) return r.res
    Log.e(TAG, "apiArchiveReceivedReports bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiDeleteReceivedReports(rh: Long?, groupId: Long, itemIds: List<Long>, mode: CIDeleteMode): List<ChatItemDeletion>? {
    val r = sendCmd(rh, CC.ApiDeleteReceivedReports(groupId, itemIds, mode))
    if (r is API.Result && r.res is CR.ChatItemsDeleted) return r.res.chatItemDeletions
    Log.e(TAG, "apiDeleteReceivedReports bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun testProtoServer(rh: Long?, server: String): ProtocolTestFailure? {
    val userId = currentUserId("testProtoServer")
    val r = sendCmd(rh, CC.APITestProtoServer(userId, server))
    if (r is API.Result && r.res is CR.ServerTestResult) return r.res.testFailure
    Log.e(TAG, "testProtoServer bad response: ${r.responseType} ${r.details}")
    throw Exception("testProtoServer bad response: ${r.responseType} ${r.details}")
  }

  suspend fun getServerOperators(rh: Long?): ServerOperatorConditionsDetail? {
    val r = sendCmd(rh, CC.ApiGetServerOperators())
    if (r is API.Result && r.res is CR.ServerOperatorConditions) return r.res.conditions
    Log.e(TAG, "getServerOperators bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun setServerOperators(rh: Long?, operators: List<ServerOperator>): ServerOperatorConditionsDetail? {
    val r = sendCmd(rh, CC.ApiSetServerOperators(operators))
    if (r is API.Result && r.res is CR.ServerOperatorConditions) return r.res.conditions
    Log.e(TAG, "setServerOperators bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun getUserServers(rh: Long?): List<UserOperatorServers>? {
    val userId = currentUserId("getUserServers")
    val r = sendCmd(rh, CC.ApiGetUserServers(userId))
    if (r is API.Result && r.res is CR.UserServers) return r.res.userServers
    Log.e(TAG, "getUserServers bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun setUserServers(rh: Long?, userServers: List<UserOperatorServers>): Boolean {
    val userId = currentUserId("setUserServers")
    val r = sendCmd(rh, CC.ApiSetUserServers(userId, userServers))
    if (r.result is CR.CmdOk) return true
    AlertManager.shared.showAlertMsg(
      generalGetString(MR.strings.failed_to_save_servers),
      "${r.responseType}: ${r.details}"
    )
    Log.e(TAG, "setUserServers bad response: ${r.responseType} ${r.details}")
    return false
  }

  suspend fun validateServers(rh: Long?, userServers: List<UserOperatorServers>): List<UserServersError>? {
    val userId = currentUserId("validateServers")
    val r = sendCmd(rh, CC.ApiValidateServers(userId, userServers))
    if (r is API.Result && r.res is CR.UserServersValidation) return r.res.serverErrors
    Log.e(TAG, "validateServers bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun getUsageConditions(rh: Long?): Triple<UsageConditionsDetail, String?, UsageConditionsDetail?>? {
    val r = sendCmd(rh, CC.ApiGetUsageConditions())
    if (r is API.Result && r.res is CR.UsageConditions) return Triple(r.res.usageConditions, r.res.conditionsText, r.res.acceptedConditions)
    Log.e(TAG, "getUsageConditions bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun setConditionsNotified(rh: Long?, conditionsId: Long): Boolean {
    val r = sendCmd(rh, CC.ApiSetConditionsNotified(conditionsId))
    if (r.result is CR.CmdOk) return true
    Log.e(TAG, "setConditionsNotified bad response: ${r.responseType} ${r.details}")
    return false
  }

  suspend fun acceptConditions(rh: Long?, conditionsId: Long, operatorIds: List<Long>): ServerOperatorConditionsDetail? {
    val r = sendCmd(rh, CC.ApiAcceptConditions(conditionsId, operatorIds))
    if (r is API.Result && r.res is CR.ServerOperatorConditions) return r.res.conditions
    AlertManager.shared.showAlertMsg(
      generalGetString(MR.strings.error_accepting_operator_conditions),
      "${r.responseType}: ${r.details}"
    )
    Log.e(TAG, "acceptConditions bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun getChatItemTTL(rh: Long?): ChatItemTTL {
    val userId = currentUserId("getChatItemTTL")
    val r = sendCmd(rh, CC.APIGetChatItemTTL(userId))
    if (r is API.Result && r.res is CR.ChatItemTTL) {
      return if (r.res.chatItemTTL != null) {
        ChatItemTTL.fromSeconds(r.res.chatItemTTL)
      } else {
        ChatItemTTL.None
      }
    }
    throw Exception("failed to get chat item TTL: ${r.responseType} ${r.details}")
  }

  suspend fun setChatItemTTL(rh: Long?, chatItemTTL: ChatItemTTL) {
    val userId = currentUserId("setChatItemTTL")
    val r = sendCmd(rh, CC.APISetChatItemTTL(userId, chatItemTTL.seconds))
    if (r.result is CR.CmdOk) return
    throw Exception("failed to set chat item TTL: ${r.responseType} ${r.details}")
  }

  suspend fun setChatTTL(rh: Long?, chatType: ChatType, id: Long, chatItemTTL: ChatItemTTL?) {
    val userId = currentUserId("setChatTTL")
    val r = sendCmd(rh, CC.APISetChatTTL(userId, chatType, id, chatItemTTL?.seconds))
    if (r.result is CR.CmdOk) return
    throw Exception("failed to set chat TTL: ${r.responseType} ${r.details}")
  }

  suspend fun apiSetNetworkConfig(cfg: NetCfg, showAlertOnError: Boolean = true, ctrl: ChatCtrl? = null): Boolean {
    val r = sendCmd(null, CC.APISetNetworkConfig(cfg), ctrl)
    if (r.result is CR.CmdOk) return true
    Log.e(TAG, "apiSetNetworkConfig bad response: ${r.responseType} ${r.details}")
    if (showAlertOnError) {
      AlertManager.shared.showAlertMsg(
        generalGetString(MR.strings.error_setting_network_config),
        "${r.responseType}: ${r.details}"
      )
    }
    return false
  }

  suspend fun reconnectServer(rh: Long?, server: String): Boolean {
    val userId = currentUserId("reconnectServer")
    return sendCommandOkResp(rh, CC.ReconnectServer(userId, server))
  }

  suspend fun reconnectAllServers(rh: Long?): Boolean = sendCommandOkResp(rh, CC.ReconnectAllServers())

  suspend fun apiSetSettings(rh: Long?, type: ChatType, id: Long, settings: ChatSettings): Boolean {
    val r = sendCmd(rh, CC.APISetChatSettings(type, id, settings))
    if (r.result is CR.CmdOk) return true
    Log.e(TAG, "apiSetSettings bad response: ${r.responseType} ${r.details}")
    return false
  }

  suspend fun apiSetNetworkInfo(networkInfo: UserNetworkInfo): Boolean =
    sendCommandOkResp(null, CC.APISetNetworkInfo(networkInfo))

  suspend fun apiSetMemberSettings(rh: Long?, groupId: Long, groupMemberId: Long, memberSettings: GroupMemberSettings): Boolean =
    sendCommandOkResp(rh, CC.ApiSetMemberSettings(groupId, groupMemberId, memberSettings))

  suspend fun apiContactInfo(rh: Long?, contactId: Long): Pair<ConnectionStats?, Profile?>? {
    val r = sendCmd(rh, CC.APIContactInfo(contactId))
    if (r is API.Result && r.res is CR.ContactInfo) return r.res.connectionStats_ to r.res.customUserProfile
    Log.e(TAG, "apiContactInfo bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiGroupMemberInfo(rh: Long?, groupId: Long, groupMemberId: Long): Pair<GroupMember, ConnectionStats?>? {
    val r = sendCmd(rh, CC.APIGroupMemberInfo(groupId, groupMemberId))
    if (r is API.Result && r.res is CR.GroupMemberInfo) return r.res.member to r.res.connectionStats_
    Log.e(TAG, "apiGroupMemberInfo bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiContactQueueInfo(rh: Long?, contactId: Long): Pair<RcvMsgInfo?, ServerQueueInfo>? {
    val r = sendCmd(rh, CC.APIContactQueueInfo(contactId))
    if (r is API.Result && r.res is CR.QueueInfoR) return r.res.rcvMsgInfo to r.res.queueInfo
    apiErrorAlert("apiContactQueueInfo", generalGetString(MR.strings.error), r)
    return null
  }

  suspend fun apiGroupMemberQueueInfo(rh: Long?, groupId: Long, groupMemberId: Long): Pair<RcvMsgInfo?, ServerQueueInfo>? {
    val r = sendCmd(rh, CC.APIGroupMemberQueueInfo(groupId, groupMemberId))
    if (r is API.Result && r.res is CR.QueueInfoR) return r.res.rcvMsgInfo to r.res.queueInfo
    apiErrorAlert("apiGroupMemberQueueInfo", generalGetString(MR.strings.error), r)
    return null
  }

  suspend fun apiSwitchContact(rh: Long?, contactId: Long): ConnectionStats? {
    val r = sendCmd(rh, CC.APISwitchContact(contactId))
    if (r is API.Result && r.res is CR.ContactSwitchStarted) return r.res.connectionStats
    apiErrorAlert("apiSwitchContact", generalGetString(MR.strings.error_changing_address), r)
    return null
  }

  suspend fun apiSwitchGroupMember(rh: Long?, groupId: Long, groupMemberId: Long): Pair<GroupMember, ConnectionStats>? {
    val r = sendCmd(rh, CC.APISwitchGroupMember(groupId, groupMemberId))
    if (r is API.Result && r.res is CR.GroupMemberSwitchStarted) return r.res.member to r.res.connectionStats
    apiErrorAlert("apiSwitchGroupMember", generalGetString(MR.strings.error_changing_address), r)
    return null
  }

  suspend fun apiAbortSwitchContact(rh: Long?, contactId: Long): ConnectionStats? {
    val r = sendCmd(rh, CC.APIAbortSwitchContact(contactId))
    if (r is API.Result && r.res is CR.ContactSwitchAborted) return r.res.connectionStats
    apiErrorAlert("apiAbortSwitchContact", generalGetString(MR.strings.error_aborting_address_change), r)
    return null
  }

  suspend fun apiAbortSwitchGroupMember(rh: Long?, groupId: Long, groupMemberId: Long): Pair<GroupMember, ConnectionStats>? {
    val r = sendCmd(rh, CC.APIAbortSwitchGroupMember(groupId, groupMemberId))
    if (r is API.Result && r.res is CR.GroupMemberSwitchAborted) return r.res.member to r.res.connectionStats
    apiErrorAlert("apiAbortSwitchGroupMember", generalGetString(MR.strings.error_aborting_address_change), r)
    return null
  }

  suspend fun apiSyncContactRatchet(rh: Long?, contactId: Long, force: Boolean): ConnectionStats? {
    val r = sendCmd(rh, CC.APISyncContactRatchet(contactId, force))
    if (r is API.Result && r.res is CR.ContactRatchetSyncStarted) return r.res.connectionStats
    apiErrorAlert("apiSyncContactRatchet", generalGetString(MR.strings.error_synchronizing_connection), r)
    return null
  }

  suspend fun apiSyncGroupMemberRatchet(rh: Long?, groupId: Long, groupMemberId: Long, force: Boolean): Pair<GroupMember, ConnectionStats>? {
    val r = sendCmd(rh, CC.APISyncGroupMemberRatchet(groupId, groupMemberId, force))
    if (r is API.Result && r.res is CR.GroupMemberRatchetSyncStarted) return r.res.member to r.res.connectionStats
    apiErrorAlert("apiSyncGroupMemberRatchet", generalGetString(MR.strings.error_synchronizing_connection), r)
    return null
  }

  suspend fun apiGetContactCode(rh: Long?, contactId: Long): Pair<Contact, String>? {
    val r = sendCmd(rh, CC.APIGetContactCode(contactId))
    if (r is API.Result && r.res is CR.ContactCode) return r.res.contact to r.res.connectionCode
    Log.e(TAG,"failed to get contact code: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiGetGroupMemberCode(rh: Long?, groupId: Long, groupMemberId: Long): Pair<GroupMember, String>? {
    val r = sendCmd(rh, CC.APIGetGroupMemberCode(groupId, groupMemberId))
    if (r is API.Result && r.res is CR.GroupMemberCode) return r.res.member to r.res.connectionCode
    Log.e(TAG,"failed to get group member code: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiVerifyContact(rh: Long?, contactId: Long, connectionCode: String?): Pair<Boolean, String>? {
    val r = sendCmd(rh, CC.APIVerifyContact(contactId, connectionCode))
    if (r is API.Result && r.res is CR.ConnectionVerified) return r.res.verified to r.res.expectedCode
    Log.e(TAG, "apiVerifyContact bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiVerifyGroupMember(rh: Long?, groupId: Long, groupMemberId: Long, connectionCode: String?): Pair<Boolean, String>? {
    val r = sendCmd(rh, CC.APIVerifyGroupMember(groupId, groupMemberId, connectionCode))
    if (r is API.Result && r.res is CR.ConnectionVerified) return r.res.verified to r.res.expectedCode
    Log.e(TAG, "apiVerifyGroupMember bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiAddContact(rh: Long?, incognito: Boolean): Pair<Pair<CreatedConnLink, PendingContactConnection>?, (() -> Unit)?> {
    val userId = try { currentUserId("apiAddContact") } catch (e: Exception) { return null to null }
    val short = appPrefs.privacyShortLinks.get()
    val r = sendCmd(rh, CC.APIAddContact(userId, short = short, incognito = incognito))
    return when {
      r is API.Result && r.res is CR.Invitation -> (r.res.connLinkInvitation to r.res.connection) to null
      !(networkErrorAlert(r)) -> null to { apiErrorAlert("apiAddContact", generalGetString(MR.strings.connection_error), r) }
      else -> null to null
    }
  }

  suspend fun apiSetConnectionIncognito(rh: Long?, connId: Long, incognito: Boolean): PendingContactConnection? {
    val r = sendCmd(rh, CC.ApiSetConnectionIncognito(connId, incognito))
    if (r is API.Result && r.res is CR.ConnectionIncognitoUpdated) return r.res.toConnection
    if (!(networkErrorAlert(r))) {
      apiErrorAlert("apiSetConnectionIncognito", generalGetString(MR.strings.error_sending_message), r)
    }
    return null
  }

  suspend fun apiChangeConnectionUser(rh: Long?, connId: Long, userId: Long): PendingContactConnection? {
    val r = sendCmd(rh, CC.ApiChangeConnectionUser(connId, userId))
    if (r is API.Result && r.res is CR.ConnectionUserChanged) return r.res.toConnection
    if (!(networkErrorAlert(r))) {
      apiErrorAlert("apiChangeConnectionUser", generalGetString(MR.strings.error_sending_message), r)
    }
    return null
  }

  suspend fun apiConnectPlan(rh: Long?, connLink: String): Pair<CreatedConnLink, ConnectionPlan>? {
    val userId = kotlin.runCatching { currentUserId("apiConnectPlan") }.getOrElse { return null }
    val r = sendCmd(rh, CC.APIConnectPlan(userId, connLink))
    if (r is API.Result && r.res is CR.CRConnectionPlan) return r.res.connLink to r.res.connectionPlan
    apiConnectResponseAlert(r)
    return null
  }

  suspend fun apiConnect(rh: Long?, incognito: Boolean, connLink: CreatedConnLink): PendingContactConnection?  {
    val userId = try { currentUserId("apiConnect") } catch (e: Exception) { return null }
    val r = sendCmd(rh, CC.APIConnect(userId, incognito, connLink))
    when {
      r is API.Result && r.res is CR.SentConfirmation -> return r.res.connection
      r is API.Result && r.res is CR.SentInvitation -> return r.res.connection
      r is API.Result && r.res is CR.ContactAlreadyExists ->
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.contact_already_exists),
          String.format(generalGetString(MR.strings.you_are_already_connected_to_vName_via_this_link), r.res.contact.displayName)
        )
      else -> apiConnectResponseAlert(r)
    }
    return null
  }

  private fun apiConnectResponseAlert(r: API) {
    when {
      r is API.Error && r.err is ChatError.ChatErrorChat
          && r.err.errorType is ChatErrorType.InvalidConnReq -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.invalid_connection_link),
          generalGetString(MR.strings.please_check_correct_link_and_maybe_ask_for_a_new_one)
        )
      }
      r is API.Error && r.err is ChatError.ChatErrorChat
          && r.err.errorType is ChatErrorType.UnsupportedConnReq -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.unsupported_connection_link),
          generalGetString(MR.strings.link_requires_newer_app_version_please_upgrade)
        )
      }
      r is API.Error && r.err is ChatError.ChatErrorAgent
          && r.err.agentError is AgentErrorType.SMP
          && r.err.agentError.smpErr is SMPErrorType.AUTH -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.connection_error_auth),
          generalGetString(MR.strings.connection_error_auth_desc)
        )
      }
      r is API.Error && r.err is ChatError.ChatErrorAgent
          && r.err.agentError is AgentErrorType.SMP
          && r.err.agentError.smpErr is SMPErrorType.BLOCKED -> {
        showContentBlockedAlert(
          generalGetString(MR.strings.connection_error_blocked),
          generalGetString(MR.strings.connection_error_blocked_desc).format(r.err.agentError.smpErr.blockInfo.reason.text),
        )
      }
      r is API.Error && r.err is ChatError.ChatErrorAgent
          && r.err.agentError is AgentErrorType.SMP
          && r.err.agentError.smpErr is SMPErrorType.QUOTA -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.connection_error_quota),
          generalGetString(MR.strings.connection_error_quota_desc)
        )
      }
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiConnect", generalGetString(MR.strings.connection_error), r)
        }
      }
    }
  }

  suspend fun apiConnectContactViaAddress(rh: Long?, incognito: Boolean, contactId: Long): Contact? {
    val userId = try { currentUserId("apiConnectContactViaAddress") } catch (e: Exception) { return null }
    val r = sendCmd(rh, CC.ApiConnectContactViaAddress(userId, incognito, contactId))
    if (r is API.Result && r.res is CR.SentInvitationToContact) return r.res.contact
    if (!(networkErrorAlert(r))) {
      apiErrorAlert("apiConnectContactViaAddress", generalGetString(MR.strings.connection_error), r)
    }
    return null
  }

  suspend fun deleteChat(chat: Chat, chatDeleteMode: ChatDeleteMode = ChatDeleteMode.Full(notify = true)) {
    val cInfo = chat.chatInfo
    if (apiDeleteChat(rh = chat.remoteHostId, type = cInfo.chatType, id = cInfo.apiId, chatDeleteMode = chatDeleteMode)) {
      withContext(Dispatchers.Main) {
        chatModel.chatsContext.removeChat(chat.remoteHostId, cInfo.id)
      }
    }
  }

  suspend fun apiDeleteChat(rh: Long?, type: ChatType, id: Long, chatDeleteMode: ChatDeleteMode = ChatDeleteMode.Full(notify = true)): Boolean {
    chatModel.deletedChats.value += rh to type.type + id
    val r = sendCmd(rh, CC.ApiDeleteChat(type, id, chatDeleteMode))
    val res = r.result
    val success = when {
      res is CR.ContactDeleted && type == ChatType.Direct -> true
      res is CR.ContactConnectionDeleted && type == ChatType.ContactConnection -> true
      res is CR.GroupDeletedUser && type == ChatType.Group -> true
      else -> {
        val titleId = when (type) {
          ChatType.Direct -> MR.strings.error_deleting_contact
          ChatType.Group -> MR.strings.error_deleting_group
          ChatType.Local -> MR.strings.error_deleting_note_folder
          ChatType.ContactRequest -> MR.strings.error_deleting_contact_request
          ChatType.ContactConnection -> MR.strings.error_deleting_pending_contact_connection
        }
        apiErrorAlert("apiDeleteChat", generalGetString(titleId), r)
        false
      }
    }
    chatModel.deletedChats.value -= rh to type.type + id
    return success
  }

  suspend fun apiDeleteContact(rh: Long?, id: Long, chatDeleteMode: ChatDeleteMode = ChatDeleteMode.Full(notify = true)): Contact? {
    val type = ChatType.Direct
    chatModel.deletedChats.value += rh to type.type + id
    val r = sendCmd(rh, CC.ApiDeleteChat(type, id, chatDeleteMode))
    val contact = if (r is API.Result && r.res is CR.ContactDeleted) {
      r.res.contact
    } else {
      val titleId = MR.strings.error_deleting_contact
      apiErrorAlert("apiDeleteChat", generalGetString(titleId), r)
      null
    }
    chatModel.deletedChats.value -= rh to type.type + id
    return contact
  }

  fun clearChat(chat: Chat, close: (() -> Unit)? = null) {
    withBGApi {
      val updatedChatInfo = apiClearChat(chat.remoteHostId, chat.chatInfo.chatType, chat.chatInfo.apiId)
      if (updatedChatInfo != null) {
        withContext(Dispatchers.Main) {
          chatModel.chatsContext.clearChat(chat.remoteHostId, updatedChatInfo)
        }
        withContext(Dispatchers.Main) {
          chatModel.secondaryChatsContext.value?.clearChat(chat.remoteHostId, updatedChatInfo)
        }
        ntfManager.cancelNotificationsForChat(chat.chatInfo.id)
        close?.invoke()
      }
    }
  }

  suspend fun apiClearChat(rh: Long?, type: ChatType, id: Long): ChatInfo? {
    val r = sendCmd(rh, CC.ApiClearChat(type, id))
    if (r is API.Result && r.res is CR.ChatCleared) return r.res.chatInfo
    Log.e(TAG, "apiClearChat bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiUpdateProfile(rh: Long?, profile: Profile): Pair<Profile, List<Contact>>? {
    val userId = kotlin.runCatching { currentUserId("apiUpdateProfile") }.getOrElse { return null }
    val r = sendCmd(rh, CC.ApiUpdateProfile(userId, profile))
    if (r is API.Result && r.res is CR.UserProfileNoChange) return profile to emptyList()
    if (r is API.Result && r.res is CR.UserProfileUpdated) return r.res.toProfile to r.res.updateSummary.changedContacts
    if (r is API.Error && r.err is ChatError.ChatErrorStore && r.err.storeError is StoreError.DuplicateName) {
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.failed_to_create_user_duplicate_title), generalGetString(MR.strings.failed_to_create_user_duplicate_desc))
    }
    Log.e(TAG, "apiUpdateProfile bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiSetProfileAddress(rh: Long?, on: Boolean): User? {
    val userId = try { currentUserId("apiSetProfileAddress") } catch (e: Exception) { return null }
    val r = sendCmd(rh, CC.ApiSetProfileAddress(userId, on))
    return when {
      r is API.Result && r.res is CR.UserProfileNoChange -> null
      r is API.Result && r.res is CR.UserProfileUpdated -> r.res.user.updateRemoteHostId(rh)
      else -> throw Exception("failed to set profile address: ${r.responseType} ${r.details}")
    }
  }

  suspend fun apiSetContactPrefs(rh: Long?, contactId: Long, prefs: ChatPreferences): Contact? {
    val r = sendCmd(rh, CC.ApiSetContactPrefs(contactId, prefs))
    if (r is API.Result && r.res is CR.ContactPrefsUpdated) return r.res.toContact
    Log.e(TAG, "apiSetContactPrefs bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiSetContactAlias(rh: Long?, contactId: Long, localAlias: String): Contact? {
    val r = sendCmd(rh, CC.ApiSetContactAlias(contactId, localAlias))
    if (r is API.Result && r.res is CR.ContactAliasUpdated) return r.res.toContact
    Log.e(TAG, "apiSetContactAlias bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiSetGroupAlias(rh: Long?, groupId: Long, localAlias: String): GroupInfo? {
    val r = sendCmd(rh, CC.ApiSetGroupAlias(groupId, localAlias))
    if (r is API.Result && r.res is CR.GroupAliasUpdated) return r.res.toGroup
    Log.e(TAG, "apiSetGroupAlias bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiSetConnectionAlias(rh: Long?, connId: Long, localAlias: String): PendingContactConnection? {
    val r = sendCmd(rh, CC.ApiSetConnectionAlias(connId, localAlias))
    if (r is API.Result && r.res is CR.ConnectionAliasUpdated) return r.res.toConnection
    Log.e(TAG, "apiSetConnectionAlias bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiSetUserUIThemes(rh: Long?, userId: Long, themes: ThemeModeOverrides?): Boolean {
    val r = sendCmd(rh, CC.ApiSetUserUIThemes(userId, themes))
    if (r.result is CR.CmdOk) return true
    Log.e(TAG, "apiSetUserUIThemes bad response: ${r.responseType} ${r.details}")
    return false
  }

  suspend fun apiSetChatUIThemes(rh: Long?, chatId: ChatId, themes: ThemeModeOverrides?): Boolean {
    val r = sendCmd(rh, CC.ApiSetChatUIThemes(chatId, themes))
    if (r.result is CR.CmdOk) return true
    Log.e(TAG, "apiSetChatUIThemes bad response: ${r.responseType} ${r.details}")
    return false
  }

  suspend fun apiCreateUserAddress(rh: Long?, short: Boolean): CreatedConnLink? {
    val userId = kotlin.runCatching { currentUserId("apiCreateUserAddress") }.getOrElse { return null }
    val r = sendCmd(rh, CC.ApiCreateMyAddress(userId, short))
    if (r is API.Result && r.res is CR.UserContactLinkCreated) return r.res.connLinkContact
    if (!(networkErrorAlert(r))) {
      apiErrorAlert("apiCreateUserAddress", generalGetString(MR.strings.error_creating_address), r)
    }
    return null
  }

  suspend fun apiDeleteUserAddress(rh: Long?): User? {
    val userId = try { currentUserId("apiDeleteUserAddress") } catch (e: Exception) { return null }
    val r = sendCmd(rh, CC.ApiDeleteMyAddress(userId))
    if (r is API.Result && r.res is CR.UserContactLinkDeleted) return r.res.user.updateRemoteHostId(rh)
    Log.e(TAG, "apiDeleteUserAddress bad response: ${r.responseType} ${r.details}")
    return null
  }

  private suspend fun apiGetUserAddress(rh: Long?): UserContactLinkRec? {
    val userId = kotlin.runCatching { currentUserId("apiGetUserAddress") }.getOrElse { return null }
    val r = sendCmd(rh, CC.ApiShowMyAddress(userId))
    if (r is API.Result && r.res is CR.UserContactLink) return r.res.contactLink
    if (r is API.Error && r.err is ChatError.ChatErrorStore
      && r.err.storeError is StoreError.UserContactLinkNotFound
    ) {
      return null
    }
    Log.e(TAG, "apiGetUserAddress bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun userAddressAutoAccept(rh: Long?, autoAccept: AutoAccept?): UserContactLinkRec? {
    val userId = kotlin.runCatching { currentUserId("userAddressAutoAccept") }.getOrElse { return null }
    val r = sendCmd(rh, CC.ApiAddressAutoAccept(userId, autoAccept))
    if (r is API.Result && r.res is CR.UserContactLinkUpdated) return r.res.contactLink
    if (r is API.Error && r.err is ChatError.ChatErrorStore
      && r.err.storeError is StoreError.UserContactLinkNotFound
    ) {
      return null
    }
    Log.e(TAG, "userAddressAutoAccept bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiAcceptContactRequest(rh: Long?, incognito: Boolean, contactReqId: Long): Contact? {
    val r = sendCmd(rh, CC.ApiAcceptContact(incognito, contactReqId))
    return when {
      r is API.Result && r.res is CR.AcceptingContactRequest -> r.res.contact
      r is API.Error && r.err is ChatError.ChatErrorAgent
          && r.err.agentError is AgentErrorType.SMP
          && r.err.agentError.smpErr is SMPErrorType.AUTH -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.connection_error_auth),
          generalGetString(MR.strings.sender_may_have_deleted_the_connection_request)
        )
        null
      }
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiAcceptContactRequest", generalGetString(MR.strings.error_accepting_contact_request), r)
        }
        null
      }
    }
  }

  suspend fun apiRejectContactRequest(rh: Long?, contactReqId: Long): Boolean {
    val r = sendCmd(rh, CC.ApiRejectContact(contactReqId))
    if (r is API.Result && r.res is CR.ContactRequestRejected) return true
    Log.e(TAG, "apiRejectContactRequest bad response: ${r.responseType} ${r.details}")
    return false
  }

  suspend fun apiGetCallInvitations(rh: Long?): List<RcvCallInvitation> {
    val r = sendCmd(rh, CC.ApiGetCallInvitations())
    if (r is API.Result && r.res is CR.CallInvitations) return r.res.callInvitations
    Log.e(TAG, "apiGetCallInvitations bad response: ${r.responseType} ${r.details}")
    return emptyList()
  }

  suspend fun apiSendCallInvitation(rh: Long?, contact: Contact, callType: CallType): Boolean {
    val r = sendCmd(rh, CC.ApiSendCallInvitation(contact, callType))
    return r.result is CR.CmdOk
  }

  suspend fun apiRejectCall(rh: Long?, contact: Contact): Boolean {
    val r = sendCmd(rh, CC.ApiRejectCall(contact))
    return r.result is CR.CmdOk
  }

  suspend fun apiSendCallOffer(rh: Long?, contact: Contact, rtcSession: String, rtcIceCandidates: String, media: CallMediaType, capabilities: CallCapabilities): Boolean {
    val webRtcSession = WebRTCSession(rtcSession, rtcIceCandidates)
    val callOffer = WebRTCCallOffer(CallType(media, capabilities), webRtcSession)
    val r = sendCmd(rh, CC.ApiSendCallOffer(contact, callOffer))
    return r.result is CR.CmdOk
  }

  suspend fun apiSendCallAnswer(rh: Long?, contact: Contact, rtcSession: String, rtcIceCandidates: String): Boolean {
    val answer = WebRTCSession(rtcSession, rtcIceCandidates)
    val r = sendCmd(rh, CC.ApiSendCallAnswer(contact, answer))
    return r.result is CR.CmdOk
  }

  suspend fun apiSendCallExtraInfo(rh: Long?, contact: Contact, rtcIceCandidates: String): Boolean {
    val extraInfo = WebRTCExtraInfo(rtcIceCandidates)
    val r = sendCmd(rh, CC.ApiSendCallExtraInfo(contact, extraInfo))
    return r.result is CR.CmdOk
  }

  suspend fun apiEndCall(rh: Long?, contact: Contact): Boolean {
    val r = sendCmd(rh, CC.ApiEndCall(contact))
    return r.result is CR.CmdOk
  }

  suspend fun apiCallStatus(rh: Long?, contact: Contact, status: WebRTCCallStatus): Boolean {
    val r = sendCmd(rh, CC.ApiCallStatus(contact, status))
    return r.result is CR.CmdOk
  }

  suspend fun apiGetNetworkStatuses(rh: Long?): List<ConnNetworkStatus>? {
    val r = sendCmd(rh, CC.ApiGetNetworkStatuses())
    if (r is API.Result && r.res is CR.NetworkStatuses) return r.res.networkStatuses
    Log.e(TAG, "apiGetNetworkStatuses bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiChatRead(rh: Long?, type: ChatType, id: Long): Boolean {
    val r = sendCmd(rh, CC.ApiChatRead(type, id))
    if (r.result is CR.CmdOk) return true
    Log.e(TAG, "apiChatRead bad response: ${r.responseType} ${r.details}")
    return false
  }

  suspend fun apiChatItemsRead(rh: Long?, type: ChatType, id: Long, itemIds: List<Long>): Boolean {
    val r = sendCmd(rh, CC.ApiChatItemsRead(type, id, itemIds))
    if (r.result is CR.CmdOk) return true
    Log.e(TAG, "apiChatItemsRead bad response: ${r.responseType} ${r.details}")
    return false
  }

  suspend fun apiChatUnread(rh: Long?, type: ChatType, id: Long, unreadChat: Boolean): Boolean {
    val r = sendCmd(rh, CC.ApiChatUnread(type, id, unreadChat))
    if (r.result is CR.CmdOk) return true
    Log.e(TAG, "apiChatUnread bad response: ${r.responseType} ${r.details}")
    return false
  }

  suspend fun uploadStandaloneFile(user: UserLike, file: CryptoFile, ctrl: ChatCtrl? = null): Pair<FileTransferMeta?, String?> {
    val r = sendCmd(null, CC.ApiUploadStandaloneFile(user.userId, file), ctrl)
    return if (r is API.Result && r.res is CR.SndStandaloneFileCreated) {
      r.res.fileTransferMeta to null
    } else {
      Log.e(TAG, "uploadStandaloneFile error: $r")
      null to r.toString()
    }
  }

  suspend fun downloadStandaloneFile(user: UserLike, url: String, file: CryptoFile, ctrl: ChatCtrl? = null): Pair<RcvFileTransfer?, String?> {
    val r = sendCmd(null, CC.ApiDownloadStandaloneFile(user.userId, url, file), ctrl)
    return if (r is API.Result && r.res is CR.RcvStandaloneFileCreated) {
      r.res.rcvFileTransfer to null
    } else {
      Log.e(TAG, "downloadStandaloneFile error: $r")
      null to r.toString()
    }
  }

  suspend fun standaloneFileInfo(url: String, ctrl: ChatCtrl? = null): MigrationFileLinkData? {
    val r = sendCmd(null, CC.ApiStandaloneFileInfo(url), ctrl)
    return if (r is API.Result && r.res is CR.StandaloneFileInfo) {
      r.res.fileMeta
    } else {
      Log.e(TAG, "standaloneFileInfo error: $r")
      null
    }
  }

  suspend fun receiveFiles(rhId: Long?, user: UserLike, fileIds: List<Long>, userApprovedRelays: Boolean = false, auto: Boolean = false) {
    val fileIdsToApprove = mutableListOf<Long>()
    val srvsToApprove = mutableSetOf<String>()
    val otherFileErrs = mutableListOf<API>()

    for (fileId in fileIds) {
      val r = sendCmd(
        rhId, CC.ReceiveFile(
          fileId,
          userApprovedRelays = userApprovedRelays || !appPrefs.privacyAskToApproveRelays.get(),
          encrypt = appPrefs.privacyEncryptLocalFiles.get(),
          inline = null
        )
      )
      if (r is API.Result && r.res is CR.RcvFileAccepted) {
        chatItemSimpleUpdate(rhId, user, r.res.chatItem)
      } else {
        val maybeChatError = apiChatErrorType(r)
        if (maybeChatError is ChatErrorType.FileNotApproved) {
          fileIdsToApprove.add(maybeChatError.fileId)
          srvsToApprove.addAll(maybeChatError.unknownServers.map { serverHostname(it) })
        } else {
          otherFileErrs.add(r)
        }
      }
    }

    if (!auto) {
      // If there are not approved files, alert is shown the same way both in case of singular and plural files reception
      if (fileIdsToApprove.isNotEmpty()) {
        showFilesToApproveAlert(
          srvsToApprove = srvsToApprove,
          otherFileErrs = otherFileErrs,
          approveFiles = {
            withBGApi {
              receiveFiles(
                rhId = rhId,
                user = user,
                fileIds = fileIdsToApprove,
                userApprovedRelays = true
              )
            }
          }
        )
      } else if (otherFileErrs.size == 1) { // If there is a single other error, we differentiate on it
        val errCR = otherFileErrs.first()
        if (errCR is API.Result && errCR.res is CR.RcvFileAcceptedSndCancelled) {
          Log.d(TAG, "receiveFiles error: sender cancelled file transfer")
          AlertManager.shared.showAlertMsg(
            generalGetString(MR.strings.cannot_receive_file),
            generalGetString(MR.strings.sender_cancelled_file_transfer)
          )
        } else {
          val maybeChatError = apiChatErrorType(errCR)
          if (maybeChatError is ChatErrorType.FileCancelled || maybeChatError is ChatErrorType.FileAlreadyReceiving) {
            Log.d(TAG, "receiveFiles ignoring FileCancelled or FileAlreadyReceiving error")
          } else {
            apiErrorAlert("receiveFiles", generalGetString(MR.strings.error_receiving_file), errCR)
          }
        }
      } else if (otherFileErrs.size > 1) { // If there are multiple other errors, we show general alert
        val errsStr = otherFileErrs.map { json.encodeToString(it) }.joinToString(separator = "\n")
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.error_receiving_file),
          text = String.format(generalGetString(MR.strings.n_file_errors), otherFileErrs.size, errsStr),
          shareText = true
        )
      }
    }
  }

  private fun showFilesToApproveAlert(
    srvsToApprove: Set<String>,
    otherFileErrs: List<API>,
    approveFiles: (() -> Unit)
  ) {
    val srvsToApproveStr = srvsToApprove.sorted().joinToString(separator = ", ")
    val alertText =
      generalGetString(MR.strings.file_not_approved_descr).format(srvsToApproveStr) +
          (if (otherFileErrs.isNotEmpty()) "\n" + generalGetString(MR.strings.n_other_file_errors).format(otherFileErrs.size) else "")

    AlertManager.shared.showAlertDialogButtonsColumn(generalGetString(MR.strings.file_not_approved_title), alertText, belowTextContent = {
      if (otherFileErrs.isNotEmpty()) {
        val clipboard = LocalClipboardManager.current
        SimpleButtonFrame(click = {
          clipboard.setText(AnnotatedString(otherFileErrs.map { json.encodeToString(it) }.joinToString(separator = "\n")))
        }) {
          Icon(
            painterResource(MR.images.ic_content_copy),
            contentDescription = null,
            tint = MaterialTheme.colors.primary,
            modifier = Modifier.padding(end = 8.dp)
          )
          Text(generalGetString(MR.strings.copy_error), color = MaterialTheme.colors.primary)
        }
      }
    }) {
      Row(
        Modifier.fillMaxWidth().padding(horizontal = DEFAULT_PADDING),
        horizontalArrangement = Arrangement.SpaceBetween
      ) {
        val focusRequester = remember { FocusRequester() }
        LaunchedEffect(Unit) {
          // Wait before focusing to prevent auto-confirming if a user used Enter key on hardware keyboard
          delay(200)
          focusRequester.requestFocus()
        }
        TextButton(onClick = AlertManager.shared::hideAlert) { Text(generalGetString(MR.strings.cancel_verb)) }
        TextButton(onClick = {
          approveFiles.invoke()
          AlertManager.shared.hideAlert()
        }, Modifier.focusRequester(focusRequester)) { Text(generalGetString(MR.strings.download_file)) }
      }
    }
  }

  suspend fun receiveFile(rhId: Long?, user: UserLike, fileId: Long, userApprovedRelays: Boolean = false, auto: Boolean = false) {
    receiveFiles(
      rhId = rhId,
      user = user,
      fileIds = listOf(fileId),
      userApprovedRelays = userApprovedRelays,
      auto = auto
    )
  }

  suspend fun cancelFile(rh: Long?, user: User, fileId: Long) {
    val chatItem = apiCancelFile(rh, fileId)
    if (chatItem != null) {
      chatItemSimpleUpdate(rh, user, chatItem)
      cleanupFile(chatItem)
    }
  }

  suspend fun apiCancelFile(rh: Long?, fileId: Long, ctrl: ChatCtrl? = null): AChatItem? {
    val r = sendCmd(rh, CC.CancelFile(fileId), ctrl)
    return when {
      r is API.Result && r.res is CR.SndFileCancelled -> r.res.chatItem_
      r is API.Result && r.res is CR.RcvFileCancelled -> r.res.chatItem_
      else -> {
        Log.d(TAG, "apiCancelFile bad response: ${r.responseType} ${r.details}")
        null
      }
    }
  }

  suspend fun apiNewGroup(rh: Long?, incognito: Boolean, groupProfile: GroupProfile): GroupInfo? {
    val userId = kotlin.runCatching { currentUserId("apiNewGroup") }.getOrElse { return null }
    val r = sendCmd(rh, CC.ApiNewGroup(userId, incognito, groupProfile))
    if (r is API.Result && r.res is CR.GroupCreated) return r.res.groupInfo
    Log.e(TAG, "apiNewGroup bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiAddMember(rh: Long?, groupId: Long, contactId: Long, memberRole: GroupMemberRole): GroupMember? {
    val r = sendCmd(rh, CC.ApiAddMember(groupId, contactId, memberRole))
    if (r is API.Result && r.res is CR.SentGroupInvitation) return r.res.member
    if (!(networkErrorAlert(r))) {
      apiErrorAlert("apiAddMember", generalGetString(MR.strings.error_adding_members), r)
    }
    return null
  }

  suspend fun apiJoinGroup(rh: Long?, groupId: Long) {
    val r = sendCmd(rh, CC.ApiJoinGroup(groupId))
    when {
      r is API.Result && r.res is CR.UserAcceptedGroupSent ->
        withContext(Dispatchers.Main) {
          chatModel.chatsContext.updateGroup(rh, r.res.groupInfo)
        }
      r is API.Error -> {
        val e = r.err
        suspend fun deleteGroup() { if (apiDeleteChat(rh, ChatType.Group, groupId)) {
          withContext(Dispatchers.Main) { chatModel.chatsContext.removeChat(rh, "#$groupId") } }
        }
        if (e is ChatError.ChatErrorAgent && e.agentError is AgentErrorType.SMP && e.agentError.smpErr is SMPErrorType.AUTH) {
          deleteGroup()
          AlertManager.shared.showAlertMsg(generalGetString(MR.strings.alert_title_group_invitation_expired), generalGetString(MR.strings.alert_message_group_invitation_expired))
        } else if (e is ChatError.ChatErrorStore && e.storeError is StoreError.GroupNotFound) {
          deleteGroup()
          AlertManager.shared.showAlertMsg(generalGetString(MR.strings.alert_title_no_group), generalGetString(MR.strings.alert_message_no_group))
        } else if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiJoinGroup", generalGetString(MR.strings.error_joining_group), r)
        }
      }
      else -> apiErrorAlert("apiJoinGroup", generalGetString(MR.strings.error_joining_group), r)
    }
  }

  suspend fun apiRemoveMembers(rh: Long?, groupId: Long, memberIds: List<Long>, withMessages: Boolean = false): List<GroupMember>? {
    val r = sendCmd(rh, CC.ApiRemoveMembers(groupId, memberIds, withMessages))
    if (r is API.Result && r.res is CR.UserDeletedMembers) return r.res.members
    if (!(networkErrorAlert(r))) {
      apiErrorAlert("apiRemoveMembers", generalGetString(MR.strings.error_removing_member), r)
    }
    return null
  }

  suspend fun apiMembersRole(rh: Long?, groupId: Long, memberIds: List<Long>, memberRole: GroupMemberRole): List<GroupMember> {
    val r = sendCmd(rh, CC.ApiMembersRole(groupId, memberIds, memberRole))
    if (r is API.Result && r.res is CR.MembersRoleUser) return r.res.members
    if (!(networkErrorAlert(r))) {
      apiErrorAlert("apiMembersRole", generalGetString(MR.strings.error_changing_role), r)
    }
    throw Exception("failed to change member role: ${r.responseType} ${r.details}")
  }

  suspend fun apiBlockMembersForAll(rh: Long?, groupId: Long, memberIds: List<Long>, blocked: Boolean): List<GroupMember> {
    val r = sendCmd(rh, CC.ApiBlockMembersForAll(groupId, memberIds, blocked))
    if (r is API.Result && r.res is CR.MembersBlockedForAllUser) return r.res.members
    if (!(networkErrorAlert(r))) {
      apiErrorAlert("apiBlockMembersForAll", generalGetString(MR.strings.error_blocking_member_for_all), r)
    }
    throw Exception("failed to block member for all: ${r.responseType} ${r.details}")
  }

  suspend fun apiLeaveGroup(rh: Long?, groupId: Long): GroupInfo? {
    val r = sendCmd(rh, CC.ApiLeaveGroup(groupId))
    if (r is API.Result && r.res is CR.LeftMemberUser) return r.res.groupInfo
    Log.e(TAG, "apiLeaveGroup bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiListMembers(rh: Long?, groupId: Long): List<GroupMember> {
    val r = sendCmd(rh, CC.ApiListMembers(groupId))
    if (r is API.Result && r.res is CR.GroupMembers) return r.res.group.members
    Log.e(TAG, "apiListMembers bad response: ${r.responseType} ${r.details}")
    return emptyList()
  }

  suspend fun apiUpdateGroup(rh: Long?, groupId: Long, groupProfile: GroupProfile): GroupInfo? {
    val r = sendCmd(rh, CC.ApiUpdateGroupProfile(groupId, groupProfile))
    return when {
      r is API.Result && r.res is CR.GroupUpdated -> r.res.toGroup
      r is API.Error -> {
        AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_saving_group_profile), "$r.err")
        null
      }
      else -> {
        Log.e(TAG, "apiUpdateGroup bad response: ${r.responseType} ${r.details}")
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.error_saving_group_profile),
          "${r.responseType}: ${r.details}"
        )
        null
      }
    }
  }

  suspend fun apiCreateGroupLink(rh: Long?, groupId: Long, memberRole: GroupMemberRole = GroupMemberRole.Member): Pair<CreatedConnLink, GroupMemberRole>? {
    val short = appPrefs.privacyShortLinks.get()
    val r = sendCmd(rh, CC.APICreateGroupLink(groupId, memberRole, short))
    if (r is API.Result && r.res is CR.GroupLinkCreated) return r.res.connLinkContact to r.res.memberRole
    if (!(networkErrorAlert(r))) {
      apiErrorAlert("apiCreateGroupLink", generalGetString(MR.strings.error_creating_link_for_group), r)
    }
    return null
  }

  suspend fun apiGroupLinkMemberRole(rh: Long?, groupId: Long, memberRole: GroupMemberRole = GroupMemberRole.Member): Pair<CreatedConnLink, GroupMemberRole>? {
    val r = sendCmd(rh, CC.APIGroupLinkMemberRole(groupId, memberRole))
    if (r is API.Result && r.res is CR.GroupLink) return r.res.connLinkContact to r.res.memberRole
    if (!(networkErrorAlert(r))) {
      apiErrorAlert("apiGroupLinkMemberRole", generalGetString(MR.strings.error_updating_link_for_group), r)
    }
    return null
  }

  suspend fun apiDeleteGroupLink(rh: Long?, groupId: Long): Boolean {
    val r = sendCmd(rh, CC.APIDeleteGroupLink(groupId))
    if (r is API.Result && r.res is CR.GroupLinkDeleted) return true
    if (!(networkErrorAlert(r))) {
      apiErrorAlert("apiDeleteGroupLink", generalGetString(MR.strings.error_deleting_link_for_group), r)
    }
    return false
  }

  suspend fun apiGetGroupLink(rh: Long?, groupId: Long): Pair<CreatedConnLink, GroupMemberRole>? {
    val r = sendCmd(rh, CC.APIGetGroupLink(groupId))
    if (r is API.Result && r.res is CR.GroupLink) return r.res.connLinkContact to r.res.memberRole
    Log.e(TAG, "apiGetGroupLink bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiCreateMemberContact(rh: Long?, groupId: Long, groupMemberId: Long): Contact? {
    val r = sendCmd(rh, CC.APICreateMemberContact(groupId, groupMemberId))
    if (r is API.Result && r.res is CR.NewMemberContact) return r.res.contact
    if (!(networkErrorAlert(r))) {
      apiErrorAlert("apiCreateMemberContact", generalGetString(MR.strings.error_creating_member_contact), r)
    }
    return null
  }

  suspend fun apiSendMemberContactInvitation(rh: Long?, contactId: Long, mc: MsgContent): Contact? {
    val r = sendCmd(rh, CC.APISendMemberContactInvitation(contactId, mc))
    if (r is API.Result && r.res is CR.NewMemberContactSentInv) return r.res.contact
    if (!(networkErrorAlert(r))) {
      apiErrorAlert("apiSendMemberContactInvitation", generalGetString(MR.strings.error_sending_message_contact_invitation), r)
    }
    return null
  }

  suspend fun allowFeatureToContact(rh: Long?, contact: Contact, feature: ChatFeature, param: Int? = null) {
    val prefs = contact.mergedPreferences.toPreferences().setAllowed(feature, param = param)
    val toContact = apiSetContactPrefs(rh, contact.contactId, prefs)
    if (toContact != null) {
      withContext(Dispatchers.Main) {
        chatModel.chatsContext.updateContact(rh, toContact)
      }
    }
  }

  suspend fun setLocalDeviceName(displayName: String): Boolean = sendCommandOkResp(null, CC.SetLocalDeviceName(displayName))

  suspend fun listRemoteHosts(): List<RemoteHostInfo>? {
    val r = sendCmd(null, CC.ListRemoteHosts())
    if (r is API.Result && r.res is CR.RemoteHostList) return r.res.remoteHosts
    apiErrorAlert("listRemoteHosts", generalGetString(MR.strings.error_alert_title), r)
    return null
  }

  suspend fun reloadRemoteHosts() {
    val hosts = listRemoteHosts() ?: return
    chatModel.remoteHosts.clear()
    chatModel.remoteHosts.addAll(hosts)
  }

  suspend fun startRemoteHost(rhId: Long?, multicast: Boolean = true, address: RemoteCtrlAddress?, port: Int?): CR.RemoteHostStarted? {
    val r = sendCmd(null, CC.StartRemoteHost(rhId, multicast, address, port))
    if (r is API.Result && r.res is CR.RemoteHostStarted) return r.res
    apiErrorAlert("startRemoteHost", generalGetString(MR.strings.error_alert_title), r)
    return null
  }

  suspend fun switchRemoteHost (rhId: Long?): RemoteHostInfo? {
    val r = sendCmd(null, CC.SwitchRemoteHost(rhId))
    if (r is API.Result && r.res is CR.CurrentRemoteHost) return r.res.remoteHost_
    apiErrorAlert("switchRemoteHost", generalGetString(MR.strings.error_alert_title), r)
    return null
  }

  suspend fun stopRemoteHost(rhId: Long?): Boolean = sendCommandOkResp(null, CC.StopRemoteHost(rhId))

  fun stopRemoteHostAndReloadHosts(h: RemoteHostInfo, switchToLocal: Boolean) {
    withBGApi {
      stopRemoteHost(h.remoteHostId)
      if (switchToLocal) {
        switchUIRemoteHost(null)
      } else {
        reloadRemoteHosts()
      }
    }
  }

  suspend fun deleteRemoteHost(rhId: Long): Boolean = sendCommandOkResp(null, CC.DeleteRemoteHost(rhId))

  suspend fun storeRemoteFile(rhId: Long, storeEncrypted: Boolean?, localPath: String): CryptoFile? {
    val r = sendCmd(null, CC.StoreRemoteFile(rhId, storeEncrypted, localPath))
    if (r is API.Result && r.res is CR.RemoteFileStored) return r.res.remoteFileSource
    apiErrorAlert("storeRemoteFile", generalGetString(MR.strings.error_alert_title), r)
    return null
  }

  suspend fun getRemoteFile(rhId: Long, file: RemoteFile): Boolean = sendCmd(null, CC.GetRemoteFile(rhId, file)).result is CR.CmdOk

  suspend fun connectRemoteCtrl(desktopAddress: String): Pair<SomeRemoteCtrl?, ChatError?> {
    val r = sendCmd(null, CC.ConnectRemoteCtrl(desktopAddress))
    return when {
      r is API.Result && r.res is CR.RemoteCtrlConnecting -> SomeRemoteCtrl(r.res.remoteCtrl_, r.res.ctrlAppInfo, r.res.appVersion) to null
      r is API.Error -> null to r.err
      else -> {
        apiErrorAlert("connectRemoteCtrl", generalGetString(MR.strings.error_alert_title), r)
        null to null
      }
    }
  }

  suspend fun findKnownRemoteCtrl(): Boolean = sendCommandOkResp(null, CC.FindKnownRemoteCtrl())

  suspend fun confirmRemoteCtrl(rcId: Long): Pair<SomeRemoteCtrl?, ChatError?> {
    val r = sendCmd(null, CC.ConfirmRemoteCtrl(remoteCtrlId = rcId))
    return when {
      r is API.Result && r.res is CR.RemoteCtrlConnecting -> SomeRemoteCtrl(r.res.remoteCtrl_, r.res.ctrlAppInfo, r.res.appVersion) to null
      r is API.Error -> null to r.err
      else -> {
        apiErrorAlert("confirmRemoteCtrl", generalGetString(MR.strings.error_alert_title), r)
        null to null
      }
    }
  }

  suspend fun verifyRemoteCtrlSession(sessionCode: String): RemoteCtrlInfo? {
    val r = sendCmd(null, CC.VerifyRemoteCtrlSession(sessionCode))
    if (r is API.Result && r.res is CR.RemoteCtrlConnected) return r.res.remoteCtrl
    apiErrorAlert("verifyRemoteCtrlSession", generalGetString(MR.strings.error_alert_title), r)
    return null
  }

  suspend fun listRemoteCtrls(): List<RemoteCtrlInfo>? {
    val r = sendCmd(null, CC.ListRemoteCtrls())
    if (r is API.Result && r.res is CR.RemoteCtrlList) return r.res.remoteCtrls
    apiErrorAlert("listRemoteCtrls", generalGetString(MR.strings.error_alert_title), r)
    return null
  }

  suspend fun stopRemoteCtrl(): Boolean = sendCommandOkResp(null, CC.StopRemoteCtrl())

  suspend fun deleteRemoteCtrl(rcId: Long): Boolean = sendCommandOkResp(null, CC.DeleteRemoteCtrl(rcId))

  private suspend fun sendCommandOkResp(rh: Long?, cmd: CC, ctrl: ChatCtrl? = null): Boolean {
    val r = sendCmd(rh, cmd, ctrl)
    val ok = r is API.Result && r.res is CR.CmdOk
    if (!ok) apiErrorAlert(cmd.cmdType, generalGetString(MR.strings.error_alert_title), r)
    return ok
  }

  suspend fun apiGetVersion(): CoreVersionInfo? {
    val r = sendCmd(null, CC.ShowVersion())
    if (r is API.Result && r.res is CR.VersionInfo) return r.res.versionInfo
    Log.e(TAG, "apiGetVersion bad response: ${r.responseType} ${r.details}")
    return null
  }

  private fun networkErrorAlert(r: API): Boolean {
    if (r !is API.Error) return false
    val e = r.err
    return when {
      e is ChatError.ChatErrorAgent
          && e.agentError is AgentErrorType.BROKER
          && e.agentError.brokerErr is BrokerErrorType.TIMEOUT -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.connection_timeout),
          String.format(generalGetString(MR.strings.network_error_desc), serverHostname(e.agentError.brokerAddress))
        )
        true
      }
      e is ChatError.ChatErrorAgent
          && e.agentError is AgentErrorType.BROKER
          && e.agentError.brokerErr is BrokerErrorType.NETWORK -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.connection_error),
          String.format(generalGetString(MR.strings.network_error_desc), serverHostname(e.agentError.brokerAddress))
        )
        true
      }
      e is ChatError.ChatErrorAgent
          && e.agentError is AgentErrorType.BROKER
          && e.agentError.brokerErr is BrokerErrorType.HOST -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.connection_error),
          String.format(generalGetString(MR.strings.network_error_broker_host_desc), serverHostname(e.agentError.brokerAddress))
        )
        true
      }
      e is ChatError.ChatErrorAgent
          && e.agentError is AgentErrorType.BROKER
          && e.agentError.brokerErr is BrokerErrorType.TRANSPORT
          && e.agentError.brokerErr.transportErr is SMPTransportError.Version -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.connection_error),
          String.format(generalGetString(MR.strings.network_error_broker_version_desc), serverHostname(e.agentError.brokerAddress))
        )
        true
      }
      e is ChatError.ChatErrorAgent
          && e.agentError is AgentErrorType.SMP
          && e.agentError.smpErr is SMPErrorType.PROXY ->
        smpProxyErrorAlert(e.agentError.smpErr.proxyErr, e.agentError.serverAddress)
      e is ChatError.ChatErrorAgent
          && e.agentError is AgentErrorType.PROXY
          && e.agentError.proxyErr is ProxyClientError.ProxyProtocolError
          && e.agentError.proxyErr.protocolErr is SMPErrorType.PROXY ->
        proxyDestinationErrorAlert(
          e.agentError.proxyErr.protocolErr.proxyErr,
          e.agentError.proxyServer,
          e.agentError.relayServer
        )
      else -> false
    }
  }

  private fun smpProxyErrorAlert(pe: ProxyError, srvAddr: String): Boolean {
    return when {
      pe is ProxyError.BROKER
          && pe.brokerErr is BrokerErrorType.TIMEOUT -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.private_routing_error),
          String.format(generalGetString(MR.strings.smp_proxy_error_connecting), serverHostname(srvAddr))
        )
        true
      }
      pe is ProxyError.BROKER
          && pe.brokerErr is BrokerErrorType.NETWORK -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.private_routing_error),
          String.format(generalGetString(MR.strings.smp_proxy_error_connecting), serverHostname(srvAddr))
        )
        true
      }
      pe is ProxyError.BROKER
          && pe.brokerErr is BrokerErrorType.HOST -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.private_routing_error),
          String.format(generalGetString(MR.strings.smp_proxy_error_broker_host), serverHostname(srvAddr))
        )
        true
      }
      pe is ProxyError.BROKER
          && pe.brokerErr is BrokerErrorType.TRANSPORT
          && pe.brokerErr.transportErr is SMPTransportError.Version -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.private_routing_error),
          String.format(generalGetString(MR.strings.smp_proxy_error_broker_version), serverHostname(srvAddr))
        )
        true
      }
      else -> false
    }
  }

  private fun proxyDestinationErrorAlert(pe: ProxyError, proxyServer: String, relayServer: String): Boolean {
    return when {
      pe is ProxyError.BROKER
          && pe.brokerErr is BrokerErrorType.TIMEOUT -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.private_routing_error),
          String.format(generalGetString(MR.strings.proxy_destination_error_failed_to_connect), serverHostname(proxyServer), serverHostname(relayServer))
        )
        true
      }
      pe is ProxyError.BROKER
          && pe.brokerErr is BrokerErrorType.NETWORK -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.private_routing_error),
          String.format(generalGetString(MR.strings.proxy_destination_error_failed_to_connect), serverHostname(proxyServer), serverHostname(relayServer))
        )
        true
      }
      pe is ProxyError.NO_SESSION -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.private_routing_error),
          String.format(generalGetString(MR.strings.proxy_destination_error_failed_to_connect), serverHostname(proxyServer), serverHostname(relayServer))
        )
        true
      }
      pe is ProxyError.BROKER
          && pe.brokerErr is BrokerErrorType.HOST -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.private_routing_error),
          String.format(generalGetString(MR.strings.proxy_destination_error_broker_host), serverHostname(relayServer), serverHostname(proxyServer))
        )
        true
      }
      pe is ProxyError.BROKER
          && pe.brokerErr is BrokerErrorType.TRANSPORT
          && pe.brokerErr.transportErr is SMPTransportError.Version -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.private_routing_error),
          String.format(generalGetString(MR.strings.proxy_destination_error_broker_version), serverHostname(relayServer), serverHostname(proxyServer))
        )
        true
      }
      else -> false
    }
  }

  private fun apiErrorAlert(method: String, title: String, r: API) {
    val errMsg = "${r.responseType}: ${r.details}"
    Log.e(TAG, "$method bad response: $errMsg")
    AlertManager.shared.showAlertMsg(title, errMsg)
  }

  private suspend fun processReceivedMsg(msg: API) {
    lastMsgReceivedTimestamp = System.currentTimeMillis()
    val rhId = msg.rhId
    fun active(user: UserLike): Boolean = activeUser(rhId, user)
    chatModel.addTerminalItem(TerminalItem.resp(rhId, msg))
    val r = msg.result
    when (r) {
      is CR.ContactDeletedByContact -> {
        if (active(r.user) && r.contact.directOrUsed) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.updateContact(rhId, r.contact)
          }
        }
      }
      is CR.ContactConnected -> {
        if (active(r.user) && r.contact.directOrUsed) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.updateContact(rhId, r.contact)
            val conn = r.contact.activeConn
            if (conn != null) {
              chatModel.replaceConnReqView(conn.id, "@${r.contact.contactId}")
              chatModel.chatsContext.removeChat(rhId, conn.id)
            }
          }
        }
        if (r.contact.directOrUsed) {
          ntfManager.notifyContactConnected(r.user, r.contact)
        }
        chatModel.setContactNetworkStatus(r.contact, NetworkStatus.Connected())
      }
      is CR.ContactConnecting -> {
        if (active(r.user) && r.contact.directOrUsed) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.updateContact(rhId, r.contact)
            val conn = r.contact.activeConn
            if (conn != null) {
              chatModel.replaceConnReqView(conn.id, "@${r.contact.contactId}")
              chatModel.chatsContext.removeChat(rhId, conn.id)
            }
          }
        }
      }
      is CR.ContactSndReady -> {
        if (active(r.user) && r.contact.directOrUsed) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.updateContact(rhId, r.contact)
            val conn = r.contact.activeConn
            if (conn != null) {
              chatModel.replaceConnReqView(conn.id, "@${r.contact.contactId}")
              chatModel.chatsContext.removeChat(rhId, conn.id)
            }
          }
        }
        chatModel.setContactNetworkStatus(r.contact, NetworkStatus.Connected())
      }
      is CR.ReceivedContactRequest -> {
        val contactRequest = r.contactRequest
        val cInfo = ChatInfo.ContactRequest(contactRequest)
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            if (chatModel.chatsContext.hasChat(rhId, contactRequest.id)) {
              chatModel.chatsContext.updateChatInfo(rhId, cInfo)
            } else {
              chatModel.chatsContext.addChat(Chat(remoteHostId = rhId, chatInfo = cInfo, chatItems = listOf()))
            }
          }
        }
        ntfManager.notifyContactRequestReceived(r.user, cInfo)
      }
      is CR.ContactUpdated -> {
        if (active(r.user) && chatModel.chatsContext.hasChat(rhId, r.toContact.id)) {
          val cInfo = ChatInfo.Direct(r.toContact)
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.updateChatInfo(rhId, cInfo)
          }
        }
      }
      is CR.GroupMemberUpdated -> {
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.upsertGroupMember(rhId, r.groupInfo, r.toMember)
          }
          withContext(Dispatchers.Main) {
            chatModel.secondaryChatsContext.value?.upsertGroupMember(rhId, r.groupInfo, r.toMember)
          }
        }
      }
      is CR.ContactsMerged -> {
        if (active(r.user) && chatModel.chatsContext.hasChat(rhId, r.mergedContact.id)) {
          if (chatModel.chatId.value == r.mergedContact.id) {
            chatModel.chatId.value = r.intoContact.id
          }
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.removeChat(rhId, r.mergedContact.id)
          }
        }
      }
      // ContactsSubscribed, ContactsDisconnected and ContactSubSummary are only used in CLI,
      // They have to be used here for remote desktop to process these status updates.
      is CR.ContactsSubscribed -> updateContactsStatus(r.contactRefs, NetworkStatus.Connected())
      is CR.ContactsDisconnected -> updateContactsStatus(r.contactRefs, NetworkStatus.Disconnected())
      is CR.ContactSubSummary -> {
        for (sub in r.contactSubscriptions) {
          if (active(r.user)) {
            withContext(Dispatchers.Main) {
              chatModel.chatsContext.updateContact(rhId, sub.contact)
            }
          }
          val err = sub.contactError
          if (err == null) {
            chatModel.setContactNetworkStatus(sub.contact, NetworkStatus.Connected())
          } else {
            processContactSubError(sub.contact, sub.contactError)
          }
        }
      }
      is CR.NetworkStatusResp -> {
        for (cId in r.connections) {
          chatModel.networkStatuses[cId] = r.networkStatus
        }
      }
      is CR.NetworkStatuses -> {
        for (s in r.networkStatuses) {
          chatModel.networkStatuses[s.agentConnId] = s.networkStatus
        }
      }
      is CR.NewChatItems -> withBGApi {
        r.chatItems.forEach { chatItem ->
          val cInfo = chatItem.chatInfo
          val cItem = chatItem.chatItem
          if (active(r.user)) {
            withContext(Dispatchers.Main) {
              chatModel.chatsContext.addChatItem(rhId, cInfo, cItem)
              if (cItem.isActiveReport) {
                chatModel.chatsContext.increaseGroupReportsCounter(rhId, cInfo.id)
              }
            }
            withContext(Dispatchers.Main) {
              if (cItem.isReport) {
                chatModel.secondaryChatsContext.value?.addChatItem(rhId, cInfo, cItem)
              }
            }
          } else if (cItem.isRcvNew && cInfo.ntfsEnabled(cItem)) {
            withContext(Dispatchers.Main) {
              chatModel.chatsContext.increaseUnreadCounter(rhId, r.user)
            }
          }
          val file = cItem.file
          val mc = cItem.content.msgContent
          if (file != null &&
            appPrefs.privacyAcceptImages.get() &&
            ((mc is MsgContent.MCImage && file.fileSize <= MAX_IMAGE_SIZE_AUTO_RCV)
                || (mc is MsgContent.MCVideo && file.fileSize <= MAX_VIDEO_SIZE_AUTO_RCV)
                || (mc is MsgContent.MCVoice && file.fileSize <= MAX_VOICE_SIZE_AUTO_RCV && file.fileStatus !is CIFileStatus.RcvAccepted))
          ) {
            receiveFile(rhId, r.user, file.fileId, auto = true)
          }
          ntfManager.notifyMessageReceived(rhId, r.user, cInfo, cItem)
        }
      }
      is CR.ChatItemsStatusesUpdated ->
        r.chatItems.forEach { chatItem ->
          val cInfo = chatItem.chatInfo
          val cItem = chatItem.chatItem
          if (!cItem.isDeletedContent && active(r.user)) {
            withContext(Dispatchers.Main) {
              chatModel.chatsContext.updateChatItem(cInfo, cItem, status = cItem.meta.itemStatus)
            }
            withContext(Dispatchers.Main) {
              if (cItem.isReport) {
                chatModel.secondaryChatsContext.value?.updateChatItem(cInfo, cItem, status = cItem.meta.itemStatus)
              }
            }
          }
        }
      is CR.ChatItemUpdated ->
        chatItemUpdateNotify(rhId, r.user, r.chatItem)
      is CR.ChatItemReaction -> {
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.updateChatItem(r.reaction.chatInfo, r.reaction.chatReaction.chatItem)
          }
          withContext(Dispatchers.Main) {
            if (r.reaction.chatReaction.chatItem.isReport) {
              chatModel.secondaryChatsContext.value?.updateChatItem(r.reaction.chatInfo, r.reaction.chatReaction.chatItem)
            }
          }
        }
      }
      is CR.ChatItemsDeleted -> {
        if (!active(r.user)) {
          r.chatItemDeletions.forEach { (deletedChatItem, toChatItem) ->
            if (toChatItem == null && deletedChatItem.chatItem.isRcvNew && deletedChatItem.chatInfo.ntfsEnabled(deletedChatItem.chatItem)) {
              withContext(Dispatchers.Main) {
                chatModel.chatsContext.decreaseUnreadCounter(rhId, r.user)
              }
            }
          }
          return
        }
        r.chatItemDeletions.forEach { (deletedChatItem, toChatItem) ->
          val cInfo = deletedChatItem.chatInfo
          val cItem = deletedChatItem.chatItem
          if (chatModel.chatId.value != null) {
            // Stop voice playback only inside a chat, allow to play in a chat list
            AudioPlayer.stop(cItem)
          }
          val isLastChatItem = chatModel.getChat(cInfo.id)?.chatItems?.lastOrNull()?.id == cItem.id
          if (isLastChatItem && ntfManager.hasNotificationsForChat(cInfo.id)) {
            ntfManager.cancelNotificationsForChat(cInfo.id)
            ntfManager.displayNotification(
              r.user,
              cInfo.id,
              cInfo.displayName,
              generalGetString(if (toChatItem != null) MR.strings.marked_deleted_description else MR.strings.deleted_description)
            )
          }
          withContext(Dispatchers.Main) {
            if (toChatItem == null) {
              chatModel.chatsContext.removeChatItem(rhId, cInfo, cItem)
            } else {
              chatModel.chatsContext.upsertChatItem(rhId, cInfo, toChatItem.chatItem)
            }
            if (cItem.isActiveReport) {
              chatModel.chatsContext.decreaseGroupReportsCounter(rhId, cInfo.id)
            }
          }
          withContext(Dispatchers.Main) {
            if (cItem.isReport) {
              if (toChatItem == null) {
                chatModel.secondaryChatsContext.value?.removeChatItem(rhId, cInfo, cItem)
              } else {
                chatModel.secondaryChatsContext.value?.upsertChatItem(rhId, cInfo, toChatItem.chatItem)
              }
            }
          }
        }
      }
      is CR.GroupChatItemsDeleted -> {
        groupChatItemsDeleted(rhId, r)
      }
      is CR.ReceivedGroupInvitation -> {
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            // update so that repeat group invitations are not duplicated
            chatModel.chatsContext.updateGroup(rhId, r.groupInfo)
          }
          // TODO NtfManager.shared.notifyGroupInvitation
        }
      }
      is CR.UserAcceptedGroupSent -> {
        if (!active(r.user)) return

        withContext(Dispatchers.Main) {
          chatModel.chatsContext.updateGroup(rhId, r.groupInfo)
          val conn = r.hostContact?.activeConn
          if (conn != null) {
            chatModel.replaceConnReqView(conn.id, "#${r.groupInfo.groupId}")
            chatModel.chatsContext.removeChat(rhId, conn.id)
          }
        }
      }
      is CR.GroupLinkConnecting -> {
        if (!active(r.user)) return

        withContext(Dispatchers.Main) {
          chatModel.chatsContext.updateGroup(rhId, r.groupInfo)
          val hostConn = r.hostMember.activeConn
          if (hostConn != null) {
            chatModel.replaceConnReqView(hostConn.id, "#${r.groupInfo.groupId}")
            chatModel.chatsContext.removeChat(rhId, hostConn.id)
          }
        }
      }
      is CR.BusinessLinkConnecting -> {
        if (!active(r.user)) return

        withContext(Dispatchers.Main) {
          chatModel.chatsContext.updateGroup(rhId, r.groupInfo)
        }
        if (chatModel.chatId.value == r.fromContact.id) {
          openGroupChat(rhId, r.groupInfo.groupId)
        }
        withContext(Dispatchers.Main) {
          chatModel.chatsContext.removeChat(rhId, r.fromContact.id)
        }
      }
      is CR.JoinedGroupMemberConnecting ->
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.upsertGroupMember(rhId, r.groupInfo, r.member)
          }
        }
      is CR.DeletedMemberUser -> // TODO update user member
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.updateGroup(rhId, r.groupInfo)
            if (r.withMessages) {
              chatModel.chatsContext.removeMemberItems(rhId, r.groupInfo.membership, byMember = r.member, r.groupInfo)
            }
          }
          withContext(Dispatchers.Main) {
            if (r.withMessages) {
              chatModel.secondaryChatsContext.value?.removeMemberItems(rhId, r.groupInfo.membership, byMember = r.member, r.groupInfo)
            }
          }
        }
      is CR.DeletedMember ->
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.upsertGroupMember(rhId, r.groupInfo, r.deletedMember)
            if (r.withMessages) {
              chatModel.chatsContext.removeMemberItems(rhId, r.deletedMember, byMember = r.byMember, r.groupInfo)
            }
          }
          withContext(Dispatchers.Main) {
            chatModel.secondaryChatsContext.value?.upsertGroupMember(rhId, r.groupInfo, r.deletedMember)
            if (r.withMessages) {
              chatModel.secondaryChatsContext.value?.removeMemberItems(rhId, r.deletedMember, byMember = r.byMember, r.groupInfo)
            }
          }
        }
      is CR.LeftMember ->
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.upsertGroupMember(rhId, r.groupInfo, r.member)
          }
          withContext(Dispatchers.Main) {
            chatModel.secondaryChatsContext.value?.upsertGroupMember(rhId, r.groupInfo, r.member)
          }
        }
      is CR.MemberRole ->
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.upsertGroupMember(rhId, r.groupInfo, r.member)
          }
          withContext(Dispatchers.Main) {
            chatModel.secondaryChatsContext.value?.upsertGroupMember(rhId, r.groupInfo, r.member)
          }
        }
      is CR.MembersRoleUser ->
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            r.members.forEach { member ->
              chatModel.chatsContext.upsertGroupMember(rhId, r.groupInfo, member)
            }
          }
          withContext(Dispatchers.Main) {
            r.members.forEach { member ->
              chatModel.secondaryChatsContext.value?.upsertGroupMember(rhId, r.groupInfo, member)
            }
          }
        }
      is CR.MemberBlockedForAll ->
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.upsertGroupMember(rhId, r.groupInfo, r.member)
          }
          withContext(Dispatchers.Main) {
            chatModel.secondaryChatsContext.value?.upsertGroupMember(rhId, r.groupInfo, r.member)
          }
        }
      is CR.GroupDeleted -> // TODO update user member
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.updateGroup(rhId, r.groupInfo)
          }
        }
      is CR.UserJoinedGroup ->
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.updateGroup(rhId, r.groupInfo)
          }
        }
      is CR.JoinedGroupMember ->
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.upsertGroupMember(rhId, r.groupInfo, r.member)
          }
        }
      is CR.ConnectedToGroupMember -> {
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.upsertGroupMember(rhId, r.groupInfo, r.member)
          }
        }
        if (r.memberContact != null) {
          chatModel.setContactNetworkStatus(r.memberContact, NetworkStatus.Connected())
        }
      }
      is CR.GroupUpdated ->
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.updateGroup(rhId, r.toGroup)
          }
        }
      is CR.NewMemberContactReceivedInv ->
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.updateContact(rhId, r.contact)
          }
        }
      is CR.RcvFileStart ->
        chatItemSimpleUpdate(rhId, r.user, r.chatItem)
      is CR.RcvFileComplete ->
        chatItemSimpleUpdate(rhId, r.user, r.chatItem)
      is CR.RcvFileSndCancelled -> {
        chatItemSimpleUpdate(rhId, r.user, r.chatItem)
        cleanupFile(r.chatItem)
      }
      is CR.RcvFileProgressXFTP -> {
        if (r.chatItem_ != null) {
          chatItemSimpleUpdate(rhId, r.user, r.chatItem_)
        }
      }
      is CR.RcvFileError -> {
        if (r.chatItem_ != null) {
          chatItemSimpleUpdate(rhId, r.user, r.chatItem_)
          cleanupFile(r.chatItem_)
        }
      }
      is CR.RcvFileWarning -> {
        if (r.chatItem_ != null) {
          chatItemSimpleUpdate(rhId, r.user, r.chatItem_)
        }
      }
      is CR.SndFileStart ->
        chatItemSimpleUpdate(rhId, r.user, r.chatItem)
      is CR.SndFileComplete -> {
        chatItemSimpleUpdate(rhId, r.user, r.chatItem)
        cleanupDirectFile(r.chatItem)
      }
      is CR.SndFileRcvCancelled -> {
        if (r.chatItem_ != null) {
          chatItemSimpleUpdate(rhId, r.user, r.chatItem_)
          cleanupDirectFile(r.chatItem_)
        }
      }
      is CR.SndFileProgressXFTP -> {
        if (r.chatItem_ != null) {
          chatItemSimpleUpdate(rhId, r.user, r.chatItem_)
        }
      }
      is CR.SndFileCompleteXFTP -> {
        chatItemSimpleUpdate(rhId, r.user, r.chatItem)
      }
      is CR.SndFileError -> {
        if (r.chatItem_ != null) {
          chatItemSimpleUpdate(rhId, r.user, r.chatItem_)
          cleanupFile(r.chatItem_)
        }
      }
      is CR.SndFileWarning -> {
        if (r.chatItem_ != null) {
          chatItemSimpleUpdate(rhId, r.user, r.chatItem_)
        }
      }
      is CR.CallInvitation -> {
        chatModel.callManager.reportNewIncomingCall(r.callInvitation.copy(remoteHostId = rhId))
      }
      is CR.CallOffer -> {
        // TODO askConfirmation?
        // TODO check encryption is compatible
        withCall(r, r.contact) { call ->
          chatModel.activeCall.value = call.copy(callState = CallState.OfferReceived, sharedKey = r.sharedKey)
          val useRelay = appPrefs.webrtcPolicyRelay.get()
          val iceServers = getIceServers()
          Log.d(TAG, ".callOffer iceServers $iceServers")
          chatModel.callCommand.add(WCallCommand.Offer(
            offer = r.offer.rtcSession,
            iceCandidates = r.offer.rtcIceCandidates,
            media = r.callType.media,
            aesKey = r.sharedKey,
            iceServers = iceServers,
            relay = useRelay
          ))
        }
      }
      is CR.CallAnswer -> {
        withCall(r, r.contact) { call ->
          chatModel.activeCall.value = call.copy(callState = CallState.AnswerReceived)
          chatModel.callCommand.add(WCallCommand.Answer(answer = r.answer.rtcSession, iceCandidates = r.answer.rtcIceCandidates))
        }
      }
      is CR.CallExtraInfo -> {
        withCall(r, r.contact) { _ ->
          chatModel.callCommand.add(WCallCommand.Ice(iceCandidates = r.extraInfo.rtcIceCandidates))
        }
      }
      is CR.CallEnded -> {
        val invitation = chatModel.callInvitations.remove(r.contact.id)
        if (invitation != null) {
          chatModel.callManager.reportCallRemoteEnded(invitation = invitation)
        }
        withCall(r, r.contact) { call ->
          withBGApi { chatModel.callManager.endCall(call) }
        }
      }
      is CR.ContactSwitch ->
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.updateContactConnectionStats(rhId, r.contact, r.switchProgress.connectionStats)
          }
        }
      is CR.GroupMemberSwitch ->
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.updateGroupMemberConnectionStats(rhId, r.groupInfo, r.member, r.switchProgress.connectionStats)
          }
        }
      is CR.ContactRatchetSync ->
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.updateContactConnectionStats(rhId, r.contact, r.ratchetSyncProgress.connectionStats)
          }
        }
      is CR.GroupMemberRatchetSync ->
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.updateGroupMemberConnectionStats(rhId, r.groupInfo, r.member, r.ratchetSyncProgress.connectionStats)
          }
        }
      is CR.RemoteHostSessionCode -> {
        chatModel.remoteHostPairing.value = r.remoteHost_ to RemoteHostSessionState.PendingConfirmation(r.sessionCode)
      }
      is CR.RemoteHostConnected -> {
        // TODO needs to update it instead in sessions
        chatModel.currentRemoteHost.value = r.remoteHost
        ModalManager.start.closeModals()
        switchUIRemoteHost(r.remoteHost.remoteHostId)
      }
      is CR.ContactDisabled -> {
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.updateContact(rhId, r.contact)
          }
        }
      }
      is CR.RemoteHostStopped -> {
        val disconnectedHost = chatModel.remoteHosts.firstOrNull { it.remoteHostId == r.remoteHostId_ }
        chatModel.remoteHostPairing.value = null
        if (disconnectedHost != null) {
          val deviceName = disconnectedHost.hostDeviceName.ifEmpty { disconnectedHost.remoteHostId.toString() }
          when (r.rhStopReason) {
            is RemoteHostStopReason.ConnectionFailed -> {
              AlertManager.shared.showAlertMsg(
                generalGetString(MR.strings.remote_host_was_disconnected_title),
                if (r.rhStopReason.chatError is ChatError.ChatErrorRemoteHost) {
                  r.rhStopReason.chatError.remoteHostError.localizedString(deviceName)
                } else {
                  generalGetString(MR.strings.remote_host_disconnected_from).format(deviceName, r.rhStopReason.chatError.string)
                }
              )
            }
            is RemoteHostStopReason.Crashed -> {
              AlertManager.shared.showAlertMsg(
                generalGetString(MR.strings.remote_host_was_disconnected_title),
                if (r.rhStopReason.chatError is ChatError.ChatErrorRemoteHost) {
                  r.rhStopReason.chatError.remoteHostError.localizedString(deviceName)
                } else {
                  generalGetString(MR.strings.remote_host_disconnected_from).format(deviceName, r.rhStopReason.chatError.string)
                }
              )
            }
            is RemoteHostStopReason.Disconnected -> {
              if (r.rhsState is RemoteHostSessionState.Connected || r.rhsState is RemoteHostSessionState.Confirmed) {
                showToast(generalGetString(MR.strings.remote_host_was_disconnected_toast).format(deviceName))
              }
            }
          }
        }
        if (chatModel.remoteHostId() == r.remoteHostId_) {
          chatModel.currentRemoteHost.value = null
          switchUIRemoteHost(null)
        }
      }
      is CR.RemoteCtrlFound -> {
        val sess = chatModel.remoteCtrlSession.value
        if (sess != null && sess.sessionState is UIRemoteCtrlSessionState.Searching) {
          val state = UIRemoteCtrlSessionState.Found(remoteCtrl = r.remoteCtrl, compatible = r.compatible)
          chatModel.remoteCtrlSession.value = RemoteCtrlSession(
            ctrlAppInfo = r.ctrlAppInfo_,
            appVersion = r.appVersion,
            sessionState = state
          )
        }
      }
      is CR.RemoteCtrlSessionCode -> {
        val state = UIRemoteCtrlSessionState.PendingConfirmation(remoteCtrl_ = r.remoteCtrl_, sessionCode = r.sessionCode)
        chatModel.remoteCtrlSession.value = chatModel.remoteCtrlSession.value?.copy(sessionState = state)
      }
      is CR.RemoteCtrlConnected -> {
        // TODO currently it is returned in response to command, so it is redundant
        val state = UIRemoteCtrlSessionState.Connected(remoteCtrl = r.remoteCtrl, sessionCode = chatModel.remoteCtrlSession.value?.sessionCode ?: "")
        chatModel.remoteCtrlSession.value = chatModel.remoteCtrlSession.value?.copy(sessionState = state)
      }
      is CR.RemoteCtrlStopped -> {
        val sess = chatModel.remoteCtrlSession.value
        if (sess != null) {
          chatModel.remoteCtrlSession.value = null
          ModalManager.fullscreen.closeModals()
          fun showAlert(chatError: ChatError) {
            when {
              r.rcStopReason is RemoteCtrlStopReason.Disconnected ->
                {}
              r.rcStopReason is RemoteCtrlStopReason.ConnectionFailed
                  && r.rcStopReason.chatError is ChatError.ChatErrorAgent
                  && r.rcStopReason.chatError.agentError is AgentErrorType.RCP
                  && r.rcStopReason.chatError.agentError.rcpErr is RCErrorType.IDENTITY ->
                AlertManager.shared.showAlertMsg(
                  title = generalGetString(MR.strings.remote_ctrl_was_disconnected_title),
                  text = generalGetString(MR.strings.remote_ctrl_connection_stopped_identity_desc)
                )
              else ->
                AlertManager.shared.showAlertDialogButtonsColumn(
                  title = generalGetString(MR.strings.remote_ctrl_was_disconnected_title),
                  text = if (chatError is ChatError.ChatErrorRemoteCtrl) {
                    chatError.remoteCtrlError.localizedString
                  } else {
                    generalGetString(MR.strings.remote_ctrl_connection_stopped_desc)
                  },
                  buttons = {
                    Column {
                      SectionItemView({
                        AlertManager.shared.hideAlert()
                      }) {
                        Text(stringResource(MR.strings.ok), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
                      }
                      val clipboard = LocalClipboardManager.current
                      SectionItemView({
                        clipboard.setText(AnnotatedString(json.encodeToString(r.rcStopReason)))
                        AlertManager.shared.hideAlert()
                      }) {
                        Text(stringResource(MR.strings.copy_error), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
                      }
                    }
                  }
                )
            }
          }
          when (r.rcStopReason) {
            is RemoteCtrlStopReason.DiscoveryFailed -> showAlert(r.rcStopReason.chatError)
            is RemoteCtrlStopReason.ConnectionFailed -> showAlert(r.rcStopReason.chatError)
            is RemoteCtrlStopReason.SetupFailed -> showAlert(r.rcStopReason.chatError)
            is RemoteCtrlStopReason.Disconnected -> {
              /*AlertManager.shared.showAlertMsg(
                generalGetString(MR.strings.remote_ctrl_was_disconnected_title),
              )*/
            }
          }

          if (sess.sessionState is UIRemoteCtrlSessionState.Connected) {
            switchToLocalSession()
          }
        }
      }
      is CR.ContactPQEnabled ->
        if (active(r.user)) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.updateContact(rhId, r.contact)
          }
        }
      else ->
        Log.d(TAG , "unsupported event: ${msg.responseType}")
    }
    val e = (msg as? API.Error)?.err
    when {
      e is ChatError.ChatErrorAgent && e.agentError is AgentErrorType.CRITICAL ->
        chatModel.processedCriticalError.newError(e.agentError, e.agentError.offerRestart)
      e is ChatError.ChatErrorAgent && e.agentError is AgentErrorType.INTERNAL && appPrefs.developerTools.get() && appPrefs.showInternalErrors.get() ->
        chatModel.processedInternalError.newError(e.agentError, false)
      else ->
        Log.d(TAG , "unsupported event: ${msg.responseType}")
    }
  }

  private fun cleanupDirectFile(aChatItem: AChatItem) {
    if (aChatItem.chatInfo.chatType == ChatType.Direct) {
      cleanupFile(aChatItem)
    }
  }

  private fun cleanupFile(aChatItem: AChatItem) {
    val cItem = aChatItem.chatItem
    val mc = cItem.content.msgContent
    val fileName = cItem.file?.fileName
    if (
      mc is MsgContent.MCFile
      && fileName != null
    ) {
      removeFile(fileName)
    }
  }

  suspend fun switchToLocalSession() {
    val m = chatModel
    m.remoteCtrlSession.value = null
    val users = listUsers(null)
    m.users.clear()
    m.users.addAll(users)
    getUserChatData(null)
    val statuses = apiGetNetworkStatuses(null)
    if (statuses != null) {
      chatModel.networkStatuses.clear()
      val ss = statuses.associate { it.agentConnId to it.networkStatus }.toMap()
      chatModel.networkStatuses.putAll(ss)
    }
  }

  private fun activeUser(rhId: Long?, user: UserLike): Boolean =
    rhId == chatModel.remoteHostId() && user.userId == chatModel.currentUser.value?.userId

  private fun withCall(r: CR, contact: Contact, perform: (Call) -> Unit) {
    val call = chatModel.activeCall.value
    if (call != null && call.contact.apiId == contact.apiId) {
      perform(call)
    } else {
      Log.d(TAG, "processReceivedMsg: ignoring ${r.responseType}, not in call with the contact ${contact.id}")
    }
  }

  suspend fun leaveGroup(rh: Long?, groupId: Long) {
    val groupInfo = apiLeaveGroup(rh, groupId)
    if (groupInfo != null) {
      withContext(Dispatchers.Main) {
        chatModel.chatsContext.updateGroup(rh, groupInfo)
      }
    }
  }

  private suspend fun chatItemSimpleUpdate(rh: Long?, user: UserLike, aChatItem: AChatItem) {
    if (activeUser(rh, user)) {
      val cInfo = aChatItem.chatInfo
      val cItem = aChatItem.chatItem
      withContext(Dispatchers.Main) { chatModel.chatsContext.upsertChatItem(rh, cInfo, cItem) }
      withContext(Dispatchers.Main) {
        if (cItem.isReport) {
          chatModel.secondaryChatsContext.value?.upsertChatItem(rh, cInfo, cItem)
        }
      }
    }
  }

  suspend fun groupChatItemsDeleted(rhId: Long?, r: CR.GroupChatItemsDeleted) {
    if (!activeUser(rhId, r.user)) {
      val users = chatController.listUsers(rhId)
      chatModel.users.clear()
      chatModel.users.addAll(users)
      return
    }
    val cInfo = ChatInfo.Group(r.groupInfo)
    withContext(Dispatchers.Main) {
      val chatsCtx = chatModel.chatsContext
      r.chatItemIDs.forEach { itemId ->
        chatsCtx.decreaseGroupReportsCounter(rhId, cInfo.id)
        val cItem = chatsCtx.chatItems.value.lastOrNull { it.id == itemId } ?: return@forEach
        if (chatModel.chatId.value != null) {
          // Stop voice playback only inside a chat, allow to play in a chat list
          AudioPlayer.stop(cItem)
        }
        val isLastChatItem = chatsCtx.getChat(cInfo.id)?.chatItems?.lastOrNull()?.id == cItem.id
        if (isLastChatItem && ntfManager.hasNotificationsForChat(cInfo.id)) {
          ntfManager.cancelNotificationsForChat(cInfo.id)
          ntfManager.displayNotification(
            r.user,
            cInfo.id,
            cInfo.displayName,
            generalGetString(MR.strings.marked_deleted_description)
          )
        }
        val deleted = if (r.member_ != null && (cItem.chatDir as CIDirection.GroupRcv?)?.groupMember?.groupMemberId != r.member_.groupMemberId) {
          CIDeleted.Moderated(Clock.System.now(), r.member_)
        } else {
          CIDeleted.Deleted(Clock.System.now())
        }
        chatsCtx.upsertChatItem(rhId, cInfo, cItem.copy(meta = cItem.meta.copy(itemDeleted = deleted)))
      }
    }
    withContext(Dispatchers.Main) {
      val chatsCtx = chatModel.secondaryChatsContext.value
      if (chatsCtx != null) {
        r.chatItemIDs.forEach { itemId ->
          val cItem = chatsCtx.chatItems.value.lastOrNull { it.id == itemId } ?: return@forEach
          if (chatModel.chatId.value != null) {
            // Stop voice playback only inside a chat, allow to play in a chat list
            AudioPlayer.stop(cItem)
          }
          val deleted = if (r.member_ != null && (cItem.chatDir as CIDirection.GroupRcv?)?.groupMember?.groupMemberId != r.member_.groupMemberId) {
            CIDeleted.Moderated(Clock.System.now(), r.member_)
          } else {
            CIDeleted.Deleted(Clock.System.now())
          }
          chatsCtx.upsertChatItem(rhId, cInfo, cItem.copy(meta = cItem.meta.copy(itemDeleted = deleted)))
        }
      }
    }
  }

  private suspend fun chatItemUpdateNotify(rh: Long?, user: UserLike, aChatItem: AChatItem) {
    val cInfo = aChatItem.chatInfo
    val cItem = aChatItem.chatItem
    val notify = { ntfManager.notifyMessageReceived(rh, user, cInfo, cItem) }
    if (!activeUser(rh, user)) {
      notify()
    } else {
      val createdChat = withContext(Dispatchers.Main) { chatModel.chatsContext.upsertChatItem(rh, cInfo, cItem) }
      withContext(Dispatchers.Main) {
        if (cItem.content.msgContent is MsgContent.MCReport) {
          chatModel.secondaryChatsContext.value?.upsertChatItem(rh, cInfo, cItem)
        }
      }
      if (createdChat) {
        notify()
      } else if (cItem.content is CIContent.RcvCall && cItem.content.status == CICallStatus.Missed) {
        notify()
      }
    }
  }

  private fun updateContactsStatus(contactRefs: List<ContactRef>, status: NetworkStatus) {
    for (c in contactRefs) {
      chatModel.networkStatuses[c.agentConnId] = status
    }
  }

  private fun processContactSubError(contact: Contact, chatError: ChatError) {
    val e = chatError
    val err: String =
      if (e is ChatError.ChatErrorAgent) {
        val a = e.agentError
        when {
          a is AgentErrorType.BROKER && a.brokerErr is BrokerErrorType.NETWORK -> "network"
          a is AgentErrorType.SMP && a.smpErr is SMPErrorType.AUTH -> "contact deleted"
          else -> e.string
        }
      }
      else e.string
    chatModel.setContactNetworkStatus(contact, NetworkStatus.Error(err))
  }

  suspend fun switchUIRemoteHost(rhId: Long?) = showProgressIfNeeded {
    // TODO lock the switch so that two switches can't run concurrently?
    chatModel.chatId.value = null
    ModalManager.center.closeModals()
    ModalManager.end.closeModals()
    AlertManager.shared.hideAllAlerts()
    AlertManager.privacySensitive.hideAllAlerts()
    chatModel.currentRemoteHost.value = switchRemoteHost(rhId)
    reloadRemoteHosts()
    val user = apiGetActiveUser(rhId)
    val users = listUsers(rhId)
    chatModel.users.clear()
    chatModel.users.addAll(users)
    chatModel.currentUser.value = user
    if (user == null) {
      withContext(Dispatchers.Main) {
        chatModel.chatsContext.chatItems.clearAndNotify()
        chatModel.chatsContext.chats.clear()
        chatModel.chatsContext.popChatCollector.clear()
      }
      withContext(Dispatchers.Main) {
        chatModel.secondaryChatsContext.value?.chatItems?.clearAndNotify()
        chatModel.secondaryChatsContext.value?.chats?.clear()
        chatModel.secondaryChatsContext.value?.popChatCollector?.clear()
      }
    }
    val statuses = apiGetNetworkStatuses(rhId)
    if (statuses != null) {
      chatModel.networkStatuses.clear()
      val ss = statuses.associate { it.agentConnId to it.networkStatus }.toMap()
      chatModel.networkStatuses.putAll(ss)
    }
    getUserChatData(rhId)
  }

  suspend fun showProgressIfNeeded(block: suspend () -> Unit) {
    val job = withBGApi {
      try {
        delay(500)
        chatModel.switchingUsersAndHosts.value = true
      } catch (e: Throwable) {
        chatModel.switchingUsersAndHosts.value = false
      }
    }
    try {
      block()
    } finally {
      job.cancel()
      chatModel.switchingUsersAndHosts.value = false
    }
  }

  fun getNetCfg(): NetCfg {
    val useSocksProxy = appPrefs.networkUseSocksProxy.get()
    val networkProxy  = appPrefs.networkProxy.get()
    val socksProxy = if (useSocksProxy) {
      networkProxy.toProxyString()
    } else {
      null
    }
    val hostMode = appPrefs.networkHostMode.get()
    val requiredHostMode = appPrefs.networkRequiredHostMode.get()
    val sessionMode = appPrefs.networkSessionMode.get()
    val smpProxyMode = appPrefs.networkSMPProxyMode.get()
    val smpProxyFallback = appPrefs.networkSMPProxyFallback.get()
    val smpWebPortServers = appPrefs.networkSMPWebPortServers.get()
    val tcpConnectTimeout = appPrefs.networkTCPConnectTimeout.get()
    val tcpTimeout = appPrefs.networkTCPTimeout.get()
    val tcpTimeoutPerKb = appPrefs.networkTCPTimeoutPerKb.get()
    val rcvConcurrency = appPrefs.networkRcvConcurrency.get()
    val smpPingInterval = appPrefs.networkSMPPingInterval.get()
    val smpPingCount = appPrefs.networkSMPPingCount.get()
    val enableKeepAlive = appPrefs.networkEnableKeepAlive.get()
    val tcpKeepAlive = if (enableKeepAlive) {
      val keepIdle = appPrefs.networkTCPKeepIdle.get()
      val keepIntvl = appPrefs.networkTCPKeepIntvl.get()
      val keepCnt = appPrefs.networkTCPKeepCnt.get()
      KeepAliveOpts(keepIdle = keepIdle, keepIntvl = keepIntvl, keepCnt = keepCnt)
    } else {
      null
    }
    return NetCfg(
      socksProxy = socksProxy,
      hostMode = hostMode,
      requiredHostMode = requiredHostMode,
      sessionMode = sessionMode,
      smpProxyMode = smpProxyMode,
      smpProxyFallback = smpProxyFallback,
      smpWebPortServers = smpWebPortServers,
      tcpConnectTimeout = tcpConnectTimeout,
      tcpTimeout = tcpTimeout,
      tcpTimeoutPerKb = tcpTimeoutPerKb,
      rcvConcurrency = rcvConcurrency,
      tcpKeepAlive = tcpKeepAlive,
      smpPingInterval = smpPingInterval,
      smpPingCount = smpPingCount
    )
  }

  /**
   * [AppPreferences.networkProxy] is not changed here, use appPrefs to set it
   * */
  fun setNetCfg(cfg: NetCfg) {
    appPrefs.networkUseSocksProxy.set(cfg.useSocksProxy)
    appPrefs.networkHostMode.set(cfg.hostMode)
    appPrefs.networkRequiredHostMode.set(cfg.requiredHostMode)
    appPrefs.networkSessionMode.set(cfg.sessionMode)
    appPrefs.networkSMPProxyMode.set(cfg.smpProxyMode)
    appPrefs.networkSMPProxyFallback.set(cfg.smpProxyFallback)
    appPrefs.networkSMPWebPortServers.set(cfg.smpWebPortServers)
    appPrefs.networkTCPConnectTimeout.set(cfg.tcpConnectTimeout)
    appPrefs.networkTCPTimeout.set(cfg.tcpTimeout)
    appPrefs.networkTCPTimeoutPerKb.set(cfg.tcpTimeoutPerKb)
    appPrefs.networkRcvConcurrency.set(cfg.rcvConcurrency)
    appPrefs.networkSMPPingInterval.set(cfg.smpPingInterval)
    appPrefs.networkSMPPingCount.set(cfg.smpPingCount)
    if (cfg.tcpKeepAlive != null) {
      appPrefs.networkEnableKeepAlive.set(true)
      appPrefs.networkTCPKeepIdle.set(cfg.tcpKeepAlive.keepIdle)
      appPrefs.networkTCPKeepIntvl.set(cfg.tcpKeepAlive.keepIntvl)
      appPrefs.networkTCPKeepCnt.set(cfg.tcpKeepAlive.keepCnt)
    } else {
      appPrefs.networkEnableKeepAlive.set(false)
    }
  }
}

class SharedPreference<T>(val get: () -> T, set: (T) -> Unit) {
  val set: (T) -> Unit
  private val _state: MutableState<T> = mutableStateOf(get())
  val state: State<T> = _state

  init {
    this.set = { value ->
      try {
        set(value)
        _state.value = value
      } catch (e: Exception) {
        Log.e(TAG, "Error saving settings: ${e.stackTraceToString()}")
      }
    }
  }
}

// ChatCommand
sealed class CC {
  class Console(val cmd: String): CC()
  class ShowActiveUser: CC()
  class CreateActiveUser(val profile: Profile?, val pastTimestamp: Boolean): CC()
  class ListUsers: CC()
  class ApiSetActiveUser(val userId: Long, val viewPwd: String?): CC()
  class SetAllContactReceipts(val enable: Boolean): CC()
  class ApiSetUserContactReceipts(val userId: Long, val userMsgReceiptSettings: UserMsgReceiptSettings): CC()
  class ApiSetUserGroupReceipts(val userId: Long, val userMsgReceiptSettings: UserMsgReceiptSettings): CC()
  class ApiHideUser(val userId: Long, val viewPwd: String): CC()
  class ApiUnhideUser(val userId: Long, val viewPwd: String): CC()
  class ApiMuteUser(val userId: Long): CC()
  class ApiUnmuteUser(val userId: Long): CC()
  class ApiDeleteUser(val userId: Long, val delSMPQueues: Boolean, val viewPwd: String?): CC()
  class StartChat(val mainApp: Boolean): CC()
  class CheckChatRunning: CC()
  class ApiStopChat: CC()
  @Serializable
  class ApiSetAppFilePaths(val appFilesFolder: String, val appTempFolder: String, val appAssetsFolder: String, val appRemoteHostsFolder: String): CC()
  class ApiSetEncryptLocalFiles(val enable: Boolean): CC()
  class ApiExportArchive(val config: ArchiveConfig): CC()
  class ApiImportArchive(val config: ArchiveConfig): CC()
  class ApiDeleteStorage: CC()
  class ApiStorageEncryption(val config: DBEncryptionConfig): CC()
  class TestStorageEncryption(val key: String): CC()
  class ApiSaveSettings(val settings: AppSettings): CC()
  class ApiGetSettings(val settings: AppSettings): CC()
  class ApiGetChatTags(val userId: Long): CC()
  class ApiGetChats(val userId: Long): CC()
  class ApiGetChat(val type: ChatType, val id: Long, val contentTag: MsgContentTag?, val pagination: ChatPagination, val search: String = ""): CC()
  class ApiGetChatItemInfo(val type: ChatType, val id: Long, val itemId: Long): CC()
  class ApiSendMessages(val type: ChatType, val id: Long, val live: Boolean, val ttl: Int?, val composedMessages: List<ComposedMessage>): CC()
  class ApiCreateChatTag(val tag: ChatTagData): CC()
  class ApiSetChatTags(val type: ChatType, val id: Long, val tagIds: List<Long>): CC()
  class ApiDeleteChatTag(val tagId: Long): CC()
  class ApiUpdateChatTag(val tagId: Long, val tagData: ChatTagData): CC()
  class ApiReorderChatTags(val tagIds: List<Long>): CC()
  class ApiCreateChatItems(val noteFolderId: Long, val composedMessages: List<ComposedMessage>): CC()
  class ApiReportMessage(val groupId: Long, val chatItemId: Long, val reportReason: ReportReason, val reportText: String): CC()
  class ApiUpdateChatItem(val type: ChatType, val id: Long, val itemId: Long, val updatedMessage: UpdatedMessage, val live: Boolean): CC()
  class ApiDeleteChatItem(val type: ChatType, val id: Long, val itemIds: List<Long>, val mode: CIDeleteMode): CC()
  class ApiDeleteMemberChatItem(val groupId: Long, val itemIds: List<Long>): CC()
  class ApiArchiveReceivedReports(val groupId: Long): CC()
  class ApiDeleteReceivedReports(val groupId: Long, val itemIds: List<Long>, val mode: CIDeleteMode): CC()
  class ApiChatItemReaction(val type: ChatType, val id: Long, val itemId: Long, val add: Boolean, val reaction: MsgReaction): CC()
  class ApiGetReactionMembers(val userId: Long, val groupId: Long, val itemId: Long, val reaction: MsgReaction): CC()
  class ApiPlanForwardChatItems(val fromChatType: ChatType, val fromChatId: Long, val chatItemIds: List<Long>): CC()
  class ApiForwardChatItems(val toChatType: ChatType, val toChatId: Long, val fromChatType: ChatType, val fromChatId: Long, val itemIds: List<Long>, val ttl: Int?): CC()
  class ApiNewGroup(val userId: Long, val incognito: Boolean, val groupProfile: GroupProfile): CC()
  class ApiAddMember(val groupId: Long, val contactId: Long, val memberRole: GroupMemberRole): CC()
  class ApiJoinGroup(val groupId: Long): CC()
  class ApiMembersRole(val groupId: Long, val memberIds: List<Long>, val memberRole: GroupMemberRole): CC()
  class ApiBlockMembersForAll(val groupId: Long, val memberIds: List<Long>, val blocked: Boolean): CC()
  class ApiRemoveMembers(val groupId: Long, val memberIds: List<Long>, val withMessages: Boolean): CC()
  class ApiLeaveGroup(val groupId: Long): CC()
  class ApiListMembers(val groupId: Long): CC()
  class ApiUpdateGroupProfile(val groupId: Long, val groupProfile: GroupProfile): CC()
  class APICreateGroupLink(val groupId: Long, val memberRole: GroupMemberRole, val short: Boolean): CC()
  class APIGroupLinkMemberRole(val groupId: Long, val memberRole: GroupMemberRole): CC()
  class APIDeleteGroupLink(val groupId: Long): CC()
  class APIGetGroupLink(val groupId: Long): CC()
  class APICreateMemberContact(val groupId: Long, val groupMemberId: Long): CC()
  class APISendMemberContactInvitation(val contactId: Long, val mc: MsgContent): CC()
  class APITestProtoServer(val userId: Long, val server: String): CC()
  class ApiGetServerOperators(): CC()
  class ApiSetServerOperators(val operators: List<ServerOperator>): CC()
  class ApiGetUserServers(val userId: Long): CC()
  class ApiSetUserServers(val userId: Long, val userServers: List<UserOperatorServers>): CC()
  class ApiValidateServers(val userId: Long, val userServers: List<UserOperatorServers>): CC()
  class ApiGetUsageConditions(): CC()
  class ApiSetConditionsNotified(val conditionsId: Long): CC()
  class ApiAcceptConditions(val conditionsId: Long, val operatorIds: List<Long>): CC()
  class APISetChatItemTTL(val userId: Long, val seconds: Long): CC()
  class APIGetChatItemTTL(val userId: Long): CC()
  class APISetChatTTL(val userId: Long, val chatType: ChatType, val id: Long, val seconds: Long?): CC()
  class APISetNetworkConfig(val networkConfig: NetCfg): CC()
  class APIGetNetworkConfig: CC()
  class APISetNetworkInfo(val networkInfo: UserNetworkInfo): CC()
  class ReconnectServer(val userId: Long, val server: String): CC()
  class ReconnectAllServers: CC()
  class APISetChatSettings(val type: ChatType, val id: Long, val chatSettings: ChatSettings): CC()
  class ApiSetMemberSettings(val groupId: Long, val groupMemberId: Long, val memberSettings: GroupMemberSettings): CC()
  class APIContactInfo(val contactId: Long): CC()
  class APIGroupMemberInfo(val groupId: Long, val groupMemberId: Long): CC()
  class APIContactQueueInfo(val contactId: Long): CC()
  class APIGroupMemberQueueInfo(val groupId: Long, val groupMemberId: Long): CC()
  class APISwitchContact(val contactId: Long): CC()
  class APISwitchGroupMember(val groupId: Long, val groupMemberId: Long): CC()
  class APIAbortSwitchContact(val contactId: Long): CC()
  class APIAbortSwitchGroupMember(val groupId: Long, val groupMemberId: Long): CC()
  class APISyncContactRatchet(val contactId: Long, val force: Boolean): CC()
  class APISyncGroupMemberRatchet(val groupId: Long, val groupMemberId: Long, val force: Boolean): CC()
  class APIGetContactCode(val contactId: Long): CC()
  class APIGetGroupMemberCode(val groupId: Long, val groupMemberId: Long): CC()
  class APIVerifyContact(val contactId: Long, val connectionCode: String?): CC()
  class APIVerifyGroupMember(val groupId: Long, val groupMemberId: Long, val connectionCode: String?): CC()
  class APIAddContact(val userId: Long, val short: Boolean, val incognito: Boolean): CC()
  class ApiSetConnectionIncognito(val connId: Long, val incognito: Boolean): CC()
  class ApiChangeConnectionUser(val connId: Long, val userId: Long): CC()
  class APIConnectPlan(val userId: Long, val connLink: String): CC()
  class APIConnect(val userId: Long, val incognito: Boolean, val connLink: CreatedConnLink): CC()
  class ApiConnectContactViaAddress(val userId: Long, val incognito: Boolean, val contactId: Long): CC()
  class ApiDeleteChat(val type: ChatType, val id: Long, val chatDeleteMode: ChatDeleteMode): CC()
  class ApiClearChat(val type: ChatType, val id: Long): CC()
  class ApiListContacts(val userId: Long): CC()
  class ApiUpdateProfile(val userId: Long, val profile: Profile): CC()
  class ApiSetContactPrefs(val contactId: Long, val prefs: ChatPreferences): CC()
  class ApiSetContactAlias(val contactId: Long, val localAlias: String): CC()
  class ApiSetGroupAlias(val groupId: Long, val localAlias: String): CC()
  class ApiSetConnectionAlias(val connId: Long, val localAlias: String): CC()
  class ApiSetUserUIThemes(val userId: Long, val themes: ThemeModeOverrides?): CC()
  class ApiSetChatUIThemes(val chatId: String, val themes: ThemeModeOverrides?): CC()
  class ApiCreateMyAddress(val userId: Long, val short: Boolean): CC()
  class ApiDeleteMyAddress(val userId: Long): CC()
  class ApiShowMyAddress(val userId: Long): CC()
  class ApiSetProfileAddress(val userId: Long, val on: Boolean): CC()
  class ApiAddressAutoAccept(val userId: Long, val autoAccept: AutoAccept?): CC()
  class ApiGetCallInvitations: CC()
  class ApiSendCallInvitation(val contact: Contact, val callType: CallType): CC()
  class ApiRejectCall(val contact: Contact): CC()
  class ApiSendCallOffer(val contact: Contact, val callOffer: WebRTCCallOffer): CC()
  class ApiSendCallAnswer(val contact: Contact, val answer: WebRTCSession): CC()
  class ApiSendCallExtraInfo(val contact: Contact, val extraInfo: WebRTCExtraInfo): CC()
  class ApiEndCall(val contact: Contact): CC()
  class ApiCallStatus(val contact: Contact, val callStatus: WebRTCCallStatus): CC()
  class ApiGetNetworkStatuses(): CC()
  class ApiAcceptContact(val incognito: Boolean, val contactReqId: Long): CC()
  class ApiRejectContact(val contactReqId: Long): CC()
  class ApiChatRead(val type: ChatType, val id: Long): CC()
  class ApiChatItemsRead(val type: ChatType, val id: Long, val itemIds: List<Long>): CC()
  class ApiChatUnread(val type: ChatType, val id: Long, val unreadChat: Boolean): CC()
  class ReceiveFile(val fileId: Long, val userApprovedRelays: Boolean, val encrypt: Boolean, val inline: Boolean?): CC()
  class CancelFile(val fileId: Long): CC()
  // Remote control
  class SetLocalDeviceName(val displayName: String): CC()
  class ListRemoteHosts(): CC()
  class StartRemoteHost(val remoteHostId: Long?, val multicast: Boolean, val address: RemoteCtrlAddress?, val port: Int?): CC()
  class SwitchRemoteHost (val remoteHostId: Long?): CC()
  class StopRemoteHost(val remoteHostKey: Long?): CC()
  class DeleteRemoteHost(val remoteHostId: Long): CC()
  class StoreRemoteFile(val remoteHostId: Long, val storeEncrypted: Boolean?, val localPath: String): CC()
  class GetRemoteFile(val remoteHostId: Long, val file: RemoteFile): CC()
  class ConnectRemoteCtrl(val xrcpInvitation: String): CC()
  class FindKnownRemoteCtrl(): CC()
  class ConfirmRemoteCtrl(val remoteCtrlId: Long): CC()
  class VerifyRemoteCtrlSession(val sessionCode: String): CC()
  class ListRemoteCtrls(): CC()
  class StopRemoteCtrl(): CC()
  class DeleteRemoteCtrl(val remoteCtrlId: Long): CC()
  class ApiUploadStandaloneFile(val userId: Long, val file: CryptoFile): CC()
  class ApiDownloadStandaloneFile(val userId: Long, val url: String, val file: CryptoFile): CC()
  class ApiStandaloneFileInfo(val url: String): CC()
  // misc
  class ShowVersion(): CC()
  class ResetAgentServersStats(): CC()
  class GetAgentSubsTotal(val userId: Long): CC()
  class GetAgentServersSummary(val userId: Long): CC()

  val cmdString: String get() = when (this) {
    is Console -> cmd
    is ShowActiveUser -> "/u"
    is CreateActiveUser -> {
      val user = NewUser(profile, pastTimestamp = pastTimestamp)
      "/_create user ${json.encodeToString(user)}"
    }
    is ListUsers -> "/users"
    is ApiSetActiveUser -> "/_user $userId${maybePwd(viewPwd)}"
    is SetAllContactReceipts -> "/set receipts all ${onOff(enable)}"
    is ApiSetUserContactReceipts -> {
      val mrs = userMsgReceiptSettings
      "/_set receipts contacts $userId ${onOff(mrs.enable)} clear_overrides=${onOff(mrs.clearOverrides)}"
    }
    is ApiSetUserGroupReceipts -> {
      val mrs = userMsgReceiptSettings
      "/_set receipts groups $userId ${onOff(mrs.enable)} clear_overrides=${onOff(mrs.clearOverrides)}"
    }
    is ApiHideUser -> "/_hide user $userId ${json.encodeToString(viewPwd)}"
    is ApiUnhideUser -> "/_unhide user $userId ${json.encodeToString(viewPwd)}"
    is ApiMuteUser -> "/_mute user $userId"
    is ApiUnmuteUser -> "/_unmute user $userId"
    is ApiDeleteUser -> "/_delete user $userId del_smp=${onOff(delSMPQueues)}${maybePwd(viewPwd)}"
    is StartChat -> "/_start main=${onOff(mainApp)}"
    is CheckChatRunning -> "/_check running"
    is ApiStopChat -> "/_stop"
    is ApiSetAppFilePaths -> "/set file paths ${json.encodeToString(this)}"
    is ApiSetEncryptLocalFiles -> "/_files_encrypt ${onOff(enable)}"
    is ApiExportArchive -> "/_db export ${json.encodeToString(config)}"
    is ApiImportArchive -> "/_db import ${json.encodeToString(config)}"
    is ApiDeleteStorage -> "/_db delete"
    is ApiStorageEncryption -> "/_db encryption ${json.encodeToString(config)}"
    is TestStorageEncryption -> "/db test key $key"
    is ApiSaveSettings -> "/_save app settings ${json.encodeToString(settings)}"
    is ApiGetSettings -> "/_get app settings ${json.encodeToString(settings)}"
    is ApiGetChatTags -> "/_get tags $userId"
    is ApiGetChats -> "/_get chats $userId pcc=on"
    is ApiGetChat -> {
      val tag = if (contentTag == null) {
        ""
      } else {
        " content=${contentTag.name.lowercase()}"
      }
      "/_get chat ${chatRef(type, id)}$tag ${pagination.cmdString}" + (if (search == "") "" else " search=$search")
    }
    is ApiGetChatItemInfo -> "/_get item info ${chatRef(type, id)} $itemId"
    is ApiSendMessages -> {
      val msgs = json.encodeToString(composedMessages)
      val ttlStr = if (ttl != null) "$ttl" else "default"
      "/_send ${chatRef(type, id)} live=${onOff(live)} ttl=${ttlStr} json $msgs"
    }
    is ApiCreateChatTag -> "/_create tag ${json.encodeToString(tag)}"
    is ApiSetChatTags -> "/_tags ${chatRef(type, id)} ${tagIds.joinToString(",")}"
    is ApiDeleteChatTag -> "/_delete tag $tagId"
    is ApiUpdateChatTag -> "/_update tag $tagId ${json.encodeToString(tagData)}"
    is ApiReorderChatTags -> "/_reorder tags ${tagIds.joinToString(",")}"
    is ApiCreateChatItems -> {
      val msgs = json.encodeToString(composedMessages)
      "/_create *$noteFolderId json $msgs"
    }
    is ApiReportMessage -> "/_report #$groupId $chatItemId reason=${json.encodeToString(reportReason).trim('"')} $reportText"
    is ApiUpdateChatItem -> "/_update item ${chatRef(type, id)} $itemId live=${onOff(live)} ${updatedMessage.cmdString}"
    is ApiDeleteChatItem -> "/_delete item ${chatRef(type, id)} ${itemIds.joinToString(",")} ${mode.deleteMode}"
    is ApiDeleteMemberChatItem -> "/_delete member item #$groupId ${itemIds.joinToString(",")}"
    is ApiArchiveReceivedReports -> "/_archive reports #$groupId"
    is ApiDeleteReceivedReports -> "/_delete reports #$groupId ${itemIds.joinToString(",")} ${mode.deleteMode}"
    is ApiChatItemReaction -> "/_reaction ${chatRef(type, id)} $itemId ${onOff(add)} ${json.encodeToString(reaction)}"
    is ApiGetReactionMembers -> "/_reaction members $userId #$groupId $itemId ${json.encodeToString(reaction)}"
    is ApiForwardChatItems -> {
      val ttlStr = if (ttl != null) "$ttl" else "default"
      "/_forward ${chatRef(toChatType, toChatId)} ${chatRef(fromChatType, fromChatId)} ${itemIds.joinToString(",")} ttl=${ttlStr}"
    }
    is ApiPlanForwardChatItems -> {
      "/_forward plan ${chatRef(fromChatType, fromChatId)} ${chatItemIds.joinToString(",")}"
    }
    is ApiNewGroup -> "/_group $userId incognito=${onOff(incognito)} ${json.encodeToString(groupProfile)}"
    is ApiAddMember -> "/_add #$groupId $contactId ${memberRole.memberRole}"
    is ApiJoinGroup -> "/_join #$groupId"
    is ApiMembersRole -> "/_member role #$groupId ${memberIds.joinToString(",")} ${memberRole.memberRole}"
    is ApiBlockMembersForAll -> "/_block #$groupId ${memberIds.joinToString(",")} blocked=${onOff(blocked)}"
    is ApiRemoveMembers -> "/_remove #$groupId ${memberIds.joinToString(",")} messages=${onOff(withMessages)}"
    is ApiLeaveGroup -> "/_leave #$groupId"
    is ApiListMembers -> "/_members #$groupId"
    is ApiUpdateGroupProfile -> "/_group_profile #$groupId ${json.encodeToString(groupProfile)}"
    is APICreateGroupLink -> "/_create link #$groupId ${memberRole.name.lowercase()} short=${onOff(short)}"
    is APIGroupLinkMemberRole -> "/_set link role #$groupId ${memberRole.name.lowercase()}"
    is APIDeleteGroupLink -> "/_delete link #$groupId"
    is APIGetGroupLink -> "/_get link #$groupId"
    is APICreateMemberContact -> "/_create member contact #$groupId $groupMemberId"
    is APISendMemberContactInvitation -> "/_invite member contact @$contactId ${mc.cmdString}"
    is APITestProtoServer -> "/_server test $userId $server"
    is ApiGetServerOperators -> "/_operators"
    is ApiSetServerOperators -> "/_operators ${json.encodeToString(operators)}"
    is ApiGetUserServers -> "/_servers $userId"
    is ApiSetUserServers -> "/_servers $userId ${json.encodeToString(userServers)}"
    is ApiValidateServers -> "/_validate_servers $userId ${json.encodeToString(userServers)}"
    is ApiGetUsageConditions -> "/_conditions"
    is ApiSetConditionsNotified -> "/_conditions_notified ${conditionsId}"
    is ApiAcceptConditions -> "/_accept_conditions ${conditionsId} ${operatorIds.joinToString(",")}"
    is APISetChatItemTTL -> "/_ttl $userId ${chatItemTTLStr(seconds)}"
    is APIGetChatItemTTL -> "/_ttl $userId"
    is APISetChatTTL -> "/_ttl $userId ${chatRef(chatType, id)} ${chatItemTTLStr(seconds)}"
    is APISetNetworkConfig -> "/_network ${json.encodeToString(networkConfig)}"
    is APIGetNetworkConfig -> "/network"
    is APISetNetworkInfo -> "/_network info ${json.encodeToString(networkInfo)}"
    is ReconnectServer -> "/reconnect $userId $server"
    is ReconnectAllServers -> "/reconnect"
    is APISetChatSettings -> "/_settings ${chatRef(type, id)} ${json.encodeToString(chatSettings)}"
    is ApiSetMemberSettings -> "/_member settings #$groupId $groupMemberId ${json.encodeToString(memberSettings)}"
    is APIContactInfo -> "/_info @$contactId"
    is APIGroupMemberInfo -> "/_info #$groupId $groupMemberId"
    is APIContactQueueInfo -> "/_queue info @$contactId"
    is APIGroupMemberQueueInfo -> "/_queue info #$groupId $groupMemberId"
    is APISwitchContact -> "/_switch @$contactId"
    is APISwitchGroupMember -> "/_switch #$groupId $groupMemberId"
    is APIAbortSwitchContact -> "/_abort switch @$contactId"
    is APIAbortSwitchGroupMember -> "/_abort switch #$groupId $groupMemberId"
    is APISyncContactRatchet -> if (force) "/_sync @$contactId force=on" else "/_sync @$contactId"
    is APISyncGroupMemberRatchet -> if (force) "/_sync #$groupId $groupMemberId force=on" else "/_sync #$groupId $groupMemberId"
    is APIGetContactCode -> "/_get code @$contactId"
    is APIGetGroupMemberCode -> "/_get code #$groupId $groupMemberId"
    is APIVerifyContact -> "/_verify code @$contactId" + if (connectionCode != null) " $connectionCode" else ""
    is APIVerifyGroupMember -> "/_verify code #$groupId $groupMemberId" + if (connectionCode != null) " $connectionCode" else ""
    is APIAddContact -> "/_connect $userId short=${onOff(short)} incognito=${onOff(incognito)}"
    is ApiSetConnectionIncognito -> "/_set incognito :$connId ${onOff(incognito)}"
    is ApiChangeConnectionUser -> "/_set conn user :$connId $userId"
    is APIConnectPlan -> "/_connect plan $userId $connLink"
    is APIConnect -> "/_connect $userId incognito=${onOff(incognito)} ${connLink.connFullLink} ${connLink.connShortLink ?: ""}"
    is ApiConnectContactViaAddress -> "/_connect contact $userId incognito=${onOff(incognito)} $contactId"
    is ApiDeleteChat -> "/_delete ${chatRef(type, id)} ${chatDeleteMode.cmdString}"
    is ApiClearChat -> "/_clear chat ${chatRef(type, id)}"
    is ApiListContacts -> "/_contacts $userId"
    is ApiUpdateProfile -> "/_profile $userId ${json.encodeToString(profile)}"
    is ApiSetContactPrefs -> "/_set prefs @$contactId ${json.encodeToString(prefs)}"
    is ApiSetContactAlias -> "/_set alias @$contactId ${localAlias.trim()}"
    is ApiSetGroupAlias -> "/_set alias #$groupId ${localAlias.trim()}"
    is ApiSetConnectionAlias -> "/_set alias :$connId ${localAlias.trim()}"
    is ApiSetUserUIThemes -> "/_set theme user $userId ${if (themes != null) json.encodeToString(themes) else ""}"
    is ApiSetChatUIThemes -> "/_set theme $chatId ${if (themes != null) json.encodeToString(themes) else ""}"
    is ApiCreateMyAddress -> "/_address $userId short=${onOff(short)}"
    is ApiDeleteMyAddress -> "/_delete_address $userId"
    is ApiShowMyAddress -> "/_show_address $userId"
    is ApiSetProfileAddress -> "/_profile_address $userId ${onOff(on)}"
    is ApiAddressAutoAccept -> "/_auto_accept $userId ${AutoAccept.cmdString(autoAccept)}"
    is ApiAcceptContact -> "/_accept incognito=${onOff(incognito)} $contactReqId"
    is ApiRejectContact -> "/_reject $contactReqId"
    is ApiGetCallInvitations -> "/_call get"
    is ApiSendCallInvitation -> "/_call invite @${contact.apiId} ${json.encodeToString(callType)}"
    is ApiRejectCall -> "/_call reject @${contact.apiId}"
    is ApiSendCallOffer -> "/_call offer @${contact.apiId} ${json.encodeToString(callOffer)}"
    is ApiSendCallAnswer -> "/_call answer @${contact.apiId} ${json.encodeToString(answer)}"
    is ApiSendCallExtraInfo -> "/_call extra @${contact.apiId} ${json.encodeToString(extraInfo)}"
    is ApiEndCall -> "/_call end @${contact.apiId}"
    is ApiCallStatus -> "/_call status @${contact.apiId} ${callStatus.value}"
    is ApiGetNetworkStatuses -> "/_network_statuses"
    is ApiChatRead -> "/_read chat ${chatRef(type, id)}"
    is ApiChatItemsRead -> "/_read chat items ${chatRef(type, id)} ${itemIds.joinToString(",")}"
    is ApiChatUnread -> "/_unread chat ${chatRef(type, id)} ${onOff(unreadChat)}"
    is ReceiveFile ->
      "/freceive $fileId" +
          " approved_relays=${onOff(userApprovedRelays)}" +
          " encrypt=${onOff(encrypt)}" +
          (if (inline == null) "" else " inline=${onOff(inline)}")
    is CancelFile -> "/fcancel $fileId"
    is SetLocalDeviceName -> "/set device name $displayName"
    is ListRemoteHosts -> "/list remote hosts"
    is StartRemoteHost -> "/start remote host " + (if (remoteHostId == null) "new" else "$remoteHostId multicast=${onOff(multicast)}") + (if (address != null) " addr=${address.address} iface=${json.encodeToString(address.`interface`)}" else "") + (if (port != null) " port=$port" else "")
    is SwitchRemoteHost -> "/switch remote host " + if (remoteHostId == null) "local" else "$remoteHostId"
    is StopRemoteHost -> "/stop remote host " + if (remoteHostKey == null) "new" else "$remoteHostKey"
    is DeleteRemoteHost -> "/delete remote host $remoteHostId"
    is StoreRemoteFile ->
      "/store remote file $remoteHostId " +
          (if (storeEncrypted == null) "" else "encrypt=${onOff(storeEncrypted)} ") +
          localPath
    is GetRemoteFile -> "/get remote file $remoteHostId ${json.encodeToString(file)}"
    is ConnectRemoteCtrl -> "/connect remote ctrl $xrcpInvitation"
    is FindKnownRemoteCtrl -> "/find remote ctrl"
    is ConfirmRemoteCtrl -> "/confirm remote ctrl $remoteCtrlId"
    is VerifyRemoteCtrlSession -> "/verify remote ctrl $sessionCode"
    is ListRemoteCtrls -> "/list remote ctrls"
    is StopRemoteCtrl -> "/stop remote ctrl"
    is DeleteRemoteCtrl -> "/delete remote ctrl $remoteCtrlId"
    is ApiUploadStandaloneFile -> "/_upload $userId ${file.filePath}"
    is ApiDownloadStandaloneFile -> "/_download $userId $url ${file.filePath}"
    is ApiStandaloneFileInfo -> "/_download info $url"
    is ShowVersion -> "/version"
    is ResetAgentServersStats -> "/reset servers stats"
    is GetAgentSubsTotal -> "/get subs total $userId"
    is GetAgentServersSummary -> "/get servers summary $userId"
  }

  val cmdType: String get() = when (this) {
    is Console -> "console command"
    is ShowActiveUser -> "showActiveUser"
    is CreateActiveUser -> "createActiveUser"
    is ListUsers -> "listUsers"
    is ApiSetActiveUser -> "apiSetActiveUser"
    is SetAllContactReceipts -> "setAllContactReceipts"
    is ApiSetUserContactReceipts -> "apiSetUserContactReceipts"
    is ApiSetUserGroupReceipts -> "apiSetUserGroupReceipts"
    is ApiHideUser -> "apiHideUser"
    is ApiUnhideUser -> "apiUnhideUser"
    is ApiMuteUser -> "apiMuteUser"
    is ApiUnmuteUser -> "apiUnmuteUser"
    is ApiDeleteUser -> "apiDeleteUser"
    is StartChat -> "startChat"
    is CheckChatRunning -> "checkChatRunning"
    is ApiStopChat -> "apiStopChat"
    is ApiSetAppFilePaths -> "apiSetAppFilePaths"
    is ApiSetEncryptLocalFiles -> "apiSetEncryptLocalFiles"
    is ApiExportArchive -> "apiExportArchive"
    is ApiImportArchive -> "apiImportArchive"
    is ApiDeleteStorage -> "apiDeleteStorage"
    is ApiStorageEncryption -> "apiStorageEncryption"
    is TestStorageEncryption -> "testStorageEncryption"
    is ApiSaveSettings -> "apiSaveSettings"
    is ApiGetSettings -> "apiGetSettings"
    is ApiGetChatTags -> "apiGetChatTags"
    is ApiGetChats -> "apiGetChats"
    is ApiGetChat -> "apiGetChat"
    is ApiGetChatItemInfo -> "apiGetChatItemInfo"
    is ApiSendMessages -> "apiSendMessages"
    is ApiCreateChatTag -> "apiCreateChatTag"
    is ApiSetChatTags -> "apiSetChatTags"
    is ApiDeleteChatTag -> "apiDeleteChatTag"
    is ApiUpdateChatTag -> "apiUpdateChatTag"
    is ApiReorderChatTags -> "apiReorderChatTags"
    is ApiCreateChatItems -> "apiCreateChatItems"
    is ApiReportMessage -> "apiReportMessage"
    is ApiUpdateChatItem -> "apiUpdateChatItem"
    is ApiDeleteChatItem -> "apiDeleteChatItem"
    is ApiDeleteMemberChatItem -> "apiDeleteMemberChatItem"
    is ApiArchiveReceivedReports -> "apiArchiveReceivedReports"
    is ApiDeleteReceivedReports -> "apiDeleteReceivedReports"
    is ApiChatItemReaction -> "apiChatItemReaction"
    is ApiGetReactionMembers -> "apiGetReactionMembers"
    is ApiForwardChatItems -> "apiForwardChatItems"
    is ApiPlanForwardChatItems -> "apiPlanForwardChatItems"
    is ApiNewGroup -> "apiNewGroup"
    is ApiAddMember -> "apiAddMember"
    is ApiJoinGroup -> "apiJoinGroup"
    is ApiMembersRole -> "apiMembersRole"
    is ApiBlockMembersForAll -> "apiBlockMembersForAll"
    is ApiRemoveMembers -> "apiRemoveMembers"
    is ApiLeaveGroup -> "apiLeaveGroup"
    is ApiListMembers -> "apiListMembers"
    is ApiUpdateGroupProfile -> "apiUpdateGroupProfile"
    is APICreateGroupLink -> "apiCreateGroupLink"
    is APIGroupLinkMemberRole -> "apiGroupLinkMemberRole"
    is APIDeleteGroupLink -> "apiDeleteGroupLink"
    is APIGetGroupLink -> "apiGetGroupLink"
    is APICreateMemberContact -> "apiCreateMemberContact"
    is APISendMemberContactInvitation -> "apiSendMemberContactInvitation"
    is APITestProtoServer -> "testProtoServer"
    is ApiGetServerOperators -> "apiGetServerOperators"
    is ApiSetServerOperators -> "apiSetServerOperators"
    is ApiGetUserServers -> "apiGetUserServers"
    is ApiSetUserServers -> "apiSetUserServers"
    is ApiValidateServers -> "apiValidateServers"
    is ApiGetUsageConditions -> "apiGetUsageConditions"
    is ApiSetConditionsNotified -> "apiSetConditionsNotified"
    is ApiAcceptConditions -> "apiAcceptConditions"
    is APISetChatItemTTL -> "apiSetChatItemTTL"
    is APIGetChatItemTTL -> "apiGetChatItemTTL"
    is APISetChatTTL -> "apiSetChatTTL"
    is APISetNetworkConfig -> "apiSetNetworkConfig"
    is APIGetNetworkConfig -> "apiGetNetworkConfig"
    is APISetNetworkInfo -> "apiSetNetworkInfo"
    is ReconnectServer -> "reconnectServer"
    is ReconnectAllServers -> "reconnectAllServers"
    is APISetChatSettings -> "apiSetChatSettings"
    is ApiSetMemberSettings -> "apiSetMemberSettings"
    is APIContactInfo -> "apiContactInfo"
    is APIGroupMemberInfo -> "apiGroupMemberInfo"
    is APIContactQueueInfo -> "apiContactQueueInfo"
    is APIGroupMemberQueueInfo -> "apiGroupMemberQueueInfo"
    is APISwitchContact -> "apiSwitchContact"
    is APISwitchGroupMember -> "apiSwitchGroupMember"
    is APIAbortSwitchContact -> "apiAbortSwitchContact"
    is APIAbortSwitchGroupMember -> "apiAbortSwitchGroupMember"
    is APISyncContactRatchet -> "apiSyncContactRatchet"
    is APISyncGroupMemberRatchet -> "apiSyncGroupMemberRatchet"
    is APIGetContactCode -> "apiGetContactCode"
    is APIGetGroupMemberCode -> "apiGetGroupMemberCode"
    is APIVerifyContact -> "apiVerifyContact"
    is APIVerifyGroupMember -> "apiVerifyGroupMember"
    is APIAddContact -> "apiAddContact"
    is ApiSetConnectionIncognito -> "apiSetConnectionIncognito"
    is ApiChangeConnectionUser -> "apiChangeConnectionUser"
    is APIConnectPlan -> "apiConnectPlan"
    is APIConnect -> "apiConnect"
    is ApiConnectContactViaAddress -> "apiConnectContactViaAddress"
    is ApiDeleteChat -> "apiDeleteChat"
    is ApiClearChat -> "apiClearChat"
    is ApiListContacts -> "apiListContacts"
    is ApiUpdateProfile -> "apiUpdateProfile"
    is ApiSetContactPrefs -> "apiSetContactPrefs"
    is ApiSetContactAlias -> "apiSetContactAlias"
    is ApiSetGroupAlias -> "apiSetGroupAlias"
    is ApiSetConnectionAlias -> "apiSetConnectionAlias"
    is ApiSetUserUIThemes -> "apiSetUserUIThemes"
    is ApiSetChatUIThemes -> "apiSetChatUIThemes"
    is ApiCreateMyAddress -> "apiCreateMyAddress"
    is ApiDeleteMyAddress -> "apiDeleteMyAddress"
    is ApiShowMyAddress -> "apiShowMyAddress"
    is ApiSetProfileAddress -> "apiSetProfileAddress"
    is ApiAddressAutoAccept -> "apiAddressAutoAccept"
    is ApiAcceptContact -> "apiAcceptContact"
    is ApiRejectContact -> "apiRejectContact"
    is ApiGetCallInvitations -> "apiGetCallInvitations"
    is ApiSendCallInvitation -> "apiSendCallInvitation"
    is ApiRejectCall -> "apiRejectCall"
    is ApiSendCallOffer -> "apiSendCallOffer"
    is ApiSendCallAnswer -> "apiSendCallAnswer"
    is ApiSendCallExtraInfo -> "apiSendCallExtraInfo"
    is ApiEndCall -> "apiEndCall"
    is ApiCallStatus -> "apiCallStatus"
    is ApiGetNetworkStatuses -> "apiGetNetworkStatuses"
    is ApiChatRead -> "apiChatRead"
    is ApiChatItemsRead -> "apiChatItemsRead"
    is ApiChatUnread -> "apiChatUnread"
    is ReceiveFile -> "receiveFile"
    is CancelFile -> "cancelFile"
    is SetLocalDeviceName -> "setLocalDeviceName"
    is ListRemoteHosts -> "listRemoteHosts"
    is StartRemoteHost -> "startRemoteHost"
    is SwitchRemoteHost -> "switchRemoteHost"
    is StopRemoteHost -> "stopRemoteHost"
    is DeleteRemoteHost -> "deleteRemoteHost"
    is StoreRemoteFile -> "storeRemoteFile"
    is GetRemoteFile -> "getRemoteFile"
    is ConnectRemoteCtrl -> "connectRemoteCtrl"
    is FindKnownRemoteCtrl -> "FindKnownRemoteCtrl"
    is ConfirmRemoteCtrl -> "confirmRemoteCtrl"
    is VerifyRemoteCtrlSession -> "verifyRemoteCtrlSession"
    is ListRemoteCtrls -> "listRemoteCtrls"
    is StopRemoteCtrl -> "stopRemoteCtrl"
    is DeleteRemoteCtrl -> "deleteRemoteCtrl"
    is ApiUploadStandaloneFile -> "apiUploadStandaloneFile"
    is ApiDownloadStandaloneFile -> "apiDownloadStandaloneFile"
    is ApiStandaloneFileInfo -> "apiStandaloneFileInfo"
    is ShowVersion -> "showVersion"
    is ResetAgentServersStats -> "resetAgentServersStats"
    is GetAgentSubsTotal -> "getAgentSubsTotal"
    is GetAgentServersSummary -> "getAgentServersSummary"
  }

  data class ItemRange(val from: Long, val to: Long)

  fun chatItemTTLStr(seconds: Long?): String {
    if (seconds == null) return "default"
    return seconds.toString()
  }

  val obfuscated: CC
    get() = when (this) {
      is ApiStorageEncryption -> ApiStorageEncryption(DBEncryptionConfig(obfuscate(config.currentKey), obfuscate(config.newKey)))
      is ApiSetActiveUser -> ApiSetActiveUser(userId, obfuscateOrNull(viewPwd))
      is ApiHideUser -> ApiHideUser(userId, obfuscate(viewPwd))
      is ApiUnhideUser -> ApiUnhideUser(userId, obfuscate(viewPwd))
      is ApiDeleteUser -> ApiDeleteUser(userId, delSMPQueues, obfuscateOrNull(viewPwd))
      is TestStorageEncryption -> TestStorageEncryption(obfuscate(key))
      else -> this
    }

  private fun obfuscate(s: String): String = if (s.isEmpty()) "" else "***"

  private fun obfuscateOrNull(s: String?): String? =
    if (s != null) {
      obfuscate(s)
    } else {
      null
    }

  private fun maybePwd(pwd: String?): String = if (pwd == "" || pwd == null) "" else " " + json.encodeToString(pwd)

  companion object {
    fun chatRef(chatType: ChatType, id: Long) = "${chatType.type}${id}"
  }
}

fun onOff(b: Boolean): String = if (b) "on" else "off"

@Serializable
data class NewUser(
  val profile: Profile?,
  val pastTimestamp: Boolean
)

sealed class ChatPagination {
  class Last(val count: Int): ChatPagination()
  class After(val chatItemId: Long, val count: Int): ChatPagination()
  class Before(val chatItemId: Long, val count: Int): ChatPagination()
  class Around(val chatItemId: Long, val count: Int): ChatPagination()
  class Initial(val count: Int): ChatPagination()

  val cmdString: String get() = when (this) {
    is Last -> "count=${this.count}"
    is After -> "after=${this.chatItemId} count=${this.count}"
    is Before -> "before=${this.chatItemId} count=${this.count}"
    is Around -> "around=${this.chatItemId} count=${this.count}"
    is Initial -> "initial=${this.count}"
  }

  companion object {
    val INITIAL_COUNT = if (appPlatform.isDesktop) 100 else 75
    const val PRELOAD_COUNT = 100
    const val UNTIL_PRELOAD_COUNT = 50
  }
}

@Serializable
class ComposedMessage(val fileSource: CryptoFile?, val quotedItemId: Long?, val msgContent: MsgContent, val mentions: Map<String, Long>)

@Serializable
class UpdatedMessage(val msgContent: MsgContent, val mentions: Map<String, Long>) {
  val cmdString: String get() =
    if (msgContent is MCUnknown) "json $json" else "json ${json.encodeToString(this)}"
}

@Serializable
class ChatTagData(val emoji: String?, val text: String)

@Serializable
class ArchiveConfig(val archivePath: String, val disableCompression: Boolean? = null, val parentTempDirectory: String? = null)

@Serializable
class DBEncryptionConfig(val currentKey: String, val newKey: String)

@Serializable
enum class ServerProtocol {
  @SerialName("smp") SMP,
  @SerialName("xftp") XFTP;
}

@Serializable
enum class OperatorTag {
  @SerialName("simplex") SimpleX,
  @SerialName("flux") Flux
}

data class ServerOperatorInfo(
  val description: List<String>,
  val website: String,
  val selfhost: Pair<String, String>? = null,
  val logo: ImageResource,
  val largeLogo: ImageResource,
  val logoDarkMode: ImageResource,
  val largeLogoDarkMode: ImageResource
)
val operatorsInfo: Map<OperatorTag, ServerOperatorInfo> = mapOf(
  OperatorTag.SimpleX to ServerOperatorInfo(
    description = listOf(
      "SimpleX Chat is the first communication network that has no user profile IDs of any kind, not even random numbers or keys that identify the users.",
      "SimpleX Chat Ltd develops the communication software for SimpleX network."
    ),
    website = "https://simplex.chat",
    logo = MR.images.decentralized,
    largeLogo = MR.images.logo,
    logoDarkMode = MR.images.decentralized_light,
    largeLogoDarkMode = MR.images.logo_light
  ),
  OperatorTag.Flux to ServerOperatorInfo(
    description = listOf(
      "Flux is the largest decentralized cloud, based on a global network of user-operated nodes.",
      "Flux offers a powerful, scalable, and affordable cutting edge technology platform for all.",
      "Flux operates servers in SimpleX network to improve its privacy and decentralization."
    ),
    website = "https://runonflux.com",
    selfhost = "Self-host SimpleX servers on Flux" to "https://home.runonflux.io/apps/marketplace?q=simplex",
    logo = MR.images.flux_logo_symbol,
    largeLogo = MR.images.flux_logo,
    logoDarkMode = MR.images.flux_logo_symbol,
    largeLogoDarkMode = MR.images.flux_logo_light
  ),
)

@Serializable
data class UsageConditionsDetail(
  val conditionsId: Long,
  val conditionsCommit: String,
  val notifiedAt: Instant?,
  val createdAt: Instant
) {
  companion object {
    val sampleData = UsageConditionsDetail(
      conditionsId = 1,
      conditionsCommit = "11a44dc1fd461a93079f897048b46998db55da5c",
      notifiedAt = null,
      createdAt = Clock.System.now()
    )
  }
}

@Serializable
sealed class UsageConditionsAction {
  @Serializable @SerialName("review") data class Review(val operators: List<ServerOperator>, val deadline: Instant?, val showNotice: Boolean) : UsageConditionsAction()
  @Serializable @SerialName("accepted") data class Accepted(val operators: List<ServerOperator>) : UsageConditionsAction()

  val shouldShowNotice: Boolean
    get() = when (this) {
      is Review -> showNotice
      else -> false
    }
}

@Serializable
data class ServerOperatorConditionsDetail(
  val serverOperators: List<ServerOperator>,
  val currentConditions: UsageConditionsDetail,
  val conditionsAction: UsageConditionsAction?
) {
  companion object {
    val empty = ServerOperatorConditionsDetail(
      serverOperators = emptyList(),
      currentConditions = UsageConditionsDetail(conditionsId = 0, conditionsCommit = "empty", notifiedAt = null, createdAt = Clock.System.now()),
      conditionsAction = null
    )
  }
}

@Serializable()
sealed class ConditionsAcceptance {
  @Serializable @SerialName("accepted") data class Accepted(val acceptedAt: Instant?, val autoAccepted: Boolean) : ConditionsAcceptance()
  @Serializable @SerialName("required") data class Required(val deadline: Instant?) : ConditionsAcceptance()

  val conditionsAccepted: Boolean
    get() = when (this) {
      is Accepted -> true
      is Required -> false
    }

  val usageAllowed: Boolean
    get() = when (this) {
      is Accepted -> true
      is Required -> this.deadline != null
    }
}

@Serializable
data class ServerOperator(
  val operatorId: Long,
  val operatorTag: OperatorTag?,
  val tradeName: String,
  val legalName: String?,
  val serverDomains: List<String>,
  val conditionsAcceptance: ConditionsAcceptance,
  val enabled: Boolean,
  val smpRoles: ServerRoles,
  val xftpRoles: ServerRoles,
) {
  companion object {
    val dummyOperatorInfo = ServerOperatorInfo(
      description = listOf("Default"),
      website = "https://simplex.chat",
      logo = MR.images.decentralized,
      largeLogo = MR.images.logo,
      logoDarkMode = MR.images.decentralized_light,
      largeLogoDarkMode = MR.images.logo_light
    )

    val sampleData1 = ServerOperator(
      operatorId = 1,
      operatorTag = OperatorTag.SimpleX,
      tradeName = "SimpleX Chat",
      legalName = "SimpleX Chat Ltd",
      serverDomains = listOf("simplex.im"),
      conditionsAcceptance = ConditionsAcceptance.Accepted(acceptedAt = null, autoAccepted = false),
      enabled = true,
      smpRoles = ServerRoles(storage = true, proxy = true),
      xftpRoles = ServerRoles(storage = true, proxy = true)
    )
  }

  val id: Long
    get() = operatorId

  override fun equals(other: Any?): Boolean {
    if (other !is ServerOperator) return false
    return other.operatorId == this.operatorId &&
        other.operatorTag == this.operatorTag &&
        other.tradeName == this.tradeName &&
        other.legalName == this.legalName &&
        other.serverDomains == this.serverDomains &&
        other.conditionsAcceptance == this.conditionsAcceptance &&
        other.enabled == this.enabled &&
        other.smpRoles == this.smpRoles &&
        other.xftpRoles == this.xftpRoles
  }

  override fun hashCode(): Int {
    var result = operatorId.hashCode()
    result = 31 * result + (operatorTag?.hashCode() ?: 0)
    result = 31 * result + tradeName.hashCode()
    result = 31 * result + (legalName?.hashCode() ?: 0)
    result = 31 * result + serverDomains.hashCode()
    result = 31 * result + conditionsAcceptance.hashCode()
    result = 31 * result + enabled.hashCode()
    result = 31 * result + smpRoles.hashCode()
    result = 31 * result + xftpRoles.hashCode()
    return result
  }

  val legalName_: String
    get() = legalName ?: tradeName

  val info: ServerOperatorInfo get() {
      return if (this.operatorTag != null) {
        operatorsInfo[this.operatorTag] ?: dummyOperatorInfo
      } else {
        dummyOperatorInfo
      }
    }

  val logo: ImageResource
    @Composable
    get() {
      return if (isInDarkTheme()) info.logoDarkMode else info.logo
    }

  val largeLogo: ImageResource
    @Composable
    get() {
      return if (isInDarkTheme()) info.largeLogoDarkMode else info.largeLogo
    }
}

@Serializable
data class ServerRoles(
  val storage: Boolean,
  val proxy: Boolean
)

@Serializable
data class UserOperatorServers(
  val operator: ServerOperator?,
  val smpServers: List<UserServer>,
  val xftpServers: List<UserServer>
) {
  val id: String
    get() = operator?.operatorId?.toString() ?: "nil operator"

  val operator_: ServerOperator
    get() = operator ?: ServerOperator(
      operatorId = 0,
      operatorTag = null,
      tradeName = "",
      legalName = null,
      serverDomains = emptyList(),
      conditionsAcceptance = ConditionsAcceptance.Accepted(null, autoAccepted = false),
      enabled = false,
      smpRoles = ServerRoles(storage = true, proxy = true),
      xftpRoles = ServerRoles(storage = true, proxy = true)
    )

  companion object {
    val sampleData1 = UserOperatorServers(
      operator = ServerOperator.sampleData1,
      smpServers = listOf(UserServer.sampleData.preset),
      xftpServers = listOf(UserServer.sampleData.xftpPreset)
    )

    val sampleDataNilOperator = UserOperatorServers(
      operator = null,
      smpServers = listOf(UserServer.sampleData.preset),
      xftpServers = listOf(UserServer.sampleData.xftpPreset)
    )
  }
}

@Serializable
sealed class UserServersError {
  @Serializable @SerialName("noServers") data class NoServers(val protocol: ServerProtocol, val user: UserRef?): UserServersError()
  @Serializable @SerialName("storageMissing") data class StorageMissing(val protocol: ServerProtocol, val user: UserRef?): UserServersError()
  @Serializable @SerialName("proxyMissing") data class ProxyMissing(val protocol: ServerProtocol, val user: UserRef?): UserServersError()
  @Serializable @SerialName("duplicateServer") data class DuplicateServer(val protocol: ServerProtocol, val duplicateServer: String, val duplicateHost: String): UserServersError()

  val globalError: String?
    get() = when (this.protocol_) {
      ServerProtocol.SMP -> globalSMPError
      ServerProtocol.XFTP -> globalXFTPError
    }

  private val protocol_: ServerProtocol
    get() = when (this) {
      is NoServers -> this.protocol
      is StorageMissing -> this.protocol
      is ProxyMissing -> this.protocol
      is DuplicateServer -> this.protocol
    }

  val globalSMPError: String?
    get() = if (this.protocol_ == ServerProtocol.SMP) {
      when (this) {
        is NoServers -> this.user?.let { "${userStr(it)} ${generalGetString(MR.strings.no_message_servers_configured)}" }
          ?: generalGetString(MR.strings.no_message_servers_configured)

        is StorageMissing -> this.user?.let { "${userStr(it)} ${generalGetString(MR.strings.no_message_servers_configured_for_receiving)}" }
          ?: generalGetString(MR.strings.no_message_servers_configured_for_receiving)

        is ProxyMissing -> this.user?.let { "${userStr(it)} ${generalGetString(MR.strings.no_message_servers_configured_for_private_routing)}" }
          ?: generalGetString(MR.strings.no_message_servers_configured_for_private_routing)

        else -> null
      }
    } else {
      null
    }

  val globalXFTPError: String?
    get() = if (this.protocol_ == ServerProtocol.XFTP) {
      when (this) {
        is NoServers -> this.user?.let { "${userStr(it)} ${generalGetString(MR.strings.no_media_servers_configured)}" }
          ?: generalGetString(MR.strings.no_media_servers_configured)

        is StorageMissing -> this.user?.let { "${userStr(it)} ${generalGetString(MR.strings.no_media_servers_configured_for_sending)}" }
          ?: generalGetString(MR.strings.no_media_servers_configured_for_sending)

        is ProxyMissing -> this.user?.let { "${userStr(it)} ${generalGetString(MR.strings.no_media_servers_configured_for_private_routing)}" }
          ?: generalGetString(MR.strings.no_media_servers_configured_for_private_routing)

        else -> null
      }
    } else {
      null
    }

  private fun userStr(user: UserRef): String {
    return String.format(generalGetString(MR.strings.for_chat_profile), user.localDisplayName)
  }
}

@Serializable
data class UserServer(
  val remoteHostId: Long?,
  val serverId: Long?,
  val server: String,
  val preset: Boolean,
  val tested: Boolean? = null,
  val enabled: Boolean,
  val deleted: Boolean
) {
  @Transient
  private val createdAt: Date = Date()
  // val sendEnabled: Boolean // can we potentially want to prevent sending on the servers we use to receive?
  // Even if we don't see the use case, it's probably better to allow it in the model
  // In any case, "trusted/known" servers are out of scope of this change
  val id: String
    get() = "$server $createdAt"

  val isBlank: Boolean
    get() = server.isBlank()

  companion object {
    val empty = UserServer(remoteHostId = null, serverId = null, server = "", preset = false, tested = null, enabled = false, deleted = false)

    class SampleData(
      val preset: UserServer,
      val custom: UserServer,
      val untested: UserServer,
      val xftpPreset: UserServer
    )

    val sampleData = SampleData(
      preset = UserServer(
        remoteHostId = null,
        serverId = 1,
        server = "smp://abcd@smp8.simplex.im",
        preset = true,
        tested = true,
        enabled = true,
        deleted = false
      ),
      custom = UserServer(
        remoteHostId = null,
        serverId = 2,
        server = "smp://abcd@smp9.simplex.im",
        preset = false,
        tested = false,
        enabled = false,
        deleted = false
      ),
      untested = UserServer(
        remoteHostId = null,
        serverId = 3,
        server = "smp://abcd@smp10.simplex.im",
        preset = false,
        tested = null,
        enabled = true,
        deleted = false
      ),
      xftpPreset = UserServer(
        remoteHostId = null,
        serverId = 4,
        server = "xftp://abcd@xftp8.simplex.im",
        preset = true,
        tested = true,
        enabled = true,
        deleted = false
      )
    )
  }
}

@Serializable
enum class ProtocolTestStep {
  @SerialName("connect") Connect,
  @SerialName("disconnect") Disconnect,
  @SerialName("createQueue") CreateQueue,
  @SerialName("secureQueue") SecureQueue,
  @SerialName("deleteQueue") DeleteQueue,
  @SerialName("createFile") CreateFile,
  @SerialName("uploadFile") UploadFile,
  @SerialName("downloadFile") DownloadFile,
  @SerialName("compareFile") CompareFile,
  @SerialName("deleteFile") DeleteFile;

  val text: String get() = when (this) {
    Connect -> generalGetString(MR.strings.smp_server_test_connect)
    Disconnect -> generalGetString(MR.strings.smp_server_test_disconnect)
    CreateQueue -> generalGetString(MR.strings.smp_server_test_create_queue)
    SecureQueue -> generalGetString(MR.strings.smp_server_test_secure_queue)
    DeleteQueue -> generalGetString(MR.strings.smp_server_test_delete_queue)
    CreateFile -> generalGetString(MR.strings.smp_server_test_create_file)
    UploadFile -> generalGetString(MR.strings.smp_server_test_upload_file)
    DownloadFile -> generalGetString(MR.strings.smp_server_test_download_file)
    CompareFile -> generalGetString(MR.strings.smp_server_test_compare_file)
    DeleteFile -> generalGetString(MR.strings.smp_server_test_delete_file)
  }
}

@Serializable
data class ProtocolTestFailure(
  val testStep: ProtocolTestStep,
  val testError: AgentErrorType
) {
  override fun equals(other: Any?): Boolean {
    if (other !is ProtocolTestFailure) return false
    return other.testStep == this.testStep
  }

  override fun hashCode(): Int {
    return testStep.hashCode()
  }

  val localizedDescription: String get() {
    val err = String.format(generalGetString(MR.strings.error_smp_test_failed_at_step), testStep.text)
    return when  {
      testError is AgentErrorType.SMP && testError.smpErr is SMPErrorType.AUTH ->
        err + " " + generalGetString(MR.strings.error_smp_test_server_auth)
      testError is AgentErrorType.XFTP && testError.xftpErr is XFTPErrorType.AUTH ->
        err + " " + generalGetString(MR.strings.error_xftp_test_server_auth)
      testError is AgentErrorType.BROKER && testError.brokerErr is BrokerErrorType.NETWORK ->
        err + " " + generalGetString(MR.strings.error_smp_test_certificate)
      else -> err
    }
  }
}

@Serializable
data class ServerAddress(
  val serverProtocol: ServerProtocol,
  val hostnames: List<String>,
  val port: String,
  val keyHash: String,
  val basicAuth: String = ""
) {
  val uri: String
    get() =
      "${serverProtocol}://${keyHash}${if (basicAuth.isEmpty()) "" else ":$basicAuth"}@${hostnames.joinToString(",")}"

  val valid: Boolean
    get() = hostnames.isNotEmpty() && hostnames.toSet().size == hostnames.size

  companion object {
    fun empty(serverProtocol: ServerProtocol) = ServerAddress(
      serverProtocol = serverProtocol,
      hostnames = emptyList(),
      port = "",
      keyHash = "",
      basicAuth = ""
    )
    val sampleData = ServerAddress(
      serverProtocol = ServerProtocol.SMP,
      hostnames = listOf("smp.simplex.im", "1234.onion"),
      port = "",
      keyHash = "LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=",
      basicAuth = "server_password"
    )

    fun parseServerAddress(s: String): ServerAddress? {
      val parsed = chatParseServer(s)
      return runCatching { json.decodeFromString(ParsedServerAddress.serializer(), parsed) }
        .onFailure { Log.d(TAG, "parseServerAddress decode error: $it") }
        .getOrNull()?.serverAddress
    }
  }
}

@Serializable
data class ParsedServerAddress (
  var serverAddress: ServerAddress?,
  var parseError: String
)

@Serializable
data class NetCfg(
  val socksProxy: String?,
  val socksMode: SocksMode = SocksMode.default,
  val hostMode: HostMode = HostMode.default,
  val requiredHostMode: Boolean = false,
  val sessionMode: TransportSessionMode = TransportSessionMode.default,
  val smpProxyMode: SMPProxyMode = SMPProxyMode.default,
  val smpProxyFallback: SMPProxyFallback = SMPProxyFallback.default,
  val smpWebPortServers: SMPWebPortServers = SMPWebPortServers.default,
  val tcpConnectTimeout: Long, // microseconds
  val tcpTimeout: Long, // microseconds
  val tcpTimeoutPerKb: Long, // microseconds
  val rcvConcurrency: Int, // pool size
  val tcpKeepAlive: KeepAliveOpts? = KeepAliveOpts.defaults,
  val smpPingInterval: Long, // microseconds
  val smpPingCount: Int = 3,
  val logTLSErrors: Boolean = false,
) {
  val useSocksProxy: Boolean get() = socksProxy != null
  val enableKeepAlive: Boolean get() = tcpKeepAlive != null

  fun withProxy(proxy: NetworkProxy?, default: String? = ":9050"): NetCfg {
    return copy(socksProxy = proxy?.toProxyString() ?: default)
  }

  companion object {
    val defaults: NetCfg =
      NetCfg(
        socksProxy = null,
        tcpConnectTimeout = 25_000_000,
        tcpTimeout = 15_000_000,
        tcpTimeoutPerKb = 10_000,
        rcvConcurrency = 12,
        smpPingInterval = 1200_000_000
      )

    val proxyDefaults: NetCfg =
      NetCfg(
        socksProxy = ":9050",
        tcpConnectTimeout = 35_000_000,
        tcpTimeout = 20_000_000,
        tcpTimeoutPerKb = 15_000,
        rcvConcurrency = 8,
        smpPingInterval = 1200_000_000
      )
  }

  val onionHosts: OnionHosts get() = when {
    hostMode == HostMode.Public && requiredHostMode -> OnionHosts.NEVER
    hostMode == HostMode.OnionViaSocks && !requiredHostMode -> OnionHosts.PREFER
    hostMode == HostMode.OnionViaSocks && requiredHostMode -> OnionHosts.REQUIRED
    else -> OnionHosts.PREFER
  }

  fun withOnionHosts(mode: OnionHosts): NetCfg = when (mode) {
    OnionHosts.NEVER ->
      this.copy(hostMode = HostMode.Public, requiredHostMode = true)
    OnionHosts.PREFER ->
      this.copy(hostMode = HostMode.OnionViaSocks, requiredHostMode = false)
    OnionHosts.REQUIRED ->
      this.copy(hostMode = HostMode.OnionViaSocks, requiredHostMode = true)
  }
}

@Serializable
data class NetworkProxy(
  val username: String = "",
  val password: String = "",
  val auth: NetworkProxyAuth = NetworkProxyAuth.ISOLATE,
  val host: String = "localhost",
  val port: Int = 9050
) {
  fun toProxyString(): String {
    var res = ""
    if (auth == NetworkProxyAuth.USERNAME && (username.isNotBlank() || password.isNotBlank())) {
      res += username.trim() + ":" + password.trim() + "@"
    } else if (auth == NetworkProxyAuth.USERNAME) {
      res += "@"
    }
    if (host != "localhost") {
      res += if (host.contains(':')) "[${host.trim(' ', '[', ']')}]" else host.trim()
    }
    if (port != 9050 || res.isEmpty()) {
      res += ":$port"
    }
    return res
  }
}

@Serializable
enum class NetworkProxyAuth {
  @SerialName("isolate")
  ISOLATE,
  @SerialName("username")
  USERNAME,
}

enum class OnionHosts {
  NEVER, PREFER, REQUIRED
}

@Serializable
enum class HostMode {
  @SerialName("onionViaSocks") OnionViaSocks,
  @SerialName("onion") Onion,
  @SerialName("public") Public;

  companion object {
    val default = OnionViaSocks
  }
}

@Serializable
enum class SocksMode {
  @SerialName("always") Always,
  @SerialName("onion") Onion;

  companion object {
    val default = Always
  }
}

@Serializable
enum class SMPProxyMode {
  @SerialName("always") Always,
  @SerialName("unknown") Unknown,
  @SerialName("unprotected") Unprotected,
  @SerialName("never") Never;

  companion object {
    val default = Always
  }
}

@Serializable
enum class SMPProxyFallback {
  @SerialName("allow") Allow,
  @SerialName("allowProtected") AllowProtected,
  @SerialName("prohibit") Prohibit;

  companion object {
    val default = AllowProtected
  }
}

@Serializable
enum class SMPWebPortServers {
  @SerialName("all") All,
  @SerialName("preset") Preset,
  @SerialName("off") Off;

  val text get(): StringResource = when (this) {
    All -> MR.strings.network_smp_web_port_all
    Preset -> MR.strings.network_smp_web_port_preset
    Off -> MR.strings.network_smp_web_port_off
  }

  companion object {
    val default = Preset
  }
}

@Serializable
enum class TransportSessionMode {
  @SerialName("user") User,
  @SerialName("session") Session,
  @SerialName("server") Server,
  @SerialName("entity") Entity;

  companion object {
    val default = Session
    val safeValues = arrayOf(User, Session, Server)
  }
}

@Serializable
data class KeepAliveOpts(
  val keepIdle: Int, // seconds
  val keepIntvl: Int, // seconds
  val keepCnt: Int // times
) {
  companion object {
    val defaults: KeepAliveOpts =
      KeepAliveOpts(keepIdle = 30, keepIntvl = 15, keepCnt = 4)
  }
}

@Serializable
data class ChatSettings(
  val enableNtfs: MsgFilter,
  val sendRcpts: Boolean?,
  val favorite: Boolean
) {
  companion object {
    val defaults: ChatSettings = ChatSettings(enableNtfs = MsgFilter.All, sendRcpts = null, favorite = false)
  }
}

@Serializable
enum class MsgFilter {
  @SerialName("all") All,
  @SerialName("none") None,
  @SerialName("mentions") Mentions;

  fun nextMode(mentions: Boolean): MsgFilter {
    return when (this) {
      All -> if (mentions) Mentions else None
      Mentions -> None
      None -> All
    }
  }

  fun text(mentions: Boolean): StringResource {
    return when (this) {
      All -> MR.strings.unmute_chat
      Mentions -> MR.strings.mute_chat
      None -> if (mentions) MR.strings.mute_all_chat else MR.strings.mute_chat
    }
  }

  val icon: ImageResource
    get() = when (this) {
      All -> MR.images.ic_notifications
      Mentions -> MR.images.ic_notification_important
      None -> MR.images.ic_notifications_off
    }

  val iconFilled: ImageResource
    get() = when (this) {
      All -> MR.images.ic_notifications
      Mentions -> MR.images.ic_notification_important_filled
      None -> MR.images.ic_notifications_off_filled
    }
}

@Serializable
data class UserMsgReceiptSettings(val enable: Boolean, val clearOverrides: Boolean)

@Serializable
data class FullChatPreferences(
  val timedMessages: TimedMessagesPreference,
  val fullDelete: SimpleChatPreference,
  val reactions: SimpleChatPreference,
  val voice: SimpleChatPreference,
  val calls: SimpleChatPreference,
) {
  fun toPreferences(): ChatPreferences = ChatPreferences(
    timedMessages = timedMessages,
    fullDelete = fullDelete,
    reactions = reactions,
    voice = voice,
    calls = calls
  )

  companion object {
    val sampleData = FullChatPreferences(
      timedMessages = TimedMessagesPreference(allow = FeatureAllowed.NO),
      fullDelete = SimpleChatPreference(allow = FeatureAllowed.NO),
      reactions = SimpleChatPreference(allow = FeatureAllowed.YES),
      voice = SimpleChatPreference(allow = FeatureAllowed.YES),
      calls = SimpleChatPreference(allow = FeatureAllowed.YES),
    )
  }
}

@Serializable
data class ChatPreferences(
  val timedMessages: TimedMessagesPreference?,
  val fullDelete: SimpleChatPreference?,
  val reactions: SimpleChatPreference?,
  val voice: SimpleChatPreference?,
  val calls: SimpleChatPreference?,
) {
  fun setAllowed(feature: ChatFeature, allowed: FeatureAllowed = FeatureAllowed.YES, param: Int? = null): ChatPreferences =
    when (feature) {
      ChatFeature.TimedMessages -> this.copy(timedMessages = TimedMessagesPreference(allow = allowed, ttl = param ?: this.timedMessages?.ttl))
      ChatFeature.FullDelete -> this.copy(fullDelete = SimpleChatPreference(allow = allowed))
      ChatFeature.Reactions -> this.copy(reactions = SimpleChatPreference(allow = allowed))
      ChatFeature.Voice -> this.copy(voice = SimpleChatPreference(allow = allowed))
      ChatFeature.Calls -> this.copy(calls = SimpleChatPreference(allow = allowed))
    }

  companion object {
    val sampleData = ChatPreferences(
      timedMessages = TimedMessagesPreference(allow = FeatureAllowed.NO),
      fullDelete = SimpleChatPreference(allow = FeatureAllowed.NO),
      reactions = SimpleChatPreference(allow = FeatureAllowed.YES),
      voice = SimpleChatPreference(allow = FeatureAllowed.YES),
      calls = SimpleChatPreference(allow = FeatureAllowed.YES),
    )
  }
}

interface ChatPreference {
  val allow: FeatureAllowed
}

@Serializable
data class SimpleChatPreference(
  override val allow: FeatureAllowed
): ChatPreference

@Serializable
data class TimedMessagesPreference(
  override val allow: FeatureAllowed,
  val ttl: Int? = null
): ChatPreference {
  companion object {
    val ttlValues: List<Int?>
      get() = listOf(600, 3600, 86400, 7 * 86400, 30 * 86400, 3 * 30 * 86400, null)
  }
}

@Serializable
data class PresentedServersSummary(
  val statsStartedAt: Instant,
  val allUsersSMP: SMPServersSummary,
  val allUsersXFTP: XFTPServersSummary,
  val currentUserSMP: SMPServersSummary,
  val currentUserXFTP: XFTPServersSummary
)

@Serializable
data class SMPServersSummary(
  val smpTotals: SMPTotals,
  val currentlyUsedSMPServers: List<SMPServerSummary>,
  val previouslyUsedSMPServers: List<SMPServerSummary>,
  val onlyProxiedSMPServers: List<SMPServerSummary>
)

@Serializable
data class SMPTotals(
  val sessions: ServerSessions,
  val subs: SMPServerSubs,
  val stats: AgentSMPServerStatsData
)

@Serializable
data class SMPServerSummary(
  val smpServer: String,
  val known: Boolean? = null,
  val sessions: ServerSessions? = null,
  val subs: SMPServerSubs? = null,
  val stats: AgentSMPServerStatsData? = null
) {
  val hasSubs: Boolean
    get() = subs != null

  val sessionsOrNew: ServerSessions
    get() = sessions ?: ServerSessions.newServerSessions

  val subsOrNew: SMPServerSubs
    get() = subs ?: SMPServerSubs.newSMPServerSubs
}

@Serializable
data class ServerSessions(
  val ssConnected: Int,
  val ssErrors: Int,
  val ssConnecting: Int
) {
  companion object {
    val newServerSessions = ServerSessions(
      ssConnected = 0,
      ssErrors = 0,
      ssConnecting = 0
    )
  }

  val hasSess: Boolean
    get() = ssConnected > 0
}

@Serializable
data class SMPServerSubs(
  val ssActive: Int,
  val ssPending: Int
) {
  companion object {
    val newSMPServerSubs = SMPServerSubs(
      ssActive = 0,
      ssPending = 0
    )
  }

  val total: Int
    get() = ssActive + ssPending

  val shareOfActive: Float
    get() = if (total != 0) ssActive.toFloat() / total else 0f
}

@Serializable
data class AgentSMPServerStatsData(
  val _sentDirect: Int,
  val _sentViaProxy: Int,
  val _sentProxied: Int,
  val _sentDirectAttempts: Int,
  val _sentViaProxyAttempts: Int,
  val _sentProxiedAttempts: Int,
  val _sentAuthErrs: Int,
  val _sentQuotaErrs: Int,
  val _sentExpiredErrs: Int,
  val _sentOtherErrs: Int,
  val _recvMsgs: Int,
  val _recvDuplicates: Int,
  val _recvCryptoErrs: Int,
  val _recvErrs: Int,
  val _ackMsgs: Int,
  val _ackAttempts: Int,
  val _ackNoMsgErrs: Int,
  val _ackOtherErrs: Int,
  val _connCreated: Int,
  val _connSecured: Int,
  val _connCompleted: Int,
  val _connDeleted: Int,
  val _connDelAttempts: Int,
  val _connDelErrs: Int,
  val _connSubscribed: Int,
  val _connSubAttempts: Int,
  val _connSubIgnored: Int,
  val _connSubErrs: Int
)

@Serializable
data class XFTPServersSummary(
  val xftpTotals: XFTPTotals,
  val currentlyUsedXFTPServers: List<XFTPServerSummary>,
  val previouslyUsedXFTPServers: List<XFTPServerSummary>
)

@Serializable
data class XFTPTotals(
  val sessions: ServerSessions,
  val stats: AgentXFTPServerStatsData
)

@Serializable
data class XFTPServerSummary(
  val xftpServer: String,
  val known: Boolean? = null,
  val sessions: ServerSessions? = null,
  val stats: AgentXFTPServerStatsData? = null,
  val rcvInProgress: Boolean,
  val sndInProgress: Boolean,
  val delInProgress: Boolean
) {}

@Serializable
data class AgentXFTPServerStatsData(
  val _uploads: Int,
  val _uploadsSize: Long,
  val _uploadAttempts: Int,
  val _uploadErrs: Int,
  val _downloads: Int,
  val _downloadsSize: Long,
  val _downloadAttempts: Int,
  val _downloadAuthErrs: Int,
  val _downloadErrs: Int,
  val _deletions: Int,
  val _deleteAttempts: Int,
  val _deleteErrs: Int
)

sealed class CustomTimeUnit {
  object Second: CustomTimeUnit()
  object Minute: CustomTimeUnit()
  object Hour: CustomTimeUnit()
  object Day: CustomTimeUnit()
  object Week: CustomTimeUnit()
  object Month: CustomTimeUnit()

  val toSeconds: Int
    get() =
      when (this) {
        Second -> 1
        Minute -> 60
        Hour -> 3600
        Day -> 86400
        Week -> 7 * 86400
        Month -> 30 * 86400
      }

  val text: String
    get() =
      when (this) {
        Second -> generalGetString(MR.strings.custom_time_unit_seconds)
        Minute -> generalGetString(MR.strings.custom_time_unit_minutes)
        Hour -> generalGetString(MR.strings.custom_time_unit_hours)
        Day -> generalGetString(MR.strings.custom_time_unit_days)
        Week -> generalGetString(MR.strings.custom_time_unit_weeks)
        Month -> generalGetString(MR.strings.custom_time_unit_months)
      }

  companion object {
    fun toTimeUnit(seconds: Int): Pair<CustomTimeUnit, Int> {
      val tryUnits = listOf(Month, Week, Day, Hour, Minute)
      var selectedUnit: Pair<CustomTimeUnit, Int>? = null
      for (unit in tryUnits) {
        val (v, r) = divMod(seconds, unit.toSeconds)
        if (r == 0) {
          selectedUnit = Pair(unit, v)
          break
        }
      }
      return selectedUnit ?: Pair(Second, seconds)
    }

    private fun divMod(n: Int, d: Int): Pair<Int, Int> =
      n / d to n % d

    fun toText(seconds: Int): String {
      val (unit, value) = toTimeUnit(seconds)
      return when (unit) {
        Second -> String.format(generalGetString(MR.strings.ttl_sec), value)
        Minute -> String.format(generalGetString(MR.strings.ttl_min), value)
        Hour -> if (value == 1) String.format(generalGetString(MR.strings.ttl_hour), 1) else String.format(generalGetString(MR.strings.ttl_hours), value)
        Day -> if (value == 1) String.format(generalGetString(MR.strings.ttl_day), 1) else String.format(generalGetString(MR.strings.ttl_days), value)
        Week -> if (value == 1) String.format(generalGetString(MR.strings.ttl_week), 1) else String.format(generalGetString(MR.strings.ttl_weeks), value)
        Month -> if (value == 1) String.format(generalGetString(MR.strings.ttl_month), 1) else String.format(generalGetString(MR.strings.ttl_months), value)
      }
    }

    fun toShortText(seconds: Int): String {
      val (unit, value) = toTimeUnit(seconds)
      return when (unit) {
        Second -> String.format(generalGetString(MR.strings.ttl_s), value)
        Minute -> String.format(generalGetString(MR.strings.ttl_m), value)
        Hour -> String.format(generalGetString(MR.strings.ttl_h), value)
        Day -> String.format(generalGetString(MR.strings.ttl_d), value)
        Week -> String.format(generalGetString(MR.strings.ttl_w), value)
        Month -> String.format(generalGetString(MR.strings.ttl_mth), value)
      }
    }
  }
}

fun timeText(seconds: Int?): String {
  if (seconds == null) {
    return generalGetString(MR.strings.feature_off)
  }
  if (seconds == 0) {
    String.format(generalGetString(MR.strings.ttl_sec), 0)
  }
  return CustomTimeUnit.toText(seconds)
}

fun shortTimeText(seconds: Int?): String {
  if (seconds == null) {
    return generalGetString(MR.strings.feature_off)
  }
  if (seconds == 0) {
    String.format(generalGetString(MR.strings.ttl_s), 0)
  }
  return CustomTimeUnit.toShortText(seconds)
}

@Serializable
data class ContactUserPreferences(
  val timedMessages: ContactUserPreferenceTimed,
  val fullDelete: ContactUserPreference,
  val reactions: ContactUserPreference,
  val voice: ContactUserPreference,
  val calls: ContactUserPreference,
) {
  fun toPreferences(): ChatPreferences = ChatPreferences(
    timedMessages = timedMessages.userPreference.pref,
    fullDelete = fullDelete.userPreference.pref,
    reactions = reactions.userPreference.pref,
    voice = voice.userPreference.pref,
    calls = calls.userPreference.pref
  )

  companion object {
    val sampleData = ContactUserPreferences(
      timedMessages = ContactUserPreferenceTimed(
        enabled = FeatureEnabled(forUser = false, forContact = false),
        userPreference = ContactUserPrefTimed.User(preference = TimedMessagesPreference(allow = FeatureAllowed.NO)),
        contactPreference = TimedMessagesPreference(allow = FeatureAllowed.NO)
      ),
      fullDelete = ContactUserPreference(
        enabled = FeatureEnabled(forUser = false, forContact = false),
        userPreference = ContactUserPref.User(preference = SimpleChatPreference(allow = FeatureAllowed.NO)),
        contactPreference = SimpleChatPreference(allow = FeatureAllowed.NO)
      ),
      reactions = ContactUserPreference(
        enabled = FeatureEnabled(forUser = true, forContact = true),
        userPreference = ContactUserPref.User(preference = SimpleChatPreference(allow = FeatureAllowed.YES)),
        contactPreference = SimpleChatPreference(allow = FeatureAllowed.YES)
      ),
      voice = ContactUserPreference(
        enabled = FeatureEnabled(forUser = true, forContact = true),
        userPreference = ContactUserPref.User(preference = SimpleChatPreference(allow = FeatureAllowed.YES)),
        contactPreference = SimpleChatPreference(allow = FeatureAllowed.YES)
      ),
      calls = ContactUserPreference(
        enabled = FeatureEnabled(forUser = true, forContact = true),
        userPreference = ContactUserPref.User(preference = SimpleChatPreference(allow = FeatureAllowed.YES)),
        contactPreference = SimpleChatPreference(allow = FeatureAllowed.YES)
      ),
    )
  }
}

@Serializable
data class ContactUserPreference (
  val enabled: FeatureEnabled,
  val userPreference: ContactUserPref,
  val contactPreference: SimpleChatPreference,
)

@Serializable
data class ContactUserPreferenceTimed (
  val enabled: FeatureEnabled,
  val userPreference: ContactUserPrefTimed,
  val contactPreference: TimedMessagesPreference,
)

@Serializable
data class FeatureEnabled(
  val forUser: Boolean,
  val forContact: Boolean
) {
  val text: String
    get() = when {
      forUser && forContact -> generalGetString(MR.strings.feature_enabled)
      forUser -> generalGetString(MR.strings.feature_enabled_for_you)
      forContact -> generalGetString(MR.strings.feature_enabled_for_contact)
      else -> generalGetString(MR.strings.feature_off)
    }

  val iconColor: Color
    get() = if (forUser) SimplexGreen else if (forContact) WarningYellow else CurrentColors.value.colors.secondary

  companion object {
    fun enabled(asymmetric: Boolean, user: ChatPreference, contact: ChatPreference): FeatureEnabled =
      when {
        user.allow == FeatureAllowed.ALWAYS && contact.allow == FeatureAllowed.NO -> FeatureEnabled(forUser = false, forContact = asymmetric)
        user.allow == FeatureAllowed.NO && contact.allow == FeatureAllowed.ALWAYS -> FeatureEnabled(forUser = asymmetric, forContact = false)
        contact.allow == FeatureAllowed.NO -> FeatureEnabled(forUser = false, forContact = false)
        user.allow == FeatureAllowed.NO -> FeatureEnabled(forUser = false, forContact = false)
        else -> FeatureEnabled(forUser = true, forContact = true)
      }
  }
}

@Serializable
sealed class ContactUserPref {
  abstract val pref: SimpleChatPreference

  // contact override is set
  @Serializable @SerialName("contact") data class Contact(val preference: SimpleChatPreference): ContactUserPref() {
    override val pref get() = preference
  }
  // global user default is used
  @Serializable @SerialName("user") data class User(val preference: SimpleChatPreference): ContactUserPref() {
    override val pref get() = preference
  }
}

@Serializable
sealed class ContactUserPrefTimed {
  abstract val pref: TimedMessagesPreference

  // contact override is set
  @Serializable @SerialName("contact") data class Contact(val preference: TimedMessagesPreference): ContactUserPrefTimed() {
    override val pref get() = preference
  }
  // global user default is used
  @Serializable @SerialName("user") data class User(val preference: TimedMessagesPreference): ContactUserPrefTimed() {
    override val pref get() = preference
  }
}

interface Feature {
//  val icon: ImageVector
  val text: String
  @Composable
  fun iconFilled(): Painter
  val hasParam: Boolean
  val hasRole: Boolean
}

@Serializable
enum class ChatFeature: Feature {
  @SerialName("timedMessages") TimedMessages,
  @SerialName("fullDelete") FullDelete,
  @SerialName("reactions") Reactions,
  @SerialName("voice") Voice,
  @SerialName("calls") Calls;

  val asymmetric: Boolean get() = when (this) {
    TimedMessages -> false
    else -> true
  }

  override val hasParam: Boolean get() = when(this) {
      TimedMessages -> true
      else -> false
    }
  override val hasRole: Boolean = false

  override val text: String
    get() = when(this) {
      TimedMessages -> generalGetString(MR.strings.timed_messages)
      FullDelete -> generalGetString(MR.strings.full_deletion)
      Reactions -> generalGetString(MR.strings.message_reactions)
      Voice -> generalGetString(MR.strings.voice_messages)
      Calls -> generalGetString(MR.strings.audio_video_calls)
    }

  val icon: Painter
    @Composable get() = when(this) {
      TimedMessages -> painterResource(MR.images.ic_timer)
      FullDelete -> painterResource(MR.images.ic_delete_forever)
      Reactions -> painterResource(MR.images.ic_add_reaction)
      Voice -> painterResource(MR.images.ic_keyboard_voice)
      Calls -> painterResource(MR.images.ic_call)
    }

  @Composable
  override fun iconFilled(): Painter = when(this) {
      TimedMessages -> painterResource(MR.images.ic_timer_filled)
      FullDelete -> painterResource(MR.images.ic_delete_forever_filled)
      Reactions -> painterResource(MR.images.ic_add_reaction_filled)
      Voice -> painterResource(MR.images.ic_keyboard_voice_filled)
      Calls -> painterResource(MR.images.ic_call_filled)
  }

  fun allowDescription(allowed: FeatureAllowed): String =
    when (this) {
      TimedMessages -> when (allowed) {
        FeatureAllowed.ALWAYS -> generalGetString(MR.strings.allow_your_contacts_to_send_disappearing_messages)
        FeatureAllowed.YES -> generalGetString(MR.strings.allow_disappearing_messages_only_if)
        FeatureAllowed.NO -> generalGetString(MR.strings.prohibit_sending_disappearing_messages)
      }
      FullDelete -> when (allowed) {
        FeatureAllowed.ALWAYS -> generalGetString(MR.strings.allow_your_contacts_irreversibly_delete)
        FeatureAllowed.YES -> generalGetString(MR.strings.allow_irreversible_message_deletion_only_if)
        FeatureAllowed.NO -> generalGetString(MR.strings.contacts_can_mark_messages_for_deletion)
      }
      Reactions -> when (allowed) {
        FeatureAllowed.ALWAYS -> generalGetString(MR.strings.allow_your_contacts_adding_message_reactions)
        FeatureAllowed.YES -> generalGetString(MR.strings.allow_message_reactions_only_if)
        FeatureAllowed.NO -> generalGetString(MR.strings.prohibit_message_reactions)
      }
        Voice -> when (allowed) {
        FeatureAllowed.ALWAYS -> generalGetString(MR.strings.allow_your_contacts_to_send_voice_messages)
        FeatureAllowed.YES -> generalGetString(MR.strings.allow_voice_messages_only_if)
        FeatureAllowed.NO -> generalGetString(MR.strings.prohibit_sending_voice_messages)
      }
      Calls -> when (allowed) {
        FeatureAllowed.ALWAYS -> generalGetString(MR.strings.allow_your_contacts_to_call)
        FeatureAllowed.YES -> generalGetString(MR.strings.allow_calls_only_if)
        FeatureAllowed.NO -> generalGetString(MR.strings.prohibit_calls)
      }
    }

  fun enabledDescription(enabled: FeatureEnabled): String =
    when (this) {
      TimedMessages -> when {
        enabled.forUser && enabled.forContact -> generalGetString(MR.strings.both_you_and_your_contact_can_send_disappearing)
        enabled.forUser -> generalGetString(MR.strings.only_you_can_send_disappearing)
        enabled.forContact -> generalGetString(MR.strings.only_your_contact_can_send_disappearing)
        else -> generalGetString(MR.strings.disappearing_prohibited_in_this_chat)
      }
      FullDelete -> when {
        enabled.forUser && enabled.forContact -> generalGetString(MR.strings.both_you_and_your_contacts_can_delete)
        enabled.forUser -> generalGetString(MR.strings.only_you_can_delete_messages)
        enabled.forContact -> generalGetString(MR.strings.only_your_contact_can_delete)
        else -> generalGetString(MR.strings.message_deletion_prohibited)
      }
      Reactions -> when {
        enabled.forUser && enabled.forContact -> generalGetString(MR.strings.both_you_and_your_contact_can_add_message_reactions)
        enabled.forUser -> generalGetString(MR.strings.only_you_can_add_message_reactions)
        enabled.forContact -> generalGetString(MR.strings.only_your_contact_can_add_message_reactions)
        else -> generalGetString(MR.strings.message_reactions_prohibited_in_this_chat)
      }
      Voice -> when {
        enabled.forUser && enabled.forContact -> generalGetString(MR.strings.both_you_and_your_contact_can_send_voice)
        enabled.forUser -> generalGetString(MR.strings.only_you_can_send_voice)
        enabled.forContact -> generalGetString(MR.strings.only_your_contact_can_send_voice)
        else -> generalGetString(MR.strings.voice_prohibited_in_this_chat)
      }
      Calls -> when {
        enabled.forUser && enabled.forContact -> generalGetString(MR.strings.both_you_and_your_contact_can_make_calls)
        enabled.forUser -> generalGetString(MR.strings.only_you_can_make_calls)
        enabled.forContact -> generalGetString(MR.strings.only_your_contact_can_make_calls)
        else -> generalGetString(MR.strings.calls_prohibited_with_this_contact)
      }
    }
}

@Serializable
enum class GroupFeature: Feature {
  @SerialName("timedMessages") TimedMessages,
  @SerialName("directMessages") DirectMessages,
  @SerialName("fullDelete") FullDelete,
  @SerialName("reactions") Reactions,
  @SerialName("voice") Voice,
  @SerialName("files") Files,
  @SerialName("simplexLinks") SimplexLinks,
  @SerialName("reports") Reports,
  @SerialName("history") History;

  override val hasParam: Boolean get() = when(this) {
    TimedMessages -> true
    else -> false
  }

  override val hasRole: Boolean
    get() = when (this) {
      TimedMessages -> false
      DirectMessages -> true
      FullDelete -> false
      Reactions -> false
      Voice -> true
      Files -> true
      SimplexLinks -> true
      Reports -> false
      History -> false
    }

  override val text: String
    get() = when(this) {
      TimedMessages -> generalGetString(MR.strings.timed_messages)
      DirectMessages -> generalGetString(MR.strings.direct_messages)
      FullDelete -> generalGetString(MR.strings.full_deletion)
      Reactions -> generalGetString(MR.strings.message_reactions)
      Voice -> generalGetString(MR.strings.voice_messages)
      Files -> generalGetString(MR.strings.files_and_media)
      SimplexLinks -> generalGetString(MR.strings.simplex_links)
      Reports -> generalGetString(MR.strings.group_reports_member_reports)
      History -> generalGetString(MR.strings.recent_history)
    }

  val icon: Painter
    @Composable get() = when(this) {
      TimedMessages -> painterResource(MR.images.ic_timer)
      DirectMessages -> painterResource(MR.images.ic_swap_horizontal_circle)
      FullDelete -> painterResource(MR.images.ic_delete_forever)
      Reactions -> painterResource(MR.images.ic_add_reaction)
      Voice -> painterResource(MR.images.ic_keyboard_voice)
      Files -> painterResource(MR.images.ic_draft)
      SimplexLinks -> painterResource(MR.images.ic_link)
      Reports -> painterResource(MR.images.ic_flag)
      History -> painterResource(MR.images.ic_schedule)
    }

  @Composable
  override fun iconFilled(): Painter = when(this) {
    TimedMessages -> painterResource(MR.images.ic_timer_filled)
    DirectMessages -> painterResource(MR.images.ic_swap_horizontal_circle_filled)
    FullDelete -> painterResource(MR.images.ic_delete_forever_filled)
    Reactions -> painterResource(MR.images.ic_add_reaction_filled)
    Voice -> painterResource(MR.images.ic_keyboard_voice_filled)
    Files -> painterResource(MR.images.ic_draft_filled)
    SimplexLinks -> painterResource(MR.images.ic_link)
    Reports -> painterResource(MR.images.ic_flag_filled)
    History -> painterResource(MR.images.ic_schedule_filled)
  }

  fun enableDescription(enabled: GroupFeatureEnabled, canEdit: Boolean): String =
    if (canEdit) {
      when(this) {
        TimedMessages -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(MR.strings.allow_to_send_disappearing)
          GroupFeatureEnabled.OFF -> generalGetString(MR.strings.prohibit_sending_disappearing)
        }
        DirectMessages -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(MR.strings.allow_direct_messages)
          GroupFeatureEnabled.OFF -> generalGetString(MR.strings.prohibit_direct_messages)
        }
        FullDelete -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(MR.strings.allow_to_delete_messages)
          GroupFeatureEnabled.OFF -> generalGetString(MR.strings.prohibit_message_deletion)
        }
        Reactions -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(MR.strings.allow_message_reactions)
          GroupFeatureEnabled.OFF -> generalGetString(MR.strings.prohibit_message_reactions_group)
        }
        Voice -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(MR.strings.allow_to_send_voice)
          GroupFeatureEnabled.OFF -> generalGetString(MR.strings.prohibit_sending_voice)
        }
        Files -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(MR.strings.allow_to_send_files)
          GroupFeatureEnabled.OFF -> generalGetString(MR.strings.prohibit_sending_files)
        }
        SimplexLinks -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(MR.strings.allow_to_send_simplex_links)
          GroupFeatureEnabled.OFF -> generalGetString(MR.strings.prohibit_sending_simplex_links)
        }
        Reports -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(MR.strings.enable_sending_member_reports)
          GroupFeatureEnabled.OFF -> generalGetString(MR.strings.disable_sending_member_reports)
        }
        History -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(MR.strings.enable_sending_recent_history)
          GroupFeatureEnabled.OFF -> generalGetString(MR.strings.disable_sending_recent_history)
        }
      }
    } else {
      when(this) {
        TimedMessages -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(MR.strings.group_members_can_send_disappearing)
          GroupFeatureEnabled.OFF -> generalGetString(MR.strings.disappearing_messages_are_prohibited)
        }
        DirectMessages -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(MR.strings.group_members_can_send_dms)
          GroupFeatureEnabled.OFF -> generalGetString(MR.strings.direct_messages_are_prohibited)
        }
        FullDelete -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(MR.strings.group_members_can_delete)
          GroupFeatureEnabled.OFF -> generalGetString(MR.strings.message_deletion_prohibited_in_chat)
        }
        Reactions -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(MR.strings.group_members_can_add_message_reactions)
          GroupFeatureEnabled.OFF -> generalGetString(MR.strings.message_reactions_are_prohibited)
        }
        Voice -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(MR.strings.group_members_can_send_voice)
          GroupFeatureEnabled.OFF -> generalGetString(MR.strings.voice_messages_are_prohibited)
        }
        Files -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(MR.strings.group_members_can_send_files)
          GroupFeatureEnabled.OFF -> generalGetString(MR.strings.files_are_prohibited_in_group)
        }
        SimplexLinks -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(MR.strings.group_members_can_send_simplex_links)
          GroupFeatureEnabled.OFF -> generalGetString(MR.strings.simplex_links_are_prohibited_in_group)
        }
        Reports -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(MR.strings.group_members_can_send_reports)
          GroupFeatureEnabled.OFF -> generalGetString(MR.strings.member_reports_are_prohibited)
        }
        History -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(MR.strings.recent_history_is_sent_to_new_members)
          GroupFeatureEnabled.OFF -> generalGetString(MR.strings.recent_history_is_not_sent_to_new_members)
        }
      }
    }
}

@Serializable
sealed class ContactFeatureAllowed {
  @Serializable @SerialName("userDefault") data class UserDefault(val default: FeatureAllowed): ContactFeatureAllowed()
  @Serializable @SerialName("always") object Always: ContactFeatureAllowed()
  @Serializable @SerialName("yes") object Yes: ContactFeatureAllowed()
  @Serializable @SerialName("no") object No: ContactFeatureAllowed()

  companion object {
    fun values(def: FeatureAllowed): List<ContactFeatureAllowed> = listOf(UserDefault(def), Always, Yes, No)
  }

  val allowed: FeatureAllowed
    get() = when (this) {
      is UserDefault -> this.default
      is Always -> FeatureAllowed.ALWAYS
      is Yes -> FeatureAllowed.YES
      is No -> FeatureAllowed.NO
    }
  val text: String
    get() = when (this) {
      is UserDefault -> String.format(generalGetString(MR.strings.chat_preferences_default), default.text)
      is Always -> generalGetString(MR.strings.chat_preferences_always)
      is Yes -> generalGetString(MR.strings.chat_preferences_yes)
      is No -> generalGetString(MR.strings.chat_preferences_no)
    }
}

@Serializable
data class ContactFeaturesAllowed(
  val timedMessagesAllowed: Boolean,
  val timedMessagesTTL: Int?,
  val fullDelete: ContactFeatureAllowed,
  val reactions: ContactFeatureAllowed,
  val voice: ContactFeatureAllowed,
  val calls: ContactFeatureAllowed,
) {
  companion object {
    val sampleData = ContactFeaturesAllowed(
      timedMessagesAllowed = false,
      timedMessagesTTL = null,
      fullDelete = ContactFeatureAllowed.UserDefault(FeatureAllowed.NO),
      reactions = ContactFeatureAllowed.UserDefault(FeatureAllowed.YES),
      voice = ContactFeatureAllowed.UserDefault(FeatureAllowed.YES),
      calls = ContactFeatureAllowed.UserDefault(FeatureAllowed.YES),
    )
  }
}

fun contactUserPrefsToFeaturesAllowed(contactUserPreferences: ContactUserPreferences): ContactFeaturesAllowed {
  val pref = contactUserPreferences.timedMessages.userPreference
  val allow = pref.pref.allow
  return ContactFeaturesAllowed(
    timedMessagesAllowed = allow == FeatureAllowed.YES || allow == FeatureAllowed.ALWAYS,
    timedMessagesTTL = pref.pref.ttl,
    fullDelete = contactUserPrefToFeatureAllowed(contactUserPreferences.fullDelete),
    reactions = contactUserPrefToFeatureAllowed(contactUserPreferences.reactions),
    voice = contactUserPrefToFeatureAllowed(contactUserPreferences.voice),
    calls = contactUserPrefToFeatureAllowed(contactUserPreferences.calls),
  )
}

fun contactUserPrefToFeatureAllowed(contactUserPreference: ContactUserPreference): ContactFeatureAllowed =
  when (val pref = contactUserPreference.userPreference) {
    is ContactUserPref.User -> ContactFeatureAllowed.UserDefault(pref.preference.allow)
    is ContactUserPref.Contact -> when (pref.preference.allow) {
      FeatureAllowed.ALWAYS -> ContactFeatureAllowed.Always
      FeatureAllowed.YES -> ContactFeatureAllowed.Yes
      FeatureAllowed.NO -> ContactFeatureAllowed.No
    }
  }

fun contactFeaturesAllowedToPrefs(contactFeaturesAllowed: ContactFeaturesAllowed): ChatPreferences =
  ChatPreferences(
    timedMessages = TimedMessagesPreference(if (contactFeaturesAllowed.timedMessagesAllowed) FeatureAllowed.YES else FeatureAllowed.NO, contactFeaturesAllowed.timedMessagesTTL),
    fullDelete = contactFeatureAllowedToPref(contactFeaturesAllowed.fullDelete),
    reactions = contactFeatureAllowedToPref(contactFeaturesAllowed.reactions),
    voice = contactFeatureAllowedToPref(contactFeaturesAllowed.voice),
    calls = contactFeatureAllowedToPref(contactFeaturesAllowed.calls),
  )

fun contactFeatureAllowedToPref(contactFeatureAllowed: ContactFeatureAllowed): SimpleChatPreference? =
  when(contactFeatureAllowed) {
    is ContactFeatureAllowed.UserDefault -> null
    is ContactFeatureAllowed.Always -> SimpleChatPreference(allow = FeatureAllowed.ALWAYS)
    is ContactFeatureAllowed.Yes -> SimpleChatPreference(allow = FeatureAllowed.YES)
    is ContactFeatureAllowed.No -> SimpleChatPreference(allow = FeatureAllowed.NO)
  }

@Serializable
enum class FeatureAllowed {
  @SerialName("yes") YES,
  @SerialName("no") NO,
  @SerialName("always") ALWAYS;

  val text: String
    get() = when(this) {
      ALWAYS -> generalGetString(MR.strings.chat_preferences_always)
      YES -> generalGetString(MR.strings.chat_preferences_yes)
      NO -> generalGetString(MR.strings.chat_preferences_no)
    }
}

@Serializable
data class FullGroupPreferences(
  val timedMessages: TimedMessagesGroupPreference,
  val directMessages: RoleGroupPreference,
  val fullDelete: GroupPreference,
  val reactions: GroupPreference,
  val voice: RoleGroupPreference,
  val files: RoleGroupPreference,
  val simplexLinks: RoleGroupPreference,
  val reports: GroupPreference,
  val history: GroupPreference,
) {
  fun toGroupPreferences(): GroupPreferences =
    GroupPreferences(
      timedMessages = timedMessages,
      directMessages = directMessages,
      fullDelete = fullDelete,
      reactions = reactions,
      voice = voice,
      files = files,
      simplexLinks = simplexLinks,
      reports = reports,
      history = history,
    )

  companion object {
    val sampleData = FullGroupPreferences(
      timedMessages = TimedMessagesGroupPreference(GroupFeatureEnabled.OFF),
      directMessages = RoleGroupPreference(GroupFeatureEnabled.OFF, role = null),
      fullDelete = GroupPreference(GroupFeatureEnabled.OFF),
      reactions = GroupPreference(GroupFeatureEnabled.ON),
      voice = RoleGroupPreference(GroupFeatureEnabled.ON, role = null),
      files = RoleGroupPreference(GroupFeatureEnabled.ON, role = null),
      simplexLinks = RoleGroupPreference(GroupFeatureEnabled.ON, role = null),
      reports = GroupPreference(GroupFeatureEnabled.ON),
      history = GroupPreference(GroupFeatureEnabled.ON),
    )
  }
}

@Serializable
data class GroupPreferences(
  val timedMessages: TimedMessagesGroupPreference? = null,
  val directMessages: RoleGroupPreference? = null,
  val fullDelete: GroupPreference? = null,
  val reactions: GroupPreference? = null,
  val voice: RoleGroupPreference? = null,
  val files: RoleGroupPreference? = null,
  val simplexLinks: RoleGroupPreference? = null,
  val reports: GroupPreference? = null,
  val history: GroupPreference? = null,
) {
  companion object {
    val sampleData = GroupPreferences(
      timedMessages = TimedMessagesGroupPreference(GroupFeatureEnabled.OFF),
      directMessages = RoleGroupPreference(GroupFeatureEnabled.OFF, role = null),
      fullDelete = GroupPreference(GroupFeatureEnabled.OFF),
      reactions = GroupPreference(GroupFeatureEnabled.ON),
      voice = RoleGroupPreference(GroupFeatureEnabled.ON, role = null),
      files = RoleGroupPreference(GroupFeatureEnabled.ON, role = null),
      simplexLinks = RoleGroupPreference(GroupFeatureEnabled.ON, role = null),
      reports = GroupPreference(GroupFeatureEnabled.ON),
      history = GroupPreference(GroupFeatureEnabled.ON),
    )
  }
}

@Serializable
data class GroupPreference(
  val enable: GroupFeatureEnabled
) {
  val on: Boolean get() = enable == GroupFeatureEnabled.ON

  fun enabled(role: GroupMemberRole?, m: GroupMember?): GroupFeatureEnabled =
    when (enable) {
      GroupFeatureEnabled.OFF -> GroupFeatureEnabled.OFF
      GroupFeatureEnabled.ON ->
        if (role != null && m != null) {
          if (m.memberRole >= role) GroupFeatureEnabled.ON else GroupFeatureEnabled.OFF
        } else {
          GroupFeatureEnabled.ON
        }
    }
}

@Serializable
data class RoleGroupPreference(
  val enable: GroupFeatureEnabled,
  val role: GroupMemberRole? = null,
) {
  fun on(m: GroupMember): Boolean =
    enable == GroupFeatureEnabled.ON && m.memberRole >= (role ?: GroupMemberRole.Observer)
}

@Serializable
data class TimedMessagesGroupPreference(
  val enable: GroupFeatureEnabled,
  val ttl: Int? = null
) {
  val on: Boolean get() = enable == GroupFeatureEnabled.ON
}

@Serializable
enum class GroupFeatureEnabled {
  @SerialName("on") ON,
  @SerialName("off") OFF;

  val text: String
    get() = when (this) {
      ON -> generalGetString(MR.strings.chat_preferences_on)
      OFF -> generalGetString(MR.strings.chat_preferences_off)
    }

  val iconColor: Color
    get() = if (this == ON) SimplexGreen else CurrentColors.value.colors.secondary

}

@Serializable
data class RemoteCtrl (
  val remoteCtrlId: Long,
  val displayName: String,
  val fingerprint: String,
  val accepted: Boolean?
)

@Serializable
data class RemoteCtrlInfo (
  val remoteCtrlId: Long,
  val ctrlDeviceName: String,
  val sessionState: RemoteCtrlSessionState?
) {
  val deviceViewName: String
    get() = ctrlDeviceName.ifEmpty { remoteCtrlId.toString() }
}

@Serializable
data class RemoteHostInfo(
  val remoteHostId: Long,
  val hostDeviceName: String,
  val storePath: String,
  val bindAddress_: RemoteCtrlAddress?,
  val bindPort_: Int?,
  val sessionState: RemoteHostSessionState?
) {
  val activeHost: Boolean
    @Composable get() = chatModel.currentRemoteHost.value?.remoteHostId == remoteHostId

  fun activeHost(): Boolean = chatModel.currentRemoteHost.value?.remoteHostId == remoteHostId
}

@Serializable
data class RemoteCtrlAddress(
  val address: String,
  val `interface`: String
)

@Serializable
sealed class RemoteHostSessionState {
  @Serializable @SerialName("starting") object Starting: RemoteHostSessionState()
  @Serializable @SerialName("connecting") class Connecting(val invitation: String): RemoteHostSessionState()
  @Serializable @SerialName("pendingConfirmation") class PendingConfirmation(val sessionCode: String): RemoteHostSessionState()
  @Serializable @SerialName("confirmed") data class Confirmed(val sessionCode: String): RemoteHostSessionState()
  @Serializable @SerialName("connected") data class Connected(val sessionCode: String): RemoteHostSessionState()
}

@Serializable
sealed class RemoteHostStopReason {
  @Serializable @SerialName("connectionFailed") data class ConnectionFailed(val chatError: ChatError): RemoteHostStopReason()
  @Serializable @SerialName("crashed") data class Crashed(val chatError: ChatError): RemoteHostStopReason()
  @Serializable @SerialName("disconnected") object Disconnected: RemoteHostStopReason()
}

val json = Json {
  prettyPrint = true
  ignoreUnknownKeys = true
  encodeDefaults = true
  explicitNulls = false
}

// Can decode unknown enum to default value specified for this field
val jsonCoerceInputValues = Json {
  prettyPrint = true
  ignoreUnknownKeys = true
  encodeDefaults = true
  explicitNulls = false
  coerceInputValues = true
}

val jsonShort = Json {
  prettyPrint = false
  ignoreUnknownKeys = true
  encodeDefaults = true
  explicitNulls = false
}

val yaml = Yaml(configuration = YamlConfiguration(
  strictMode = false,
  encodeDefaults = false,
  /** ~5.5 MB limitation since wallpaper is limited by 5 MB, see [saveWallpaperFile] */
  codePointLimit = 5500000,
))

@Suppress("SERIALIZER_TYPE_INCOMPATIBLE")
@Serializable(with = APISerializer::class)
sealed class API {
  @Serializable(with = APISerializer::class)  class Result(val remoteHostId: Long?, val res: CR) : API()
  @Serializable(with = APISerializer::class)  class Error(val remoteHostId: Long?, val err: ChatError) : API()

  val ok: Boolean get() = this is API.Result && this.res is CR.CmdOk
  val result: CR? get() = (this as? API.Result)?.res
  val rhId: Long? get() = when (this) {
    is Result -> remoteHostId
    is Error -> remoteHostId
  }

  val pair: Pair<CR?, ChatError?> get() = when (this) {
    is Result -> res to null
    is Error -> null to err
  }

  val responseType: String get() = when (this) {
    is Result -> res.responseType
    is Error -> "error ${err.resultType}"
  }

  val details: String get() = when (this) {
    is Result -> res.details
    is Error -> "error ${err.string}"
  }
}

object APISerializer : KSerializer<API> {
  override val descriptor: SerialDescriptor = buildSerialDescriptor("API", PolymorphicKind.SEALED) {
    element("Result", buildClassSerialDescriptor("Result") {
      element<Long?>("remoteHostId")
      element<CR>("result")
    })
    element("Error", buildClassSerialDescriptor("Error") {
      element<Long?>("remoteHostId")
      element<ChatError>("error")
    })
  }

  override fun deserialize(decoder: Decoder): API {
    require(decoder is JsonDecoder)
    val j = try { decoder.decodeJsonElement() } catch(e: Exception) { null } catch(e: Throwable) { null }
    if (j == null) return API.Error(remoteHostId = null, ChatError.ChatErrorInvalidJSON(""))
    if (j !is JsonObject) return API.Error(remoteHostId = null, ChatError.ChatErrorInvalidJSON(json.encodeToString(j)))
    val remoteHostId = j["remoteHostId"]?.jsonPrimitive?.longOrNull
    val jRes = j["result"]
    if (jRes != null) {
      val result = try {
        decoder.json.decodeFromJsonElement<CR>(jRes)
      } catch (e: Exception) {
        fallbackResult(jRes)
      } catch (e: Throwable) {
        fallbackResult(jRes)
      }
      return API.Result(remoteHostId, result)
    }
    val jErr = j["error"]
    if (jErr != null) {
      val error = try {
        decoder.json.decodeFromJsonElement<ChatError>(jErr)
      } catch (e: Exception) {
        fallbackChatError(jErr)
      } catch (e: Throwable) {
        AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error), e.stackTraceToString())
        fallbackChatError(jErr)
      }
      return API.Error(remoteHostId, error)
    }
    return API.Error(remoteHostId, fallbackChatError(j))
  }

  private fun fallbackResult(jRes: JsonElement): CR {
    if (jRes is JsonObject) {
      val type = jRes["type"]?.jsonPrimitive?.contentOrNull ?: "invalid"
      try {
        if (type == "apiChats") {
          val user: UserRef = json.decodeFromJsonElement(jRes["user"]!!.jsonObject)
          val chats: List<Chat> = jRes["chats"]!!.jsonArray.map {
            parseChatData(it)
          }
          return CR.ApiChats(user, chats)
        } else if (type == "apiChat") {
          val user: UserRef = json.decodeFromJsonElement(jRes["user"]!!.jsonObject)
          val chat = parseChatData(jRes["chat"]!!)
          return CR.ApiChat(user, chat)
        }
      } catch (e: Exception) {
        Log.e(TAG, "Exception while parsing chat(s): " + e.stackTraceToString())
      } catch (e: Throwable) {
        Log.e(TAG, "Throwable while parsing chat(s): " + e.stackTraceToString())
        AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error), e.stackTraceToString())
      }
      return CR.Response(type, json.encodeToString(jRes))
    }
    return CR.Response(type = "invalid", json.encodeToString(jRes))
  }

  private fun fallbackChatError(jErr: JsonElement): ChatError {
    return ChatError.ChatErrorInvalidJSON(json.encodeToString(jErr))
  }

  override fun serialize(encoder: Encoder, value: API) {
    require(encoder is JsonEncoder)
    val json = when (value) {
      is API.Result -> buildJsonObject {
        value.remoteHostId?.let { put("remoteHostId", it) }
        put("result", encoder.json.encodeToJsonElement(value.res))
      }
      is API.Error -> buildJsonObject {
        value.remoteHostId?.let { put("remoteHostId", it) }
        put("error", encoder.json.encodeToJsonElement(value.err))
      }
    }
    encoder.encodeJsonElement(json)
  }
}

private fun parseChatData(chat: JsonElement): Chat {
  val chatInfo: ChatInfo = decodeObject(ChatInfo.serializer(), chat.jsonObject["chatInfo"])
    ?: ChatInfo.InvalidJSON(json.encodeToString(chat.jsonObject["chatInfo"]))
  val chatStats = decodeObject(Chat.ChatStats.serializer(), chat.jsonObject["chatStats"])!!
  val chatItems: List<ChatItem> = chat.jsonObject["chatItems"]!!.jsonArray.map {
    decodeObject(ChatItem.serializer(), it) ?: parseChatItem(it)
  }
  return Chat(remoteHostId = null, chatInfo, chatItems, chatStats)
}

private fun parseChatItem(j: JsonElement): ChatItem {
  val chatDir: CIDirection? = decodeObject(CIDirection.serializer(), j.jsonObject["chatDir"])
  val meta: CIMeta? = decodeObject(CIMeta.serializer(), j.jsonObject["meta"])
  return ChatItem.invalidJSON(chatDir, meta, json.encodeToString(j))
}

private fun <T> decodeObject(deserializer: DeserializationStrategy<T>, obj: JsonElement?): T? =
  runCatching { json.decodeFromJsonElement(deserializer, obj!!) }.getOrNull()

// ChatResponse
@Serializable
sealed class CR {
  @Serializable @SerialName("activeUser") class ActiveUser(val user: User): CR()
  @Serializable @SerialName("usersList") class UsersList(val users: List<UserInfo>): CR()
  @Serializable @SerialName("chatStarted") class ChatStarted: CR()
  @Serializable @SerialName("chatRunning") class ChatRunning: CR()
  @Serializable @SerialName("chatStopped") class ChatStopped: CR()
  @Serializable @SerialName("apiChats") class ApiChats(val user: UserRef, val chats: List<Chat>): CR()
  @Serializable @SerialName("apiChat") class ApiChat(val user: UserRef, val chat: Chat, val navInfo: NavigationInfo = NavigationInfo()): CR()
  @Serializable @SerialName("chatTags") class ChatTags(val user: UserRef, val userTags: List<ChatTag>): CR()
  @Serializable @SerialName("chatItemInfo") class ApiChatItemInfo(val user: UserRef, val chatItem: AChatItem, val chatItemInfo: ChatItemInfo): CR()
  @Serializable @SerialName("serverTestResult") class ServerTestResult(val user: UserRef, val testServer: String, val testFailure: ProtocolTestFailure? = null): CR()
  @Serializable @SerialName("serverOperatorConditions") class ServerOperatorConditions(val conditions: ServerOperatorConditionsDetail): CR()
  @Serializable @SerialName("userServers") class UserServers(val user: UserRef, val userServers: List<UserOperatorServers>): CR()
  @Serializable @SerialName("userServersValidation") class UserServersValidation(val user: UserRef, val serverErrors: List<UserServersError>): CR()
  @Serializable @SerialName("usageConditions") class UsageConditions(val usageConditions: UsageConditionsDetail, val conditionsText: String?, val acceptedConditions: UsageConditionsDetail?): CR()
  @Serializable @SerialName("chatItemTTL") class ChatItemTTL(val user: UserRef, val chatItemTTL: Long? = null): CR()
  @Serializable @SerialName("networkConfig") class NetworkConfig(val networkConfig: NetCfg): CR()
  @Serializable @SerialName("contactInfo") class ContactInfo(val user: UserRef, val contact: Contact, val connectionStats_: ConnectionStats? = null, val customUserProfile: Profile? = null): CR()
  @Serializable @SerialName("groupMemberInfo") class GroupMemberInfo(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember, val connectionStats_: ConnectionStats? = null): CR()
  @Serializable @SerialName("queueInfo") class QueueInfoR(val user: UserRef, val rcvMsgInfo: RcvMsgInfo?, val queueInfo: ServerQueueInfo): CR()
  @Serializable @SerialName("contactSwitchStarted") class ContactSwitchStarted(val user: UserRef, val contact: Contact, val connectionStats: ConnectionStats): CR()
  @Serializable @SerialName("groupMemberSwitchStarted") class GroupMemberSwitchStarted(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember, val connectionStats: ConnectionStats): CR()
  @Serializable @SerialName("contactSwitchAborted") class ContactSwitchAborted(val user: UserRef, val contact: Contact, val connectionStats: ConnectionStats): CR()
  @Serializable @SerialName("groupMemberSwitchAborted") class GroupMemberSwitchAborted(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember, val connectionStats: ConnectionStats): CR()
  @Serializable @SerialName("contactSwitch") class ContactSwitch(val user: UserRef, val contact: Contact, val switchProgress: SwitchProgress): CR()
  @Serializable @SerialName("groupMemberSwitch") class GroupMemberSwitch(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember, val switchProgress: SwitchProgress): CR()
  @Serializable @SerialName("contactRatchetSyncStarted") class ContactRatchetSyncStarted(val user: UserRef, val contact: Contact, val connectionStats: ConnectionStats): CR()
  @Serializable @SerialName("groupMemberRatchetSyncStarted") class GroupMemberRatchetSyncStarted(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember, val connectionStats: ConnectionStats): CR()
  @Serializable @SerialName("contactRatchetSync") class ContactRatchetSync(val user: UserRef, val contact: Contact, val ratchetSyncProgress: RatchetSyncProgress): CR()
  @Serializable @SerialName("groupMemberRatchetSync") class GroupMemberRatchetSync(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember, val ratchetSyncProgress: RatchetSyncProgress): CR()
  @Serializable @SerialName("contactCode") class ContactCode(val user: UserRef, val contact: Contact, val connectionCode: String): CR()
  @Serializable @SerialName("groupMemberCode") class GroupMemberCode(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember, val connectionCode: String): CR()
  @Serializable @SerialName("connectionVerified") class ConnectionVerified(val user: UserRef, val verified: Boolean, val expectedCode: String): CR()
  @Serializable @SerialName("tagsUpdated") class TagsUpdated(val user: UserRef, val userTags: List<ChatTag>, val chatTags: List<Long>): CR()
  @Serializable @SerialName("invitation") class Invitation(val user: UserRef, val connLinkInvitation: CreatedConnLink, val connection: PendingContactConnection): CR()
  @Serializable @SerialName("connectionIncognitoUpdated") class ConnectionIncognitoUpdated(val user: UserRef, val toConnection: PendingContactConnection): CR()
  @Serializable @SerialName("connectionUserChanged") class ConnectionUserChanged(val user: UserRef, val fromConnection: PendingContactConnection, val toConnection: PendingContactConnection, val newUser: UserRef): CR()
  @Serializable @SerialName("connectionPlan") class CRConnectionPlan(val user: UserRef, val connLink: CreatedConnLink, val connectionPlan: ConnectionPlan): CR()
  @Serializable @SerialName("sentConfirmation") class SentConfirmation(val user: UserRef, val connection: PendingContactConnection): CR()
  @Serializable @SerialName("sentInvitation") class SentInvitation(val user: UserRef, val connection: PendingContactConnection): CR()
  @Serializable @SerialName("sentInvitationToContact") class SentInvitationToContact(val user: UserRef, val contact: Contact, val customUserProfile: Profile?): CR()
  @Serializable @SerialName("contactAlreadyExists") class ContactAlreadyExists(val user: UserRef, val contact: Contact): CR()
  @Serializable @SerialName("contactDeleted") class ContactDeleted(val user: UserRef, val contact: Contact): CR()
  @Serializable @SerialName("contactDeletedByContact") class ContactDeletedByContact(val user: UserRef, val contact: Contact): CR()
  @Serializable @SerialName("chatCleared") class ChatCleared(val user: UserRef, val chatInfo: ChatInfo): CR()
  @Serializable @SerialName("userProfileNoChange") class UserProfileNoChange(val user: User): CR()
  @Serializable @SerialName("userProfileUpdated") class UserProfileUpdated(val user: User, val fromProfile: Profile, val toProfile: Profile, val updateSummary: UserProfileUpdateSummary): CR()
  @Serializable @SerialName("userPrivacy") class UserPrivacy(val user: User, val updatedUser: User): CR()
  @Serializable @SerialName("contactAliasUpdated") class ContactAliasUpdated(val user: UserRef, val toContact: Contact): CR()
  @Serializable @SerialName("groupAliasUpdated") class GroupAliasUpdated(val user: UserRef, val toGroup: GroupInfo): CR()
  @Serializable @SerialName("connectionAliasUpdated") class ConnectionAliasUpdated(val user: UserRef, val toConnection: PendingContactConnection): CR()
  @Serializable @SerialName("contactPrefsUpdated") class ContactPrefsUpdated(val user: UserRef, val fromContact: Contact, val toContact: Contact): CR()
  @Serializable @SerialName("userContactLink") class UserContactLink(val user: User, val contactLink: UserContactLinkRec): CR()
  @Serializable @SerialName("userContactLinkUpdated") class UserContactLinkUpdated(val user: User, val contactLink: UserContactLinkRec): CR()
  @Serializable @SerialName("userContactLinkCreated") class UserContactLinkCreated(val user: User, val connLinkContact: CreatedConnLink): CR()
  @Serializable @SerialName("userContactLinkDeleted") class UserContactLinkDeleted(val user: User): CR()
  @Serializable @SerialName("contactConnected") class ContactConnected(val user: UserRef, val contact: Contact, val userCustomProfile: Profile? = null): CR()
  @Serializable @SerialName("contactConnecting") class ContactConnecting(val user: UserRef, val contact: Contact): CR()
  @Serializable @SerialName("contactSndReady") class ContactSndReady(val user: UserRef, val contact: Contact): CR()
  @Serializable @SerialName("receivedContactRequest") class ReceivedContactRequest(val user: UserRef, val contactRequest: UserContactRequest): CR()
  @Serializable @SerialName("acceptingContactRequest") class AcceptingContactRequest(val user: UserRef, val contact: Contact): CR()
  @Serializable @SerialName("contactRequestRejected") class ContactRequestRejected(val user: UserRef): CR()
  @Serializable @SerialName("contactUpdated") class ContactUpdated(val user: UserRef, val toContact: Contact): CR()
  @Serializable @SerialName("groupMemberUpdated") class GroupMemberUpdated(val user: UserRef, val groupInfo: GroupInfo, val fromMember: GroupMember, val toMember: GroupMember): CR()
  // TODO remove below
  @Serializable @SerialName("contactsSubscribed") class ContactsSubscribed(val server: String, val contactRefs: List<ContactRef>): CR()
  @Serializable @SerialName("contactsDisconnected") class ContactsDisconnected(val server: String, val contactRefs: List<ContactRef>): CR()
  @Serializable @SerialName("contactSubSummary") class ContactSubSummary(val user: UserRef, val contactSubscriptions: List<ContactSubStatus>): CR()
  // TODO remove above
  @Serializable @SerialName("networkStatus") class NetworkStatusResp(val networkStatus: NetworkStatus, val connections: List<String>): CR()
  @Serializable @SerialName("networkStatuses") class NetworkStatuses(val user_: UserRef?, val networkStatuses: List<ConnNetworkStatus>): CR()
  @Serializable @SerialName("newChatItems") class NewChatItems(val user: UserRef, val chatItems: List<AChatItem>): CR()
  @Serializable @SerialName("chatItemsStatusesUpdated") class ChatItemsStatusesUpdated(val user: UserRef, val chatItems: List<AChatItem>): CR()
  @Serializable @SerialName("chatItemUpdated") class ChatItemUpdated(val user: UserRef, val chatItem: AChatItem): CR()
  @Serializable @SerialName("chatItemNotChanged") class ChatItemNotChanged(val user: UserRef, val chatItem: AChatItem): CR()
  @Serializable @SerialName("chatItemReaction") class ChatItemReaction(val user: UserRef, val added: Boolean, val reaction: ACIReaction): CR()
  @Serializable @SerialName("reactionMembers") class ReactionMembers(val user: UserRef, val memberReactions: List<MemberReaction>): CR()
  @Serializable @SerialName("chatItemsDeleted") class ChatItemsDeleted(val user: UserRef, val chatItemDeletions: List<ChatItemDeletion>, val byUser: Boolean): CR()
  @Serializable @SerialName("groupChatItemsDeleted") class GroupChatItemsDeleted(val user: UserRef, val groupInfo: GroupInfo, val chatItemIDs: List<Long>, val byUser: Boolean, val member_: GroupMember?): CR()
  @Serializable @SerialName("forwardPlan") class ForwardPlan(val user: UserRef, val itemsCount: Int, val chatItemIds: List<Long>, val forwardConfirmation: ForwardConfirmation? = null): CR()
  // group events
  @Serializable @SerialName("groupCreated") class GroupCreated(val user: UserRef, val groupInfo: GroupInfo): CR()
  @Serializable @SerialName("sentGroupInvitation") class SentGroupInvitation(val user: UserRef, val groupInfo: GroupInfo, val contact: Contact, val member: GroupMember): CR()
  @Serializable @SerialName("userAcceptedGroupSent") class UserAcceptedGroupSent (val user: UserRef, val groupInfo: GroupInfo, val hostContact: Contact? = null): CR()
  @Serializable @SerialName("groupLinkConnecting") class GroupLinkConnecting (val user: UserRef, val groupInfo: GroupInfo, val hostMember: GroupMember): CR()
  @Serializable @SerialName("businessLinkConnecting") class BusinessLinkConnecting (val user: UserRef, val groupInfo: GroupInfo, val hostMember: GroupMember, val fromContact: Contact): CR()
  @Serializable @SerialName("userDeletedMembers") class UserDeletedMembers(val user: UserRef, val groupInfo: GroupInfo, val members: List<GroupMember>, val withMessages: Boolean): CR()
  @Serializable @SerialName("leftMemberUser") class LeftMemberUser(val user: UserRef, val groupInfo: GroupInfo): CR()
  @Serializable @SerialName("groupMembers") class GroupMembers(val user: UserRef, val group: Group): CR()
  @Serializable @SerialName("receivedGroupInvitation") class ReceivedGroupInvitation(val user: UserRef, val groupInfo: GroupInfo, val contact: Contact, val memberRole: GroupMemberRole): CR()
  @Serializable @SerialName("groupDeletedUser") class GroupDeletedUser(val user: UserRef, val groupInfo: GroupInfo): CR()
  @Serializable @SerialName("joinedGroupMemberConnecting") class JoinedGroupMemberConnecting(val user: UserRef, val groupInfo: GroupInfo, val hostMember: GroupMember, val member: GroupMember): CR()
  @Serializable @SerialName("memberRole") class MemberRole(val user: UserRef, val groupInfo: GroupInfo, val byMember: GroupMember, val member: GroupMember, val fromRole: GroupMemberRole, val toRole: GroupMemberRole): CR()
  @Serializable @SerialName("membersRoleUser") class MembersRoleUser(val user: UserRef, val groupInfo: GroupInfo, val members: List<GroupMember>, val toRole: GroupMemberRole): CR()
  @Serializable @SerialName("memberBlockedForAll") class MemberBlockedForAll(val user: UserRef, val groupInfo: GroupInfo, val byMember: GroupMember, val member: GroupMember, val blocked: Boolean): CR()
  @Serializable @SerialName("membersBlockedForAllUser") class MembersBlockedForAllUser(val user: UserRef, val groupInfo: GroupInfo, val members: List<GroupMember>, val blocked: Boolean): CR()
  @Serializable @SerialName("deletedMemberUser") class DeletedMemberUser(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember, val withMessages: Boolean): CR()
  @Serializable @SerialName("deletedMember") class DeletedMember(val user: UserRef, val groupInfo: GroupInfo, val byMember: GroupMember, val deletedMember: GroupMember, val withMessages: Boolean): CR()
  @Serializable @SerialName("leftMember") class LeftMember(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("groupDeleted") class GroupDeleted(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("contactsMerged") class ContactsMerged(val user: UserRef, val intoContact: Contact, val mergedContact: Contact): CR()
  @Serializable @SerialName("userJoinedGroup") class UserJoinedGroup(val user: UserRef, val groupInfo: GroupInfo): CR()
  @Serializable @SerialName("joinedGroupMember") class JoinedGroupMember(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("connectedToGroupMember") class ConnectedToGroupMember(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember, val memberContact: Contact? = null): CR()
  @Serializable @SerialName("groupUpdated") class GroupUpdated(val user: UserRef, val toGroup: GroupInfo): CR()
  @Serializable @SerialName("groupLinkCreated") class GroupLinkCreated(val user: UserRef, val groupInfo: GroupInfo, val connLinkContact: CreatedConnLink, val memberRole: GroupMemberRole): CR()
  @Serializable @SerialName("groupLink") class GroupLink(val user: UserRef, val groupInfo: GroupInfo, val connLinkContact: CreatedConnLink, val memberRole: GroupMemberRole): CR()
  @Serializable @SerialName("groupLinkDeleted") class GroupLinkDeleted(val user: UserRef, val groupInfo: GroupInfo): CR()
  @Serializable @SerialName("newMemberContact") class NewMemberContact(val user: UserRef, val contact: Contact,  val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("newMemberContactSentInv") class NewMemberContactSentInv(val user: UserRef, val contact: Contact,  val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("newMemberContactReceivedInv") class NewMemberContactReceivedInv(val user: UserRef, val contact: Contact,  val groupInfo: GroupInfo, val member: GroupMember): CR()
  // receiving file events
  @Serializable @SerialName("rcvFileAccepted") class RcvFileAccepted(val user: UserRef, val chatItem: AChatItem): CR()
  @Serializable @SerialName("rcvFileAcceptedSndCancelled") class RcvFileAcceptedSndCancelled(val user: UserRef, val rcvFileTransfer: RcvFileTransfer): CR()
  @Serializable @SerialName("standaloneFileInfo") class StandaloneFileInfo(val fileMeta: MigrationFileLinkData?): CR()
  @Serializable @SerialName("rcvStandaloneFileCreated") class RcvStandaloneFileCreated(val user: UserRef, val rcvFileTransfer: RcvFileTransfer): CR()
  @Serializable @SerialName("rcvFileStart") class RcvFileStart(val user: UserRef, val chatItem: AChatItem): CR() // send by chats
  @Serializable @SerialName("rcvFileProgressXFTP") class RcvFileProgressXFTP(val user: UserRef, val chatItem_: AChatItem?, val receivedSize: Long, val totalSize: Long, val rcvFileTransfer: RcvFileTransfer): CR()
  @Serializable @SerialName("rcvFileComplete") class RcvFileComplete(val user: UserRef, val chatItem: AChatItem): CR()
  @Serializable @SerialName("rcvStandaloneFileComplete") class RcvStandaloneFileComplete(val user: UserRef, val targetPath: String, val rcvFileTransfer: RcvFileTransfer): CR()
  @Serializable @SerialName("rcvFileCancelled") class RcvFileCancelled(val user: UserRef, val chatItem_: AChatItem?, val rcvFileTransfer: RcvFileTransfer): CR()
  @Serializable @SerialName("rcvFileSndCancelled") class RcvFileSndCancelled(val user: UserRef, val chatItem: AChatItem, val rcvFileTransfer: RcvFileTransfer): CR()
  @Serializable @SerialName("rcvFileError") class RcvFileError(val user: UserRef, val chatItem_: AChatItem?, val agentError: AgentErrorType, val rcvFileTransfer: RcvFileTransfer): CR()
  @Serializable @SerialName("rcvFileWarning") class RcvFileWarning(val user: UserRef, val chatItem_: AChatItem?, val agentError: AgentErrorType, val rcvFileTransfer: RcvFileTransfer): CR()
  // sending file events
  @Serializable @SerialName("sndFileStart") class SndFileStart(val user: UserRef, val chatItem: AChatItem, val sndFileTransfer: SndFileTransfer): CR()
  @Serializable @SerialName("sndFileComplete") class SndFileComplete(val user: UserRef, val chatItem: AChatItem, val sndFileTransfer: SndFileTransfer): CR()
  @Serializable @SerialName("sndFileRcvCancelled") class SndFileRcvCancelled(val user: UserRef, val chatItem_: AChatItem?, val sndFileTransfer: SndFileTransfer): CR()
  @Serializable @SerialName("sndFileCancelled") class SndFileCancelled(val user: UserRef, val chatItem_: AChatItem?, val fileTransferMeta: FileTransferMeta, val sndFileTransfers: List<SndFileTransfer>): CR()
  @Serializable @SerialName("sndStandaloneFileCreated") class SndStandaloneFileCreated(val user: UserRef, val fileTransferMeta: FileTransferMeta): CR() // returned by _upload
  @Serializable @SerialName("sndFileStartXFTP") class SndFileStartXFTP(val user: UserRef, val chatItem: AChatItem, val fileTransferMeta: FileTransferMeta): CR() // not used
  @Serializable @SerialName("sndFileProgressXFTP") class SndFileProgressXFTP(val user: UserRef, val chatItem_: AChatItem?, val fileTransferMeta: FileTransferMeta, val sentSize: Long, val totalSize: Long): CR()
  @Serializable @SerialName("sndFileRedirectStartXFTP") class SndFileRedirectStartXFTP(val user: UserRef, val fileTransferMeta: FileTransferMeta, val redirectMeta: FileTransferMeta): CR()
  @Serializable @SerialName("sndFileCompleteXFTP") class SndFileCompleteXFTP(val user: UserRef, val chatItem: AChatItem, val fileTransferMeta: FileTransferMeta): CR()
  @Serializable @SerialName("sndStandaloneFileComplete") class SndStandaloneFileComplete(val user: UserRef, val fileTransferMeta: FileTransferMeta, val rcvURIs: List<String>): CR()
  @Serializable @SerialName("sndFileCancelledXFTP") class SndFileCancelledXFTP(val user: UserRef, val chatItem_: AChatItem?, val fileTransferMeta: FileTransferMeta): CR()
  @Serializable @SerialName("sndFileError") class SndFileError(val user: UserRef, val chatItem_: AChatItem?, val fileTransferMeta: FileTransferMeta, val errorMessage: String): CR()
  @Serializable @SerialName("sndFileWarning") class SndFileWarning(val user: UserRef, val chatItem_: AChatItem?, val fileTransferMeta: FileTransferMeta, val errorMessage: String): CR()
  // call events
  @Serializable @SerialName("callInvitation") class CallInvitation(val callInvitation: RcvCallInvitation): CR()
  @Serializable @SerialName("callInvitations") class CallInvitations(val callInvitations: List<RcvCallInvitation>): CR()
  @Serializable @SerialName("callOffer") class CallOffer(val user: UserRef, val contact: Contact, val callType: CallType, val offer: WebRTCSession, val sharedKey: String? = null, val askConfirmation: Boolean): CR()
  @Serializable @SerialName("callAnswer") class CallAnswer(val user: UserRef, val contact: Contact, val answer: WebRTCSession): CR()
  @Serializable @SerialName("callExtraInfo") class CallExtraInfo(val user: UserRef, val contact: Contact, val extraInfo: WebRTCExtraInfo): CR()
  @Serializable @SerialName("callEnded") class CallEnded(val user: UserRef, val contact: Contact): CR()
  @Serializable @SerialName("contactConnectionDeleted") class ContactConnectionDeleted(val user: UserRef, val connection: PendingContactConnection): CR()
  @Serializable @SerialName("contactDisabled") class ContactDisabled(val user: UserRef, val contact: Contact): CR()
  // remote events (desktop)
  @Serializable @SerialName("remoteHostList") class RemoteHostList(val remoteHosts: List<RemoteHostInfo>): CR()
  @Serializable @SerialName("currentRemoteHost") class CurrentRemoteHost(val remoteHost_: RemoteHostInfo?): CR()
  @Serializable @SerialName("remoteHostStarted") class RemoteHostStarted(val remoteHost_: RemoteHostInfo?, val invitation: String, val localAddrs: List<RemoteCtrlAddress>, val ctrlPort: String): CR()
  @Serializable @SerialName("remoteHostSessionCode") class RemoteHostSessionCode(val remoteHost_: RemoteHostInfo?, val sessionCode: String): CR()
  @Serializable @SerialName("newRemoteHost") class NewRemoteHost(val remoteHost: RemoteHostInfo): CR()
  @Serializable @SerialName("remoteHostConnected") class RemoteHostConnected(val remoteHost: RemoteHostInfo): CR()
  @Serializable @SerialName("remoteHostStopped") class RemoteHostStopped(val remoteHostId_: Long?, val rhsState: RemoteHostSessionState, val rhStopReason: RemoteHostStopReason): CR()
  @Serializable @SerialName("remoteFileStored") class RemoteFileStored(val remoteHostId: Long, val remoteFileSource: CryptoFile): CR()
  // remote events (mobile)
  @Serializable @SerialName("remoteCtrlList") class RemoteCtrlList(val remoteCtrls: List<RemoteCtrlInfo>): CR()
  @Serializable @SerialName("remoteCtrlFound") class RemoteCtrlFound(val remoteCtrl: RemoteCtrlInfo, val ctrlAppInfo_: CtrlAppInfo?, val appVersion: String, val compatible: Boolean): CR()
  @Serializable @SerialName("remoteCtrlConnecting") class RemoteCtrlConnecting(val remoteCtrl_: RemoteCtrlInfo?, val ctrlAppInfo: CtrlAppInfo, val appVersion: String): CR()
  @Serializable @SerialName("remoteCtrlSessionCode") class RemoteCtrlSessionCode(val remoteCtrl_: RemoteCtrlInfo?, val sessionCode: String): CR()
  @Serializable @SerialName("remoteCtrlConnected") class RemoteCtrlConnected(val remoteCtrl: RemoteCtrlInfo): CR()
  @Serializable @SerialName("remoteCtrlStopped") class RemoteCtrlStopped(val rcsState: RemoteCtrlSessionState, val rcStopReason: RemoteCtrlStopReason): CR()
  // pq
  @Serializable @SerialName("contactPQAllowed") class ContactPQAllowed(val user: UserRef, val contact: Contact, val pqEncryption: Boolean): CR()
  @Serializable @SerialName("contactPQEnabled") class ContactPQEnabled(val user: UserRef, val contact: Contact, val pqEnabled: Boolean): CR()
  // misc
  @Serializable @SerialName("versionInfo") class VersionInfo(val versionInfo: CoreVersionInfo, val chatMigrations: List<UpMigration>, val agentMigrations: List<UpMigration>): CR()
  @Serializable @SerialName("cmdOk") class CmdOk(val user: UserRef?): CR()
  @Serializable @SerialName("archiveExported") class ArchiveExported(val archiveErrors: List<ArchiveError>): CR()
  @Serializable @SerialName("archiveImported") class ArchiveImported(val archiveErrors: List<ArchiveError>): CR()
  @Serializable @SerialName("appSettings") class AppSettingsR(val appSettings: AppSettings): CR()
  @Serializable @SerialName("agentSubsTotal") class AgentSubsTotal(val user: UserRef, val subsTotal: SMPServerSubs, val hasSession: Boolean): CR()
  @Serializable @SerialName("agentServersSummary") class AgentServersSummary(val user: UserRef, val serversSummary: PresentedServersSummary): CR()
  // general
  @Serializable class Response(val type: String, val json: String): CR()
  @Serializable class Invalid(val str: String): CR()

  val responseType: String get() = when(this) {
    is ActiveUser -> "activeUser"
    is UsersList -> "usersList"
    is ChatStarted -> "chatStarted"
    is ChatRunning -> "chatRunning"
    is ChatStopped -> "chatStopped"
    is ApiChats -> "apiChats"
    is ApiChat -> "apiChat"
    is ChatTags -> "chatTags"
    is ApiChatItemInfo -> "chatItemInfo"
    is ServerTestResult -> "serverTestResult"
    is ServerOperatorConditions -> "serverOperatorConditions"
    is UserServers -> "userServers"
    is UserServersValidation -> "userServersValidation"
    is UsageConditions -> "usageConditions"
    is ChatItemTTL -> "chatItemTTL"
    is NetworkConfig -> "networkConfig"
    is ContactInfo -> "contactInfo"
    is GroupMemberInfo -> "groupMemberInfo"
    is QueueInfoR -> "queueInfo"
    is ContactSwitchStarted -> "contactSwitchStarted"
    is GroupMemberSwitchStarted -> "groupMemberSwitchStarted"
    is ContactSwitchAborted -> "contactSwitchAborted"
    is GroupMemberSwitchAborted -> "groupMemberSwitchAborted"
    is ContactSwitch -> "contactSwitch"
    is GroupMemberSwitch -> "groupMemberSwitch"
    is ContactRatchetSyncStarted -> "contactRatchetSyncStarted"
    is GroupMemberRatchetSyncStarted -> "groupMemberRatchetSyncStarted"
    is ContactRatchetSync -> "contactRatchetSync"
    is GroupMemberRatchetSync -> "groupMemberRatchetSync"
    is ContactCode -> "contactCode"
    is GroupMemberCode -> "groupMemberCode"
    is ConnectionVerified -> "connectionVerified"
    is TagsUpdated -> "tagsUpdated"
    is Invitation -> "invitation"
    is ConnectionIncognitoUpdated -> "connectionIncognitoUpdated"
    is ConnectionUserChanged -> "ConnectionUserChanged"
    is CRConnectionPlan -> "connectionPlan"
    is SentConfirmation -> "sentConfirmation"
    is SentInvitation -> "sentInvitation"
    is SentInvitationToContact -> "sentInvitationToContact"
    is ContactAlreadyExists -> "contactAlreadyExists"
    is ContactDeleted -> "contactDeleted"
    is ContactDeletedByContact -> "contactDeletedByContact"
    is ChatCleared -> "chatCleared"
    is UserProfileNoChange -> "userProfileNoChange"
    is UserProfileUpdated -> "userProfileUpdated"
    is UserPrivacy -> "userPrivacy"
    is ContactAliasUpdated -> "contactAliasUpdated"
    is GroupAliasUpdated -> "groupAliasUpdated"
    is ConnectionAliasUpdated -> "connectionAliasUpdated"
    is ContactPrefsUpdated -> "contactPrefsUpdated"
    is UserContactLink -> "userContactLink"
    is UserContactLinkUpdated -> "userContactLinkUpdated"
    is UserContactLinkCreated -> "userContactLinkCreated"
    is UserContactLinkDeleted -> "userContactLinkDeleted"
    is ContactConnected -> "contactConnected"
    is ContactConnecting -> "contactConnecting"
    is ContactSndReady -> "contactSndReady"
    is ReceivedContactRequest -> "receivedContactRequest"
    is AcceptingContactRequest -> "acceptingContactRequest"
    is ContactRequestRejected -> "contactRequestRejected"
    is ContactUpdated -> "contactUpdated"
    is GroupMemberUpdated -> "groupMemberUpdated"
    is ContactsSubscribed -> "contactsSubscribed"
    is ContactsDisconnected -> "contactsDisconnected"
    is ContactSubSummary -> "contactSubSummary"
    is NetworkStatusResp -> "networkStatus"
    is NetworkStatuses -> "networkStatuses"
    is NewChatItems -> "newChatItems"
    is ChatItemsStatusesUpdated -> "chatItemsStatusesUpdated"
    is ChatItemUpdated -> "chatItemUpdated"
    is ChatItemNotChanged -> "chatItemNotChanged"
    is ChatItemReaction -> "chatItemReaction"
    is ReactionMembers -> "reactionMembers"
    is ChatItemsDeleted -> "chatItemsDeleted"
    is GroupChatItemsDeleted -> "groupChatItemsDeleted"
    is ForwardPlan -> "forwardPlan"
    is GroupCreated -> "groupCreated"
    is SentGroupInvitation -> "sentGroupInvitation"
    is UserAcceptedGroupSent -> "userAcceptedGroupSent"
    is GroupLinkConnecting -> "groupLinkConnecting"
    is BusinessLinkConnecting -> "businessLinkConnecting"
    is UserDeletedMembers -> "userDeletedMembers"
    is LeftMemberUser -> "leftMemberUser"
    is GroupMembers -> "groupMembers"
    is ReceivedGroupInvitation -> "receivedGroupInvitation"
    is GroupDeletedUser -> "groupDeletedUser"
    is JoinedGroupMemberConnecting -> "joinedGroupMemberConnecting"
    is MemberRole -> "memberRole"
    is MembersRoleUser -> "membersRoleUser"
    is MemberBlockedForAll -> "memberBlockedForAll"
    is MembersBlockedForAllUser -> "membersBlockedForAllUser"
    is DeletedMemberUser -> "deletedMemberUser"
    is DeletedMember -> "deletedMember"
    is LeftMember -> "leftMember"
    is GroupDeleted -> "groupDeleted"
    is ContactsMerged -> "contactsMerged"
    is UserJoinedGroup -> "userJoinedGroup"
    is JoinedGroupMember -> "joinedGroupMember"
    is ConnectedToGroupMember -> "connectedToGroupMember"
    is GroupUpdated -> "groupUpdated"
    is GroupLinkCreated -> "groupLinkCreated"
    is GroupLink -> "groupLink"
    is GroupLinkDeleted -> "groupLinkDeleted"
    is NewMemberContact -> "newMemberContact"
    is NewMemberContactSentInv -> "newMemberContactSentInv"
    is NewMemberContactReceivedInv -> "newMemberContactReceivedInv"
    is RcvFileAcceptedSndCancelled -> "rcvFileAcceptedSndCancelled"
    is StandaloneFileInfo -> "standaloneFileInfo"
    is RcvStandaloneFileCreated -> "rcvStandaloneFileCreated"
    is RcvFileAccepted -> "rcvFileAccepted"
    is RcvFileStart -> "rcvFileStart"
    is RcvFileComplete -> "rcvFileComplete"
    is RcvStandaloneFileComplete -> "rcvStandaloneFileComplete"
    is RcvFileCancelled -> "rcvFileCancelled"
    is SndStandaloneFileCreated -> "sndStandaloneFileCreated"
    is SndFileStartXFTP -> "sndFileStartXFTP"
    is RcvFileSndCancelled -> "rcvFileSndCancelled"
    is RcvFileProgressXFTP -> "rcvFileProgressXFTP"
    is SndFileRedirectStartXFTP -> "sndFileRedirectStartXFTP"
    is RcvFileError -> "rcvFileError"
    is RcvFileWarning -> "rcvFileWarning"
    is SndFileStart -> "sndFileStart"
    is SndFileComplete -> "sndFileComplete"
    is SndFileRcvCancelled -> "sndFileRcvCancelled"
    is SndFileCancelled -> "sndFileCancelled"
    is SndFileProgressXFTP -> "sndFileProgressXFTP"
    is SndFileCompleteXFTP -> "sndFileCompleteXFTP"
    is SndStandaloneFileComplete -> "sndStandaloneFileComplete"
    is SndFileCancelledXFTP -> "sndFileCancelledXFTP"
    is SndFileError -> "sndFileError"
    is SndFileWarning -> "sndFileWarning"
    is CallInvitations -> "callInvitations"
    is CallInvitation -> "callInvitation"
    is CallOffer -> "callOffer"
    is CallAnswer -> "callAnswer"
    is CallExtraInfo -> "callExtraInfo"
    is CallEnded -> "callEnded"
    is ContactConnectionDeleted -> "contactConnectionDeleted"
    is ContactDisabled -> "contactDisabled"
    is RemoteHostList -> "remoteHostList"
    is CurrentRemoteHost -> "currentRemoteHost"
    is RemoteHostStarted -> "remoteHostStarted"
    is RemoteHostSessionCode -> "remoteHostSessionCode"
    is NewRemoteHost -> "newRemoteHost"
    is RemoteHostConnected -> "remoteHostConnected"
    is RemoteHostStopped -> "remoteHostStopped"
    is RemoteFileStored -> "remoteFileStored"
    is RemoteCtrlList -> "remoteCtrlList"
    is RemoteCtrlFound -> "remoteCtrlFound"
    is RemoteCtrlConnecting -> "remoteCtrlConnecting"
    is RemoteCtrlSessionCode -> "remoteCtrlSessionCode"
    is RemoteCtrlConnected -> "remoteCtrlConnected"
    is RemoteCtrlStopped -> "remoteCtrlStopped"
    is ContactPQAllowed -> "contactPQAllowed"
    is ContactPQEnabled -> "contactPQEnabled"
    is VersionInfo -> "versionInfo"
    is AgentSubsTotal -> "agentSubsTotal"
    is AgentServersSummary -> "agentServersSummary"
    is CmdOk -> "cmdOk"
    is ArchiveExported -> "archiveExported"
    is ArchiveImported -> "archiveImported"
    is AppSettingsR -> "appSettings"
    is Response -> "* $type"
    is Invalid -> "* invalid json"
  }

  val details: String get() = when(this) {
    is ActiveUser -> withUser(user, json.encodeToString(user))
    is UsersList -> json.encodeToString(users)
    is ChatStarted -> noDetails()
    is ChatRunning -> noDetails()
    is ChatStopped -> noDetails()
    is ApiChats -> withUser(user, json.encodeToString(chats))
    is ApiChat -> withUser(user, "remoteHostId: ${chat.remoteHostId}\nchatInfo: ${chat.chatInfo}\nchatStats: ${chat.chatStats}\nnavInfo: ${navInfo}\nchatItems: ${chat.chatItems}")
    is ChatTags -> withUser(user, "userTags: ${json.encodeToString(userTags)}")
    is ApiChatItemInfo -> withUser(user, "chatItem: ${json.encodeToString(chatItem)}\n${json.encodeToString(chatItemInfo)}")
    is ServerTestResult -> withUser(user, "server: $testServer\nresult: ${json.encodeToString(testFailure)}")
    is ServerOperatorConditions -> "conditions: ${json.encodeToString(conditions)}"
    is UserServers -> withUser(user, "userServers: ${json.encodeToString(userServers)}")
    is UserServersValidation -> withUser(user, "serverErrors: ${json.encodeToString(serverErrors)}")
    is UsageConditions -> "usageConditions: ${json.encodeToString(usageConditions)}\nnacceptedConditions: ${json.encodeToString(acceptedConditions)}"
    is ChatItemTTL -> withUser(user, json.encodeToString(chatItemTTL))
    is NetworkConfig -> json.encodeToString(networkConfig)
    is ContactInfo -> withUser(user, "contact: ${json.encodeToString(contact)}\nconnectionStats: ${json.encodeToString(connectionStats_)}")
    is GroupMemberInfo -> withUser(user, "group: ${json.encodeToString(groupInfo)}\nmember: ${json.encodeToString(member)}\nconnectionStats: ${json.encodeToString(connectionStats_)}")
    is QueueInfoR -> withUser(user, "rcvMsgInfo: ${json.encodeToString(rcvMsgInfo)}\nqueueInfo: ${json.encodeToString(queueInfo)}\n")
    is ContactSwitchStarted -> withUser(user, "contact: ${json.encodeToString(contact)}\nconnectionStats: ${json.encodeToString(connectionStats)}")
    is GroupMemberSwitchStarted -> withUser(user, "group: ${json.encodeToString(groupInfo)}\nmember: ${json.encodeToString(member)}\nconnectionStats: ${json.encodeToString(connectionStats)}")
    is ContactSwitchAborted -> withUser(user, "contact: ${json.encodeToString(contact)}\nconnectionStats: ${json.encodeToString(connectionStats)}")
    is GroupMemberSwitchAborted -> withUser(user, "group: ${json.encodeToString(groupInfo)}\nmember: ${json.encodeToString(member)}\nconnectionStats: ${json.encodeToString(connectionStats)}")
    is ContactSwitch -> withUser(user, "contact: ${json.encodeToString(contact)}\nswitchProgress: ${json.encodeToString(switchProgress)}")
    is GroupMemberSwitch -> withUser(user, "group: ${json.encodeToString(groupInfo)}\nmember: ${json.encodeToString(member)}\nswitchProgress: ${json.encodeToString(switchProgress)}")
    is ContactRatchetSyncStarted -> withUser(user, "contact: ${json.encodeToString(contact)}\nconnectionStats: ${json.encodeToString(connectionStats)}")
    is GroupMemberRatchetSyncStarted -> withUser(user, "group: ${json.encodeToString(groupInfo)}\nmember: ${json.encodeToString(member)}\nconnectionStats: ${json.encodeToString(connectionStats)}")
    is ContactRatchetSync -> withUser(user, "contact: ${json.encodeToString(contact)}\nratchetSyncProgress: ${json.encodeToString(ratchetSyncProgress)}")
    is GroupMemberRatchetSync -> withUser(user, "group: ${json.encodeToString(groupInfo)}\nmember: ${json.encodeToString(member)}\nratchetSyncProgress: ${json.encodeToString(ratchetSyncProgress)}")
    is ContactCode -> withUser(user, "contact: ${json.encodeToString(contact)}\nconnectionCode: $connectionCode")
    is GroupMemberCode -> withUser(user, "groupInfo: ${json.encodeToString(groupInfo)}\nmember: ${json.encodeToString(member)}\nconnectionCode: $connectionCode")
    is ConnectionVerified -> withUser(user, "verified: $verified\nconnectionCode: $expectedCode")
    is TagsUpdated -> withUser(user, "userTags: ${json.encodeToString(userTags)}\nchatTags: ${json.encodeToString(chatTags)}")
    is Invitation -> withUser(user, "connLinkInvitation: ${json.encodeToString(connLinkInvitation)}\nconnection: $connection")
    is ConnectionIncognitoUpdated -> withUser(user, json.encodeToString(toConnection))
    is ConnectionUserChanged -> withUser(user, "fromConnection: ${json.encodeToString(fromConnection)}\ntoConnection: ${json.encodeToString(toConnection)}\nnewUser: ${json.encodeToString(newUser)}" )
    is CRConnectionPlan -> withUser(user, "connLink: ${json.encodeToString(connLink)}\nconnectionPlan: ${json.encodeToString(connectionPlan)}")
    is SentConfirmation -> withUser(user, json.encodeToString(connection))
    is SentInvitation -> withUser(user, json.encodeToString(connection))
    is SentInvitationToContact -> withUser(user, json.encodeToString(contact))
    is ContactAlreadyExists -> withUser(user, json.encodeToString(contact))
    is ContactDeleted -> withUser(user, json.encodeToString(contact))
    is ContactDeletedByContact -> withUser(user, json.encodeToString(contact))
    is ChatCleared -> withUser(user, json.encodeToString(chatInfo))
    is UserProfileNoChange -> withUser(user, noDetails())
    is UserProfileUpdated -> withUser(user, json.encodeToString(toProfile))
    is UserPrivacy -> withUser(user, json.encodeToString(updatedUser))
    is ContactAliasUpdated -> withUser(user, json.encodeToString(toContact))
    is GroupAliasUpdated -> withUser(user, json.encodeToString(toGroup))
    is ConnectionAliasUpdated -> withUser(user, json.encodeToString(toConnection))
    is ContactPrefsUpdated -> withUser(user, "fromContact: $fromContact\ntoContact: \n${json.encodeToString(toContact)}")
    is UserContactLink -> withUser(user, contactLink.responseDetails)
    is UserContactLinkUpdated -> withUser(user, contactLink.responseDetails)
    is UserContactLinkCreated -> withUser(user, json.encodeToString(connLinkContact))
    is UserContactLinkDeleted -> withUser(user, noDetails())
    is ContactConnected -> withUser(user, json.encodeToString(contact))
    is ContactConnecting -> withUser(user, json.encodeToString(contact))
    is ContactSndReady -> withUser(user, json.encodeToString(contact))
    is ReceivedContactRequest -> withUser(user, json.encodeToString(contactRequest))
    is AcceptingContactRequest -> withUser(user, json.encodeToString(contact))
    is ContactRequestRejected -> withUser(user, noDetails())
    is ContactUpdated -> withUser(user, json.encodeToString(toContact))
    is GroupMemberUpdated -> withUser(user, "groupInfo: $groupInfo\nfromMember: $fromMember\ntoMember: $toMember")
    is ContactsSubscribed -> "server: $server\ncontacts:\n${json.encodeToString(contactRefs)}"
    is ContactsDisconnected -> "server: $server\ncontacts:\n${json.encodeToString(contactRefs)}"
    is ContactSubSummary -> withUser(user, json.encodeToString(contactSubscriptions))
    is NetworkStatusResp -> "networkStatus $networkStatus\nconnections: $connections"
    is NetworkStatuses -> withUser(user_, json.encodeToString(networkStatuses))
    is NewChatItems -> withUser(user, chatItems.joinToString("\n") { json.encodeToString(it) })
    is ChatItemsStatusesUpdated -> withUser(user, chatItems.joinToString("\n") { json.encodeToString(it) })
    is ChatItemUpdated -> withUser(user, json.encodeToString(chatItem))
    is ChatItemNotChanged -> withUser(user, json.encodeToString(chatItem))
    is ChatItemReaction -> withUser(user, "added: $added\n${json.encodeToString(reaction)}")
    is ReactionMembers -> withUser(user, "memberReactions: ${json.encodeToString(memberReactions)}")
    is ChatItemsDeleted -> withUser(user, "${chatItemDeletions.map { (deletedChatItem, toChatItem) -> "deletedChatItem: ${json.encodeToString(deletedChatItem)}\ntoChatItem: ${json.encodeToString(toChatItem)}" }} \nbyUser: $byUser")
    is GroupChatItemsDeleted -> withUser(user, "chatItemIDs: $chatItemIDs\nbyUser: $byUser\nmember_: $member_")
    is ForwardPlan -> withUser(user, "itemsCount: $itemsCount\nchatItemIds: ${json.encodeToString(chatItemIds)}\nforwardConfirmation: ${json.encodeToString(forwardConfirmation)}")
    is GroupCreated -> withUser(user, json.encodeToString(groupInfo))
    is SentGroupInvitation -> withUser(user, "groupInfo: $groupInfo\ncontact: $contact\nmember: $member")
    is UserAcceptedGroupSent -> json.encodeToString(groupInfo)
    is GroupLinkConnecting -> withUser(user, "groupInfo: $groupInfo\nhostMember: $hostMember")
    is BusinessLinkConnecting -> withUser(user, "groupInfo: $groupInfo\nhostMember: $hostMember\nfromContact: $fromContact")
    is UserDeletedMembers -> withUser(user, "groupInfo: $groupInfo\nmembers: $members\nwithMessages: $withMessages")
    is LeftMemberUser -> withUser(user, json.encodeToString(groupInfo))
    is GroupMembers -> withUser(user, json.encodeToString(group))
    is ReceivedGroupInvitation -> withUser(user, "groupInfo: $groupInfo\ncontact: $contact\nmemberRole: $memberRole")
    is GroupDeletedUser -> withUser(user, json.encodeToString(groupInfo))
    is JoinedGroupMemberConnecting -> withUser(user, "groupInfo: $groupInfo\nhostMember: $hostMember\nmember: $member")
    is MemberRole -> withUser(user, "groupInfo: $groupInfo\nbyMember: $byMember\nmember: $member\nfromRole: $fromRole\ntoRole: $toRole")
    is MembersRoleUser -> withUser(user, "groupInfo: $groupInfo\nmembers: $members\ntoRole: $toRole")
    is MemberBlockedForAll -> withUser(user, "groupInfo: $groupInfo\nbyMember: $byMember\nmember: $member\nblocked: $blocked")
    is MembersBlockedForAllUser -> withUser(user, "groupInfo: $groupInfo\nmembers: $members\nblocked: $blocked")
    is DeletedMemberUser -> withUser(user, "groupInfo: $groupInfo\nmember: $member\nwithMessages: ${withMessages}")
    is DeletedMember -> withUser(user, "groupInfo: $groupInfo\nbyMember: $byMember\ndeletedMember: $deletedMember\nwithMessages: ${withMessages}")
    is LeftMember -> withUser(user, "groupInfo: $groupInfo\nmember: $member")
    is GroupDeleted -> withUser(user, "groupInfo: $groupInfo\nmember: $member")
    is ContactsMerged -> withUser(user, "intoContact: $intoContact\nmergedContact: $mergedContact")
    is UserJoinedGroup -> withUser(user, json.encodeToString(groupInfo))
    is JoinedGroupMember -> withUser(user, "groupInfo: $groupInfo\nmember: $member")
    is ConnectedToGroupMember -> withUser(user, "groupInfo: $groupInfo\nmember: $member\nmemberContact: $memberContact")
    is GroupUpdated -> withUser(user, json.encodeToString(toGroup))
    is GroupLinkCreated -> withUser(user, "groupInfo: $groupInfo\nconnLinkContact: $connLinkContact\nmemberRole: $memberRole")
    is GroupLink -> withUser(user, "groupInfo: $groupInfo\nconnLinkContact: $connLinkContact\nmemberRole: $memberRole")
    is GroupLinkDeleted -> withUser(user, json.encodeToString(groupInfo))
    is NewMemberContact -> withUser(user, "contact: $contact\ngroupInfo: $groupInfo\nmember: $member")
    is NewMemberContactSentInv -> withUser(user, "contact: $contact\ngroupInfo: $groupInfo\nmember: $member")
    is NewMemberContactReceivedInv -> withUser(user, "contact: $contact\ngroupInfo: $groupInfo\nmember: $member")
    is RcvFileAcceptedSndCancelled -> withUser(user, noDetails())
    is StandaloneFileInfo -> json.encodeToString(fileMeta)
    is RcvStandaloneFileCreated -> noDetails()
    is RcvFileAccepted -> withUser(user, json.encodeToString(chatItem))
    is RcvFileStart -> withUser(user, json.encodeToString(chatItem))
    is RcvFileComplete -> withUser(user, json.encodeToString(chatItem))
    is RcvFileCancelled -> withUser(user, json.encodeToString(chatItem_))
    is RcvFileSndCancelled -> withUser(user, json.encodeToString(chatItem))
    is RcvFileProgressXFTP -> withUser(user, "chatItem: ${json.encodeToString(chatItem_)}\nreceivedSize: $receivedSize\ntotalSize: $totalSize")
    is RcvStandaloneFileComplete -> withUser(user, targetPath)
    is RcvFileError -> withUser(user, "chatItem_: ${json.encodeToString(chatItem_)}\nagentError: ${agentError.string}\nrcvFileTransfer: $rcvFileTransfer")
    is RcvFileWarning -> withUser(user, "chatItem_: ${json.encodeToString(chatItem_)}\nagentError: ${agentError.string}\nrcvFileTransfer: $rcvFileTransfer")
    is SndFileCancelled -> json.encodeToString(chatItem_)
    is SndStandaloneFileCreated -> noDetails()
    is SndFileStartXFTP -> withUser(user, json.encodeToString(chatItem))
    is SndFileComplete -> withUser(user, json.encodeToString(chatItem))
    is SndFileRcvCancelled -> withUser(user, json.encodeToString(chatItem_))
    is SndFileStart -> withUser(user, json.encodeToString(chatItem))
    is SndFileProgressXFTP -> withUser(user, "chatItem: ${json.encodeToString(chatItem_)}\nsentSize: $sentSize\ntotalSize: $totalSize")
    is SndFileRedirectStartXFTP -> withUser(user, json.encodeToString(redirectMeta))
    is SndFileCompleteXFTP -> withUser(user, json.encodeToString(chatItem))
    is SndStandaloneFileComplete -> withUser(user, rcvURIs.size.toString())
    is SndFileCancelledXFTP -> withUser(user, json.encodeToString(chatItem_))
    is SndFileError -> withUser(user, "errorMessage: ${json.encodeToString(errorMessage)}\nchatItem: ${json.encodeToString(chatItem_)}")
    is SndFileWarning -> withUser(user, "errorMessage: ${json.encodeToString(errorMessage)}\nchatItem: ${json.encodeToString(chatItem_)}")
    is CallInvitations -> "callInvitations: ${json.encodeToString(callInvitations)}"
    is CallInvitation -> "contact: ${callInvitation.contact.id}\ncallType: $callInvitation.callType\nsharedKey: ${callInvitation.sharedKey ?: ""}"
    is CallOffer -> withUser(user, "contact: ${contact.id}\ncallType: $callType\nsharedKey: ${sharedKey ?: ""}\naskConfirmation: $askConfirmation\noffer: ${json.encodeToString(offer)}")
    is CallAnswer -> withUser(user, "contact: ${contact.id}\nanswer: ${json.encodeToString(answer)}")
    is CallExtraInfo -> withUser(user, "contact: ${contact.id}\nextraInfo: ${json.encodeToString(extraInfo)}")
    is CallEnded -> withUser(user, "contact: ${contact.id}")
    is ContactConnectionDeleted -> withUser(user, json.encodeToString(connection))
    is ContactDisabled -> withUser(user, json.encodeToString(contact))
    // remote events (mobile)
    is RemoteHostList -> json.encodeToString(remoteHosts)
    is CurrentRemoteHost -> if (remoteHost_ == null) "local" else json.encodeToString(remoteHost_)
    is RemoteHostStarted -> if (remoteHost_ == null) "new" else json.encodeToString(remoteHost_)
    is RemoteHostSessionCode ->
      "remote host: " +
          (if (remoteHost_ == null) "new" else json.encodeToString(remoteHost_)) +
          "\nsession code: $sessionCode"
    is NewRemoteHost -> json.encodeToString(remoteHost)
    is RemoteHostConnected -> json.encodeToString(remoteHost)
    is RemoteHostStopped -> "remote host ID: $remoteHostId_"
    is RemoteFileStored -> "remote host ID: $remoteHostId\nremoteFileSource:\n" + json.encodeToString(remoteFileSource)
    is RemoteCtrlList -> json.encodeToString(remoteCtrls)
    is RemoteCtrlFound -> "remote ctrl: " + json.encodeToString(remoteCtrl) +
        "\nctrlAppInfo: " +
        (if (ctrlAppInfo_ == null) "null" else json.encodeToString(ctrlAppInfo_)) +
        "\nappVersion: $appVersion" +
        "\ncompatible: $compatible"
    is RemoteCtrlConnecting ->
      "remote ctrl: " +
          (if (remoteCtrl_ == null) "null" else json.encodeToString(remoteCtrl_)) +
          "\nctrlAppInfo:\n${json.encodeToString(ctrlAppInfo)}" +
          "\nappVersion: $appVersion"
    is RemoteCtrlSessionCode ->
      "remote ctrl: " +
          (if (remoteCtrl_ == null) "null" else json.encodeToString(remoteCtrl_)) +
          "\nsessionCode: $sessionCode"
    is RemoteCtrlConnected -> json.encodeToString(remoteCtrl)
    is RemoteCtrlStopped -> "rcsState: $rcsState\nrcsStopReason: $rcStopReason"
    is ContactPQAllowed -> withUser(user, "contact: ${contact.id}\npqEncryption: $pqEncryption")
    is ContactPQEnabled -> withUser(user, "contact: ${contact.id}\npqEnabled: $pqEnabled")
    is AgentSubsTotal -> withUser(user, "subsTotal: ${subsTotal}\nhasSession: $hasSession")
    is AgentServersSummary -> withUser(user, json.encodeToString(serversSummary))
    is VersionInfo -> "version ${json.encodeToString(versionInfo)}\n\n" +
        "chat migrations: ${json.encodeToString(chatMigrations.map { it.upName })}\n\n" +
        "agent migrations: ${json.encodeToString(agentMigrations.map { it.upName })}"
    is CmdOk -> withUser(user, noDetails())
    is ArchiveExported -> "${archiveErrors.map { it.string } }"
    is ArchiveImported -> "${archiveErrors.map { it.string } }"
    is AppSettingsR -> json.encodeToString(appSettings)
    is Response -> json
    is Invalid -> str
  }

  fun noDetails(): String ="${responseType}: " + generalGetString(MR.strings.no_details)

  private fun withUser(u: UserLike?, s: String): String = if (u != null) "userId: ${u.userId}\n$s" else s
}

fun apiChatErrorType(r: API): ChatErrorType? =
  if (r is API.Error && r.err is ChatError.ChatErrorChat) r.err.errorType
  else null

@Serializable
sealed class ChatDeleteMode {
  @Serializable @SerialName("full") class Full(val notify: Boolean): ChatDeleteMode()
  @Serializable @SerialName("entity") class Entity(val notify: Boolean): ChatDeleteMode()
  @Serializable @SerialName("messages") class Messages: ChatDeleteMode()

  val cmdString: String get() = when (this) {
    is ChatDeleteMode.Full -> "full notify=${onOff(notify)}"
    is ChatDeleteMode.Entity -> "entity notify=${onOff(notify)}"
    is ChatDeleteMode.Messages -> "messages"
  }
}

@Serializable
data class CreatedConnLink(val connFullLink: String, val connShortLink: String?) {
  fun simplexChatUri(short: Boolean): String =
    if (short) connShortLink ?: simplexChatLink(connFullLink)
    else simplexChatLink(connFullLink)

  companion object {
    val nullableStateSaver: Saver<CreatedConnLink?, Pair<String?, String?>> = Saver(
      save = { link -> link?.connFullLink to link?.connShortLink },
      restore = { saved ->
        val connFullLink = saved.first
        if (connFullLink == null) null
        else CreatedConnLink(connFullLink = connFullLink,  connShortLink = saved.second)
      }
    )
  }
}

fun simplexChatLink(uri: String): String =
  if (uri.startsWith("simplex:/")) uri.replace("simplex:/", "https://simplex.chat/")
  else uri

@Serializable
sealed class ConnectionPlan {
  @Serializable @SerialName("invitationLink") class InvitationLink(val invitationLinkPlan: InvitationLinkPlan): ConnectionPlan()
  @Serializable @SerialName("contactAddress") class ContactAddress(val contactAddressPlan: ContactAddressPlan): ConnectionPlan()
  @Serializable @SerialName("groupLink") class GroupLink(val groupLinkPlan: GroupLinkPlan): ConnectionPlan()
  @Serializable @SerialName("error") class Error(val chatError: ChatError): ConnectionPlan()
}

@Serializable
sealed class InvitationLinkPlan {
  @Serializable @SerialName("ok") object Ok: InvitationLinkPlan()
  @Serializable @SerialName("ownLink") object OwnLink: InvitationLinkPlan()
  @Serializable @SerialName("connecting") class Connecting(val contact_: Contact? = null): InvitationLinkPlan()
  @Serializable @SerialName("known") class Known(val contact: Contact): InvitationLinkPlan()
}

@Serializable
sealed class ContactAddressPlan {
  @Serializable @SerialName("ok") object Ok: ContactAddressPlan()
  @Serializable @SerialName("ownLink") object OwnLink: ContactAddressPlan()
  @Serializable @SerialName("connectingConfirmReconnect") object ConnectingConfirmReconnect: ContactAddressPlan()
  @Serializable @SerialName("connectingProhibit") class ConnectingProhibit(val contact: Contact): ContactAddressPlan()
  @Serializable @SerialName("known") class Known(val contact: Contact): ContactAddressPlan()
  @Serializable @SerialName("contactViaAddress") class ContactViaAddress(val contact: Contact): ContactAddressPlan()
}

@Serializable
sealed class GroupLinkPlan {
  @Serializable @SerialName("ok") object Ok: GroupLinkPlan()
  @Serializable @SerialName("ownLink") class OwnLink(val groupInfo: GroupInfo): GroupLinkPlan()
  @Serializable @SerialName("connectingConfirmReconnect") object ConnectingConfirmReconnect: GroupLinkPlan()
  @Serializable @SerialName("connectingProhibit") class ConnectingProhibit(val groupInfo_: GroupInfo? = null): GroupLinkPlan()
  @Serializable @SerialName("known") class Known(val groupInfo: GroupInfo): GroupLinkPlan()
}

abstract class TerminalItem {
  abstract val id: Long
  abstract val remoteHostId: Long?
  val date: Instant = Clock.System.now()
  abstract val label: String
  abstract val details: String
  val createdAtNanos: Long = System.nanoTime()

  class Cmd(override val id: Long, override val remoteHostId: Long?, val cmd: CC): TerminalItem() {
    override val label get() = "> ${cmd.cmdString}"
    override val details get() = cmd.cmdString
  }

  class Resp(override val id: Long, override val remoteHostId: Long?, val resp: API): TerminalItem() {
    override val label get() = "< ${resp.responseType}"
    override val details get() = resp.details
  }

  companion object {
    val sampleData = listOf(
        Cmd(0, null, CC.ShowActiveUser()),
        Resp(1, null, API.Result(null, CR.ActiveUser(User.sampleData)))
    )

    fun cmd(rhId: Long?, c: CC) = Cmd(System.currentTimeMillis(), rhId, c)
    fun resp(rhId: Long?, r: API) = Resp(System.currentTimeMillis(), rhId, r)
  }
}

@Serializable
class ConnectionStats(
  val connAgentVersion: Int,
  val rcvQueuesInfo: List<RcvQueueInfo>,
  val sndQueuesInfo: List<SndQueueInfo>,
  val ratchetSyncState: RatchetSyncState,
  val ratchetSyncSupported: Boolean
) {
  val ratchetSyncAllowed: Boolean get() =
    ratchetSyncSupported && listOf(RatchetSyncState.Allowed, RatchetSyncState.Required).contains(ratchetSyncState)

  val ratchetSyncSendProhibited: Boolean get() =
    listOf(RatchetSyncState.Required, RatchetSyncState.Started, RatchetSyncState.Agreed).contains(ratchetSyncState)

  val ratchetSyncInProgress: Boolean get() =
    listOf(RatchetSyncState.Started, RatchetSyncState.Agreed).contains(ratchetSyncState)
}

@Serializable
class RcvQueueInfo(
  val rcvServer: String,
  val rcvSwitchStatus: RcvSwitchStatus?,
  var canAbortSwitch: Boolean
)

@Serializable
enum class RcvSwitchStatus {
  @SerialName("switch_started") SwitchStarted,
  @SerialName("sending_qadd") SendingQADD,
  @SerialName("sending_quse") SendingQUSE,
  @SerialName("received_message") ReceivedMessage
}

@Serializable
class SndQueueInfo(
  val sndServer: String,
  val sndSwitchStatus: SndSwitchStatus?
)

@Serializable
enum class SndSwitchStatus {
  @SerialName("sending_qkey") SendingQKEY,
  @SerialName("sending_qtest") SendingQTEST
}

@Serializable
enum class QueueDirection {
  @SerialName("rcv") Rcv,
  @SerialName("snd") Snd
}

@Serializable
class SwitchProgress(
  val queueDirection: QueueDirection,
  val switchPhase: SwitchPhase,
  val connectionStats: ConnectionStats
)

@Serializable
class RatchetSyncProgress(
  val ratchetSyncStatus: RatchetSyncState,
  val connectionStats: ConnectionStats
)

@Serializable
enum class RatchetSyncState {
  @SerialName("ok") Ok,
  @SerialName("allowed") Allowed,
  @SerialName("required") Required,
  @SerialName("started") Started,
  @SerialName("agreed") Agreed
}

@Serializable
class UserContactLinkRec(val connLinkContact: CreatedConnLink, val autoAccept: AutoAccept? = null) {
  val responseDetails: String get() = "connLinkContact: ${connLinkContact}\nautoAccept: ${AutoAccept.cmdString(autoAccept)}"
}

@Serializable
class AutoAccept(val businessAddress: Boolean, val acceptIncognito: Boolean, val autoReply: MsgContent?) {
  companion object {
    fun cmdString(autoAccept: AutoAccept?): String {
      if (autoAccept == null) return "off"
      var s = "on"
      if (autoAccept.acceptIncognito) {
        s += " incognito=on"
      } else if (autoAccept.businessAddress) {
        s += " business"
      }
      val msg = autoAccept.autoReply ?: return s
      return s + " " + msg.cmdString
    }
  }
}

@Serializable
data class CoreVersionInfo(
  val version: String,
  val simplexmqVersion: String,
  val simplexmqCommit: String
)

data class SomeRemoteCtrl(
  val remoteCtrl_: RemoteCtrlInfo?,
  val ctrlAppInfo: CtrlAppInfo,
  val appVersion: String
)

@Serializable
data class CtrlAppInfo(val appVersionRange: AppVersionRange, val deviceName: String)

@Serializable
data class AppVersionRange(val minVersion: String, val maxVersion: String)

@Serializable
data class RemoteFile(
  val userId: Long,
  val fileId: Long,
  val sent: Boolean,
  val fileSource: CryptoFile
)

@Serializable
sealed class ChatError {
  val string: String get() = when (this) {
    is ChatErrorChat -> "chat ${errorType.string}"
    is ChatErrorAgent -> "agent ${agentError.string}"
    is ChatErrorStore -> "store ${storeError.string}"
    is ChatErrorDatabase -> "database ${databaseError.string}"
    is ChatErrorRemoteHost -> "remoteHost ${remoteHostError.string}"
    is ChatErrorRemoteCtrl -> "remoteCtrl ${remoteCtrlError.string}"
    is ChatErrorInvalidJSON -> "invalid json ${json}"
  }
  @Serializable @SerialName("error") class ChatErrorChat(val errorType: ChatErrorType): ChatError()
  @Serializable @SerialName("errorAgent") class ChatErrorAgent(val agentError: AgentErrorType): ChatError()
  @Serializable @SerialName("errorStore") class ChatErrorStore(val storeError: StoreError): ChatError()
  @Serializable @SerialName("errorDatabase") class ChatErrorDatabase(val databaseError: DatabaseError): ChatError()
  @Serializable @SerialName("errorRemoteHost") class ChatErrorRemoteHost(val remoteHostError: RemoteHostError): ChatError()
  @Serializable @SerialName("errorRemoteCtrl") class ChatErrorRemoteCtrl(val remoteCtrlError: RemoteCtrlError): ChatError()
  @Serializable @SerialName("invalidJSON") class ChatErrorInvalidJSON(val json: String): ChatError()

  val resultType: String get() = when (this) {
    is ChatErrorChat -> "chat"
    is ChatErrorAgent -> "agent"
    is ChatErrorStore -> "store"
    is ChatErrorDatabase -> "database"
    is ChatErrorRemoteHost -> "remoteHost"
    is ChatErrorRemoteCtrl -> "remoteCtrl"
    is ChatErrorInvalidJSON -> "invalid json"
  }
}

@Serializable
sealed class ChatErrorType {
  val string: String
    get() = when (this) {
      is NoActiveUser -> "noActiveUser"
      is NoConnectionUser -> "noConnectionUser"
      is NoSndFileUser -> "noSndFileUser"
      is NoRcvFileUser -> "noRcvFileUser"
      is UserUnknown -> "userUnknown"
      is ActiveUserExists -> "activeUserExists"
      is UserExists -> "userExists"
      is DifferentActiveUser -> "differentActiveUser"
      is CantDeleteActiveUser -> "cantDeleteActiveUser"
      is CantDeleteLastUser -> "cantDeleteLastUser"
      is CantHideLastUser -> "cantHideLastUser"
      is HiddenUserAlwaysMuted -> "hiddenUserAlwaysMuted"
      is EmptyUserPassword -> "emptyUserPassword"
      is UserAlreadyHidden -> "userAlreadyHidden"
      is UserNotHidden -> "userNotHidden"
      is InvalidDisplayName -> "invalidDisplayName"
      is ChatNotStarted -> "chatNotStarted"
      is ChatNotStopped -> "chatNotStopped"
      is ChatStoreChanged -> "chatStoreChanged"
      is ConnectionPlanChatError -> "connectionPlan"
      is InvalidConnReq -> "invalidConnReq"
      is UnsupportedConnReq -> "unsupportedConnReq"
      is InvalidChatMessage -> "invalidChatMessage"
      is ContactNotReady -> "contactNotReady"
      is ContactNotActive -> "contactNotActive"
      is ContactDisabled -> "contactDisabled"
      is ConnectionDisabled -> "connectionDisabled"
      is GroupUserRole -> "groupUserRole"
      is GroupMemberInitialRole -> "groupMemberInitialRole"
      is ContactIncognitoCantInvite -> "contactIncognitoCantInvite"
      is GroupIncognitoCantInvite -> "groupIncognitoCantInvite"
      is GroupContactRole -> "groupContactRole"
      is GroupDuplicateMember -> "groupDuplicateMember"
      is GroupDuplicateMemberId -> "groupDuplicateMemberId"
      is GroupNotJoined -> "groupNotJoined"
      is GroupMemberNotActive -> "groupMemberNotActive"
      is GroupMemberUserRemoved -> "groupMemberUserRemoved"
      is GroupMemberNotFound -> "groupMemberNotFound"
      is GroupCantResendInvitation -> "groupCantResendInvitation"
      is GroupInternal -> "groupInternal"
      is FileNotFound -> "fileNotFound"
      is FileSize -> "fileSize"
      is FileAlreadyReceiving -> "fileAlreadyReceiving"
      is FileCancelled -> "fileCancelled"
      is FileCancel -> "fileCancel"
      is FileAlreadyExists -> "fileAlreadyExists"
      is FileRead -> "fileRead"
      is FileWrite -> "fileWrite $message"
      is FileSend -> "fileSend"
      is FileRcvChunk -> "fileRcvChunk"
      is FileInternal -> "fileInternal"
      is FileImageType -> "fileImageType"
      is FileImageSize -> "fileImageSize"
      is FileNotReceived -> "fileNotReceived"
      is FileNotApproved -> "fileNotApproved"
      is FallbackToSMPProhibited -> "fallbackToSMPProhibited"
      is InlineFileProhibited -> "inlineFileProhibited"
      is InvalidQuote -> "invalidQuote"
      is InvalidForward -> "invalidForward"
      is InvalidChatItemUpdate -> "invalidChatItemUpdate"
      is InvalidChatItemDelete -> "invalidChatItemDelete"
      is HasCurrentCall -> "hasCurrentCall"
      is NoCurrentCall -> "noCurrentCall"
      is CallContact -> "callContact"
      is CallState -> "callState"
      is DirectMessagesProhibited -> "directMessagesProhibited"
      is AgentVersion -> "agentVersion"
      is AgentNoSubResult -> "agentNoSubResult"
      is CommandError -> "commandError $message"
      is ServerProtocol -> "serverProtocol"
      is AgentCommandError -> "agentCommandError"
      is InvalidFileDescription -> "invalidFileDescription"
      is ConnectionIncognitoChangeProhibited -> "connectionIncognitoChangeProhibited"
      is ConnectionUserChangeProhibited -> "connectionUserChangeProhibited"
      is PeerChatVRangeIncompatible -> "peerChatVRangeIncompatible"
      is InternalError -> "internalError"
      is CEException -> "exception $message"
    }

  @Serializable @SerialName("noActiveUser") object NoActiveUser: ChatErrorType()
  @Serializable @SerialName("noConnectionUser") class NoConnectionUser(val agentConnId: String): ChatErrorType()
  @Serializable @SerialName("noSndFileUser") class NoSndFileUser(val agentSndFileId: String): ChatErrorType()
  @Serializable @SerialName("noRcvFileUser") class NoRcvFileUser(val agentRcvFileId: String): ChatErrorType()
  @Serializable @SerialName("userUnknown") object UserUnknown: ChatErrorType()
  @Serializable @SerialName("activeUserExists") object ActiveUserExists: ChatErrorType()
  @Serializable @SerialName("userExists") class UserExists(val contactName: String): ChatErrorType()
  @Serializable @SerialName("differentActiveUser") class DifferentActiveUser(val commandUserId: Long, val activeUserId: Long): ChatErrorType()
  @Serializable @SerialName("cantDeleteActiveUser") class CantDeleteActiveUser(val userId: Long): ChatErrorType()
  @Serializable @SerialName("cantDeleteLastUser") class CantDeleteLastUser(val userId: Long): ChatErrorType()
  @Serializable @SerialName("cantHideLastUser") class CantHideLastUser(val userId: Long): ChatErrorType()
  @Serializable @SerialName("hiddenUserAlwaysMuted") class HiddenUserAlwaysMuted(val userId: Long): ChatErrorType()
  @Serializable @SerialName("emptyUserPassword") class EmptyUserPassword(val userId: Long): ChatErrorType()
  @Serializable @SerialName("userAlreadyHidden") class UserAlreadyHidden(val userId: Long): ChatErrorType()
  @Serializable @SerialName("userNotHidden") class UserNotHidden(val userId: Long): ChatErrorType()
  @Serializable @SerialName("invalidDisplayName") object InvalidDisplayName: ChatErrorType()
  @Serializable @SerialName("chatNotStarted") object ChatNotStarted: ChatErrorType()
  @Serializable @SerialName("chatNotStopped") object ChatNotStopped: ChatErrorType()
  @Serializable @SerialName("chatStoreChanged") object ChatStoreChanged: ChatErrorType()
  @Serializable @SerialName("connectionPlan") class ConnectionPlanChatError(val connectionPlan: ConnectionPlan): ChatErrorType()
  @Serializable @SerialName("invalidConnReq") object InvalidConnReq: ChatErrorType()
  @Serializable @SerialName("unsupportedConnReq") object UnsupportedConnReq: ChatErrorType()
  @Serializable @SerialName("invalidChatMessage") class InvalidChatMessage(val connection: Connection, val message: String): ChatErrorType()
  @Serializable @SerialName("contactNotReady") class ContactNotReady(val contact: Contact): ChatErrorType()
  @Serializable @SerialName("contactNotActive") class ContactNotActive(val contact: Contact): ChatErrorType()
  @Serializable @SerialName("contactDisabled") class ContactDisabled(val contact: Contact): ChatErrorType()
  @Serializable @SerialName("connectionDisabled") class ConnectionDisabled(val connection: Connection): ChatErrorType()
  @Serializable @SerialName("groupUserRole") class GroupUserRole(val groupInfo: GroupInfo, val requiredRole: GroupMemberRole): ChatErrorType()
  @Serializable @SerialName("groupMemberInitialRole") class GroupMemberInitialRole(val groupInfo: GroupInfo, val initialRole: GroupMemberRole): ChatErrorType()
  @Serializable @SerialName("contactIncognitoCantInvite") object ContactIncognitoCantInvite: ChatErrorType()
  @Serializable @SerialName("groupIncognitoCantInvite") object GroupIncognitoCantInvite: ChatErrorType()
  @Serializable @SerialName("groupContactRole") class GroupContactRole(val contactName: String): ChatErrorType()
  @Serializable @SerialName("groupDuplicateMember") class GroupDuplicateMember(val contactName: String): ChatErrorType()
  @Serializable @SerialName("groupDuplicateMemberId") object GroupDuplicateMemberId: ChatErrorType()
  @Serializable @SerialName("groupNotJoined") class GroupNotJoined(val groupInfo: GroupInfo): ChatErrorType()
  @Serializable @SerialName("groupMemberNotActive") object GroupMemberNotActive: ChatErrorType()
  @Serializable @SerialName("groupMemberUserRemoved") object GroupMemberUserRemoved: ChatErrorType()
  @Serializable @SerialName("groupMemberNotFound") object GroupMemberNotFound: ChatErrorType()
  @Serializable @SerialName("groupCantResendInvitation") class GroupCantResendInvitation(val groupInfo: GroupInfo, val contactName: String): ChatErrorType()
  @Serializable @SerialName("groupInternal") class GroupInternal(val message: String): ChatErrorType()
  @Serializable @SerialName("fileNotFound") class FileNotFound(val message: String): ChatErrorType()
  @Serializable @SerialName("fileSize") class FileSize(val filePath: String): ChatErrorType()
  @Serializable @SerialName("fileAlreadyReceiving") class FileAlreadyReceiving(val message: String): ChatErrorType()
  @Serializable @SerialName("fileCancelled") class FileCancelled(val message: String): ChatErrorType()
  @Serializable @SerialName("fileCancel") class FileCancel(val fileId: Long, val message: String): ChatErrorType()
  @Serializable @SerialName("fileAlreadyExists") class FileAlreadyExists(val filePath: String): ChatErrorType()
  @Serializable @SerialName("fileRead") class FileRead(val filePath: String, val message: String): ChatErrorType()
  @Serializable @SerialName("fileWrite") class FileWrite(val filePath: String, val message: String): ChatErrorType()
  @Serializable @SerialName("fileSend") class FileSend(val fileId: Long, val agentError: String): ChatErrorType()
  @Serializable @SerialName("fileRcvChunk") class FileRcvChunk(val message: String): ChatErrorType()
  @Serializable @SerialName("fileInternal") class FileInternal(val message: String): ChatErrorType()
  @Serializable @SerialName("fileImageType") class FileImageType(val filePath: String): ChatErrorType()
  @Serializable @SerialName("fileImageSize") class FileImageSize(val filePath: String): ChatErrorType()
  @Serializable @SerialName("fileNotReceived") class FileNotReceived(val fileId: Long): ChatErrorType()
  @Serializable @SerialName("fileNotApproved") class FileNotApproved(val fileId: Long, val unknownServers: List<String>): ChatErrorType()
  @Serializable @SerialName("fallbackToSMPProhibited") class FallbackToSMPProhibited(val fileId: Long): ChatErrorType()
  @Serializable @SerialName("inlineFileProhibited") class InlineFileProhibited(val fileId: Long): ChatErrorType()
  @Serializable @SerialName("invalidQuote") object InvalidQuote: ChatErrorType()
  @Serializable @SerialName("invalidForward") object InvalidForward: ChatErrorType()
  @Serializable @SerialName("invalidChatItemUpdate") object InvalidChatItemUpdate: ChatErrorType()
  @Serializable @SerialName("invalidChatItemDelete") object InvalidChatItemDelete: ChatErrorType()
  @Serializable @SerialName("hasCurrentCall") object HasCurrentCall: ChatErrorType()
  @Serializable @SerialName("noCurrentCall") object NoCurrentCall: ChatErrorType()
  @Serializable @SerialName("callContact") class CallContact(val contactId: Long): ChatErrorType()
  @Serializable @SerialName("callState") object CallState: ChatErrorType()
  @Serializable @SerialName("directMessagesProhibited") class DirectMessagesProhibited(val contact: Contact): ChatErrorType()
  @Serializable @SerialName("agentVersion") object AgentVersion: ChatErrorType()
  @Serializable @SerialName("agentNoSubResult") class AgentNoSubResult(val agentConnId: String): ChatErrorType()
  @Serializable @SerialName("commandError") class CommandError(val message: String): ChatErrorType()
  @Serializable @SerialName("serverProtocol") object ServerProtocol: ChatErrorType()
  @Serializable @SerialName("agentCommandError") class AgentCommandError(val message: String): ChatErrorType()
  @Serializable @SerialName("invalidFileDescription") class InvalidFileDescription(val message: String): ChatErrorType()
  @Serializable @SerialName("connectionIncognitoChangeProhibited") object ConnectionIncognitoChangeProhibited: ChatErrorType()
  @Serializable @SerialName("connectionUserChangeProhibited") object ConnectionUserChangeProhibited: ChatErrorType()
  @Serializable @SerialName("peerChatVRangeIncompatible") object PeerChatVRangeIncompatible: ChatErrorType()
  @Serializable @SerialName("internalError") class InternalError(val message: String): ChatErrorType()
  @Serializable @SerialName("exception") class CEException(val message: String): ChatErrorType()
}

@Serializable
sealed class StoreError {
  val string: String
    get() = when (this) {
      is DuplicateName -> "duplicateName"
      is UserNotFound -> "userNotFound $userId"
      is UserNotFoundByName -> "userNotFoundByName $contactName"
      is UserNotFoundByContactId -> "userNotFoundByContactId $contactId"
      is UserNotFoundByGroupId -> "userNotFoundByGroupId $groupId"
      is UserNotFoundByFileId -> "userNotFoundByFileId $fileId"
      is UserNotFoundByContactRequestId -> "userNotFoundByContactRequestId $contactRequestId"
      is ContactNotFound -> "contactNotFound $contactId"
      is ContactNotFoundByName -> "contactNotFoundByName $contactName"
      is ContactNotFoundByMemberId -> "contactNotFoundByMemberId $groupMemberId"
      is ContactNotReady -> "contactNotReady $contactName"
      is DuplicateContactLink -> "duplicateContactLink"
      is UserContactLinkNotFound -> "userContactLinkNotFound"
      is ContactRequestNotFound -> "contactRequestNotFound $contactRequestId"
      is ContactRequestNotFoundByName -> "contactRequestNotFoundByName $contactName"
      is GroupNotFound -> "groupNotFound $groupId"
      is GroupNotFoundByName -> "groupNotFoundByName $groupName"
      is GroupMemberNameNotFound -> "groupMemberNameNotFound $groupId $groupMemberName"
      is GroupMemberNotFound -> "groupMemberNotFound $groupMemberId"
      is GroupMemberNotFoundByMemberId -> "groupMemberNotFoundByMemberId $memberId"
      is MemberContactGroupMemberNotFound -> "memberContactGroupMemberNotFound $contactId"
      is GroupWithoutUser -> "groupWithoutUser"
      is DuplicateGroupMember -> "duplicateGroupMember"
      is GroupAlreadyJoined -> "groupAlreadyJoined"
      is GroupInvitationNotFound -> "groupInvitationNotFound"
      is NoteFolderAlreadyExists -> "noteFolderAlreadyExists $noteFolderId"
      is NoteFolderNotFound -> "noteFolderNotFound $noteFolderId"
      is UserNoteFolderNotFound -> "userNoteFolderNotFound"
      is SndFileNotFound -> "sndFileNotFound $fileId"
      is SndFileInvalid -> "sndFileInvalid $fileId"
      is RcvFileNotFound -> "rcvFileNotFound $fileId"
      is RcvFileDescrNotFound -> "rcvFileDescrNotFound $fileId"
      is FileNotFound -> "fileNotFound $fileId"
      is RcvFileInvalid -> "rcvFileInvalid $fileId"
      is RcvFileInvalidDescrPart -> "rcvFileInvalidDescrPart"
      is LocalFileNoTransfer -> "localFileNoTransfer $fileId"
      is SharedMsgIdNotFoundByFileId -> "sharedMsgIdNotFoundByFileId $fileId"
      is FileIdNotFoundBySharedMsgId -> "fileIdNotFoundBySharedMsgId $sharedMsgId"
      is SndFileNotFoundXFTP -> "sndFileNotFoundXFTP $agentSndFileId"
      is RcvFileNotFoundXFTP -> "rcvFileNotFoundXFTP $agentRcvFileId"
      is ConnectionNotFound -> "connectionNotFound $agentConnId"
      is ConnectionNotFoundById -> "connectionNotFoundById $connId"
      is ConnectionNotFoundByMemberId -> "connectionNotFoundByMemberId $groupMemberId"
      is PendingConnectionNotFound -> "pendingConnectionNotFound $connId"
      is IntroNotFound -> "introNotFound"
      is UniqueID -> "uniqueID"
      is LargeMsg -> "largeMsg"
      is InternalError -> "internalError $message"
      is DBException -> "dBException $message"
      is DBBusyError -> "dBBusyError $message"
      is BadChatItem -> "badChatItem $itemId"
      is ChatItemNotFound -> "chatItemNotFound $itemId"
      is ChatItemNotFoundByText -> "chatItemNotFoundByText $text"
      is ChatItemSharedMsgIdNotFound -> "chatItemSharedMsgIdNotFound $sharedMsgId"
      is ChatItemNotFoundByFileId -> "chatItemNotFoundByFileId $fileId"
      is ChatItemNotFoundByContactId -> "chatItemNotFoundByContactId $contactId"
      is ChatItemNotFoundByGroupId -> "chatItemNotFoundByGroupId $groupId"
      is ProfileNotFound -> "profileNotFound $profileId"
      is DuplicateGroupLink -> "duplicateGroupLink ${groupInfo.groupId}"
      is GroupLinkNotFound -> "groupLinkNotFound ${groupInfo.groupId}"
      is HostMemberIdNotFound -> "hostMemberIdNotFound $groupId"
      is ContactNotFoundByFileId -> "contactNotFoundByFileId $fileId"
      is NoGroupSndStatus -> "noGroupSndStatus $itemId $groupMemberId"
      is DuplicateGroupMessage -> "duplicateGroupMessage $groupId $sharedMsgId $authorGroupMemberId $authorGroupMemberId"
      is RemoteHostNotFound -> "remoteHostNotFound $remoteHostId"
      is RemoteHostUnknown -> "remoteHostUnknown"
      is RemoteHostDuplicateCA -> "remoteHostDuplicateCA"
      is RemoteCtrlNotFound -> "remoteCtrlNotFound $remoteCtrlId"
      is RemoteCtrlDuplicateCA -> "remoteCtrlDuplicateCA"
      is ProhibitedDeleteUser -> "prohibitedDeleteUser $userId $contactId"
      is OperatorNotFound -> "operatorNotFound $serverOperatorId"
      is UsageConditionsNotFound -> "usageConditionsNotFound"
      is InvalidQuote -> "invalidQuote"
      is InvalidMention -> "invalidMention"
    }

  @Serializable @SerialName("duplicateName") object DuplicateName: StoreError()
  @Serializable @SerialName("userNotFound") class UserNotFound(val userId: Long): StoreError()
  @Serializable @SerialName("userNotFoundByName") class UserNotFoundByName(val contactName: String): StoreError()
  @Serializable @SerialName("userNotFoundByContactId") class UserNotFoundByContactId(val contactId: Long): StoreError()
  @Serializable @SerialName("userNotFoundByGroupId") class UserNotFoundByGroupId(val groupId: Long): StoreError()
  @Serializable @SerialName("userNotFoundByFileId") class UserNotFoundByFileId(val fileId: Long): StoreError()
  @Serializable @SerialName("userNotFoundByContactRequestId") class UserNotFoundByContactRequestId(val contactRequestId: Long): StoreError()
  @Serializable @SerialName("contactNotFound") class ContactNotFound(val contactId: Long): StoreError()
  @Serializable @SerialName("contactNotFoundByName") class ContactNotFoundByName(val contactName: String): StoreError()
  @Serializable @SerialName("contactNotFoundByMemberId") class ContactNotFoundByMemberId(val groupMemberId: Long): StoreError()
  @Serializable @SerialName("contactNotReady") class ContactNotReady(val contactName: String): StoreError()
  @Serializable @SerialName("duplicateContactLink") object DuplicateContactLink: StoreError()
  @Serializable @SerialName("userContactLinkNotFound") object UserContactLinkNotFound: StoreError()
  @Serializable @SerialName("contactRequestNotFound") class ContactRequestNotFound(val contactRequestId: Long): StoreError()
  @Serializable @SerialName("contactRequestNotFoundByName") class ContactRequestNotFoundByName(val contactName: String): StoreError()
  @Serializable @SerialName("groupNotFound") class GroupNotFound(val groupId: Long): StoreError()
  @Serializable @SerialName("groupNotFoundByName") class GroupNotFoundByName(val groupName: String): StoreError()
  @Serializable @SerialName("groupMemberNameNotFound") class GroupMemberNameNotFound(val groupId: Long, val groupMemberName: String): StoreError()
  @Serializable @SerialName("groupMemberNotFound") class GroupMemberNotFound(val groupMemberId: Long): StoreError()
  @Serializable @SerialName("groupMemberNotFoundByMemberId") class GroupMemberNotFoundByMemberId(val memberId: String): StoreError()
  @Serializable @SerialName("memberContactGroupMemberNotFound") class MemberContactGroupMemberNotFound(val contactId: Long): StoreError()
  @Serializable @SerialName("groupWithoutUser") object GroupWithoutUser: StoreError()
  @Serializable @SerialName("duplicateGroupMember") object DuplicateGroupMember: StoreError()
  @Serializable @SerialName("groupAlreadyJoined") object GroupAlreadyJoined: StoreError()
  @Serializable @SerialName("groupInvitationNotFound") object GroupInvitationNotFound: StoreError()
  @Serializable @SerialName("noteFolderAlreadyExists") class NoteFolderAlreadyExists(val noteFolderId: Long): StoreError()
  @Serializable @SerialName("noteFolderNotFound") class NoteFolderNotFound(val noteFolderId: Long): StoreError()
  @Serializable @SerialName("userNoteFolderNotFound") object UserNoteFolderNotFound: StoreError()
  @Serializable @SerialName("sndFileNotFound") class SndFileNotFound(val fileId: Long): StoreError()
  @Serializable @SerialName("sndFileInvalid") class SndFileInvalid(val fileId: Long): StoreError()
  @Serializable @SerialName("rcvFileNotFound") class RcvFileNotFound(val fileId: Long): StoreError()
  @Serializable @SerialName("rcvFileDescrNotFound") class RcvFileDescrNotFound(val fileId: Long): StoreError()
  @Serializable @SerialName("fileNotFound") class FileNotFound(val fileId: Long): StoreError()
  @Serializable @SerialName("rcvFileInvalid") class RcvFileInvalid(val fileId: Long): StoreError()
  @Serializable @SerialName("rcvFileInvalidDescrPart") object RcvFileInvalidDescrPart: StoreError()
  @Serializable @SerialName("localFileNoTransfer") class LocalFileNoTransfer(val fileId: Long): StoreError()
  @Serializable @SerialName("sharedMsgIdNotFoundByFileId") class SharedMsgIdNotFoundByFileId(val fileId: Long): StoreError()
  @Serializable @SerialName("fileIdNotFoundBySharedMsgId") class FileIdNotFoundBySharedMsgId(val sharedMsgId: String): StoreError()
  @Serializable @SerialName("sndFileNotFoundXFTP") class SndFileNotFoundXFTP(val agentSndFileId: String): StoreError()
  @Serializable @SerialName("rcvFileNotFoundXFTP") class RcvFileNotFoundXFTP(val agentRcvFileId: String): StoreError()
  @Serializable @SerialName("connectionNotFound") class ConnectionNotFound(val agentConnId: String): StoreError()
  @Serializable @SerialName("connectionNotFoundById") class ConnectionNotFoundById(val connId: Long): StoreError()
  @Serializable @SerialName("connectionNotFoundByMemberId") class ConnectionNotFoundByMemberId(val groupMemberId: Long): StoreError()
  @Serializable @SerialName("pendingConnectionNotFound") class PendingConnectionNotFound(val connId: Long): StoreError()
  @Serializable @SerialName("introNotFound") object IntroNotFound: StoreError()
  @Serializable @SerialName("uniqueID") object UniqueID: StoreError()
  @Serializable @SerialName("largeMsg") object LargeMsg: StoreError()
  @Serializable @SerialName("internalError") class InternalError(val message: String): StoreError()
  @Serializable @SerialName("dBException") class DBException(val message: String): StoreError()
  @Serializable @SerialName("dBBusyError") class DBBusyError(val message: String): StoreError()
  @Serializable @SerialName("badChatItem") class BadChatItem(val itemId: Long): StoreError()
  @Serializable @SerialName("chatItemNotFound") class ChatItemNotFound(val itemId: Long): StoreError()
  @Serializable @SerialName("chatItemNotFoundByText") class ChatItemNotFoundByText(val text: String): StoreError()
  @Serializable @SerialName("chatItemSharedMsgIdNotFound") class ChatItemSharedMsgIdNotFound(val sharedMsgId: String): StoreError()
  @Serializable @SerialName("chatItemNotFoundByFileId") class ChatItemNotFoundByFileId(val fileId: Long): StoreError()
  @Serializable @SerialName("chatItemNotFoundByContactId") class ChatItemNotFoundByContactId(val contactId: Long): StoreError()
  @Serializable @SerialName("chatItemNotFoundByGroupId") class ChatItemNotFoundByGroupId(val groupId: Long): StoreError()
  @Serializable @SerialName("profileNotFound") class ProfileNotFound(val profileId: Long): StoreError()
  @Serializable @SerialName("duplicateGroupLink") class DuplicateGroupLink(val groupInfo: GroupInfo): StoreError()
  @Serializable @SerialName("groupLinkNotFound") class GroupLinkNotFound(val groupInfo: GroupInfo): StoreError()
  @Serializable @SerialName("hostMemberIdNotFound") class HostMemberIdNotFound(val groupId: Long): StoreError()
  @Serializable @SerialName("contactNotFoundByFileId") class ContactNotFoundByFileId(val fileId: Long): StoreError()
  @Serializable @SerialName("noGroupSndStatus") class NoGroupSndStatus(val itemId: Long, val groupMemberId: Long): StoreError()
  @Serializable @SerialName("duplicateGroupMessage") class DuplicateGroupMessage(val groupId: Long, val sharedMsgId: String, val authorGroupMemberId: Long?, val forwardedByGroupMemberId: Long?): StoreError()
  @Serializable @SerialName("remoteHostNotFound") class RemoteHostNotFound(val remoteHostId: Long): StoreError()
  @Serializable @SerialName("remoteHostUnknown") object RemoteHostUnknown: StoreError()
  @Serializable @SerialName("remoteHostDuplicateCA") object RemoteHostDuplicateCA: StoreError()
  @Serializable @SerialName("remoteCtrlNotFound") class RemoteCtrlNotFound(val remoteCtrlId: Long): StoreError()
  @Serializable @SerialName("remoteCtrlDuplicateCA") class RemoteCtrlDuplicateCA: StoreError()
  @Serializable @SerialName("prohibitedDeleteUser") class ProhibitedDeleteUser(val userId: Long, val contactId: Long): StoreError()
  @Serializable @SerialName("operatorNotFound") class OperatorNotFound(val serverOperatorId: Long): StoreError()
  @Serializable @SerialName("usageConditionsNotFound") object UsageConditionsNotFound: StoreError()
  @Serializable @SerialName("invalidQuote") object InvalidQuote: StoreError()
  @Serializable @SerialName("invalidMention") object InvalidMention: StoreError()
}

@Serializable
sealed class DatabaseError {
  val string: String get() = when (this) {
    is ErrorEncrypted -> "errorEncrypted"
    is ErrorPlaintext -> "errorPlaintext"
    is ErrorNoFile -> "errorPlaintext"
    is ErrorExport -> "errorNoFile"
    is ErrorOpen -> "errorExport"
  }
  @Serializable @SerialName("errorEncrypted") object ErrorEncrypted: DatabaseError()
  @Serializable @SerialName("errorPlaintext") object ErrorPlaintext: DatabaseError()
  @Serializable @SerialName("errorNoFile") class ErrorNoFile(val dbFile: String): DatabaseError()
  @Serializable @SerialName("errorExport") class ErrorExport(val sqliteError: SQLiteError): DatabaseError()
  @Serializable @SerialName("errorOpen") class ErrorOpen(val sqliteError: SQLiteError): DatabaseError()
}

@Serializable
sealed class SQLiteError {
  @Serializable @SerialName("errorNotADatabase") object ErrorNotADatabase: SQLiteError()
  @Serializable @SerialName("error") class Error(val dbError: String): SQLiteError()
}

@Serializable
sealed class AgentErrorType {
  val string: String get() = when (this) {
    is CMD -> "CMD ${cmdErr.string} $errContext"
    is CONN -> "CONN ${connErr.string}"
    is SMP -> "SMP ${smpErr.string}"
    // is NTF -> "NTF ${ntfErr.string}"
    is XFTP -> "XFTP ${xftpErr.string}"
    is PROXY -> "PROXY $proxyServer $relayServer ${proxyErr.string}"
    is RCP -> "RCP ${rcpErr.string}"
    is BROKER -> "BROKER ${brokerErr.string}"
    is AGENT -> "AGENT ${agentErr.string}"
    is INTERNAL -> "INTERNAL $internalErr"
    is CRITICAL -> "CRITICAL $offerRestart $criticalErr"
    is INACTIVE -> "INACTIVE"
  }
  @Serializable @SerialName("CMD") class CMD(val cmdErr: CommandErrorType, val errContext: String): AgentErrorType()
  @Serializable @SerialName("CONN") class CONN(val connErr: ConnectionErrorType): AgentErrorType()
  @Serializable @SerialName("SMP") class SMP(val serverAddress: String, val smpErr: SMPErrorType): AgentErrorType()
  // @Serializable @SerialName("NTF") class NTF(val ntfErr: SMPErrorType): AgentErrorType()
  @Serializable @SerialName("XFTP") class XFTP(val xftpErr: XFTPErrorType): AgentErrorType()
  @Serializable @SerialName("PROXY") class PROXY(val proxyServer: String, val relayServer: String, val proxyErr: ProxyClientError): AgentErrorType()
  @Serializable @SerialName("RCP") class RCP(val rcpErr: RCErrorType): AgentErrorType()
  @Serializable @SerialName("BROKER") class BROKER(val brokerAddress: String, val brokerErr: BrokerErrorType): AgentErrorType()
  @Serializable @SerialName("AGENT") class AGENT(val agentErr: SMPAgentError): AgentErrorType()
  @Serializable @SerialName("INTERNAL") class INTERNAL(val internalErr: String): AgentErrorType()
  @Serializable @SerialName("CRITICAL") data class CRITICAL(val offerRestart: Boolean, val criticalErr: String): AgentErrorType()
  @Serializable @SerialName("INACTIVE") object INACTIVE: AgentErrorType()
}

@Serializable
sealed class CommandErrorType {
  val string: String get() = when (this) {
    is PROHIBITED -> "PROHIBITED"
    is SYNTAX -> "SYNTAX"
    is NO_CONN -> "NO_CONN"
    is SIZE -> "SIZE"
    is LARGE -> "LARGE"
  }
  @Serializable @SerialName("PROHIBITED") class PROHIBITED: CommandErrorType()
  @Serializable @SerialName("SYNTAX") class SYNTAX: CommandErrorType()
  @Serializable @SerialName("NO_CONN") class NO_CONN: CommandErrorType()
  @Serializable @SerialName("SIZE") class SIZE: CommandErrorType()
  @Serializable @SerialName("LARGE") class LARGE: CommandErrorType()
}

@Serializable
sealed class ConnectionErrorType {
  val string: String get() = when (this) {
    is NOT_FOUND -> "NOT_FOUND"
    is DUPLICATE -> "DUPLICATE"
    is SIMPLEX -> "SIMPLEX"
    is NOT_ACCEPTED -> "NOT_ACCEPTED"
    is NOT_AVAILABLE -> "NOT_AVAILABLE"
  }
  @Serializable @SerialName("NOT_FOUND") class NOT_FOUND: ConnectionErrorType()
  @Serializable @SerialName("DUPLICATE") class DUPLICATE: ConnectionErrorType()
  @Serializable @SerialName("SIMPLEX") class SIMPLEX: ConnectionErrorType()
  @Serializable @SerialName("NOT_ACCEPTED") class NOT_ACCEPTED: ConnectionErrorType()
  @Serializable @SerialName("NOT_AVAILABLE") class NOT_AVAILABLE: ConnectionErrorType()
}

@Serializable
sealed class BrokerErrorType {
  val string: String get() = when (this) {
    is RESPONSE -> "RESPONSE ${smpErr}"
    is UNEXPECTED -> "UNEXPECTED"
    is NETWORK -> "NETWORK"
    is HOST -> "HOST"
    is TRANSPORT -> "TRANSPORT ${transportErr.string}"
    is TIMEOUT -> "TIMEOUT"
  }
  @Serializable @SerialName("RESPONSE") class RESPONSE(val smpErr: String): BrokerErrorType()
  @Serializable @SerialName("UNEXPECTED") object UNEXPECTED: BrokerErrorType()
  @Serializable @SerialName("NETWORK") object NETWORK: BrokerErrorType()
  @Serializable @SerialName("HOST") object HOST: BrokerErrorType()
  @Serializable @SerialName("TRANSPORT") class TRANSPORT(val transportErr: SMPTransportError): BrokerErrorType()
  @Serializable @SerialName("TIMEOUT") object TIMEOUT: BrokerErrorType()
}

// ProtocolErrorType
@Serializable
sealed class SMPErrorType {
  val string: String get() = when (this) {
    is BLOCK -> "BLOCK"
    is SESSION -> "SESSION"
    is CMD -> "CMD ${cmdErr.string}"
    is PROXY -> "PROXY ${proxyErr.string}"
    is AUTH -> "AUTH"
    is BLOCKED -> "BLOCKED ${json.encodeToString(blockInfo)}"
    is CRYPTO -> "CRYPTO"
    is QUOTA -> "QUOTA"
    is STORE -> "STORE $storeErr"
    is NO_MSG -> "NO_MSG"
    is LARGE_MSG -> "LARGE_MSG"
    is EXPIRED -> "EXPIRED"
    is INTERNAL -> "INTERNAL"
  }
  @Serializable @SerialName("BLOCK") class BLOCK: SMPErrorType()
  @Serializable @SerialName("SESSION") class SESSION: SMPErrorType()
  @Serializable @SerialName("CMD") class CMD(val cmdErr: ProtocolCommandError): SMPErrorType()
  @Serializable @SerialName("PROXY") class PROXY(val proxyErr: ProxyError): SMPErrorType()
  @Serializable @SerialName("AUTH") class AUTH: SMPErrorType()
  @Serializable @SerialName("BLOCKED") class BLOCKED(val blockInfo: BlockingInfo): SMPErrorType()
  @Serializable @SerialName("CRYPTO") class CRYPTO: SMPErrorType()
  @Serializable @SerialName("QUOTA") class QUOTA: SMPErrorType()
  @Serializable @SerialName("STORE") class STORE(val storeErr: String): SMPErrorType()
  @Serializable @SerialName("NO_MSG") class NO_MSG: SMPErrorType()
  @Serializable @SerialName("LARGE_MSG") class LARGE_MSG: SMPErrorType()
  @Serializable @SerialName("EXPIRED") class EXPIRED: SMPErrorType()
  @Serializable @SerialName("INTERNAL") class INTERNAL: SMPErrorType()
}

@Serializable
sealed class ProxyError {
  val string: String get() = when (this) {
    is PROTOCOL -> "PROTOCOL ${protocolErr.string}"
    is BROKER -> "BROKER ${brokerErr.string}"
    is BASIC_AUTH -> "BASIC_AUTH"
    is NO_SESSION -> "NO_SESSION"
  }
  @Serializable @SerialName("PROTOCOL") class PROTOCOL(val protocolErr: SMPErrorType): ProxyError()
  @Serializable @SerialName("BROKER") class BROKER(val brokerErr: BrokerErrorType): ProxyError()
  @Serializable @SerialName("BASIC_AUTH") class BASIC_AUTH: ProxyError()
  @Serializable @SerialName("NO_SESSION") class NO_SESSION: ProxyError()
}

@Serializable
data class BlockingInfo(
  val reason: BlockingReason
)

@Serializable
enum class BlockingReason {
  @SerialName("spam") Spam,
  @SerialName("content") Content;

  val text: String get() = when (this) {
    Spam -> generalGetString(MR.strings.blocking_reason_spam)
    Content -> generalGetString(MR.strings.blocking_reason_content)
  }
}

@Serializable
sealed class ProtocolCommandError {
  val string: String get() = when (this) {
    is UNKNOWN -> "UNKNOWN"
    is SYNTAX -> "SYNTAX"
    is PROHIBITED -> "PROHIBITED"
    is NO_AUTH -> "NO_AUTH"
    is HAS_AUTH -> "HAS_AUTH"
    is NO_QUEUE -> "NO_QUEUE"
  }
  @Serializable @SerialName("UNKNOWN") object UNKNOWN: ProtocolCommandError()
  @Serializable @SerialName("SYNTAX") object SYNTAX: ProtocolCommandError()
  @Serializable @SerialName("PROHIBITED") object PROHIBITED: ProtocolCommandError()
  @Serializable @SerialName("NO_AUTH") object NO_AUTH: ProtocolCommandError()
  @Serializable @SerialName("HAS_AUTH") object HAS_AUTH: ProtocolCommandError()
  @Serializable @SerialName("NO_QUEUE") object NO_QUEUE: ProtocolCommandError()
}

@Serializable
sealed class SMPTransportError {
  val string: String get() = when (this) {
    is BadBlock -> "badBlock"
    is Version -> "version"
    is LargeMsg -> "largeMsg"
    is BadSession -> "badSession"
    is NoServerAuth -> "noServerAuth"
    is Handshake -> "handshake ${handshakeErr.string}"
  }
  @Serializable @SerialName("badBlock") class BadBlock: SMPTransportError()
  @Serializable @SerialName("version") class Version: SMPTransportError()
  @Serializable @SerialName("largeMsg") class LargeMsg: SMPTransportError()
  @Serializable @SerialName("badSession") class BadSession: SMPTransportError()
  @Serializable @SerialName("noServerAuth") class NoServerAuth: SMPTransportError()
  @Serializable @SerialName("handshake") class Handshake(val handshakeErr: SMPHandshakeError): SMPTransportError()
}

@Serializable
sealed class SMPHandshakeError {
  val string: String get() = when (this) {
    is PARSE -> "PARSE"
    is VERSION -> "VERSION"
    is IDENTITY -> "IDENTITY"
    is BAD_AUTH -> "BAD_AUTH"
  }
  @Serializable @SerialName("PARSE") class PARSE: SMPHandshakeError()
  @Serializable @SerialName("VERSION") class VERSION: SMPHandshakeError()
  @Serializable @SerialName("IDENTITY") class IDENTITY: SMPHandshakeError()
  @Serializable @SerialName("BAD_AUTH") class BAD_AUTH: SMPHandshakeError()
}

@Serializable
sealed class SMPAgentError {
  val string: String get() = when (this) {
    is A_MESSAGE -> "A_MESSAGE"
    is A_PROHIBITED -> "A_PROHIBITED"
    is A_VERSION -> "A_VERSION"
    is A_CRYPTO -> "A_CRYPTO"
    is A_DUPLICATE -> "A_DUPLICATE"
    is A_QUEUE -> "A_QUEUE"
  }
  @Serializable @SerialName("A_MESSAGE") object A_MESSAGE: SMPAgentError()
  @Serializable @SerialName("A_PROHIBITED") object A_PROHIBITED: SMPAgentError()
  @Serializable @SerialName("A_VERSION") object A_VERSION: SMPAgentError()
  @Serializable @SerialName("A_CRYPTO") object A_CRYPTO: SMPAgentError()
  @Serializable @SerialName("A_DUPLICATE") object A_DUPLICATE: SMPAgentError()
  @Serializable @SerialName("A_QUEUE") class A_QUEUE(val queueErr: String): SMPAgentError()
}

@Serializable
sealed class XFTPErrorType {
  val string: String get() = when (this) {
    is BLOCK -> "BLOCK"
    is SESSION -> "SESSION"
    is CMD -> "CMD ${cmdErr.string}"
    is AUTH -> "AUTH"
    is BLOCKED -> "BLOCKED ${json.encodeToString(blockInfo)}"
    is SIZE -> "SIZE"
    is QUOTA -> "QUOTA"
    is DIGEST -> "DIGEST"
    is CRYPTO -> "CRYPTO"
    is NO_FILE -> "NO_FILE"
    is HAS_FILE -> "HAS_FILE"
    is FILE_IO -> "FILE_IO"
    is TIMEOUT -> "TIMEOUT"
    is REDIRECT -> "REDIRECT"
    is INTERNAL -> "INTERNAL"
  }
  @Serializable @SerialName("BLOCK") object BLOCK: XFTPErrorType()
  @Serializable @SerialName("SESSION") object SESSION: XFTPErrorType()
  @Serializable @SerialName("CMD") class CMD(val cmdErr: ProtocolCommandError): XFTPErrorType()
  @Serializable @SerialName("AUTH") object AUTH: XFTPErrorType()
  @Serializable @SerialName("BLOCKED") class BLOCKED(val blockInfo: BlockingInfo): XFTPErrorType()
  @Serializable @SerialName("SIZE") object SIZE: XFTPErrorType()
  @Serializable @SerialName("QUOTA") object QUOTA: XFTPErrorType()
  @Serializable @SerialName("DIGEST") object DIGEST: XFTPErrorType()
  @Serializable @SerialName("CRYPTO") object CRYPTO: XFTPErrorType()
  @Serializable @SerialName("NO_FILE") object NO_FILE: XFTPErrorType()
  @Serializable @SerialName("HAS_FILE") object HAS_FILE: XFTPErrorType()
  @Serializable @SerialName("FILE_IO") object FILE_IO: XFTPErrorType()
  @Serializable @SerialName("TIMEOUT") object TIMEOUT: XFTPErrorType()
  @Serializable @SerialName("REDIRECT") class REDIRECT(val redirectError: String): XFTPErrorType()
  @Serializable @SerialName("INTERNAL") object INTERNAL: XFTPErrorType()
}

@Serializable
sealed class ProxyClientError {
  val string: String get() = when (this) {
    is ProxyProtocolError -> "ProxyProtocolError $protocolErr"
    is ProxyUnexpectedResponse -> "ProxyUnexpectedResponse $responseStr"
    is ProxyResponseError -> "ProxyResponseError $responseErr"
  }
  @Serializable @SerialName("protocolError") class ProxyProtocolError(val protocolErr: SMPErrorType): ProxyClientError()
  @Serializable @SerialName("unexpectedResponse") class ProxyUnexpectedResponse(val responseStr: String): ProxyClientError()
  @Serializable @SerialName("responseError") class ProxyResponseError(val responseErr: SMPErrorType): ProxyClientError()
}

@Serializable
sealed class RCErrorType {
  val string: String get() = when (this) {
    is INTERNAL -> "INTERNAL $internalErr"
    is IDENTITY -> "IDENTITY"
    is NO_LOCAL_ADDRESS -> "NO_LOCAL_ADDRESS"
    is NEW_CONTROLLER -> "NEW_CONTROLLER"
    is NOT_DISCOVERED -> "NOT_DISCOVERED"
    is TLS_START_FAILED -> "TLS_START_FAILED"
    is EXCEPTION -> "EXCEPTION $EXCEPTION"
    is CTRL_AUTH -> "CTRL_AUTH"
    is CTRL_NOT_FOUND -> "CTRL_NOT_FOUND"
    is CTRL_ERROR -> "CTRL_ERROR $ctrlErr"
    is VERSION -> "VERSION"
    is ENCRYPT -> "ENCRYPT"
    is DECRYPT -> "DECRYPT"
    is BLOCK_SIZE -> "BLOCK_SIZE"
    is SYNTAX -> "SYNTAX $syntaxErr"
  }
  @Serializable @SerialName("internal") data class INTERNAL(val internalErr: String): RCErrorType()
  @Serializable @SerialName("identity") object IDENTITY: RCErrorType()
  @Serializable @SerialName("noLocalAddress") object NO_LOCAL_ADDRESS: RCErrorType()
  @Serializable @SerialName("newController") object NEW_CONTROLLER: RCErrorType()
  @Serializable @SerialName("notDiscovered") object NOT_DISCOVERED: RCErrorType()
  @Serializable @SerialName("tlsStartFailed") object TLS_START_FAILED: RCErrorType()
  @Serializable @SerialName("exception") data class EXCEPTION(val exception: String): RCErrorType()
  @Serializable @SerialName("ctrlAuth") object CTRL_AUTH: RCErrorType()
  @Serializable @SerialName("ctrlNotFound") object CTRL_NOT_FOUND: RCErrorType()
  @Serializable @SerialName("ctrlError") data class CTRL_ERROR(val ctrlErr: String): RCErrorType()
  @Serializable @SerialName("version") object VERSION: RCErrorType()
  @Serializable @SerialName("encrypt") object ENCRYPT: RCErrorType()
  @Serializable @SerialName("decrypt") object DECRYPT: RCErrorType()
  @Serializable @SerialName("blockSize") object BLOCK_SIZE: RCErrorType()
  @Serializable @SerialName("syntax") data class SYNTAX(val syntaxErr: String): RCErrorType()
}

@Serializable
sealed class ArchiveError {
  val string: String get() = when (this) {
    is ArchiveErrorImport -> "import ${importError}"
    is ArchiveErrorFile -> "importFile $file ${fileError}"
  }
  @Serializable @SerialName("import") class ArchiveErrorImport(val importError: String): ArchiveError()
  @Serializable @SerialName("fileError") class ArchiveErrorFile(val file: String, val fileError: String): ArchiveError()
}

@Serializable
sealed class RemoteHostError {
  val string: String get() = when (this) {
    is Missing -> "missing"
    is Inactive -> "inactive"
    is Busy -> "busy"
    is Timeout -> "timeout"
    is BadState -> "badState"
    is BadVersion -> "badVersion"
    is Disconnected -> "disconnected"
  }
  fun localizedString(name: String): String = when (this) {
    is Missing -> generalGetString(MR.strings.remote_host_error_missing)
    is Inactive -> generalGetString(MR.strings.remote_host_error_inactive)
    is Busy -> generalGetString(MR.strings.remote_host_error_busy)
    is Timeout -> generalGetString(MR.strings.remote_host_error_timeout)
    is BadState -> generalGetString(MR.strings.remote_host_error_bad_state)
    is BadVersion -> generalGetString(MR.strings.remote_host_error_bad_version)
    is Disconnected -> generalGetString(MR.strings.remote_host_error_disconnected)
  }.format(name)
  @Serializable @SerialName("missing") object Missing: RemoteHostError()
  @Serializable @SerialName("inactive") object Inactive: RemoteHostError()
  @Serializable @SerialName("busy") object Busy: RemoteHostError()
  @Serializable @SerialName("timeout") object Timeout: RemoteHostError()
  @Serializable @SerialName("badState") object BadState: RemoteHostError()
  @Serializable @SerialName("badVersion") object BadVersion: RemoteHostError()
  @Serializable @SerialName("disconnected") class Disconnected(val reason: String): RemoteHostError()
}

@Serializable
sealed class RemoteCtrlError {
  val string: String get() = when (this) {
    is Inactive -> "inactive"
    is BadState -> "badState"
    is Busy -> "busy"
    is Timeout -> "timeout"
    is NoKnownControllers -> "noKnownControllers"
    is BadController -> "badController"
    is Disconnected -> "disconnected"
    is BadInvitation -> "badInvitation"
    is BadVersion -> "badVersion"
    is HTTP2Error -> "http2Error"
    is ProtocolError -> "protocolError"
  }
  val localizedString: String get() = when (this) {
    is Inactive -> generalGetString(MR.strings.remote_ctrl_error_inactive)
    is BadState -> generalGetString(MR.strings.remote_ctrl_error_bad_state)
    is Busy -> generalGetString(MR.strings.remote_ctrl_error_busy)
    is Timeout -> generalGetString(MR.strings.remote_ctrl_error_timeout)
    is NoKnownControllers -> "no known controllers"
    is BadController -> "bad controller"
    is Disconnected -> generalGetString(MR.strings.remote_ctrl_error_disconnected)
    is BadInvitation -> generalGetString(MR.strings.remote_ctrl_error_bad_invitation)
    is BadVersion -> generalGetString(MR.strings.remote_ctrl_error_bad_version)
    is HTTP2Error -> "HTTP2 error"
    is ProtocolError -> "protocol error"
  }

  @Serializable @SerialName("inactive") object Inactive: RemoteCtrlError()
  @Serializable @SerialName("badState") object BadState: RemoteCtrlError()
  @Serializable @SerialName("busy") object Busy: RemoteCtrlError()
  @Serializable @SerialName("timeout") object Timeout: RemoteCtrlError()
  @Serializable @SerialName("noKnownControllers") object NoKnownControllers: RemoteCtrlError()
  @Serializable @SerialName("badController") object BadController: RemoteCtrlError()
  @Serializable @SerialName("disconnected") class Disconnected(val remoteCtrlId: Long, val reason: String): RemoteCtrlError()
  @Serializable @SerialName("badInvitation") object BadInvitation: RemoteCtrlError()
  @Serializable @SerialName("badVersion") data class BadVersion(val appVersion: String): RemoteCtrlError()
  @Serializable @SerialName("hTTP2Error") data class HTTP2Error(val http2Error: String): RemoteCtrlError()
  @Serializable @SerialName("protocolError") object ProtocolError: RemoteCtrlError()
}

enum class NotificationsMode() {
  OFF, PERIODIC, SERVICE, /*INSTANT - for Firebase notifications */;

  companion object {
    val default: NotificationsMode = SERVICE
  }
}

@Serializable
enum class PrivacyChatListOpenLinksMode {
  @SerialName("yes") YES,
  @SerialName("no") NO,
  @SerialName("ask") ASK
}

@Serializable
data class AppSettings(
  var networkConfig: NetCfg? = null,
  var networkProxy: NetworkProxy? = null,
  var privacyEncryptLocalFiles: Boolean? = null,
  var privacyAskToApproveRelays: Boolean? = null,
  var privacyAcceptImages: Boolean? = null,
  var privacyLinkPreviews: Boolean? = null,
  var privacyChatListOpenLinks: PrivacyChatListOpenLinksMode? = null,
  var privacyShowChatPreviews: Boolean? = null,
  var privacySaveLastDraft: Boolean? = null,
  var privacyProtectScreen: Boolean? = null,
  var privacyMediaBlurRadius: Int? = null,
  var notificationMode: AppSettingsNotificationMode? = null,
  var notificationPreviewMode: AppSettingsNotificationPreviewMode? = null,
  var webrtcPolicyRelay: Boolean? = null,
  var webrtcICEServers: List<String>? = null,
  var confirmRemoteSessions: Boolean? = null,
  var connectRemoteViaMulticast: Boolean? = null,
  var connectRemoteViaMulticastAuto: Boolean? = null,
  var developerTools: Boolean? = null,
  var confirmDBUpgrades: Boolean? = null,
  var androidCallOnLockScreen: AppSettingsLockScreenCalls? = null,
  var iosCallKitEnabled: Boolean? = null,
  var iosCallKitCallsInRecents: Boolean? = null,
  var uiProfileImageCornerRadius: Float? = null,
  var uiChatItemRoundness: Float? = null,
  var uiChatItemTail: Boolean? = null,
  var uiColorScheme: String? = null,
  var uiDarkColorScheme: String? = null,
  var uiCurrentThemeIds: Map<String, String>? = null,
  var uiThemes: List<ThemeOverrides>? = null,
  var oneHandUI: Boolean? = null,
  var chatBottomBar: Boolean? = null
) {
  fun prepareForExport(): AppSettings {
    val empty = AppSettings()
    val def = defaults
    if (networkConfig != def.networkConfig) { empty.networkConfig = networkConfig }
    if (networkProxy != def.networkProxy) { empty.networkProxy = networkProxy }
    if (privacyEncryptLocalFiles != def.privacyEncryptLocalFiles) { empty.privacyEncryptLocalFiles = privacyEncryptLocalFiles }
    if (privacyAskToApproveRelays != def.privacyAskToApproveRelays) { empty.privacyAskToApproveRelays = privacyAskToApproveRelays }
    if (privacyAcceptImages != def.privacyAcceptImages) { empty.privacyAcceptImages = privacyAcceptImages }
    if (privacyLinkPreviews != def.privacyLinkPreviews) { empty.privacyLinkPreviews = privacyLinkPreviews }
    if (privacyChatListOpenLinks != def.privacyChatListOpenLinks) { empty.privacyChatListOpenLinks = privacyChatListOpenLinks }
    if (privacyShowChatPreviews != def.privacyShowChatPreviews) { empty.privacyShowChatPreviews = privacyShowChatPreviews }
    if (privacySaveLastDraft != def.privacySaveLastDraft) { empty.privacySaveLastDraft = privacySaveLastDraft }
    if (privacyProtectScreen != def.privacyProtectScreen) { empty.privacyProtectScreen = privacyProtectScreen }
    if (privacyMediaBlurRadius != def.privacyMediaBlurRadius) { empty.privacyMediaBlurRadius = privacyMediaBlurRadius }
    if (notificationMode != def.notificationMode) { empty.notificationMode = notificationMode }
    if (notificationPreviewMode != def.notificationPreviewMode) { empty.notificationPreviewMode = notificationPreviewMode }
    if (webrtcPolicyRelay != def.webrtcPolicyRelay) { empty.webrtcPolicyRelay = webrtcPolicyRelay }
    if (webrtcICEServers != def.webrtcICEServers) { empty.webrtcICEServers = webrtcICEServers }
    if (confirmRemoteSessions != def.confirmRemoteSessions) { empty.confirmRemoteSessions = confirmRemoteSessions }
    if (connectRemoteViaMulticast != def.connectRemoteViaMulticast) { empty.connectRemoteViaMulticast = connectRemoteViaMulticast }
    if (connectRemoteViaMulticastAuto != def.connectRemoteViaMulticastAuto) { empty.connectRemoteViaMulticastAuto = connectRemoteViaMulticastAuto }
    if (developerTools != def.developerTools) { empty.developerTools = developerTools }
    if (confirmDBUpgrades != def.confirmDBUpgrades) { empty.confirmDBUpgrades = confirmDBUpgrades }
    if (androidCallOnLockScreen != def.androidCallOnLockScreen) { empty.androidCallOnLockScreen = androidCallOnLockScreen }
    if (iosCallKitEnabled != def.iosCallKitEnabled) { empty.iosCallKitEnabled = iosCallKitEnabled }
    if (iosCallKitCallsInRecents != def.iosCallKitCallsInRecents) { empty.iosCallKitCallsInRecents = iosCallKitCallsInRecents }
    if (uiProfileImageCornerRadius != def.uiProfileImageCornerRadius) { empty.uiProfileImageCornerRadius = uiProfileImageCornerRadius }
    if (uiChatItemRoundness != def.uiChatItemRoundness) { empty.uiChatItemRoundness = uiChatItemRoundness }
    if (uiChatItemTail != def.uiChatItemTail) { empty.uiChatItemTail = uiChatItemTail }
    if (uiColorScheme != def.uiColorScheme) { empty.uiColorScheme = uiColorScheme }
    if (uiDarkColorScheme != def.uiDarkColorScheme) { empty.uiDarkColorScheme = uiDarkColorScheme }
    if (uiCurrentThemeIds != def.uiCurrentThemeIds) { empty.uiCurrentThemeIds = uiCurrentThemeIds }
    if (uiThemes != def.uiThemes) { empty.uiThemes = uiThemes }
    if (oneHandUI != def.oneHandUI) { empty.oneHandUI = oneHandUI }
    if (chatBottomBar != def.chatBottomBar) { empty.chatBottomBar = chatBottomBar }
    return empty
  }

  fun importIntoApp() {
    val def = appPreferences
    var net = networkConfig?.copy()
    if (net != null) {
      // migrating from iOS BUT shouldn't be here ever because it should be changed on migration stage
      if (net.hostMode == HostMode.Onion) {
        net = net.copy(hostMode = HostMode.Public, requiredHostMode = true)
      }
      if (net.socksProxy != null) {
        net = net.copy(socksProxy = networkProxy?.toProxyString())
      }
      setNetCfg(net)
    }
    networkProxy?.let { def.networkProxy.set(it) }
    privacyEncryptLocalFiles?.let { def.privacyEncryptLocalFiles.set(it) }
    privacyAskToApproveRelays?.let { def.privacyAskToApproveRelays.set(it) }
    privacyAcceptImages?.let { def.privacyAcceptImages.set(it) }
    privacyLinkPreviews?.let { def.privacyLinkPreviews.set(it) }
    privacyChatListOpenLinks?.let { def.privacyChatListOpenLinks.set(it) }
    privacyShowChatPreviews?.let { def.privacyShowChatPreviews.set(it) }
    privacySaveLastDraft?.let { def.privacySaveLastDraft.set(it) }
    privacyProtectScreen?.let { def.privacyProtectScreen.set(it) }
    privacyMediaBlurRadius?.let { def.privacyMediaBlurRadius.set(it) }
    notificationMode?.let { def.notificationsMode.set(it.toNotificationsMode()) }
    notificationPreviewMode?.let { def.notificationPreviewMode.set(it.toNotificationPreviewMode().name) }
    webrtcPolicyRelay?.let { def.webrtcPolicyRelay.set(it) }
    webrtcICEServers?.let { def.webrtcIceServers.set(it.joinToString(separator = "\n")) }
    confirmRemoteSessions?.let { def.confirmRemoteSessions.set(it) }
    connectRemoteViaMulticast?.let { def.connectRemoteViaMulticast.set(it) }
    connectRemoteViaMulticastAuto?.let { def.connectRemoteViaMulticastAuto.set(it) }
    developerTools?.let { def.developerTools.set(it) }
    confirmDBUpgrades?.let { def.confirmDBUpgrades.set(it) }
    androidCallOnLockScreen?.let { def.callOnLockScreen.set(it.toCallOnLockScreen()) }
    iosCallKitEnabled?.let { def.iosCallKitEnabled.set(it) }
    iosCallKitCallsInRecents?.let { def.iosCallKitCallsInRecents.set(it) }
    uiProfileImageCornerRadius?.let { def.profileImageCornerRadius.set(it) }
    uiChatItemRoundness?.let { def.chatItemRoundness.set(it) }
    uiChatItemTail?.let { def.chatItemTail.set(it) }
    uiColorScheme?.let { def.currentTheme.set(it) }
    uiDarkColorScheme?.let { def.systemDarkTheme.set(it) }
    uiCurrentThemeIds?.let { def.currentThemeIds.set(it) }
    uiThemes?.let { def.themeOverrides.set(it.skipDuplicates()) }
    oneHandUI?.let { def.oneHandUI.set(it) }
    chatBottomBar?.let { if (appPlatform.isAndroid) def.chatBottomBar.set(it) else def.chatBottomBar.set(true) }
  }

  companion object {
    val defaults: AppSettings
      get() = AppSettings(
        networkConfig = NetCfg.defaults,
        networkProxy = null,
        privacyEncryptLocalFiles = true,
        privacyAskToApproveRelays = true,
        privacyAcceptImages = true,
        privacyLinkPreviews = true,
        privacyChatListOpenLinks = PrivacyChatListOpenLinksMode.ASK,
        privacyShowChatPreviews = true,
        privacySaveLastDraft = true,
        privacyProtectScreen = false,
        privacyMediaBlurRadius = 0,
        notificationMode = AppSettingsNotificationMode.INSTANT,
        notificationPreviewMode = AppSettingsNotificationPreviewMode.MESSAGE,
        webrtcPolicyRelay = true,
        webrtcICEServers = emptyList(),
        confirmRemoteSessions = false,
        connectRemoteViaMulticast = true,
        connectRemoteViaMulticastAuto = true,
        developerTools = false,
        confirmDBUpgrades = false,
        androidCallOnLockScreen = AppSettingsLockScreenCalls.SHOW,
        iosCallKitEnabled = true,
        iosCallKitCallsInRecents = false,
        uiProfileImageCornerRadius = 22.5f,
        uiChatItemRoundness = 0.75f,
        uiChatItemTail = true,
        uiColorScheme = DefaultTheme.SYSTEM_THEME_NAME,
        uiDarkColorScheme = DefaultTheme.SIMPLEX.themeName,
        uiCurrentThemeIds = null,
        uiThemes = null,
        oneHandUI = true,
        chatBottomBar = true,
      )

    val current: AppSettings
      get() {
        val def = appPreferences
        return defaults.copy(
          networkConfig = getNetCfg(),
          networkProxy = def.networkProxy.get(),
          privacyEncryptLocalFiles = def.privacyEncryptLocalFiles.get(),
          privacyAskToApproveRelays = def.privacyAskToApproveRelays.get(),
          privacyAcceptImages = def.privacyAcceptImages.get(),
          privacyLinkPreviews = def.privacyLinkPreviews.get(),
          privacyChatListOpenLinks = def.privacyChatListOpenLinks.get(),
          privacyShowChatPreviews = def.privacyShowChatPreviews.get(),
          privacySaveLastDraft = def.privacySaveLastDraft.get(),
          privacyProtectScreen = def.privacyProtectScreen.get(),
          privacyMediaBlurRadius = def.privacyMediaBlurRadius.get(),
          notificationMode = AppSettingsNotificationMode.from(def.notificationsMode.get()),
          notificationPreviewMode = AppSettingsNotificationPreviewMode.from(NotificationPreviewMode.valueOf(def.notificationPreviewMode.get()!!)),
          webrtcPolicyRelay = def.webrtcPolicyRelay.get(),
          webrtcICEServers = def.webrtcIceServers.get()?.lines(),
          confirmRemoteSessions = def.confirmRemoteSessions.get(),
          connectRemoteViaMulticast = def.connectRemoteViaMulticast.get(),
          connectRemoteViaMulticastAuto = def.connectRemoteViaMulticastAuto.get(),
          developerTools = def.developerTools.get(),
          confirmDBUpgrades = def.confirmDBUpgrades.get(),
          androidCallOnLockScreen = AppSettingsLockScreenCalls.from(def.callOnLockScreen.get()),
          iosCallKitEnabled = def.iosCallKitEnabled.get(),
          iosCallKitCallsInRecents = def.iosCallKitCallsInRecents.get(),
          uiProfileImageCornerRadius = def.profileImageCornerRadius.get(),
          uiChatItemRoundness = def.chatItemRoundness.get(),
          uiChatItemTail = def.chatItemTail.get(),
          uiColorScheme = def.currentTheme.get() ?: DefaultTheme.SYSTEM_THEME_NAME,
          uiDarkColorScheme = def.systemDarkTheme.get() ?: DefaultTheme.SIMPLEX.themeName,
          uiCurrentThemeIds = def.currentThemeIds.get(),
          uiThemes = def.themeOverrides.get(),
          oneHandUI = def.oneHandUI.get(),
          chatBottomBar = def.chatBottomBar.get()
        )
    }
  }
}

@Serializable
enum class AppSettingsNotificationMode {
  @SerialName("off") OFF,
  @SerialName("periodic") PERIODIC,
  @SerialName("instant") INSTANT;

  fun toNotificationsMode(): NotificationsMode =
    when (this) {
      INSTANT -> NotificationsMode.SERVICE
      PERIODIC -> NotificationsMode.PERIODIC
      OFF -> NotificationsMode.OFF
    }

  companion object {
    fun from(mode: NotificationsMode): AppSettingsNotificationMode =
      when (mode) {
        NotificationsMode.SERVICE -> INSTANT
        NotificationsMode.PERIODIC -> PERIODIC
        NotificationsMode.OFF -> OFF
      }
  }
}

@Serializable
enum class AppSettingsNotificationPreviewMode {
  @SerialName("message") MESSAGE,
  @SerialName("contact") CONTACT,
  @SerialName("hidden") HIDDEN;

  fun toNotificationPreviewMode(): NotificationPreviewMode =
    when (this) {
      MESSAGE -> NotificationPreviewMode.MESSAGE
      CONTACT -> NotificationPreviewMode.CONTACT
      HIDDEN -> NotificationPreviewMode.HIDDEN
    }

  companion object {
    val default: AppSettingsNotificationPreviewMode = MESSAGE

    fun from(mode: NotificationPreviewMode): AppSettingsNotificationPreviewMode =
      when (mode) {
        NotificationPreviewMode.MESSAGE -> MESSAGE
        NotificationPreviewMode.CONTACT -> CONTACT
        NotificationPreviewMode.HIDDEN -> HIDDEN
      }
  }
}

@Serializable
enum class AppSettingsLockScreenCalls {
  @SerialName("disable") DISABLE,
  @SerialName("show") SHOW,
  @SerialName("accept") ACCEPT;

  fun toCallOnLockScreen(): CallOnLockScreen =
    when (this) {
      DISABLE -> CallOnLockScreen.DISABLE
      SHOW -> CallOnLockScreen.SHOW
      ACCEPT -> CallOnLockScreen.ACCEPT
    }

  companion object {
    val default = SHOW

    fun from(mode: CallOnLockScreen): AppSettingsLockScreenCalls =
      when (mode) {
        CallOnLockScreen.DISABLE -> DISABLE
        CallOnLockScreen.SHOW -> SHOW
        CallOnLockScreen.ACCEPT -> ACCEPT
      }
  }
}

@Serializable
data class UserNetworkInfo(
  val networkType: UserNetworkType,
  val online: Boolean,
)

enum class UserNetworkType {
  @SerialName("none") NONE,
  @SerialName("cellular") CELLULAR,
  @SerialName("wifi") WIFI,
  @SerialName("ethernet") ETHERNET,
  @SerialName("other") OTHER;

  val text: String
    get() = when (this) {
      NONE -> generalGetString(MR.strings.network_type_no_network_connection)
      CELLULAR -> generalGetString(MR.strings.network_type_cellular)
      WIFI -> generalGetString(MR.strings.network_type_network_wifi)
      ETHERNET -> generalGetString(MR.strings.network_type_ethernet)
      OTHER -> generalGetString(MR.strings.network_type_other)
    }
}

@Serializable
data class RcvMsgInfo (
  val msgId: Long,
  val msgDeliveryId: Long,
  val msgDeliveryStatus: String,
  val agentMsgId: Long,
  val agentMsgMeta: String
)

@Serializable
data class ServerQueueInfo (
  val server: String,
  val rcvId: String,
  val sndId: String,
  val ntfId: String? = null,
  val status: String,
  val info: QueueInfo
)

@Serializable
data class QueueInfo (
  val qiSnd: Boolean,
  val qiNtf: Boolean,
  val qiSub: QSub? = null,
  val qiSize: Int,
  val qiMsg: MsgInfo? = null
)

@Serializable
data class QSub (
  val qSubThread: QSubThread,
  val qDelivered: String? = null
)

enum class QSubThread {
  @SerialName("noSub")
  NO_SUB,
  @SerialName("subPending")
  SUB_PENDING,
  @SerialName("subThread")
  SUB_THREAD,
  @SerialName("prohibitSub")
  PROHIBIT_SUB
}

@Serializable
data class MsgInfo (
  val msgId: String,
  val msgTs: Instant,
  val msgType: MsgType,
)

enum class MsgType {
  @SerialName("message")
  MESSAGE,
  @SerialName("quota")
  QUOTA
}
