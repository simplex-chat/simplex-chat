package chat.simplex.common.model

import chat.simplex.common.views.helpers.*
import androidx.compose.runtime.*
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import chat.simplex.common.model.ChatModel.updatingChatsMutex
import dev.icerock.moko.resources.compose.painterResource
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.call.*
import chat.simplex.common.views.newchat.ConnectViaLinkTab
import chat.simplex.common.views.onboarding.OnboardingStage
import chat.simplex.common.views.usersettings.*
import com.charleskorn.kaml.Yaml
import com.charleskorn.kaml.YamlConfiguration
import chat.simplex.res.MR
import com.russhwolf.settings.Settings
import kotlinx.coroutines.*
import kotlinx.coroutines.sync.withLock
import kotlinx.datetime.Clock
import kotlinx.datetime.Instant
import kotlinx.serialization.*
import kotlinx.serialization.builtins.MapSerializer
import kotlinx.serialization.builtins.serializer
import kotlinx.serialization.json.*
import java.util.Date

typealias ChatCtrl = Long

// currentChatVersion in core
const val CURRENT_CHAT_VERSION: Int = 2

// version range that supports establishing direct connection with a group member (xGrpDirectInvVRange in core)
val CREATE_MEMBER_CONTACT_VRANGE = VersionRange(minVersion = 2, maxVersion = CURRENT_CHAT_VERSION)

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
  val backgroundServiceNoticeShown = mkBoolPreference(SHARED_PREFS_SERVICE_NOTICE_SHOWN, false)
  val backgroundServiceBatteryNoticeShown = mkBoolPreference(SHARED_PREFS_SERVICE_BATTERY_NOTICE_SHOWN, false)
  val autoRestartWorkerVersion = mkIntPreference(SHARED_PREFS_AUTO_RESTART_WORKER_VERSION, 0)
  val webrtcPolicyRelay = mkBoolPreference(SHARED_PREFS_WEBRTC_POLICY_RELAY, true)
  private val _callOnLockScreen = mkStrPreference(SHARED_PREFS_WEBRTC_CALLS_ON_LOCK_SCREEN, CallOnLockScreen.default.name)
  val callOnLockScreen: SharedPreference<CallOnLockScreen> = SharedPreference(
    get = fun(): CallOnLockScreen {
      val value = _callOnLockScreen.get() ?: return CallOnLockScreen.default
      return try {
        CallOnLockScreen.valueOf(value)
      } catch (e: Error) {
        CallOnLockScreen.default
      }
    },
    set = fun(action: CallOnLockScreen) { _callOnLockScreen.set(action.name) }
  )
  val performLA = mkBoolPreference(SHARED_PREFS_PERFORM_LA, false)
  val laMode = mkEnumPreference(SHARED_PREFS_LA_MODE, LAMode.default) { LAMode.values().firstOrNull { it.name == this } }
  val laLockDelay = mkIntPreference(SHARED_PREFS_LA_LOCK_DELAY, 30)
  val laNoticeShown = mkBoolPreference(SHARED_PREFS_LA_NOTICE_SHOWN, false)
  val webrtcIceServers = mkStrPreference(SHARED_PREFS_WEBRTC_ICE_SERVERS, null)
  val privacyProtectScreen = mkBoolPreference(SHARED_PREFS_PRIVACY_PROTECT_SCREEN, true)
  val privacyAcceptImages = mkBoolPreference(SHARED_PREFS_PRIVACY_ACCEPT_IMAGES, true)
  val privacyLinkPreviews = mkBoolPreference(SHARED_PREFS_PRIVACY_LINK_PREVIEWS, true)
  private val _simplexLinkMode = mkStrPreference(SHARED_PREFS_PRIVACY_SIMPLEX_LINK_MODE, SimplexLinkMode.default.name)
  val simplexLinkMode: SharedPreference<SimplexLinkMode> = SharedPreference(
    get = fun(): SimplexLinkMode {
      val value = _simplexLinkMode.get() ?: return SimplexLinkMode.default
      return try {
        SimplexLinkMode.valueOf(value)
      } catch (e: Error) {
        SimplexLinkMode.default
      }
    },
    set = fun(mode: SimplexLinkMode) { _simplexLinkMode.set(mode.name) }
  )
  val privacyShowChatPreviews = mkBoolPreference(SHARED_PREFS_PRIVACY_SHOW_CHAT_PREVIEWS, true)
  val privacySaveLastDraft = mkBoolPreference(SHARED_PREFS_PRIVACY_SAVE_LAST_DRAFT, true)
  val privacyDeliveryReceiptsSet = mkBoolPreference(SHARED_PREFS_PRIVACY_DELIVERY_RECEIPTS_SET, false)
  val privacyEncryptLocalFiles = mkBoolPreference(SHARED_PREFS_PRIVACY_ENCRYPT_LOCAL_FILES, true)
  val experimentalCalls = mkBoolPreference(SHARED_PREFS_EXPERIMENTAL_CALLS, false)
  val showUnreadAndFavorites = mkBoolPreference(SHARED_PREFS_SHOW_UNREAD_AND_FAVORITES, false)
  val chatArchiveName = mkStrPreference(SHARED_PREFS_CHAT_ARCHIVE_NAME, null)
  val chatArchiveTime = mkDatePreference(SHARED_PREFS_CHAT_ARCHIVE_TIME, null)
  val chatLastStart = mkDatePreference(SHARED_PREFS_CHAT_LAST_START, null)
  val developerTools = mkBoolPreference(SHARED_PREFS_DEVELOPER_TOOLS, false)
  val terminalAlwaysVisible = mkBoolPreference(SHARED_PREFS_TERMINAL_ALWAYS_VISIBLE, false)
  val networkUseSocksProxy = mkBoolPreference(SHARED_PREFS_NETWORK_USE_SOCKS_PROXY, false)
  val networkProxyHostPort = mkStrPreference(SHARED_PREFS_NETWORK_PROXY_HOST_PORT, "localhost:9050")
  private val _networkSessionMode = mkStrPreference(SHARED_PREFS_NETWORK_SESSION_MODE, TransportSessionMode.default.name)
  val networkSessionMode: SharedPreference<TransportSessionMode> = SharedPreference(
    get = fun(): TransportSessionMode {
      val value = _networkSessionMode.get() ?: return TransportSessionMode.default
      return try {
        TransportSessionMode.valueOf(value)
      } catch (e: Error) {
        TransportSessionMode.default
      }
    },
    set = fun(mode: TransportSessionMode) { _networkSessionMode.set(mode.name) }
  )
  val networkHostMode = mkStrPreference(SHARED_PREFS_NETWORK_HOST_MODE, HostMode.OnionViaSocks.name)
  val networkRequiredHostMode = mkBoolPreference(SHARED_PREFS_NETWORK_REQUIRED_HOST_MODE, false)
  val networkTCPConnectTimeout = mkTimeoutPreference(SHARED_PREFS_NETWORK_TCP_CONNECT_TIMEOUT, NetCfg.defaults.tcpConnectTimeout, NetCfg.proxyDefaults.tcpConnectTimeout)
  val networkTCPTimeout = mkTimeoutPreference(SHARED_PREFS_NETWORK_TCP_TIMEOUT, NetCfg.defaults.tcpTimeout, NetCfg.proxyDefaults.tcpTimeout)
  val networkTCPTimeoutPerKb = mkTimeoutPreference(SHARED_PREFS_NETWORK_TCP_TIMEOUT_PER_KB, NetCfg.defaults.tcpTimeoutPerKb, NetCfg.proxyDefaults.tcpTimeoutPerKb)
  val networkSMPPingInterval = mkLongPreference(SHARED_PREFS_NETWORK_SMP_PING_INTERVAL, NetCfg.defaults.smpPingInterval)
  val networkSMPPingCount = mkIntPreference(SHARED_PREFS_NETWORK_SMP_PING_COUNT, NetCfg.defaults.smpPingCount)
  val networkEnableKeepAlive = mkBoolPreference(SHARED_PREFS_NETWORK_ENABLE_KEEP_ALIVE, NetCfg.defaults.enableKeepAlive)
  val networkTCPKeepIdle = mkIntPreference(SHARED_PREFS_NETWORK_TCP_KEEP_IDLE, KeepAliveOpts.defaults.keepIdle)
  val networkTCPKeepIntvl = mkIntPreference(SHARED_PREFS_NETWORK_TCP_KEEP_INTVL, KeepAliveOpts.defaults.keepIntvl)
  val networkTCPKeepCnt = mkIntPreference(SHARED_PREFS_NETWORK_TCP_KEEP_CNT, KeepAliveOpts.defaults.keepCnt)
  val incognito = mkBoolPreference(SHARED_PREFS_INCOGNITO, false)
  val connectViaLinkTab = mkStrPreference(SHARED_PREFS_CONNECT_VIA_LINK_TAB, ConnectViaLinkTab.SCAN.name)
  val liveMessageAlertShown = mkBoolPreference(SHARED_PREFS_LIVE_MESSAGE_ALERT_SHOWN, false)
  val showHiddenProfilesNotice = mkBoolPreference(SHARED_PREFS_SHOW_HIDDEN_PROFILES_NOTICE, true)
  val showMuteProfileAlert = mkBoolPreference(SHARED_PREFS_SHOW_MUTE_PROFILE_ALERT, true)
  val appLanguage = mkStrPreference(SHARED_PREFS_APP_LANGUAGE, null)

  val onboardingStage = mkEnumPreference(SHARED_PREFS_ONBOARDING_STAGE, OnboardingStage.OnboardingComplete) { OnboardingStage.values().firstOrNull { it.name == this } }
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

  val currentTheme = mkStrPreference(SHARED_PREFS_CURRENT_THEME, DefaultTheme.SYSTEM.name)
  val systemDarkTheme = mkStrPreference(SHARED_PREFS_SYSTEM_DARK_THEME, DefaultTheme.SIMPLEX.name)
  val themeOverrides = mkMapPreference(SHARED_PREFS_THEMES, mapOf(), encode = {
    json.encodeToString(MapSerializer(String.serializer(), ThemeOverrides.serializer()), it)
  }, decode = {
    json.decodeFromString(MapSerializer(String.serializer(), ThemeOverrides.serializer()), it)
  }, settingsThemes)

  val whatsNewVersion = mkStrPreference(SHARED_PREFS_WHATS_NEW_VERSION, null)
  val lastMigratedVersionCode = mkIntPreference(SHARED_PREFS_LAST_MIGRATED_VERSION_CODE, 0)
  val customDisappearingMessageTime = mkIntPreference(SHARED_PREFS_CUSTOM_DISAPPEARING_MESSAGE_TIME, 300)
  val deviceNameForRemoteAccess = mkStrPreference(SHARED_PREFS_DEVICE_NAME_FOR_REMOTE_ACCESS, deviceName)

  val confirmRemoteSessions = mkBoolPreference(SHARED_PREFS_CONFIRM_REMOTE_SESSIONS, false)
  val connectRemoteViaMulticast = mkBoolPreference(SHARED_PREFS_CONNECT_REMOTE_VIA_MULTICAST, false)
  val connectRemoteViaMulticastAuto = mkBoolPreference(SHARED_PREFS_CONNECT_REMOTE_VIA_MULTICAST_AUTO, true)
  val offerRemoteMulticast = mkBoolPreference(SHARED_PREFS_OFFER_REMOTE_MULTICAST, true)

  val desktopWindowState = mkStrPreference(SHARED_PREFS_DESKTOP_WINDOW_STATE, null)
  
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

  // LALAL
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

  companion object {
    const val SHARED_PREFS_ID = "chat.simplex.app.SIMPLEX_APP_PREFS"
    internal const val SHARED_PREFS_THEMES_ID = "chat.simplex.app.THEMES"
    private const val SHARED_PREFS_AUTO_RESTART_WORKER_VERSION = "AutoRestartWorkerVersion"
    private const val SHARED_PREFS_RUN_SERVICE_IN_BACKGROUND = "RunServiceInBackground"
    private const val SHARED_PREFS_NOTIFICATIONS_MODE = "NotificationsMode"
    private const val SHARED_PREFS_NOTIFICATION_PREVIEW_MODE = "NotificationPreviewMode"
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
    private const val SHARED_PREFS_PRIVACY_SIMPLEX_LINK_MODE = "PrivacySimplexLinkMode"
    private const val SHARED_PREFS_PRIVACY_SHOW_CHAT_PREVIEWS = "PrivacyShowChatPreviews"
    private const val SHARED_PREFS_PRIVACY_SAVE_LAST_DRAFT = "PrivacySaveLastDraft"
    private const val SHARED_PREFS_PRIVACY_DELIVERY_RECEIPTS_SET = "PrivacyDeliveryReceiptsSet"
    private const val SHARED_PREFS_PRIVACY_ENCRYPT_LOCAL_FILES = "PrivacyEncryptLocalFiles"
    const val SHARED_PREFS_PRIVACY_FULL_BACKUP = "FullBackup"
    private const val SHARED_PREFS_EXPERIMENTAL_CALLS = "ExperimentalCalls"
    private const val SHARED_PREFS_SHOW_UNREAD_AND_FAVORITES = "ShowUnreadAndFavorites"
    private const val SHARED_PREFS_CHAT_ARCHIVE_NAME = "ChatArchiveName"
    private const val SHARED_PREFS_CHAT_ARCHIVE_TIME = "ChatArchiveTime"
    private const val SHARED_PREFS_APP_LANGUAGE = "AppLanguage"
    private const val SHARED_PREFS_ONBOARDING_STAGE = "OnboardingStage"
    private const val SHARED_PREFS_CHAT_LAST_START = "ChatLastStart"
    private const val SHARED_PREFS_DEVELOPER_TOOLS = "DeveloperTools"
    private const val SHARED_PREFS_TERMINAL_ALWAYS_VISIBLE = "TerminalAlwaysVisible"
    private const val SHARED_PREFS_NETWORK_USE_SOCKS_PROXY = "NetworkUseSocksProxy"
    private const val SHARED_PREFS_NETWORK_PROXY_HOST_PORT = "NetworkProxyHostPort"
    private const val SHARED_PREFS_NETWORK_SESSION_MODE = "NetworkSessionMode"
    private const val SHARED_PREFS_NETWORK_HOST_MODE = "NetworkHostMode"
    private const val SHARED_PREFS_NETWORK_REQUIRED_HOST_MODE = "NetworkRequiredHostMode"
    private const val SHARED_PREFS_NETWORK_TCP_CONNECT_TIMEOUT = "NetworkTCPConnectTimeout"
    private const val SHARED_PREFS_NETWORK_TCP_TIMEOUT = "NetworkTCPTimeout"
    private const val SHARED_PREFS_NETWORK_TCP_TIMEOUT_PER_KB = "networkTCPTimeoutPerKb"
    private const val SHARED_PREFS_NETWORK_SMP_PING_INTERVAL = "NetworkSMPPingInterval"
    private const val SHARED_PREFS_NETWORK_SMP_PING_COUNT = "NetworkSMPPingCount"
    private const val SHARED_PREFS_NETWORK_ENABLE_KEEP_ALIVE = "NetworkEnableKeepAlive"
    private const val SHARED_PREFS_NETWORK_TCP_KEEP_IDLE = "NetworkTCPKeepIdle"
    private const val SHARED_PREFS_NETWORK_TCP_KEEP_INTVL = "NetworkTCPKeepIntvl"
    private const val SHARED_PREFS_NETWORK_TCP_KEEP_CNT = "NetworkTCPKeepCnt"
    private const val SHARED_PREFS_INCOGNITO = "Incognito"
    private const val SHARED_PREFS_CONNECT_VIA_LINK_TAB = "ConnectViaLinkTab"
    private const val SHARED_PREFS_LIVE_MESSAGE_ALERT_SHOWN = "LiveMessageAlertShown"
    private const val SHARED_PREFS_SHOW_HIDDEN_PROFILES_NOTICE = "ShowHiddenProfilesNotice"
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
    private const val SHARED_PREFS_CONFIRM_DB_UPGRADES = "ConfirmDBUpgrades"
    private const val SHARED_PREFS_SELF_DESTRUCT = "LocalAuthenticationSelfDestruct"
    private const val SHARED_PREFS_SELF_DESTRUCT_DISPLAY_NAME = "LocalAuthenticationSelfDestructDisplayName"
    private const val SHARED_PREFS_CURRENT_THEME = "CurrentTheme"
    private const val SHARED_PREFS_SYSTEM_DARK_THEME = "SystemDarkTheme"
    private const val SHARED_PREFS_THEMES = "Themes"
    private const val SHARED_PREFS_WHATS_NEW_VERSION = "WhatsNewVersion"
    private const val SHARED_PREFS_LAST_MIGRATED_VERSION_CODE = "LastMigratedVersionCode"
    private const val SHARED_PREFS_CUSTOM_DISAPPEARING_MESSAGE_TIME = "CustomDisappearingMessageTime"
    private const val SHARED_PREFS_DEVICE_NAME_FOR_REMOTE_ACCESS = "DeviceNameForRemoteAccess"
    private const val SHARED_PREFS_CONFIRM_REMOTE_SESSIONS = "ConfirmRemoteSessions"
    private const val SHARED_PREFS_CONNECT_REMOTE_VIA_MULTICAST = "ConnectRemoteViaMulticast"
    private const val SHARED_PREFS_CONNECT_REMOTE_VIA_MULTICAST_AUTO = "ConnectRemoteViaMulticastAuto"
    private const val SHARED_PREFS_OFFER_REMOTE_MULTICAST = "OfferRemoteMulticast"
    private const val SHARED_PREFS_DESKTOP_WINDOW_STATE = "DesktopWindowState"
  }
}

private const val MESSAGE_TIMEOUT: Int = 15_000_000

object ChatController {
  var ctrl: ChatCtrl? = -1
  val appPrefs: AppPreferences by lazy { AppPreferences() }

  val chatModel = ChatModel
  private var receiverStarted = false
  var lastMsgReceivedTimestamp: Long = System.currentTimeMillis()
    private set

  private fun currentUserId(funcName: String): Long {
    val userId = chatModel.currentUser.value?.userId
    if (userId == null) {
      val error = "$funcName: no current user"
      Log.e(TAG, error)
      throw Exception(error)
    }
    return userId
  }

  suspend fun startChat(user: User) {
    Log.d(TAG, "user: $user")
    try {
      if (chatModel.chatRunning.value == true) return
      apiSetNetworkConfig(getNetCfg())
      apiSetTempFolder(coreTmpDir.absolutePath)
      apiSetFilesFolder(appFilesDir.absolutePath)
      if (appPlatform.isDesktop) {
        apiSetRemoteHostsFolder(remoteHostsDir.absolutePath)
      }
      apiSetXFTPConfig(getXFTPCfg())
      apiSetEncryptLocalFiles(appPrefs.privacyEncryptLocalFiles.get())
      val justStarted = apiStartChat()
      val users = listUsers(null)
      chatModel.users.clear()
      chatModel.users.addAll(users)
      if (justStarted) {
        chatModel.currentUser.value = user
        chatModel.localUserCreated.value = true
        getUserChatData(null)
        appPrefs.chatLastStart.set(Clock.System.now())
        chatModel.chatRunning.value = true
        startReceiver()
        setLocalDeviceName(appPrefs.deviceNameForRemoteAccess.get()!!)
        Log.d(TAG, "startChat: started")
      } else {
        updatingChatsMutex.withLock {
          val chats = apiGetChats(null)
          chatModel.updateChats(chats)
        }
        Log.d(TAG, "startChat: running")
      }
    } catch (e: Error) {
      Log.e(TAG, "failed starting chat $e")
      throw e
    }
  }

  suspend fun startChatWithoutUser() {
    Log.d(TAG, "user: null")
    try {
      if (chatModel.chatRunning.value == true) return
      apiSetTempFolder(coreTmpDir.absolutePath)
      apiSetFilesFolder(appFilesDir.absolutePath)
      if (appPlatform.isDesktop) {
        apiSetRemoteHostsFolder(remoteHostsDir.absolutePath)
      }
      apiSetXFTPConfig(getXFTPCfg())
      apiSetEncryptLocalFiles(appPrefs.privacyEncryptLocalFiles.get())
      chatModel.users.clear()
      chatModel.currentUser.value = null
      chatModel.localUserCreated.value = false
      appPrefs.chatLastStart.set(Clock.System.now())
      chatModel.chatRunning.value = true
      startReceiver()
      setLocalDeviceName(appPrefs.deviceNameForRemoteAccess.get()!!)
      Log.d(TAG, "startChat: started without user")
    } catch (e: Error) {
      Log.e(TAG, "failed starting chat without user $e")
      throw e
    }
  }

  suspend fun changeActiveUser(rhId: Long?, toUserId: Long, viewPwd: String?) {
    try {
      changeActiveUser_(rhId, toUserId, viewPwd)
    } catch (e: Exception) {
      Log.e(TAG, "Unable to set active user: ${e.stackTraceToString()}")
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.failed_to_active_user_title), e.stackTraceToString())
    }
  }

  suspend fun changeActiveUser_(rhId: Long?, toUserId: Long, viewPwd: String?) {
    val currentUser = apiSetActiveUser(rhId, toUserId, viewPwd)
    chatModel.currentUser.value = currentUser
    val users = listUsers(rhId)
    chatModel.users.clear()
    chatModel.users.addAll(users)
    getUserChatData(rhId)
    val invitation = chatModel.callInvitations.values.firstOrNull { inv -> inv.user.userId == toUserId }
    if (invitation != null) {
      chatModel.callManager.reportNewIncomingCall(invitation.copy(user = currentUser))
    }
  }

  suspend fun getUserChatData(rhId: Long?) {
    val hasUser = chatModel.currentUser.value != null
    chatModel.userAddress.value = if (hasUser) apiGetUserAddress(rhId) else null
    chatModel.chatItemTTL.value = if (hasUser) getChatItemTTL(rhId) else ChatItemTTL.None
    updatingChatsMutex.withLock {
      val chats = apiGetChats(rhId)
      chatModel.updateChats(chats)
    }
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
          if (msg != null) processReceivedMsg(msg)
        } catch (e: Exception) {
          Log.e(TAG, "ChatController recvMsg/processReceivedMsg exception: " + e.stackTraceToString());
        } catch (e: Throwable) {
          Log.e(TAG, "ChatController recvMsg/processReceivedMsg throwable: " + e.stackTraceToString())
          AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error), e.stackTraceToString())
        }
      }
    }
  }

  suspend fun sendCmd(rhId: Long?, cmd: CC): CR {
    val ctrl = ctrl ?: throw Exception("Controller is not initialized")

    return withContext(Dispatchers.IO) {
      val c = cmd.cmdString
      chatModel.addTerminalItem(TerminalItem.cmd(rhId, cmd.obfuscated))
      Log.d(TAG, "sendCmd: ${cmd.cmdType}")
      val json = if (rhId == null) chatSendCmd(ctrl, c) else chatSendRemoteCmd(ctrl, rhId.toInt(), c)
      val r = APIResponse.decodeStr(json)
      Log.d(TAG, "sendCmd response type ${r.resp.responseType}")
      if (r.resp is CR.Response || r.resp is CR.Invalid) {
        Log.d(TAG, "sendCmd response json $json")
      }
      chatModel.addTerminalItem(TerminalItem.resp(rhId, r.resp))
      r.resp
    }
  }

  private fun recvMsg(ctrl: ChatCtrl): APIResponse? {
    val json = chatRecvMsgWait(ctrl, MESSAGE_TIMEOUT)
    return if (json == "") {
      null
    } else {
      val apiResp = APIResponse.decodeStr(json)
      val r = apiResp.resp
      Log.d(TAG, "chatRecvMsg: ${r.responseType}")
      if (r is CR.Response || r is CR.Invalid) Log.d(TAG, "chatRecvMsg json: $json")
      apiResp
    }
  }

  suspend fun apiGetActiveUser(rh: Long?): User? {
    val r = sendCmd(rh, CC.ShowActiveUser())
    if (r is CR.ActiveUser) return r.user.updateRemoteHostId(rh)
    Log.d(TAG, "apiGetActiveUser: ${r.responseType} ${r.details}")
    if (rh == null) {
      chatModel.localUserCreated.value = false
    }
    return null
  }

  suspend fun apiCreateActiveUser(rh: Long?, p: Profile?, sameServers: Boolean = false, pastTimestamp: Boolean = false): User? {
    val r = sendCmd(rh, CC.CreateActiveUser(p, sameServers = sameServers, pastTimestamp = pastTimestamp))
    if (r is CR.ActiveUser) return r.user.updateRemoteHostId(rh)
    else if (
      r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorStore && r.chatError.storeError is StoreError.DuplicateName ||
      r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorChat && r.chatError.errorType is ChatErrorType.UserExists
    ) {
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.failed_to_create_user_duplicate_title), generalGetString(MR.strings.failed_to_create_user_duplicate_desc))
    } else {
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.failed_to_create_user_title), r.details)
    }
    Log.d(TAG, "apiCreateActiveUser: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun listUsers(rh: Long?): List<UserInfo> {
    val r = sendCmd(rh, CC.ListUsers())
    if (r is CR.UsersList) {
      val users = if (rh == null) r.users else r.users.map { it.copy(user = it.user.copy(remoteHostId = rh)) }
      return users.sortedBy { it.user.chatViewName }
    }
    Log.d(TAG, "listUsers: ${r.responseType} ${r.details}")
    throw Exception("failed to list users ${r.responseType} ${r.details}")
  }

  suspend fun apiSetActiveUser(rh: Long?, userId: Long, viewPwd: String?): User {
    val r = sendCmd(rh, CC.ApiSetActiveUser(userId, viewPwd))
    if (r is CR.ActiveUser) return r.user.updateRemoteHostId(rh)
    Log.d(TAG, "apiSetActiveUser: ${r.responseType} ${r.details}")
    throw Exception("failed to set the user as active ${r.responseType} ${r.details}")
  }

  suspend fun apiSetAllContactReceipts(rh: Long?, enable: Boolean) {
    val r = sendCmd(rh, CC.SetAllContactReceipts(enable))
    if (r is CR.CmdOk) return
    throw Exception("failed to set receipts for all users ${r.responseType} ${r.details}")
  }

  suspend fun apiSetUserContactReceipts(u: User, userMsgReceiptSettings: UserMsgReceiptSettings) {
    val r = sendCmd(u.remoteHostId, CC.ApiSetUserContactReceipts(u.userId, userMsgReceiptSettings))
    if (r is CR.CmdOk) return
    throw Exception("failed to set receipts for user contacts ${r.responseType} ${r.details}")
  }

  suspend fun apiSetUserGroupReceipts(u: User, userMsgReceiptSettings: UserMsgReceiptSettings) {
    val r = sendCmd(u.remoteHostId, CC.ApiSetUserGroupReceipts(u.userId, userMsgReceiptSettings))
    if (r is CR.CmdOk) return
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
    if (r is CR.UserPrivacy) return r.updatedUser.updateRemoteHostId(rh)
    else throw Exception("Failed to change user privacy: ${r.responseType} ${r.details}")
  }

  suspend fun apiDeleteUser(u: User, delSMPQueues: Boolean, viewPwd: String?) {
    val r = sendCmd(u.remoteHostId, CC.ApiDeleteUser(u.userId, delSMPQueues, viewPwd))
    if (r is CR.CmdOk) return
    Log.d(TAG, "apiDeleteUser: ${r.responseType} ${r.details}")
    throw Exception("failed to delete the user ${r.responseType} ${r.details}")
  }

  suspend fun apiStartChat(): Boolean {
    val r = sendCmd(null, CC.StartChat(expire = true))
    when (r) {
      is CR.ChatStarted -> return true
      is CR.ChatRunning -> return false
      else -> throw Error("failed starting chat: ${r.responseType} ${r.details}")
    }
  }

  suspend fun apiStopChat(): Boolean {
    val r = sendCmd(null, CC.ApiStopChat())
    when (r) {
      is CR.ChatStopped -> return true
      else -> throw Error("failed stopping chat: ${r.responseType} ${r.details}")
    }
  }

  private suspend fun apiSetTempFolder(tempFolder: String) {
    val r = sendCmd(null, CC.SetTempFolder(tempFolder))
    if (r is CR.CmdOk) return
    throw Error("failed to set temp folder: ${r.responseType} ${r.details}")
  }

  private suspend fun apiSetFilesFolder(filesFolder: String) {
    val r = sendCmd(null, CC.SetFilesFolder(filesFolder))
    if (r is CR.CmdOk) return
    throw Error("failed to set files folder: ${r.responseType} ${r.details}")
  }

  private suspend fun apiSetRemoteHostsFolder(remoteHostsFolder: String) {
    val r = sendCmd(null, CC.SetRemoteHostsFolder(remoteHostsFolder))
    if (r is CR.CmdOk) return
    throw Error("failed to set remote hosts folder: ${r.responseType} ${r.details}")
  }

  suspend fun apiSetXFTPConfig(cfg: XFTPFileConfig?) {
    val r = sendCmd(null, CC.ApiSetXFTPConfig(cfg))
    if (r is CR.CmdOk) return
    throw Error("apiSetXFTPConfig bad response: ${r.responseType} ${r.details}")
  }

  suspend fun apiSetEncryptLocalFiles(enable: Boolean) = sendCommandOkResp(null, CC.ApiSetEncryptLocalFiles(enable))

  suspend fun apiExportArchive(config: ArchiveConfig) {
    val r = sendCmd(null, CC.ApiExportArchive(config))
    if (r is CR.CmdOk) return
    throw Error("failed to export archive: ${r.responseType} ${r.details}")
  }

  suspend fun apiImportArchive(config: ArchiveConfig): List<ArchiveError> {
    val r = sendCmd(null, CC.ApiImportArchive(config))
    if (r is CR.ArchiveImported) return r.archiveErrors
    throw Error("failed to import archive: ${r.responseType} ${r.details}")
  }

  suspend fun apiDeleteStorage() {
    val r = sendCmd(null, CC.ApiDeleteStorage())
    if (r is CR.CmdOk) return
    throw Error("failed to delete storage: ${r.responseType} ${r.details}")
  }

  suspend fun apiStorageEncryption(currentKey: String = "", newKey: String = ""): CR.ChatCmdError? {
    val r = sendCmd(null, CC.ApiStorageEncryption(DBEncryptionConfig(currentKey, newKey)))
    if (r is CR.CmdOk) return null
    else if (r is CR.ChatCmdError) return r
    throw Exception("failed to set storage encryption: ${r.responseType} ${r.details}")
  }

  suspend fun apiGetChats(rh: Long?): List<Chat> {
    val userId = kotlin.runCatching { currentUserId("apiGetChats") }.getOrElse { return emptyList() }
    val r = sendCmd(rh, CC.ApiGetChats(userId))
    if (r is CR.ApiChats) return if (rh == null) r.chats else r.chats.map { it.copy(remoteHostId = rh) }
    Log.e(TAG, "failed getting the list of chats: ${r.responseType} ${r.details}")
    AlertManager.shared.showAlertMsg(generalGetString(MR.strings.failed_to_parse_chats_title), generalGetString(MR.strings.contact_developers))
    return emptyList()
  }

  suspend fun apiGetChat(rh: Long?, type: ChatType, id: Long, pagination: ChatPagination = ChatPagination.Last(ChatPagination.INITIAL_COUNT), search: String = ""): Chat? {
    val r = sendCmd(rh, CC.ApiGetChat(type, id, pagination, search))
    if (r is CR.ApiChat) return if (rh == null) r.chat else r.chat.copy(remoteHostId = rh)
    Log.e(TAG, "apiGetChat bad response: ${r.responseType} ${r.details}")
    AlertManager.shared.showAlertMsg(generalGetString(MR.strings.failed_to_parse_chat_title), generalGetString(MR.strings.contact_developers))
    return null
  }

  suspend fun apiSendMessage(rh: Long?, type: ChatType, id: Long, file: CryptoFile? = null, quotedItemId: Long? = null, mc: MsgContent, live: Boolean = false, ttl: Int? = null): AChatItem? {
    val cmd = CC.ApiSendMessage(type, id, file, quotedItemId, mc, live, ttl)
    val r = sendCmd(rh, cmd)
    return when (r) {
      is CR.NewChatItem -> r.chatItem
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiSendMessage", generalGetString(MR.strings.error_sending_message), r)
        }
        null
      }
    }
  }

  suspend fun apiGetChatItemInfo(rh: Long?, type: ChatType, id: Long, itemId: Long): ChatItemInfo? {
    return when (val r = sendCmd(rh, CC.ApiGetChatItemInfo(type, id, itemId))) {
      is CR.ApiChatItemInfo -> r.chatItemInfo
      else -> {
        apiErrorAlert("apiGetChatItemInfo", generalGetString(MR.strings.error_loading_details), r)
        null
      }
    }
  }

  suspend fun apiUpdateChatItem(rh: Long?, type: ChatType, id: Long, itemId: Long, mc: MsgContent, live: Boolean = false): AChatItem? {
    val r = sendCmd(rh, CC.ApiUpdateChatItem(type, id, itemId, mc, live))
    if (r is CR.ChatItemUpdated) return r.chatItem
    Log.e(TAG, "apiUpdateChatItem bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiChatItemReaction(rh: Long?, type: ChatType, id: Long, itemId: Long, add: Boolean, reaction: MsgReaction): ChatItem? {
    val r = sendCmd(rh, CC.ApiChatItemReaction(type, id, itemId, add, reaction))
    if (r is CR.ChatItemReaction) return r.reaction.chatReaction.chatItem
    Log.e(TAG, "apiUpdateChatItem bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiDeleteChatItem(rh: Long?, type: ChatType, id: Long, itemId: Long, mode: CIDeleteMode): CR.ChatItemDeleted? {
    val r = sendCmd(rh, CC.ApiDeleteChatItem(type, id, itemId, mode))
    if (r is CR.ChatItemDeleted) return r
    Log.e(TAG, "apiDeleteChatItem bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiDeleteMemberChatItem(rh: Long?, groupId: Long, groupMemberId: Long, itemId: Long): Pair<ChatItem, ChatItem?>? {
    val r = sendCmd(rh, CC.ApiDeleteMemberChatItem(groupId, groupMemberId, itemId))
    if (r is CR.ChatItemDeleted) return r.deletedChatItem.chatItem to r.toChatItem?.chatItem
    Log.e(TAG, "apiDeleteMemberChatItem bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun getUserProtoServers(rh: Long?, serverProtocol: ServerProtocol): UserProtocolServers? {
    val userId = kotlin.runCatching { currentUserId("getUserProtoServers") }.getOrElse { return null }
    val r = sendCmd(rh, CC.APIGetUserProtoServers(userId, serverProtocol))
    return if (r is CR.UserProtoServers) { if (rh == null) r.servers else r.servers.copy(protoServers = r.servers.protoServers.map { it.copy(remoteHostId = rh) }) }
    else {
      Log.e(TAG, "getUserProtoServers bad response: ${r.responseType} ${r.details}")
      AlertManager.shared.showAlertMsg(
        generalGetString(if (serverProtocol == ServerProtocol.SMP) MR.strings.error_loading_smp_servers else MR.strings.error_loading_xftp_servers),
        "${r.responseType}: ${r.details}"
      )
      null
    }
  }

  suspend fun setUserProtoServers(rh: Long?, serverProtocol: ServerProtocol, servers: List<ServerCfg>): Boolean {
    val userId = kotlin.runCatching { currentUserId("setUserProtoServers") }.getOrElse { return false }
    val r = sendCmd(rh, CC.APISetUserProtoServers(userId, serverProtocol, servers))
    return when (r) {
      is CR.CmdOk -> true
      else -> {
        Log.e(TAG, "setUserProtoServers bad response: ${r.responseType} ${r.details}")
        AlertManager.shared.showAlertMsg(
          generalGetString(if (serverProtocol == ServerProtocol.SMP) MR.strings.error_saving_smp_servers else MR.strings.error_saving_xftp_servers),
          generalGetString(if (serverProtocol == ServerProtocol.SMP) MR.strings.ensure_smp_server_address_are_correct_format_and_unique else MR.strings.ensure_xftp_server_address_are_correct_format_and_unique)
        )
        false
      }
    }
  }

  suspend fun testProtoServer(rh: Long?, server: String): ProtocolTestFailure? {
    val userId = currentUserId("testProtoServer")
    val r = sendCmd(rh, CC.APITestProtoServer(userId, server))
    return when (r) {
      is CR.ServerTestResult -> r.testFailure
      else -> {
        Log.e(TAG, "testProtoServer bad response: ${r.responseType} ${r.details}")
        throw Exception("testProtoServer bad response: ${r.responseType} ${r.details}")
      }
    }
  }

  suspend fun getChatItemTTL(rh: Long?): ChatItemTTL {
    val userId = currentUserId("getChatItemTTL")
    val r = sendCmd(rh, CC.APIGetChatItemTTL(userId))
    if (r is CR.ChatItemTTL) return ChatItemTTL.fromSeconds(r.chatItemTTL)
    throw Exception("failed to get chat item TTL: ${r.responseType} ${r.details}")
  }

  suspend fun setChatItemTTL(rh: Long?, chatItemTTL: ChatItemTTL) {
    val userId = currentUserId("setChatItemTTL")
    val r = sendCmd(rh, CC.APISetChatItemTTL(userId, chatItemTTL.seconds))
    if (r is CR.CmdOk) return
    throw Exception("failed to set chat item TTL: ${r.responseType} ${r.details}")
  }

  suspend fun apiSetNetworkConfig(cfg: NetCfg): Boolean {
    val r = sendCmd(null, CC.APISetNetworkConfig(cfg))
    return when (r) {
      is CR.CmdOk -> true
      else -> {
        Log.e(TAG, "apiSetNetworkConfig bad response: ${r.responseType} ${r.details}")
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.error_setting_network_config),
          "${r.responseType}: ${r.details}"
        )
        false
      }
    }
  }

  suspend fun apiSetSettings(rh: Long?, type: ChatType, id: Long, settings: ChatSettings): Boolean {
    val r = sendCmd(rh, CC.APISetChatSettings(type, id, settings))
    return when (r) {
      is CR.CmdOk -> true
      else -> {
        Log.e(TAG, "apiSetSettings bad response: ${r.responseType} ${r.details}")
        false
      }
    }
  }

  suspend fun apiSetMemberSettings(rh: Long?, groupId: Long, groupMemberId: Long, memberSettings: GroupMemberSettings): Boolean =
    sendCommandOkResp(rh, CC.ApiSetMemberSettings(groupId, groupMemberId, memberSettings))

  suspend fun apiContactInfo(rh: Long?, contactId: Long): Pair<ConnectionStats?, Profile?>? {
    val r = sendCmd(rh, CC.APIContactInfo(contactId))
    if (r is CR.ContactInfo) return r.connectionStats_ to r.customUserProfile
    Log.e(TAG, "apiContactInfo bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiGroupMemberInfo(rh: Long?, groupId: Long, groupMemberId: Long): Pair<GroupMember, ConnectionStats?>? {
    val r = sendCmd(rh, CC.APIGroupMemberInfo(groupId, groupMemberId))
    if (r is CR.GroupMemberInfo) return Pair(r.member, r.connectionStats_)
    Log.e(TAG, "apiGroupMemberInfo bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiSwitchContact(rh: Long?, contactId: Long): ConnectionStats? {
    val r = sendCmd(rh, CC.APISwitchContact(contactId))
    if (r is CR.ContactSwitchStarted) return r.connectionStats
    apiErrorAlert("apiSwitchContact", generalGetString(MR.strings.error_changing_address), r)
    return null
  }

  suspend fun apiSwitchGroupMember(rh: Long?, groupId: Long, groupMemberId: Long): Pair<GroupMember, ConnectionStats>? {
    val r = sendCmd(rh, CC.APISwitchGroupMember(groupId, groupMemberId))
    if (r is CR.GroupMemberSwitchStarted) return Pair(r.member, r.connectionStats)
    apiErrorAlert("apiSwitchGroupMember", generalGetString(MR.strings.error_changing_address), r)
    return null
  }

  suspend fun apiAbortSwitchContact(rh: Long?, contactId: Long): ConnectionStats? {
    val r = sendCmd(rh, CC.APIAbortSwitchContact(contactId))
    if (r is CR.ContactSwitchAborted) return r.connectionStats
    apiErrorAlert("apiAbortSwitchContact", generalGetString(MR.strings.error_aborting_address_change), r)
    return null
  }

  suspend fun apiAbortSwitchGroupMember(rh: Long?, groupId: Long, groupMemberId: Long): Pair<GroupMember, ConnectionStats>? {
    val r = sendCmd(rh, CC.APIAbortSwitchGroupMember(groupId, groupMemberId))
    if (r is CR.GroupMemberSwitchAborted) return Pair(r.member, r.connectionStats)
    apiErrorAlert("apiAbortSwitchGroupMember", generalGetString(MR.strings.error_aborting_address_change), r)
    return null
  }

  suspend fun apiSyncContactRatchet(rh: Long?, contactId: Long, force: Boolean): ConnectionStats? {
    val r = sendCmd(rh, CC.APISyncContactRatchet(contactId, force))
    if (r is CR.ContactRatchetSyncStarted) return r.connectionStats
    apiErrorAlert("apiSyncContactRatchet", generalGetString(MR.strings.error_synchronizing_connection), r)
    return null
  }

  suspend fun apiSyncGroupMemberRatchet(rh: Long?, groupId: Long, groupMemberId: Long, force: Boolean): Pair<GroupMember, ConnectionStats>? {
    val r = sendCmd(rh, CC.APISyncGroupMemberRatchet(groupId, groupMemberId, force))
    if (r is CR.GroupMemberRatchetSyncStarted) return Pair(r.member, r.connectionStats)
    apiErrorAlert("apiSyncGroupMemberRatchet", generalGetString(MR.strings.error_synchronizing_connection), r)
    return null
  }

  suspend fun apiGetContactCode(rh: Long?, contactId: Long): Pair<Contact, String>? {
    val r = sendCmd(rh, CC.APIGetContactCode(contactId))
    if (r is CR.ContactCode) return r.contact to r.connectionCode
    Log.e(TAG,"failed to get contact code: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiGetGroupMemberCode(rh: Long?, groupId: Long, groupMemberId: Long): Pair<GroupMember, String>? {
    val r = sendCmd(rh, CC.APIGetGroupMemberCode(groupId, groupMemberId))
    if (r is CR.GroupMemberCode) return r.member to r.connectionCode
    Log.e(TAG,"failed to get group member code: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiVerifyContact(rh: Long?, contactId: Long, connectionCode: String?): Pair<Boolean, String>? {
    return when (val r = sendCmd(rh, CC.APIVerifyContact(contactId, connectionCode))) {
      is CR.ConnectionVerified -> r.verified to r.expectedCode
      else -> null
    }
  }

  suspend fun apiVerifyGroupMember(rh: Long?, groupId: Long, groupMemberId: Long, connectionCode: String?): Pair<Boolean, String>? {
    return when (val r = sendCmd(rh, CC.APIVerifyGroupMember(groupId, groupMemberId, connectionCode))) {
      is CR.ConnectionVerified -> r.verified to r.expectedCode
      else -> null
    }
  }



  suspend fun apiAddContact(rh: Long?, incognito: Boolean): Pair<String, PendingContactConnection>? {
    val userId = chatModel.currentUser.value?.userId ?: run {
      Log.e(TAG, "apiAddContact: no current user")
      return null
    }
    val r = sendCmd(rh, CC.APIAddContact(userId, incognito))
    return when (r) {
      is CR.Invitation -> r.connReqInvitation to r.connection
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiAddContact", generalGetString(MR.strings.connection_error), r)
        }
        null
      }
    }
  }

  suspend fun apiSetConnectionIncognito(rh: Long?, connId: Long, incognito: Boolean): PendingContactConnection? {
    val r = sendCmd(rh, CC.ApiSetConnectionIncognito(connId, incognito))
    if (r is CR.ConnectionIncognitoUpdated) return r.toConnection
    Log.e(TAG, "apiSetConnectionIncognito bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiConnectPlan(rh: Long?, connReq: String): ConnectionPlan? {
    val userId = kotlin.runCatching { currentUserId("apiConnectPlan") }.getOrElse { return null }
    val r = sendCmd(rh, CC.APIConnectPlan(userId, connReq))
    if (r is CR.CRConnectionPlan) return r.connectionPlan
    Log.e(TAG, "apiConnectPlan bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiConnect(rh: Long?, incognito: Boolean, connReq: String): PendingContactConnection?  {
    val userId = chatModel.currentUser.value?.userId ?: run {
      Log.e(TAG, "apiConnect: no current user")
      return null
    }
    val r = sendCmd(rh, CC.APIConnect(userId, incognito, connReq))
    when {
      r is CR.SentConfirmation -> return r.connection
      r is CR.SentInvitation -> return r.connection
      r is CR.ContactAlreadyExists -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.contact_already_exists),
          String.format(generalGetString(MR.strings.you_are_already_connected_to_vName_via_this_link), r.contact.displayName)
        )
        return null
      }
      r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorChat
          && r.chatError.errorType is ChatErrorType.InvalidConnReq -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.invalid_connection_link),
          generalGetString(MR.strings.please_check_correct_link_and_maybe_ask_for_a_new_one)
        )
        return null
      }
      r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorAgent
          && r.chatError.agentError is AgentErrorType.SMP
          && r.chatError.agentError.smpErr is SMPErrorType.AUTH -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.connection_error_auth),
          generalGetString(MR.strings.connection_error_auth_desc)
        )
        return null
      }
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiConnect", generalGetString(MR.strings.connection_error), r)
        }
        return null
      }
    }
  }

  suspend fun apiConnectContactViaAddress(rh: Long?, incognito: Boolean, contactId: Long): Contact? {
    val userId = chatModel.currentUser.value?.userId ?: run {
      Log.e(TAG, "apiConnectContactViaAddress: no current user")
      return null
    }
    val r = sendCmd(rh, CC.ApiConnectContactViaAddress(userId, incognito, contactId))
    when {
      r is CR.SentInvitationToContact -> return r.contact
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiConnectContactViaAddress", generalGetString(MR.strings.connection_error), r)
        }
        return null
      }
    }
  }

  suspend fun apiDeleteChat(rh: Long?, type: ChatType, id: Long, notify: Boolean? = null): Boolean {
    val r = sendCmd(rh, CC.ApiDeleteChat(type, id, notify))
    when {
      r is CR.ContactDeleted && type == ChatType.Direct -> return true
      r is CR.ContactConnectionDeleted && type == ChatType.ContactConnection -> return true
      r is CR.GroupDeletedUser && type == ChatType.Group -> return true
      else -> {
        val titleId = when (type) {
          ChatType.Direct -> MR.strings.error_deleting_contact
          ChatType.Group -> MR.strings.error_deleting_group
          ChatType.ContactRequest -> MR.strings.error_deleting_contact_request
          ChatType.ContactConnection -> MR.strings.error_deleting_pending_contact_connection
        }
        apiErrorAlert("apiDeleteChat", generalGetString(titleId), r)
      }
    }
    return false
  }

  suspend fun apiClearChat(rh: Long?, type: ChatType, id: Long): ChatInfo? {
    val r = sendCmd(rh, CC.ApiClearChat(type, id))
    if (r is CR.ChatCleared) return r.chatInfo
    Log.e(TAG, "apiClearChat bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiUpdateProfile(rh: Long?, profile: Profile): Pair<Profile, List<Contact>>? {
    val userId = kotlin.runCatching { currentUserId("apiUpdateProfile") }.getOrElse { return null }
    val r = sendCmd(rh, CC.ApiUpdateProfile(userId, profile))
    if (r is CR.UserProfileNoChange) return profile to emptyList()
    if (r is CR.UserProfileUpdated) return r.toProfile to r.updateSummary.changedContacts
    if (r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorStore && r.chatError.storeError is StoreError.DuplicateName) {
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.failed_to_create_user_duplicate_title), generalGetString(MR.strings.failed_to_create_user_duplicate_desc))
    }
    Log.e(TAG, "apiUpdateProfile bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiSetProfileAddress(rh: Long?, on: Boolean): User? {
    val userId = try { currentUserId("apiSetProfileAddress") } catch (e: Exception) { return null }
    return when (val r = sendCmd(rh, CC.ApiSetProfileAddress(userId, on))) {
      is CR.UserProfileNoChange -> null
      is CR.UserProfileUpdated -> r.user.updateRemoteHostId(rh)
      else -> throw Exception("failed to set profile address: ${r.responseType} ${r.details}")
    }
  }

  suspend fun apiSetContactPrefs(rh: Long?, contactId: Long, prefs: ChatPreferences): Contact? {
    val r = sendCmd(rh, CC.ApiSetContactPrefs(contactId, prefs))
    if (r is CR.ContactPrefsUpdated) return r.toContact
    Log.e(TAG, "apiSetContactPrefs bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiSetContactAlias(rh: Long?, contactId: Long, localAlias: String): Contact? {
    val r = sendCmd(rh, CC.ApiSetContactAlias(contactId, localAlias))
    if (r is CR.ContactAliasUpdated) return r.toContact
    Log.e(TAG, "apiSetContactAlias bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiSetConnectionAlias(rh: Long?, connId: Long, localAlias: String): PendingContactConnection? {
    val r = sendCmd(rh, CC.ApiSetConnectionAlias(connId, localAlias))
    if (r is CR.ConnectionAliasUpdated) return r.toConnection
    Log.e(TAG, "apiSetConnectionAlias bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiCreateUserAddress(rh: Long?): String? {
    val userId = kotlin.runCatching { currentUserId("apiCreateUserAddress") }.getOrElse { return null }
    val r = sendCmd(rh, CC.ApiCreateMyAddress(userId))
    return when (r) {
      is CR.UserContactLinkCreated -> r.connReqContact
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiCreateUserAddress", generalGetString(MR.strings.error_creating_address), r)
        }
        null
      }
    }
  }

  suspend fun apiDeleteUserAddress(rh: Long?): User? {
    val userId = try { currentUserId("apiDeleteUserAddress") } catch (e: Exception) { return null }
    val r = sendCmd(rh, CC.ApiDeleteMyAddress(userId))
    if (r is CR.UserContactLinkDeleted) return r.user.updateRemoteHostId(rh)
    Log.e(TAG, "apiDeleteUserAddress bad response: ${r.responseType} ${r.details}")
    return null
  }

  private suspend fun apiGetUserAddress(rh: Long?): UserContactLinkRec? {
    val userId = kotlin.runCatching { currentUserId("apiGetUserAddress") }.getOrElse { return null }
    val r = sendCmd(rh, CC.ApiShowMyAddress(userId))
    if (r is CR.UserContactLink) return r.contactLink
    if (r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorStore
      && r.chatError.storeError is StoreError.UserContactLinkNotFound
    ) {
      return null
    }
    Log.e(TAG, "apiGetUserAddress bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun userAddressAutoAccept(rh: Long?, autoAccept: AutoAccept?): UserContactLinkRec? {
    val userId = kotlin.runCatching { currentUserId("userAddressAutoAccept") }.getOrElse { return null }
    val r = sendCmd(rh, CC.ApiAddressAutoAccept(userId, autoAccept))
    if (r is CR.UserContactLinkUpdated) return r.contactLink
    if (r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorStore
      && r.chatError.storeError is StoreError.UserContactLinkNotFound
    ) {
      return null
    }
    Log.e(TAG, "userAddressAutoAccept bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiAcceptContactRequest(rh: Long?, incognito: Boolean, contactReqId: Long): Contact? {
    val r = sendCmd(rh, CC.ApiAcceptContact(incognito, contactReqId))
    return when {
      r is CR.AcceptingContactRequest -> r.contact
      r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorAgent
          && r.chatError.agentError is AgentErrorType.SMP
          && r.chatError.agentError.smpErr is SMPErrorType.AUTH -> {
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
    if (r is CR.ContactRequestRejected) return true
    Log.e(TAG, "apiRejectContactRequest bad response: ${r.responseType} ${r.details}")
    return false
  }

  suspend fun apiSendCallInvitation(rh: Long?, contact: Contact, callType: CallType): Boolean {
    val r = sendCmd(rh, CC.ApiSendCallInvitation(contact, callType))
    return r is CR.CmdOk
  }

  suspend fun apiRejectCall(rh: Long?, contact: Contact): Boolean {
    val r = sendCmd(rh, CC.ApiRejectCall(contact))
    return r is CR.CmdOk
  }

  suspend fun apiSendCallOffer(rh: Long?, contact: Contact, rtcSession: String, rtcIceCandidates: String, media: CallMediaType, capabilities: CallCapabilities): Boolean {
    val webRtcSession = WebRTCSession(rtcSession, rtcIceCandidates)
    val callOffer = WebRTCCallOffer(CallType(media, capabilities), webRtcSession)
    val r = sendCmd(rh, CC.ApiSendCallOffer(contact, callOffer))
    return r is CR.CmdOk
  }

  suspend fun apiSendCallAnswer(rh: Long?, contact: Contact, rtcSession: String, rtcIceCandidates: String): Boolean {
    val answer = WebRTCSession(rtcSession, rtcIceCandidates)
    val r = sendCmd(rh, CC.ApiSendCallAnswer(contact, answer))
    return r is CR.CmdOk
  }

  suspend fun apiSendCallExtraInfo(rh: Long?, contact: Contact, rtcIceCandidates: String): Boolean {
    val extraInfo = WebRTCExtraInfo(rtcIceCandidates)
    val r = sendCmd(rh, CC.ApiSendCallExtraInfo(contact, extraInfo))
    return r is CR.CmdOk
  }

  suspend fun apiEndCall(rh: Long?, contact: Contact): Boolean {
    val r = sendCmd(rh, CC.ApiEndCall(contact))
    return r is CR.CmdOk
  }

  suspend fun apiCallStatus(rh: Long?, contact: Contact, status: WebRTCCallStatus): Boolean {
    val r = sendCmd(rh, CC.ApiCallStatus(contact, status))
    return r is CR.CmdOk
  }

  suspend fun apiGetNetworkStatuses(rh: Long?): List<ConnNetworkStatus>? {
    val r = sendCmd(rh, CC.ApiGetNetworkStatuses())
    if (r is CR.NetworkStatuses) return r.networkStatuses
    Log.e(TAG, "apiGetNetworkStatuses bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiChatRead(rh: Long?, type: ChatType, id: Long, range: CC.ItemRange): Boolean {
    val r = sendCmd(rh, CC.ApiChatRead(type, id, range))
    if (r is CR.CmdOk) return true
    Log.e(TAG, "apiChatRead bad response: ${r.responseType} ${r.details}")
    return false
  }

  suspend fun apiChatUnread(rh: Long?, type: ChatType, id: Long, unreadChat: Boolean): Boolean {
    val r = sendCmd(rh, CC.ApiChatUnread(type, id, unreadChat))
    if (r is CR.CmdOk) return true
    Log.e(TAG, "apiChatUnread bad response: ${r.responseType} ${r.details}")
    return false
  }

  suspend fun apiReceiveFile(rh: Long?, fileId: Long, encrypted: Boolean, inline: Boolean? = null, auto: Boolean = false): AChatItem? {
    // -1 here is to override default behavior of providing current remote host id because file can be asked by local device while remote is connected
    val r = sendCmd(rh, CC.ReceiveFile(fileId, encrypted, inline))
    return when (r) {
      is CR.RcvFileAccepted -> r.chatItem
      is CR.RcvFileAcceptedSndCancelled -> {
        Log.d(TAG, "apiReceiveFile error: sender cancelled file transfer")
        if (!auto) {
          AlertManager.shared.showAlertMsg(
            generalGetString(MR.strings.cannot_receive_file),
            generalGetString(MR.strings.sender_cancelled_file_transfer)
          )
        }
        null
      }

      else -> {
        if (!(networkErrorAlert(r))) {
          val maybeChatError = chatError(r)
          if (maybeChatError is ChatErrorType.FileCancelled || maybeChatError is ChatErrorType.FileAlreadyReceiving) {
            Log.d(TAG, "apiReceiveFile ignoring FileCancelled or FileAlreadyReceiving error")
          } else {
            apiErrorAlert("apiReceiveFile", generalGetString(MR.strings.error_receiving_file), r)
          }
        }
        null
      }
    }
  }

  suspend fun cancelFile(rh: Long?, user: User, fileId: Long) {
    val chatItem = apiCancelFile(rh, fileId)
    if (chatItem != null) {
      chatItemSimpleUpdate(rh, user, chatItem)
      cleanupFile(chatItem)
    }
  }

  suspend fun apiCancelFile(rh: Long?, fileId: Long): AChatItem? {
    val r = sendCmd(rh, CC.CancelFile(fileId))
    return when (r) {
      is CR.SndFileCancelled -> r.chatItem
      is CR.RcvFileCancelled -> r.chatItem
      else -> {
        Log.d(TAG, "apiCancelFile bad response: ${r.responseType} ${r.details}")
        null
      }
    }
  }

  suspend fun apiNewGroup(rh: Long?, incognito: Boolean, groupProfile: GroupProfile): GroupInfo? {
    val userId = kotlin.runCatching { currentUserId("apiNewGroup") }.getOrElse { return null }
    val r = sendCmd(rh, CC.ApiNewGroup(userId, incognito, groupProfile))
    if (r is CR.GroupCreated) return r.groupInfo
    Log.e(TAG, "apiNewGroup bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiAddMember(rh: Long?, groupId: Long, contactId: Long, memberRole: GroupMemberRole): GroupMember? {
    val r = sendCmd(rh, CC.ApiAddMember(groupId, contactId, memberRole))
    return when (r) {
      is CR.SentGroupInvitation -> r.member
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiAddMember", generalGetString(MR.strings.error_adding_members), r)
        }
        null
      }
    }
  }

  suspend fun apiJoinGroup(rh: Long?, groupId: Long) {
    val r = sendCmd(rh, CC.ApiJoinGroup(groupId))
    when (r) {
      is CR.UserAcceptedGroupSent ->
        chatModel.updateGroup(rh, r.groupInfo)
      is CR.ChatCmdError -> {
        val e = r.chatError
        suspend fun deleteGroup() { if (apiDeleteChat(rh, ChatType.Group, groupId)) { chatModel.removeChat(rh, "#$groupId") } }
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

  suspend fun apiRemoveMember(rh: Long?, groupId: Long, memberId: Long): GroupMember? =
    when (val r = sendCmd(rh, CC.ApiRemoveMember(groupId, memberId))) {
      is CR.UserDeletedMember -> r.member
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiRemoveMember", generalGetString(MR.strings.error_removing_member), r)
        }
        null
      }
    }

  suspend fun apiMemberRole(rh: Long?, groupId: Long, memberId: Long, memberRole: GroupMemberRole): GroupMember =
    when (val r = sendCmd(rh, CC.ApiMemberRole(groupId, memberId, memberRole))) {
      is CR.MemberRoleUser -> r.member
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiMemberRole", generalGetString(MR.strings.error_changing_role), r)
        }
        throw Exception("failed to change member role: ${r.responseType} ${r.details}")
      }
    }

  suspend fun apiLeaveGroup(rh: Long?, groupId: Long): GroupInfo? {
    val r = sendCmd(rh, CC.ApiLeaveGroup(groupId))
    if (r is CR.LeftMemberUser) return r.groupInfo
    Log.e(TAG, "apiLeaveGroup bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiListMembers(rh: Long?, groupId: Long): List<GroupMember> {
    val r = sendCmd(rh, CC.ApiListMembers(groupId))
    if (r is CR.GroupMembers) return r.group.members
    Log.e(TAG, "apiListMembers bad response: ${r.responseType} ${r.details}")
    return emptyList()
  }

  suspend fun apiUpdateGroup(rh: Long?, groupId: Long, groupProfile: GroupProfile): GroupInfo? {
    return when (val r = sendCmd(rh, CC.ApiUpdateGroupProfile(groupId, groupProfile))) {
      is CR.GroupUpdated -> r.toGroup
      is CR.ChatCmdError -> {
        AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_saving_group_profile), "$r.chatError")
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

  suspend fun apiCreateGroupLink(rh: Long?, groupId: Long, memberRole: GroupMemberRole = GroupMemberRole.Member): Pair<String, GroupMemberRole>? {
    return when (val r = sendCmd(rh, CC.APICreateGroupLink(groupId, memberRole))) {
      is CR.GroupLinkCreated -> r.connReqContact to r.memberRole
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiCreateGroupLink", generalGetString(MR.strings.error_creating_link_for_group), r)
        }
        null
      }
    }
  }

  suspend fun apiGroupLinkMemberRole(rh: Long?, groupId: Long, memberRole: GroupMemberRole = GroupMemberRole.Member): Pair<String, GroupMemberRole>? {
    return when (val r = sendCmd(rh, CC.APIGroupLinkMemberRole(groupId, memberRole))) {
      is CR.GroupLink -> r.connReqContact to r.memberRole
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiGroupLinkMemberRole", generalGetString(MR.strings.error_updating_link_for_group), r)
        }
        null
      }
    }
  }

  suspend fun apiDeleteGroupLink(rh: Long?, groupId: Long): Boolean {
    return when (val r = sendCmd(rh, CC.APIDeleteGroupLink(groupId))) {
      is CR.GroupLinkDeleted -> true
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiDeleteGroupLink", generalGetString(MR.strings.error_deleting_link_for_group), r)
        }
        false
      }
    }
  }

  suspend fun apiGetGroupLink(rh: Long?, groupId: Long): Pair<String, GroupMemberRole>? {
    return when (val r = sendCmd(rh, CC.APIGetGroupLink(groupId))) {
      is CR.GroupLink -> r.connReqContact to r.memberRole
      else -> {
        Log.e(TAG, "apiGetGroupLink bad response: ${r.responseType} ${r.details}")
        null
      }
    }
  }

  suspend fun apiCreateMemberContact(rh: Long?, groupId: Long, groupMemberId: Long): Contact? {
    return when (val r = sendCmd(rh, CC.APICreateMemberContact(groupId, groupMemberId))) {
      is CR.NewMemberContact -> r.contact
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiCreateMemberContact", generalGetString(MR.strings.error_creating_member_contact), r)
        }
        null
      }
    }
  }

  suspend fun apiSendMemberContactInvitation(rh: Long?, contactId: Long, mc: MsgContent): Contact? {
    return when (val r = sendCmd(rh, CC.APISendMemberContactInvitation(contactId, mc))) {
      is CR.NewMemberContactSentInv -> r.contact
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiSendMemberContactInvitation", generalGetString(MR.strings.error_sending_message_contact_invitation), r)
        }
        null
      }
    }
  }

  suspend fun allowFeatureToContact(rh: Long?, contact: Contact, feature: ChatFeature, param: Int? = null) {
    val prefs = contact.mergedPreferences.toPreferences().setAllowed(feature, param = param)
    val toContact = apiSetContactPrefs(rh, contact.contactId, prefs)
    if (toContact != null) {
      chatModel.updateContact(rh, toContact)
    }
  }

  suspend fun setLocalDeviceName(displayName: String): Boolean = sendCommandOkResp(null, CC.SetLocalDeviceName(displayName))

  suspend fun listRemoteHosts(): List<RemoteHostInfo>? {
    val r = sendCmd(null, CC.ListRemoteHosts())
    if (r is CR.RemoteHostList) return r.remoteHosts
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
    if (r is CR.RemoteHostStarted) return r
    apiErrorAlert("startRemoteHost", generalGetString(MR.strings.error_alert_title), r)
    return null
  }

  suspend fun switchRemoteHost (rhId: Long?): RemoteHostInfo? {
    val r = sendCmd(null, CC.SwitchRemoteHost(rhId))
    if (r is CR.CurrentRemoteHost) return r.remoteHost_
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
    if (r is CR.RemoteFileStored) return r.remoteFileSource
    apiErrorAlert("storeRemoteFile", generalGetString(MR.strings.error_alert_title), r)
    return null
  }

  suspend fun getRemoteFile(rhId: Long, file: RemoteFile): Boolean = sendCmd(null, CC.GetRemoteFile(rhId, file)) is CR.CmdOk

  suspend fun connectRemoteCtrl(desktopAddress: String): Pair<SomeRemoteCtrl?, CR.ChatCmdError?> {
    val r = sendCmd(null, CC.ConnectRemoteCtrl(desktopAddress))
    return if (r is CR.RemoteCtrlConnecting) SomeRemoteCtrl(r.remoteCtrl_, r.ctrlAppInfo, r.appVersion) to null
    else if (r is CR.ChatCmdError) null to r
    else {
      apiErrorAlert("connectRemoteCtrl", generalGetString(MR.strings.error_alert_title), r)
      null to null
    }
  }

  suspend fun findKnownRemoteCtrl(): Boolean = sendCommandOkResp(null, CC.FindKnownRemoteCtrl())

  suspend fun confirmRemoteCtrl(rcId: Long): Pair<SomeRemoteCtrl?, CR.ChatCmdError?> {
    val r = sendCmd(null, CC.ConfirmRemoteCtrl(remoteCtrlId = rcId))
    return if (r is CR.RemoteCtrlConnecting) SomeRemoteCtrl(r.remoteCtrl_, r.ctrlAppInfo, r.appVersion) to null
    else if (r is CR.ChatCmdError) null to r
    else {
      apiErrorAlert("confirmRemoteCtrl", generalGetString(MR.strings.error_alert_title), r)
      null to null
    }
  }

  suspend fun verifyRemoteCtrlSession(sessionCode: String): RemoteCtrlInfo? {
    val r = sendCmd(null, CC.VerifyRemoteCtrlSession(sessionCode))
    if (r is CR.RemoteCtrlConnected) return r.remoteCtrl
    apiErrorAlert("verifyRemoteCtrlSession", generalGetString(MR.strings.error_alert_title), r)
    return null
  }

  suspend fun listRemoteCtrls(): List<RemoteCtrlInfo>? {
    val r = sendCmd(null, CC.ListRemoteCtrls())
    if (r is CR.RemoteCtrlList) return r.remoteCtrls
    apiErrorAlert("listRemoteCtrls", generalGetString(MR.strings.error_alert_title), r)
    return null
  }

  suspend fun stopRemoteCtrl(): Boolean = sendCommandOkResp(null, CC.StopRemoteCtrl())

  suspend fun deleteRemoteCtrl(rcId: Long): Boolean = sendCommandOkResp(null, CC.DeleteRemoteCtrl(rcId))

  private suspend fun sendCommandOkResp(rh: Long?, cmd: CC): Boolean {
    val r = sendCmd(rh, cmd)
    val ok = r is CR.CmdOk
    if (!ok) apiErrorAlert(cmd.cmdType, generalGetString(MR.strings.error_alert_title), r)
    return ok
  }

  suspend fun apiGetVersion(): CoreVersionInfo? {
    val r = sendCmd(null, CC.ShowVersion())
    return if (r is CR.VersionInfo) {
      r.versionInfo
    } else {
      Log.e(TAG, "apiGetVersion bad response: ${r.responseType} ${r.details}")
      null
    }
  }

  private fun networkErrorAlert(r: CR): Boolean {
    return when {
      r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorAgent
          && r.chatError.agentError is AgentErrorType.BROKER
          && r.chatError.agentError.brokerErr is BrokerErrorType.TIMEOUT -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.connection_timeout),
          String.format(generalGetString(MR.strings.network_error_desc), serverHostname(r.chatError.agentError.brokerAddress))
        )
        true
      }
      r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorAgent
          && r.chatError.agentError is AgentErrorType.BROKER
          && r.chatError.agentError.brokerErr is BrokerErrorType.NETWORK -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.connection_error),
          String.format(generalGetString(MR.strings.network_error_desc), serverHostname(r.chatError.agentError.brokerAddress))
        )
        true
      }
      else -> false
    }
  }

  private fun apiErrorAlert(method: String, title: String, r: CR) {
    val errMsg = "${r.responseType}: ${r.details}"
    Log.e(TAG, "$method bad response: $errMsg")
    AlertManager.shared.showAlertMsg(title, errMsg)
  }

  private suspend fun processReceivedMsg(apiResp: APIResponse) {
    lastMsgReceivedTimestamp = System.currentTimeMillis()
    val r = apiResp.resp
    val rhId = apiResp.remoteHostId
    fun active(user: UserLike): Boolean = activeUser(rhId, user)
    chatModel.addTerminalItem(TerminalItem.resp(rhId, r))
    when (r) {
      is CR.ContactDeletedByContact -> {
        if (active(r.user) && r.contact.directOrUsed) {
          chatModel.updateContact(rhId, r.contact)
        }
      }
      is CR.ContactConnected -> {
        if (active(r.user) && r.contact.directOrUsed) {
          chatModel.updateContact(rhId, r.contact)
          val conn = r.contact.activeConn
          if (conn != null) {
            chatModel.dismissConnReqView(conn.id)
            chatModel.removeChat(rhId, conn.id)
          }
        }
        if (r.contact.directOrUsed) {
          ntfManager.notifyContactConnected(r.user, r.contact)
        }
        chatModel.setContactNetworkStatus(r.contact, NetworkStatus.Connected())
      }
      is CR.ContactConnecting -> {
        if (active(r.user) && r.contact.directOrUsed) {
          chatModel.updateContact(rhId, r.contact)
          val conn = r.contact.activeConn
          if (conn != null) {
            chatModel.dismissConnReqView(conn.id)
            chatModel.removeChat(rhId, conn.id)
          }
        }
      }
      is CR.ReceivedContactRequest -> {
        val contactRequest = r.contactRequest
        val cInfo = ChatInfo.ContactRequest(contactRequest)
        if (active(r.user)) {
          if (chatModel.hasChat(rhId, contactRequest.id)) {
            chatModel.updateChatInfo(rhId, cInfo)
          } else {
            chatModel.addChat(Chat(remoteHostId = rhId, chatInfo = cInfo, chatItems = listOf()))
          }
        }
        ntfManager.notifyContactRequestReceived(r.user, cInfo)
      }
      is CR.ContactUpdated -> {
        if (active(r.user) && chatModel.hasChat(rhId, r.toContact.id)) {
          val cInfo = ChatInfo.Direct(r.toContact)
          chatModel.updateChatInfo(rhId, cInfo)
        }
      }
      is CR.GroupMemberUpdated -> {
        if (active(r.user)) {
          chatModel.upsertGroupMember(rhId, r.groupInfo, r.toMember)
        }
      }
      is CR.ContactsMerged -> {
        if (active(r.user) && chatModel.hasChat(rhId, r.mergedContact.id)) {
          if (chatModel.chatId.value == r.mergedContact.id) {
            chatModel.chatId.value = r.intoContact.id
          }
          chatModel.removeChat(rhId, r.mergedContact.id)
        }
      }
      is CR.ContactsSubscribed -> updateContactsStatus(r.contactRefs, NetworkStatus.Connected())
      is CR.ContactsDisconnected -> updateContactsStatus(r.contactRefs, NetworkStatus.Disconnected())
      is CR.ContactSubSummary -> {
        for (sub in r.contactSubscriptions) {
          if (active(r.user)) {
            chatModel.updateContact(rhId, sub.contact)
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
      is CR.NewChatItem -> {
        val cInfo = r.chatItem.chatInfo
        val cItem = r.chatItem.chatItem
        if (active(r.user)) {
          chatModel.addChatItem(rhId, cInfo, cItem)
        } else if (cItem.isRcvNew && cInfo.ntfsEnabled) {
          chatModel.increaseUnreadCounter(rhId, r.user)
        }
        val file = cItem.file
        val mc = cItem.content.msgContent
        if (file != null &&
            appPrefs.privacyAcceptImages.get() &&
            ((mc is MsgContent.MCImage && file.fileSize <= MAX_IMAGE_SIZE_AUTO_RCV)
                || (mc is MsgContent.MCVideo && file.fileSize <= MAX_VIDEO_SIZE_AUTO_RCV)
                || (mc is MsgContent.MCVoice && file.fileSize <= MAX_VOICE_SIZE_AUTO_RCV && file.fileStatus !is CIFileStatus.RcvAccepted))) {
          withApi { receiveFile(rhId, r.user, file.fileId, encrypted = cItem.encryptLocalFile && chatController.appPrefs.privacyEncryptLocalFiles.get(), auto = true) }
        }
        if (cItem.showNotification && (allowedToShowNotification() || chatModel.chatId.value != cInfo.id || chatModel.remoteHostId() != rhId)) {
          ntfManager.notifyMessageReceived(r.user, cInfo, cItem)
        }
      }
      is CR.ChatItemStatusUpdated -> {
        val cInfo = r.chatItem.chatInfo
        val cItem = r.chatItem.chatItem
        if (!cItem.isDeletedContent && active(r.user)) {
          chatModel.updateChatItem(cInfo, cItem, status = cItem.meta.itemStatus)
        }
      }
      is CR.ChatItemUpdated ->
        chatItemSimpleUpdate(rhId, r.user, r.chatItem)
      is CR.ChatItemReaction -> {
        if (active(r.user)) {
          chatModel.updateChatItem(r.reaction.chatInfo, r.reaction.chatReaction.chatItem)
        }
      }
      is CR.ChatItemDeleted -> {
        if (!active(r.user)) {
          if (r.toChatItem == null && r.deletedChatItem.chatItem.isRcvNew && r.deletedChatItem.chatInfo.ntfsEnabled) {
            chatModel.decreaseUnreadCounter(rhId, r.user)
          }
          return
        }

        val cInfo = r.deletedChatItem.chatInfo
        val cItem = r.deletedChatItem.chatItem
        AudioPlayer.stop(cItem)
        val isLastChatItem = chatModel.getChat(cInfo.id)?.chatItems?.lastOrNull()?.id == cItem.id
        if (isLastChatItem && ntfManager.hasNotificationsForChat(cInfo.id)) {
          ntfManager.cancelNotificationsForChat(cInfo.id)
          ntfManager.displayNotification(
            r.user,
            cInfo.id,
            cInfo.displayName,
            generalGetString(if (r.toChatItem != null) MR.strings.marked_deleted_description else MR.strings.deleted_description)
          )
        }
        if (r.toChatItem == null) {
          chatModel.removeChatItem(rhId, cInfo, cItem)
        } else {
          chatModel.upsertChatItem(rhId, cInfo, r.toChatItem.chatItem)
        }
      }
      is CR.ReceivedGroupInvitation -> {
        if (active(r.user)) {
          chatModel.updateGroup(rhId, r.groupInfo) // update so that repeat group invitations are not duplicated
          // TODO NtfManager.shared.notifyGroupInvitation
        }
      }
      is CR.UserAcceptedGroupSent -> {
        if (!active(r.user)) return

        chatModel.updateGroup(rhId, r.groupInfo)
        val conn = r.hostContact?.activeConn
        if (conn != null) {
          chatModel.dismissConnReqView(conn.id)
          chatModel.removeChat(rhId, conn.id)
        }
      }
      is CR.GroupLinkConnecting -> {
        if (!active(r.user)) return

        chatModel.updateGroup(rhId, r.groupInfo)
        val hostConn = r.hostMember.activeConn
        if (hostConn != null) {
          chatModel.dismissConnReqView(hostConn.id)
          chatModel.removeChat(rhId, hostConn.id)
        }
      }
      is CR.JoinedGroupMemberConnecting ->
        if (active(r.user)) {
          chatModel.upsertGroupMember(rhId, r.groupInfo, r.member)
        }
      is CR.DeletedMemberUser -> // TODO update user member
        if (active(r.user)) {
          chatModel.updateGroup(rhId, r.groupInfo)
        }
      is CR.DeletedMember ->
        if (active(r.user)) {
          chatModel.upsertGroupMember(rhId, r.groupInfo, r.deletedMember)
        }
      is CR.LeftMember ->
        if (active(r.user)) {
          chatModel.upsertGroupMember(rhId, r.groupInfo, r.member)
        }
      is CR.MemberRole ->
        if (active(r.user)) {
          chatModel.upsertGroupMember(rhId, r.groupInfo, r.member)
        }
      is CR.MemberRoleUser ->
        if (active(r.user)) {
          chatModel.upsertGroupMember(rhId, r.groupInfo, r.member)
        }
      is CR.GroupDeleted -> // TODO update user member
        if (active(r.user)) {
          chatModel.updateGroup(rhId, r.groupInfo)
        }
      is CR.UserJoinedGroup ->
        if (active(r.user)) {
          chatModel.updateGroup(rhId, r.groupInfo)
        }
      is CR.JoinedGroupMember ->
        if (active(r.user)) {
          chatModel.upsertGroupMember(rhId, r.groupInfo, r.member)
        }
      is CR.ConnectedToGroupMember -> {
        if (active(r.user)) {
          chatModel.upsertGroupMember(rhId, r.groupInfo, r.member)
        }
        if (r.memberContact != null) {
          chatModel.setContactNetworkStatus(r.memberContact, NetworkStatus.Connected())
        }
      }
      is CR.GroupUpdated ->
        if (active(r.user)) {
          chatModel.updateGroup(rhId, r.toGroup)
        }
      is CR.NewMemberContactReceivedInv ->
        if (active(r.user)) {
          chatModel.updateContact(rhId, r.contact)
        }
      is CR.RcvFileStart ->
        chatItemSimpleUpdate(rhId, r.user, r.chatItem)
      is CR.RcvFileComplete ->
        chatItemSimpleUpdate(rhId, r.user, r.chatItem)
      is CR.RcvFileSndCancelled -> {
        chatItemSimpleUpdate(rhId, r.user, r.chatItem)
        cleanupFile(r.chatItem)
      }
      is CR.RcvFileProgressXFTP ->
        chatItemSimpleUpdate(rhId, r.user, r.chatItem)
      is CR.RcvFileError -> {
        chatItemSimpleUpdate(rhId, r.user, r.chatItem)
        cleanupFile(r.chatItem)
      }
      is CR.SndFileStart ->
        chatItemSimpleUpdate(rhId, r.user, r.chatItem)
      is CR.SndFileComplete -> {
        chatItemSimpleUpdate(rhId, r.user, r.chatItem)
        cleanupDirectFile(r.chatItem)
      }
      is CR.SndFileRcvCancelled -> {
        chatItemSimpleUpdate(rhId, r.user, r.chatItem)
        cleanupDirectFile(r.chatItem)
      }
      is CR.SndFileProgressXFTP ->
        chatItemSimpleUpdate(rhId, r.user, r.chatItem)
      is CR.SndFileCompleteXFTP -> {
        chatItemSimpleUpdate(rhId, r.user, r.chatItem)
        cleanupFile(r.chatItem)
      }
      is CR.SndFileError -> {
        chatItemSimpleUpdate(rhId, r.user, r.chatItem)
        cleanupFile(r.chatItem)
      }
      is CR.CallInvitation -> {
        chatModel.callManager.reportNewIncomingCall(r.callInvitation.copy(remoteHostId = rhId))
      }
      is CR.CallOffer -> {
        // TODO askConfirmation?
        // TODO check encryption is compatible
        withCall(r, r.contact) { call ->
          chatModel.activeCall.value = call.copy(callState = CallState.OfferReceived, peerMedia = r.callType.media, sharedKey = r.sharedKey)
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
        withCall(r, r.contact) { _ ->
          chatModel.callCommand.add(WCallCommand.End)
          withApi {
            chatModel.activeCall.value = null
            chatModel.showCallView.value = false
          }
        }
      }
      is CR.ContactSwitch ->
        chatModel.updateContactConnectionStats(rhId, r.contact, r.switchProgress.connectionStats)
      is CR.GroupMemberSwitch ->
        chatModel.updateGroupMemberConnectionStats(rhId, r.groupInfo, r.member, r.switchProgress.connectionStats)
      is CR.ContactRatchetSync ->
        chatModel.updateContactConnectionStats(rhId, r.contact, r.ratchetSyncProgress.connectionStats)
      is CR.GroupMemberRatchetSync ->
        chatModel.updateGroupMemberConnectionStats(rhId, r.groupInfo, r.member, r.ratchetSyncProgress.connectionStats)
      is CR.RemoteHostSessionCode -> {
        chatModel.remoteHostPairing.value = r.remoteHost_ to RemoteHostSessionState.PendingConfirmation(r.sessionCode)
      }
      is CR.RemoteHostConnected -> {
        // TODO needs to update it instead in sessions
        chatModel.currentRemoteHost.value = r.remoteHost
        switchUIRemoteHost(r.remoteHost.remoteHostId)
      }
      is CR.RemoteHostStopped -> {
        val disconnectedHost = chatModel.remoteHosts.firstOrNull { it.remoteHostId == r.remoteHostId_ }
        chatModel.remoteHostPairing.value = null
        if (disconnectedHost != null) {
          showToast(
            generalGetString(MR.strings.remote_host_was_disconnected_toast).format(disconnectedHost.hostDeviceName.ifEmpty { disconnectedHost.remoteHostId.toString() })
          )
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
          if (sess.sessionState is UIRemoteCtrlSessionState.Connected) {
            switchToLocalSession()
          }
        }
      }
      else ->
        Log.d(TAG , "unsupported event: ${r.responseType}")
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

  fun switchToLocalSession() {
    val m = chatModel
    m.remoteCtrlSession.value = null
    withBGApi {
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

  suspend fun receiveFile(rhId: Long?, user: UserLike, fileId: Long, encrypted: Boolean, auto: Boolean = false) {
    val chatItem = apiReceiveFile(rhId, fileId, encrypted = encrypted, auto = auto)
    if (chatItem != null) {
      chatItemSimpleUpdate(rhId, user, chatItem)
    }
  }

  suspend fun leaveGroup(rh: Long?, groupId: Long) {
    val groupInfo = apiLeaveGroup(rh, groupId)
    if (groupInfo != null) {
      chatModel.updateGroup(rh, groupInfo)
    }
  }

  private suspend fun chatItemSimpleUpdate(rh: Long?, user: UserLike, aChatItem: AChatItem) {
    val cInfo = aChatItem.chatInfo
    val cItem = aChatItem.chatItem
    val notify = { ntfManager.notifyMessageReceived(user, cInfo, cItem) }
    if (!activeUser(rh, user)) {
      notify()
    } else if (chatModel.upsertChatItem(rh, cInfo, cItem)) {
      notify()
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
      chatModel.chatItems.clear()
      chatModel.chats.clear()
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

  fun getXFTPCfg(): XFTPFileConfig {
    return XFTPFileConfig(minFileSize = 0)
  }

  fun getNetCfg(): NetCfg {
    val useSocksProxy = appPrefs.networkUseSocksProxy.get()
    val proxyHostPort  = appPrefs.networkProxyHostPort.get()
    val socksProxy = if (useSocksProxy) {
      if (proxyHostPort?.startsWith("localhost:") == true) {
        proxyHostPort.removePrefix("localhost")
      } else {
        proxyHostPort ?: ":9050"
      }
    } else {
      null
    }
    val hostMode = HostMode.valueOf(appPrefs.networkHostMode.get()!!)
    val requiredHostMode = appPrefs.networkRequiredHostMode.get()
    val sessionMode = appPrefs.networkSessionMode.get()
    val tcpConnectTimeout = appPrefs.networkTCPConnectTimeout.get()
    val tcpTimeout = appPrefs.networkTCPTimeout.get()
    val tcpTimeoutPerKb = appPrefs.networkTCPTimeoutPerKb.get()
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
      tcpConnectTimeout = tcpConnectTimeout,
      tcpTimeout = tcpTimeout,
      tcpTimeoutPerKb = tcpTimeoutPerKb,
      tcpKeepAlive = tcpKeepAlive,
      smpPingInterval = smpPingInterval,
      smpPingCount = smpPingCount
    )
  }

  /**
   * [AppPreferences.networkProxyHostPort] is not changed here, use appPrefs to set it
   * */
  fun setNetCfg(cfg: NetCfg) {
    appPrefs.networkUseSocksProxy.set(cfg.useSocksProxy)
    appPrefs.networkHostMode.set(cfg.hostMode.name)
    appPrefs.networkRequiredHostMode.set(cfg.requiredHostMode)
    appPrefs.networkSessionMode.set(cfg.sessionMode)
    appPrefs.networkTCPConnectTimeout.set(cfg.tcpConnectTimeout)
    appPrefs.networkTCPTimeout.set(cfg.tcpTimeout)
    appPrefs.networkTCPTimeoutPerKb.set(cfg.tcpTimeoutPerKb)
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
  private val _state: MutableState<T> by lazy { mutableStateOf(get()) }
  val state: State<T> by lazy { _state }

  init {
    this.set = { value ->
      set(value)
      _state.value = value
    }
  }
}

// ChatCommand
sealed class CC {
  class Console(val cmd: String): CC()
  class ShowActiveUser: CC()
  class CreateActiveUser(val profile: Profile?, val sameServers: Boolean, val pastTimestamp: Boolean): CC()
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
  class StartChat(val expire: Boolean): CC()
  class ApiStopChat: CC()
  class SetTempFolder(val tempFolder: String): CC()
  class SetFilesFolder(val filesFolder: String): CC()
  class SetRemoteHostsFolder(val remoteHostsFolder: String): CC()
  class ApiSetXFTPConfig(val config: XFTPFileConfig?): CC()
  class ApiSetEncryptLocalFiles(val enable: Boolean): CC()
  class ApiExportArchive(val config: ArchiveConfig): CC()
  class ApiImportArchive(val config: ArchiveConfig): CC()
  class ApiDeleteStorage: CC()
  class ApiStorageEncryption(val config: DBEncryptionConfig): CC()
  class ApiGetChats(val userId: Long): CC()
  class ApiGetChat(val type: ChatType, val id: Long, val pagination: ChatPagination, val search: String = ""): CC()
  class ApiGetChatItemInfo(val type: ChatType, val id: Long, val itemId: Long): CC()
  class ApiSendMessage(val type: ChatType, val id: Long, val file: CryptoFile?, val quotedItemId: Long?, val mc: MsgContent, val live: Boolean, val ttl: Int?): CC()
  class ApiUpdateChatItem(val type: ChatType, val id: Long, val itemId: Long, val mc: MsgContent, val live: Boolean): CC()
  class ApiDeleteChatItem(val type: ChatType, val id: Long, val itemId: Long, val mode: CIDeleteMode): CC()
  class ApiDeleteMemberChatItem(val groupId: Long, val groupMemberId: Long, val itemId: Long): CC()
  class ApiChatItemReaction(val type: ChatType, val id: Long, val itemId: Long, val add: Boolean, val reaction: MsgReaction): CC()
  class ApiNewGroup(val userId: Long, val incognito: Boolean, val groupProfile: GroupProfile): CC()
  class ApiAddMember(val groupId: Long, val contactId: Long, val memberRole: GroupMemberRole): CC()
  class ApiJoinGroup(val groupId: Long): CC()
  class ApiMemberRole(val groupId: Long, val memberId: Long, val memberRole: GroupMemberRole): CC()
  class ApiRemoveMember(val groupId: Long, val memberId: Long): CC()
  class ApiLeaveGroup(val groupId: Long): CC()
  class ApiListMembers(val groupId: Long): CC()
  class ApiUpdateGroupProfile(val groupId: Long, val groupProfile: GroupProfile): CC()
  class APICreateGroupLink(val groupId: Long, val memberRole: GroupMemberRole): CC()
  class APIGroupLinkMemberRole(val groupId: Long, val memberRole: GroupMemberRole): CC()
  class APIDeleteGroupLink(val groupId: Long): CC()
  class APIGetGroupLink(val groupId: Long): CC()
  class APICreateMemberContact(val groupId: Long, val groupMemberId: Long): CC()
  class APISendMemberContactInvitation(val contactId: Long, val mc: MsgContent): CC()
  class APIGetUserProtoServers(val userId: Long, val serverProtocol: ServerProtocol): CC()
  class APISetUserProtoServers(val userId: Long, val serverProtocol: ServerProtocol, val servers: List<ServerCfg>): CC()
  class APITestProtoServer(val userId: Long, val server: String): CC()
  class APISetChatItemTTL(val userId: Long, val seconds: Long?): CC()
  class APIGetChatItemTTL(val userId: Long): CC()
  class APISetNetworkConfig(val networkConfig: NetCfg): CC()
  class APIGetNetworkConfig: CC()
  class APISetChatSettings(val type: ChatType, val id: Long, val chatSettings: ChatSettings): CC()
  class ApiSetMemberSettings(val groupId: Long, val groupMemberId: Long, val memberSettings: GroupMemberSettings): CC()
  class APIContactInfo(val contactId: Long): CC()
  class APIGroupMemberInfo(val groupId: Long, val groupMemberId: Long): CC()
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
  class APIAddContact(val userId: Long, val incognito: Boolean): CC()
  class ApiSetConnectionIncognito(val connId: Long, val incognito: Boolean): CC()
  class APIConnectPlan(val userId: Long, val connReq: String): CC()
  class APIConnect(val userId: Long, val incognito: Boolean, val connReq: String): CC()
  class ApiConnectContactViaAddress(val userId: Long, val incognito: Boolean, val contactId: Long): CC()
  class ApiDeleteChat(val type: ChatType, val id: Long, val notify: Boolean?): CC()
  class ApiClearChat(val type: ChatType, val id: Long): CC()
  class ApiListContacts(val userId: Long): CC()
  class ApiUpdateProfile(val userId: Long, val profile: Profile): CC()
  class ApiSetContactPrefs(val contactId: Long, val prefs: ChatPreferences): CC()
  class ApiSetContactAlias(val contactId: Long, val localAlias: String): CC()
  class ApiSetConnectionAlias(val connId: Long, val localAlias: String): CC()
  class ApiCreateMyAddress(val userId: Long): CC()
  class ApiDeleteMyAddress(val userId: Long): CC()
  class ApiShowMyAddress(val userId: Long): CC()
  class ApiSetProfileAddress(val userId: Long, val on: Boolean): CC()
  class ApiAddressAutoAccept(val userId: Long, val autoAccept: AutoAccept?): CC()
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
  class ApiChatRead(val type: ChatType, val id: Long, val range: ItemRange): CC()
  class ApiChatUnread(val type: ChatType, val id: Long, val unreadChat: Boolean): CC()
  class ReceiveFile(val fileId: Long, val encrypt: Boolean, val inline: Boolean?): CC()
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
  // misc
  class ShowVersion(): CC()

  val cmdString: String get() = when (this) {
    is Console -> cmd
    is ShowActiveUser -> "/u"
    is CreateActiveUser -> {
      val user = NewUser(profile, sameServers = sameServers, pastTimestamp = pastTimestamp)
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
    is StartChat -> "/_start subscribe=on expire=${onOff(expire)} xftp=on"
    is ApiStopChat -> "/_stop"
    is SetTempFolder -> "/_temp_folder $tempFolder"
    is SetFilesFolder -> "/_files_folder $filesFolder"
    is SetRemoteHostsFolder -> "/remote_hosts_folder $remoteHostsFolder"
    is ApiSetXFTPConfig -> if (config != null) "/_xftp on ${json.encodeToString(config)}" else "/_xftp off"
    is ApiSetEncryptLocalFiles -> "/_files_encrypt ${onOff(enable)}"
    is ApiExportArchive -> "/_db export ${json.encodeToString(config)}"
    is ApiImportArchive -> "/_db import ${json.encodeToString(config)}"
    is ApiDeleteStorage -> "/_db delete"
    is ApiStorageEncryption -> "/_db encryption ${json.encodeToString(config)}"
    is ApiGetChats -> "/_get chats $userId pcc=on"
    is ApiGetChat -> "/_get chat ${chatRef(type, id)} ${pagination.cmdString}" + (if (search == "") "" else " search=$search")
    is ApiGetChatItemInfo -> "/_get item info ${chatRef(type, id)} $itemId"
    is ApiSendMessage -> {
      val ttlStr = if (ttl != null) "$ttl" else "default"
      "/_send ${chatRef(type, id)} live=${onOff(live)} ttl=${ttlStr} json ${json.encodeToString(ComposedMessage(file, quotedItemId, mc))}"
    }
    is ApiUpdateChatItem -> "/_update item ${chatRef(type, id)} $itemId live=${onOff(live)} ${mc.cmdString}"
    is ApiDeleteChatItem -> "/_delete item ${chatRef(type, id)} $itemId ${mode.deleteMode}"
    is ApiDeleteMemberChatItem -> "/_delete member item #$groupId $groupMemberId $itemId"
    is ApiChatItemReaction -> "/_reaction ${chatRef(type, id)} $itemId ${onOff(add)} ${json.encodeToString(reaction)}"
    is ApiNewGroup -> "/_group $userId incognito=${onOff(incognito)} ${json.encodeToString(groupProfile)}"
    is ApiAddMember -> "/_add #$groupId $contactId ${memberRole.memberRole}"
    is ApiJoinGroup -> "/_join #$groupId"
    is ApiMemberRole -> "/_member role #$groupId $memberId ${memberRole.memberRole}"
    is ApiRemoveMember -> "/_remove #$groupId $memberId"
    is ApiLeaveGroup -> "/_leave #$groupId"
    is ApiListMembers -> "/_members #$groupId"
    is ApiUpdateGroupProfile -> "/_group_profile #$groupId ${json.encodeToString(groupProfile)}"
    is APICreateGroupLink -> "/_create link #$groupId ${memberRole.name.lowercase()}"
    is APIGroupLinkMemberRole -> "/_set link role #$groupId ${memberRole.name.lowercase()}"
    is APIDeleteGroupLink -> "/_delete link #$groupId"
    is APIGetGroupLink -> "/_get link #$groupId"
    is APICreateMemberContact -> "/_create member contact #$groupId $groupMemberId"
    is APISendMemberContactInvitation -> "/_invite member contact @$contactId ${mc.cmdString}"
    is APIGetUserProtoServers -> "/_servers $userId ${serverProtocol.name.lowercase()}"
    is APISetUserProtoServers -> "/_servers $userId ${serverProtocol.name.lowercase()} ${protoServersStr(servers)}"
    is APITestProtoServer -> "/_server test $userId $server"
    is APISetChatItemTTL -> "/_ttl $userId ${chatItemTTLStr(seconds)}"
    is APIGetChatItemTTL -> "/_ttl $userId"
    is APISetNetworkConfig -> "/_network ${json.encodeToString(networkConfig)}"
    is APIGetNetworkConfig -> "/network"
    is APISetChatSettings -> "/_settings ${chatRef(type, id)} ${json.encodeToString(chatSettings)}"
    is ApiSetMemberSettings -> "/_member settings #$groupId $groupMemberId ${json.encodeToString(memberSettings)}"
    is APIContactInfo -> "/_info @$contactId"
    is APIGroupMemberInfo -> "/_info #$groupId $groupMemberId"
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
    is APIAddContact -> "/_connect $userId incognito=${onOff(incognito)}"
    is ApiSetConnectionIncognito -> "/_set incognito :$connId ${onOff(incognito)}"
    is APIConnectPlan -> "/_connect plan $userId $connReq"
    is APIConnect -> "/_connect $userId incognito=${onOff(incognito)} $connReq"
    is ApiConnectContactViaAddress -> "/_connect contact $userId incognito=${onOff(incognito)} $contactId"
    is ApiDeleteChat -> if (notify != null) {
      "/_delete ${chatRef(type, id)} notify=${onOff(notify)}"
    } else {
      "/_delete ${chatRef(type, id)}"
    }
    is ApiClearChat -> "/_clear chat ${chatRef(type, id)}"
    is ApiListContacts -> "/_contacts $userId"
    is ApiUpdateProfile -> "/_profile $userId ${json.encodeToString(profile)}"
    is ApiSetContactPrefs -> "/_set prefs @$contactId ${json.encodeToString(prefs)}"
    is ApiSetContactAlias -> "/_set alias @$contactId ${localAlias.trim()}"
    is ApiSetConnectionAlias -> "/_set alias :$connId ${localAlias.trim()}"
    is ApiCreateMyAddress -> "/_address $userId"
    is ApiDeleteMyAddress -> "/_delete_address $userId"
    is ApiShowMyAddress -> "/_show_address $userId"
    is ApiSetProfileAddress -> "/_profile_address $userId ${onOff(on)}"
    is ApiAddressAutoAccept -> "/_auto_accept $userId ${AutoAccept.cmdString(autoAccept)}"
    is ApiAcceptContact -> "/_accept incognito=${onOff(incognito)} $contactReqId"
    is ApiRejectContact -> "/_reject $contactReqId"
    is ApiSendCallInvitation -> "/_call invite @${contact.apiId} ${json.encodeToString(callType)}"
    is ApiRejectCall -> "/_call reject @${contact.apiId}"
    is ApiSendCallOffer -> "/_call offer @${contact.apiId} ${json.encodeToString(callOffer)}"
    is ApiSendCallAnswer -> "/_call answer @${contact.apiId} ${json.encodeToString(answer)}"
    is ApiSendCallExtraInfo -> "/_call extra @${contact.apiId} ${json.encodeToString(extraInfo)}"
    is ApiEndCall -> "/_call end @${contact.apiId}"
    is ApiCallStatus -> "/_call status @${contact.apiId} ${callStatus.value}"
    is ApiGetNetworkStatuses -> "/_network_statuses"
    is ApiChatRead -> "/_read chat ${chatRef(type, id)} from=${range.from} to=${range.to}"
    is ApiChatUnread -> "/_unread chat ${chatRef(type, id)} ${onOff(unreadChat)}"
    is ReceiveFile ->
      "/freceive $fileId" +
          (if (encrypt == null) "" else " encrypt=${onOff(encrypt)}") +
          (if (inline == null) "" else " inline=${onOff(inline)}")
    is CancelFile -> "/fcancel $fileId"
    is SetLocalDeviceName -> "/set device name $displayName"
    is ListRemoteHosts -> "/list remote hosts"
    is StartRemoteHost -> "/start remote host " + (if (remoteHostId == null) "new" else "$remoteHostId multicast=${onOff(multicast)}") + (if (address != null) " addr=${address.address} iface=${address.`interface`}" else "") + (if (port != null) " port=$port" else "")
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
    is ShowVersion -> "/version"
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
    is ApiStopChat -> "apiStopChat"
    is SetTempFolder -> "setTempFolder"
    is SetFilesFolder -> "setFilesFolder"
    is SetRemoteHostsFolder -> "setRemoteHostsFolder"
    is ApiSetXFTPConfig -> "apiSetXFTPConfig"
    is ApiSetEncryptLocalFiles -> "apiSetEncryptLocalFiles"
    is ApiExportArchive -> "apiExportArchive"
    is ApiImportArchive -> "apiImportArchive"
    is ApiDeleteStorage -> "apiDeleteStorage"
    is ApiStorageEncryption -> "apiStorageEncryption"
    is ApiGetChats -> "apiGetChats"
    is ApiGetChat -> "apiGetChat"
    is ApiGetChatItemInfo -> "apiGetChatItemInfo"
    is ApiSendMessage -> "apiSendMessage"
    is ApiUpdateChatItem -> "apiUpdateChatItem"
    is ApiDeleteChatItem -> "apiDeleteChatItem"
    is ApiDeleteMemberChatItem -> "apiDeleteMemberChatItem"
    is ApiChatItemReaction -> "apiChatItemReaction"
    is ApiNewGroup -> "apiNewGroup"
    is ApiAddMember -> "apiAddMember"
    is ApiJoinGroup -> "apiJoinGroup"
    is ApiMemberRole -> "apiMemberRole"
    is ApiRemoveMember -> "apiRemoveMember"
    is ApiLeaveGroup -> "apiLeaveGroup"
    is ApiListMembers -> "apiListMembers"
    is ApiUpdateGroupProfile -> "apiUpdateGroupProfile"
    is APICreateGroupLink -> "apiCreateGroupLink"
    is APIGroupLinkMemberRole -> "apiGroupLinkMemberRole"
    is APIDeleteGroupLink -> "apiDeleteGroupLink"
    is APIGetGroupLink -> "apiGetGroupLink"
    is APICreateMemberContact -> "apiCreateMemberContact"
    is APISendMemberContactInvitation -> "apiSendMemberContactInvitation"
    is APIGetUserProtoServers -> "apiGetUserProtoServers"
    is APISetUserProtoServers -> "apiSetUserProtoServers"
    is APITestProtoServer -> "testProtoServer"
    is APISetChatItemTTL -> "apiSetChatItemTTL"
    is APIGetChatItemTTL -> "apiGetChatItemTTL"
    is APISetNetworkConfig -> "apiSetNetworkConfig"
    is APIGetNetworkConfig -> "apiGetNetworkConfig"
    is APISetChatSettings -> "apiSetChatSettings"
    is ApiSetMemberSettings -> "apiSetMemberSettings"
    is APIContactInfo -> "apiContactInfo"
    is APIGroupMemberInfo -> "apiGroupMemberInfo"
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
    is APIConnectPlan -> "apiConnectPlan"
    is APIConnect -> "apiConnect"
    is ApiConnectContactViaAddress -> "apiConnectContactViaAddress"
    is ApiDeleteChat -> "apiDeleteChat"
    is ApiClearChat -> "apiClearChat"
    is ApiListContacts -> "apiListContacts"
    is ApiUpdateProfile -> "apiUpdateProfile"
    is ApiSetContactPrefs -> "apiSetContactPrefs"
    is ApiSetContactAlias -> "apiSetContactAlias"
    is ApiSetConnectionAlias -> "apiSetConnectionAlias"
    is ApiCreateMyAddress -> "apiCreateMyAddress"
    is ApiDeleteMyAddress -> "apiDeleteMyAddress"
    is ApiShowMyAddress -> "apiShowMyAddress"
    is ApiSetProfileAddress -> "apiSetProfileAddress"
    is ApiAddressAutoAccept -> "apiAddressAutoAccept"
    is ApiAcceptContact -> "apiAcceptContact"
    is ApiRejectContact -> "apiRejectContact"
    is ApiSendCallInvitation -> "apiSendCallInvitation"
    is ApiRejectCall -> "apiRejectCall"
    is ApiSendCallOffer -> "apiSendCallOffer"
    is ApiSendCallAnswer -> "apiSendCallAnswer"
    is ApiSendCallExtraInfo -> "apiSendCallExtraInfo"
    is ApiEndCall -> "apiEndCall"
    is ApiCallStatus -> "apiCallStatus"
    is ApiGetNetworkStatuses -> "apiGetNetworkStatuses"
    is ApiChatRead -> "apiChatRead"
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
    is ShowVersion -> "showVersion"
  }

  class ItemRange(val from: Long, val to: Long)

  fun chatItemTTLStr(seconds: Long?): String {
    if (seconds == null) return "none"
    return seconds.toString()
  }

  val obfuscated: CC
    get() = when (this) {
      is ApiStorageEncryption -> ApiStorageEncryption(DBEncryptionConfig(obfuscate(config.currentKey), obfuscate(config.newKey)))
      is ApiSetActiveUser -> ApiSetActiveUser(userId, obfuscateOrNull(viewPwd))
      is ApiHideUser -> ApiHideUser(userId, obfuscate(viewPwd))
      is ApiUnhideUser -> ApiUnhideUser(userId, obfuscate(viewPwd))
      is ApiDeleteUser -> ApiDeleteUser(userId, delSMPQueues, obfuscateOrNull(viewPwd))
      else -> this
    }

  private fun obfuscate(s: String): String = if (s.isEmpty()) "" else "***"

  private fun obfuscateOrNull(s: String?): String? =
    if (s != null) {
      obfuscate(s)
    } else {
      null
    }

  private fun onOff(b: Boolean): String = if (b) "on" else "off"

  private fun maybePwd(pwd: String?): String = if (pwd == "" || pwd == null) "" else " " + json.encodeToString(pwd)

  companion object {
    fun chatRef(chatType: ChatType, id: Long) = "${chatType.type}${id}"

    fun protoServersStr(servers: List<ServerCfg>) = json.encodeToString(ProtoServersConfig(servers))
  }
}

@Serializable
data class NewUser(
  val profile: Profile?,
  val sameServers: Boolean,
  val pastTimestamp: Boolean
)

sealed class ChatPagination {
  class Last(val count: Int): ChatPagination()
  class After(val chatItemId: Long, val count: Int): ChatPagination()
  class Before(val chatItemId: Long, val count: Int): ChatPagination()

  val cmdString: String get() = when (this) {
    is Last -> "count=${this.count}"
    is After -> "after=${this.chatItemId} count=${this.count}"
    is Before -> "before=${this.chatItemId} count=${this.count}"
  }

  companion object {
    const val INITIAL_COUNT = 100
    const val PRELOAD_COUNT = 100
    const val UNTIL_PRELOAD_COUNT = 50
  }
}

@Serializable
class ComposedMessage(val fileSource: CryptoFile?, val quotedItemId: Long?, val msgContent: MsgContent)

@Serializable
class XFTPFileConfig(val minFileSize: Long)

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
data class ProtoServersConfig(
  val servers: List<ServerCfg>
)

@Serializable
data class UserProtocolServers(
  val serverProtocol: ServerProtocol,
  val protoServers: List<ServerCfg>,
  val presetServers: List<String>,
)

@Serializable
data class ServerCfg(
  val remoteHostId: Long?,
  val server: String,
  val preset: Boolean,
  val tested: Boolean? = null,
  val enabled: Boolean
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
    val empty = ServerCfg(remoteHostId = null, server = "", preset = false, tested = null, enabled = true)

    class SampleData(
      val preset: ServerCfg,
      val custom: ServerCfg,
      val untested: ServerCfg
    )

    val sampleData = SampleData(
      preset = ServerCfg(
        remoteHostId = null,
        server = "smp://abcd@smp8.simplex.im",
        preset = true,
        tested = true,
        enabled = true
      ),
      custom = ServerCfg(
        remoteHostId = null,
        server = "smp://abcd@smp9.simplex.im",
        preset = false,
        tested = false,
        enabled = false
      ),
      untested = ServerCfg(
        remoteHostId = null,
        server = "smp://abcd@smp10.simplex.im",
        preset = false,
        tested = null,
        enabled = true
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
  val hostMode: HostMode,
  val requiredHostMode: Boolean,
  val sessionMode: TransportSessionMode,
  val tcpConnectTimeout: Long, // microseconds
  val tcpTimeout: Long, // microseconds
  val tcpTimeoutPerKb: Long, // microseconds
  val tcpKeepAlive: KeepAliveOpts?,
  val smpPingInterval: Long, // microseconds
  val smpPingCount: Int,
  val logTLSErrors: Boolean = false
) {
  val useSocksProxy: Boolean get() = socksProxy != null
  val enableKeepAlive: Boolean get() = tcpKeepAlive != null

  fun withHostPort(hostPort: String?, default: String? = ":9050"): NetCfg {
    val socksProxy = if (hostPort?.startsWith("localhost:") == true) {
      hostPort.removePrefix("localhost")
    } else {
      hostPort ?: default
    }
    return copy(socksProxy = socksProxy)
  }

  companion object {
    val defaults: NetCfg =
      NetCfg(
        socksProxy = null,
        hostMode = HostMode.OnionViaSocks,
        requiredHostMode = false,
        sessionMode = TransportSessionMode.User,
        tcpConnectTimeout = 20_000_000,
        tcpTimeout = 15_000_000,
        tcpTimeoutPerKb = 45_000,
        tcpKeepAlive = KeepAliveOpts.defaults,
        smpPingInterval = 1200_000_000,
        smpPingCount = 3
      )

    val proxyDefaults: NetCfg =
      NetCfg(
        socksProxy = ":9050",
        hostMode = HostMode.OnionViaSocks,
        requiredHostMode = false,
        sessionMode = TransportSessionMode.User,
        tcpConnectTimeout = 30_000_000,
        tcpTimeout = 20_000_000,
        tcpTimeoutPerKb = 60_000,
        tcpKeepAlive = KeepAliveOpts.defaults,
        smpPingInterval = 1200_000_000,
        smpPingCount = 3
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

enum class OnionHosts {
  NEVER, PREFER, REQUIRED
}

@Serializable
enum class HostMode {
  @SerialName("onionViaSocks") OnionViaSocks,
  @SerialName("onion") Onion,
  @SerialName("public") Public;
}

@Serializable
enum class TransportSessionMode {
  @SerialName("user") User,
  @SerialName("entity") Entity;

  companion object {
    val default = User
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
  @SerialName("mentions") Mentions,
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
  @SerialName("files") Files;

  override val hasParam: Boolean get() = when(this) {
    TimedMessages -> true
    else -> false
  }

  override val text: String
    get() = when(this) {
      TimedMessages -> generalGetString(MR.strings.timed_messages)
      DirectMessages -> generalGetString(MR.strings.direct_messages)
      FullDelete -> generalGetString(MR.strings.full_deletion)
      Reactions -> generalGetString(MR.strings.message_reactions)
      Voice -> generalGetString(MR.strings.voice_messages)
      Files -> generalGetString(MR.strings.files_and_media)
    }

  val icon: Painter
    @Composable get() = when(this) {
      TimedMessages -> painterResource(MR.images.ic_timer)
      DirectMessages -> painterResource(MR.images.ic_swap_horizontal_circle)
      FullDelete -> painterResource(MR.images.ic_delete_forever)
      Reactions -> painterResource(MR.images.ic_add_reaction)
      Voice -> painterResource(MR.images.ic_keyboard_voice)
      Files -> painterResource(MR.images.ic_draft)
    }

  @Composable
  override fun iconFilled(): Painter = when(this) {
    TimedMessages -> painterResource(MR.images.ic_timer_filled)
    DirectMessages -> painterResource(MR.images.ic_swap_horizontal_circle_filled)
    FullDelete -> painterResource(MR.images.ic_delete_forever_filled)
    Reactions -> painterResource(MR.images.ic_add_reaction_filled)
    Voice -> painterResource(MR.images.ic_keyboard_voice_filled)
    Files -> painterResource(MR.images.ic_draft_filled)
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
      }
    } else {
      when(this) {
        TimedMessages -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(MR.strings.group_members_can_send_disappearing)
          GroupFeatureEnabled.OFF -> generalGetString(MR.strings.disappearing_messages_are_prohibited)
        }
        DirectMessages -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(MR.strings.group_members_can_send_dms)
          GroupFeatureEnabled.OFF -> generalGetString(MR.strings.direct_messages_are_prohibited_in_chat)
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
  val directMessages: GroupPreference,
  val fullDelete: GroupPreference,
  val reactions: GroupPreference,
  val voice: GroupPreference,
  val files: GroupPreference,
) {
  fun toGroupPreferences(): GroupPreferences =
    GroupPreferences(
      timedMessages = timedMessages,
      directMessages = directMessages,
      fullDelete = fullDelete,
      reactions = reactions,
      voice = voice,
      files = files,
    )

  companion object {
    val sampleData = FullGroupPreferences(
      timedMessages = TimedMessagesGroupPreference(GroupFeatureEnabled.OFF),
      directMessages = GroupPreference(GroupFeatureEnabled.OFF),
      fullDelete = GroupPreference(GroupFeatureEnabled.OFF),
      reactions = GroupPreference(GroupFeatureEnabled.ON),
      voice = GroupPreference(GroupFeatureEnabled.ON),
      files = GroupPreference(GroupFeatureEnabled.ON),
    )
  }
}

@Serializable
data class GroupPreferences(
  val timedMessages: TimedMessagesGroupPreference?,
  val directMessages: GroupPreference?,
  val fullDelete: GroupPreference?,
  val reactions: GroupPreference?,
  val voice: GroupPreference?,
  val files: GroupPreference?,
) {
  companion object {
    val sampleData = GroupPreferences(
      timedMessages = TimedMessagesGroupPreference(GroupFeatureEnabled.OFF),
      directMessages = GroupPreference(GroupFeatureEnabled.OFF),
      fullDelete = GroupPreference(GroupFeatureEnabled.OFF),
      reactions = GroupPreference(GroupFeatureEnabled.ON),
      voice = GroupPreference(GroupFeatureEnabled.ON),
      files = GroupPreference(GroupFeatureEnabled.ON),
    )
  }
}

@Serializable
data class GroupPreference(
  val enable: GroupFeatureEnabled
) {
  val on: Boolean get() = enable == GroupFeatureEnabled.ON
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

val yaml = Yaml(configuration = YamlConfiguration(
  strictMode = false,
  encodeDefaults = false,
))

@Serializable
class APIResponse(val resp: CR, val remoteHostId: Long?, val corr: String? = null) {
  companion object {
    fun decodeStr(str: String): APIResponse {
      return try {
        json.decodeFromString(str)
      } catch(e: Throwable) {
        try {
          Log.d(TAG, e.localizedMessage ?: "")
          val data = json.parseToJsonElement(str).jsonObject
          val resp = data["resp"]!!.jsonObject
          val type = resp["type"]?.jsonPrimitive?.contentOrNull ?: "invalid"
          val corr = data["corr"]?.toString()
          val remoteHostId = data["remoteHostId"]?.jsonPrimitive?.longOrNull
          try {
            if (type == "apiChats") {
              val user: UserRef = json.decodeFromJsonElement(resp["user"]!!.jsonObject)
              val chats: List<Chat> = resp["chats"]!!.jsonArray.map {
                parseChatData(it)
              }
              return APIResponse(CR.ApiChats(user, chats), remoteHostId, corr)
            } else if (type == "apiChat") {
              val user: UserRef = json.decodeFromJsonElement(resp["user"]!!.jsonObject)
              val chat = parseChatData(resp["chat"]!!)
              return APIResponse(CR.ApiChat(user, chat), remoteHostId, corr)
            } else if (type == "chatCmdError") {
              val userObject = resp["user_"]?.jsonObject
              val user = runCatching<UserRef?> { json.decodeFromJsonElement(userObject!!) }.getOrNull()
              return APIResponse(CR.ChatCmdError(user, ChatError.ChatErrorInvalidJSON(json.encodeToString(resp["chatError"]))), remoteHostId, corr)
            } else if (type == "chatError") {
              val userObject = resp["user_"]?.jsonObject
              val user = runCatching<UserRef?> { json.decodeFromJsonElement(userObject!!) }.getOrNull()
              return APIResponse(CR.ChatRespError(user, ChatError.ChatErrorInvalidJSON(json.encodeToString(resp["chatError"]))), remoteHostId, corr)
            }
          } catch (e: Exception) {
            Log.e(TAG, "Exception while parsing chat(s): " + e.stackTraceToString())
          } catch (e: Throwable) {
            Log.e(TAG, "Throwable while parsing chat(s): " + e.stackTraceToString())
            AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error), e.stackTraceToString())
          }
          APIResponse(CR.Response(type, json.encodeToString(data)), remoteHostId, corr)
        } catch(e: Exception) {
          APIResponse(CR.Invalid(str), remoteHostId = null)
        } catch(e: Throwable) {
          Log.e(TAG, "Throwable2 while parsing chat(s): " + e.stackTraceToString())
          AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error), e.stackTraceToString())
          APIResponse(CR.Invalid(str), remoteHostId = null)
        }
      }
    }
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
  @Serializable @SerialName("apiChat") class ApiChat(val user: UserRef, val chat: Chat): CR()
  @Serializable @SerialName("chatItemInfo") class ApiChatItemInfo(val user: UserRef, val chatItem: AChatItem, val chatItemInfo: ChatItemInfo): CR()
  @Serializable @SerialName("userProtoServers") class UserProtoServers(val user: UserRef, val servers: UserProtocolServers): CR()
  @Serializable @SerialName("serverTestResult") class ServerTestResult(val user: UserRef, val testServer: String, val testFailure: ProtocolTestFailure? = null): CR()
  @Serializable @SerialName("chatItemTTL") class ChatItemTTL(val user: UserRef, val chatItemTTL: Long? = null): CR()
  @Serializable @SerialName("networkConfig") class NetworkConfig(val networkConfig: NetCfg): CR()
  @Serializable @SerialName("contactInfo") class ContactInfo(val user: UserRef, val contact: Contact, val connectionStats_: ConnectionStats? = null, val customUserProfile: Profile? = null): CR()
  @Serializable @SerialName("groupMemberInfo") class GroupMemberInfo(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember, val connectionStats_: ConnectionStats? = null): CR()
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
  @Serializable @SerialName("contactVerificationReset") class ContactVerificationReset(val user: UserRef, val contact: Contact): CR()
  @Serializable @SerialName("groupMemberVerificationReset") class GroupMemberVerificationReset(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("contactCode") class ContactCode(val user: UserRef, val contact: Contact, val connectionCode: String): CR()
  @Serializable @SerialName("groupMemberCode") class GroupMemberCode(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember, val connectionCode: String): CR()
  @Serializable @SerialName("connectionVerified") class ConnectionVerified(val user: UserRef, val verified: Boolean, val expectedCode: String): CR()
  @Serializable @SerialName("invitation") class Invitation(val user: UserRef, val connReqInvitation: String, val connection: PendingContactConnection): CR()
  @Serializable @SerialName("connectionIncognitoUpdated") class ConnectionIncognitoUpdated(val user: UserRef, val toConnection: PendingContactConnection): CR()
  @Serializable @SerialName("connectionPlan") class CRConnectionPlan(val user: UserRef, val connectionPlan: ConnectionPlan): CR()
  @Serializable @SerialName("sentConfirmation") class SentConfirmation(val user: UserRef, val connection: PendingContactConnection): CR()
  @Serializable @SerialName("sentInvitation") class SentInvitation(val user: UserRef, val connection: PendingContactConnection): CR()
  @Serializable @SerialName("sentInvitationToContact") class SentInvitationToContact(val user: UserRef, val contact: Contact, val customUserProfile: Profile?): CR()
  @Serializable @SerialName("contactAlreadyExists") class ContactAlreadyExists(val user: UserRef, val contact: Contact): CR()
  @Serializable @SerialName("contactRequestAlreadyAccepted") class ContactRequestAlreadyAccepted(val user: UserRef, val contact: Contact): CR()
  @Serializable @SerialName("contactDeleted") class ContactDeleted(val user: UserRef, val contact: Contact): CR()
  @Serializable @SerialName("contactDeletedByContact") class ContactDeletedByContact(val user: UserRef, val contact: Contact): CR()
  @Serializable @SerialName("chatCleared") class ChatCleared(val user: UserRef, val chatInfo: ChatInfo): CR()
  @Serializable @SerialName("userProfileNoChange") class UserProfileNoChange(val user: User): CR()
  @Serializable @SerialName("userProfileUpdated") class UserProfileUpdated(val user: User, val fromProfile: Profile, val toProfile: Profile, val updateSummary: UserProfileUpdateSummary): CR()
  @Serializable @SerialName("userPrivacy") class UserPrivacy(val user: User, val updatedUser: User): CR()
  @Serializable @SerialName("contactAliasUpdated") class ContactAliasUpdated(val user: UserRef, val toContact: Contact): CR()
  @Serializable @SerialName("connectionAliasUpdated") class ConnectionAliasUpdated(val user: UserRef, val toConnection: PendingContactConnection): CR()
  @Serializable @SerialName("contactPrefsUpdated") class ContactPrefsUpdated(val user: UserRef, val fromContact: Contact, val toContact: Contact): CR()
  @Serializable @SerialName("userContactLink") class UserContactLink(val user: User, val contactLink: UserContactLinkRec): CR()
  @Serializable @SerialName("userContactLinkUpdated") class UserContactLinkUpdated(val user: User, val contactLink: UserContactLinkRec): CR()
  @Serializable @SerialName("userContactLinkCreated") class UserContactLinkCreated(val user: User, val connReqContact: String): CR()
  @Serializable @SerialName("userContactLinkDeleted") class UserContactLinkDeleted(val user: User): CR()
  @Serializable @SerialName("contactConnected") class ContactConnected(val user: UserRef, val contact: Contact, val userCustomProfile: Profile? = null): CR()
  @Serializable @SerialName("contactConnecting") class ContactConnecting(val user: UserRef, val contact: Contact): CR()
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
  @Serializable @SerialName("groupSubscribed") class GroupSubscribed(val user: UserRef, val group: GroupRef): CR()
  @Serializable @SerialName("memberSubErrors") class MemberSubErrors(val user: UserRef, val memberSubErrors: List<MemberSubError>): CR()
  @Serializable @SerialName("groupEmpty") class GroupEmpty(val user: UserRef, val group: GroupInfo): CR()
  @Serializable @SerialName("userContactLinkSubscribed") class UserContactLinkSubscribed: CR()
  @Serializable @SerialName("newChatItem") class NewChatItem(val user: UserRef, val chatItem: AChatItem): CR()
  @Serializable @SerialName("chatItemStatusUpdated") class ChatItemStatusUpdated(val user: UserRef, val chatItem: AChatItem): CR()
  @Serializable @SerialName("chatItemUpdated") class ChatItemUpdated(val user: UserRef, val chatItem: AChatItem): CR()
  @Serializable @SerialName("chatItemNotChanged") class ChatItemNotChanged(val user: UserRef, val chatItem: AChatItem): CR()
  @Serializable @SerialName("chatItemReaction") class ChatItemReaction(val user: UserRef, val added: Boolean, val reaction: ACIReaction): CR()
  @Serializable @SerialName("chatItemDeleted") class ChatItemDeleted(val user: UserRef, val deletedChatItem: AChatItem, val toChatItem: AChatItem? = null, val byUser: Boolean): CR()
  // group events
  @Serializable @SerialName("groupCreated") class GroupCreated(val user: UserRef, val groupInfo: GroupInfo): CR()
  @Serializable @SerialName("sentGroupInvitation") class SentGroupInvitation(val user: UserRef, val groupInfo: GroupInfo, val contact: Contact, val member: GroupMember): CR()
  @Serializable @SerialName("userAcceptedGroupSent") class UserAcceptedGroupSent (val user: UserRef, val groupInfo: GroupInfo, val hostContact: Contact? = null): CR()
  @Serializable @SerialName("groupLinkConnecting") class GroupLinkConnecting (val user: UserRef, val groupInfo: GroupInfo, val hostMember: GroupMember): CR()
  @Serializable @SerialName("userDeletedMember") class UserDeletedMember(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("leftMemberUser") class LeftMemberUser(val user: UserRef, val groupInfo: GroupInfo): CR()
  @Serializable @SerialName("groupMembers") class GroupMembers(val user: UserRef, val group: Group): CR()
  @Serializable @SerialName("receivedGroupInvitation") class ReceivedGroupInvitation(val user: UserRef, val groupInfo: GroupInfo, val contact: Contact, val memberRole: GroupMemberRole): CR()
  @Serializable @SerialName("groupDeletedUser") class GroupDeletedUser(val user: UserRef, val groupInfo: GroupInfo): CR()
  @Serializable @SerialName("joinedGroupMemberConnecting") class JoinedGroupMemberConnecting(val user: UserRef, val groupInfo: GroupInfo, val hostMember: GroupMember, val member: GroupMember): CR()
  @Serializable @SerialName("memberRole") class MemberRole(val user: UserRef, val groupInfo: GroupInfo, val byMember: GroupMember, val member: GroupMember, val fromRole: GroupMemberRole, val toRole: GroupMemberRole): CR()
  @Serializable @SerialName("memberRoleUser") class MemberRoleUser(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember, val fromRole: GroupMemberRole, val toRole: GroupMemberRole): CR()
  @Serializable @SerialName("deletedMemberUser") class DeletedMemberUser(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("deletedMember") class DeletedMember(val user: UserRef, val groupInfo: GroupInfo, val byMember: GroupMember, val deletedMember: GroupMember): CR()
  @Serializable @SerialName("leftMember") class LeftMember(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("groupDeleted") class GroupDeleted(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("contactsMerged") class ContactsMerged(val user: UserRef, val intoContact: Contact, val mergedContact: Contact): CR()
  @Serializable @SerialName("groupInvitation") class GroupInvitation(val user: UserRef, val groupInfo: GroupInfo): CR() // unused
  @Serializable @SerialName("userJoinedGroup") class UserJoinedGroup(val user: UserRef, val groupInfo: GroupInfo): CR()
  @Serializable @SerialName("joinedGroupMember") class JoinedGroupMember(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("connectedToGroupMember") class ConnectedToGroupMember(val user: UserRef, val groupInfo: GroupInfo, val member: GroupMember, val memberContact: Contact? = null): CR()
  @Serializable @SerialName("groupRemoved") class GroupRemoved(val user: UserRef, val groupInfo: GroupInfo): CR() // unused
  @Serializable @SerialName("groupUpdated") class GroupUpdated(val user: UserRef, val toGroup: GroupInfo): CR()
  @Serializable @SerialName("groupLinkCreated") class GroupLinkCreated(val user: UserRef, val groupInfo: GroupInfo, val connReqContact: String, val memberRole: GroupMemberRole): CR()
  @Serializable @SerialName("groupLink") class GroupLink(val user: UserRef, val groupInfo: GroupInfo, val connReqContact: String, val memberRole: GroupMemberRole): CR()
  @Serializable @SerialName("groupLinkDeleted") class GroupLinkDeleted(val user: UserRef, val groupInfo: GroupInfo): CR()
  @Serializable @SerialName("newMemberContact") class NewMemberContact(val user: UserRef, val contact: Contact,  val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("newMemberContactSentInv") class NewMemberContactSentInv(val user: UserRef, val contact: Contact,  val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("newMemberContactReceivedInv") class NewMemberContactReceivedInv(val user: UserRef, val contact: Contact,  val groupInfo: GroupInfo, val member: GroupMember): CR()
  // receiving file events
  @Serializable @SerialName("rcvFileAccepted") class RcvFileAccepted(val user: UserRef, val chatItem: AChatItem): CR()
  @Serializable @SerialName("rcvFileAcceptedSndCancelled") class RcvFileAcceptedSndCancelled(val user: UserRef, val rcvFileTransfer: RcvFileTransfer): CR()
  @Serializable @SerialName("rcvFileStart") class RcvFileStart(val user: UserRef, val chatItem: AChatItem): CR()
  @Serializable @SerialName("rcvFileComplete") class RcvFileComplete(val user: UserRef, val chatItem: AChatItem): CR()
  @Serializable @SerialName("rcvFileCancelled") class RcvFileCancelled(val user: UserRef, val chatItem: AChatItem, val rcvFileTransfer: RcvFileTransfer): CR()
  @Serializable @SerialName("rcvFileSndCancelled") class RcvFileSndCancelled(val user: UserRef, val chatItem: AChatItem, val rcvFileTransfer: RcvFileTransfer): CR()
  @Serializable @SerialName("rcvFileProgressXFTP") class RcvFileProgressXFTP(val user: UserRef, val chatItem: AChatItem, val receivedSize: Long, val totalSize: Long): CR()
  @Serializable @SerialName("rcvFileError") class RcvFileError(val user: UserRef, val chatItem: AChatItem): CR()
  // sending file events
  @Serializable @SerialName("sndFileStart") class SndFileStart(val user: UserRef, val chatItem: AChatItem, val sndFileTransfer: SndFileTransfer): CR()
  @Serializable @SerialName("sndFileComplete") class SndFileComplete(val user: UserRef, val chatItem: AChatItem, val sndFileTransfer: SndFileTransfer): CR()
  @Serializable @SerialName("sndFileCancelled") class SndFileCancelled(val user: UserRef, val chatItem: AChatItem, val fileTransferMeta: FileTransferMeta, val sndFileTransfers: List<SndFileTransfer>): CR()
  @Serializable @SerialName("sndFileRcvCancelled") class SndFileRcvCancelled(val user: UserRef, val chatItem: AChatItem, val sndFileTransfer: SndFileTransfer): CR()
  @Serializable @SerialName("sndFileProgressXFTP") class SndFileProgressXFTP(val user: UserRef, val chatItem: AChatItem, val fileTransferMeta: FileTransferMeta, val sentSize: Long, val totalSize: Long): CR()
  @Serializable @SerialName("sndFileCompleteXFTP") class SndFileCompleteXFTP(val user: UserRef, val chatItem: AChatItem, val fileTransferMeta: FileTransferMeta): CR()
  @Serializable @SerialName("sndFileError") class SndFileError(val user: UserRef, val chatItem: AChatItem): CR()
  // call events
  @Serializable @SerialName("callInvitation") class CallInvitation(val callInvitation: RcvCallInvitation): CR()
  @Serializable @SerialName("callOffer") class CallOffer(val user: UserRef, val contact: Contact, val callType: CallType, val offer: WebRTCSession, val sharedKey: String? = null, val askConfirmation: Boolean): CR()
  @Serializable @SerialName("callAnswer") class CallAnswer(val user: UserRef, val contact: Contact, val answer: WebRTCSession): CR()
  @Serializable @SerialName("callExtraInfo") class CallExtraInfo(val user: UserRef, val contact: Contact, val extraInfo: WebRTCExtraInfo): CR()
  @Serializable @SerialName("callEnded") class CallEnded(val user: UserRef, val contact: Contact): CR()
  @Serializable @SerialName("contactConnectionDeleted") class ContactConnectionDeleted(val user: UserRef, val connection: PendingContactConnection): CR()
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
  @Serializable @SerialName("versionInfo") class VersionInfo(val versionInfo: CoreVersionInfo, val chatMigrations: List<UpMigration>, val agentMigrations: List<UpMigration>): CR()
  @Serializable @SerialName("cmdOk") class CmdOk(val user: UserRef?): CR()
  @Serializable @SerialName("chatCmdError") class ChatCmdError(val user_: UserRef?, val chatError: ChatError): CR()
  @Serializable @SerialName("chatError") class ChatRespError(val user_: UserRef?, val chatError: ChatError): CR()
  @Serializable @SerialName("archiveImported") class ArchiveImported(val archiveErrors: List<ArchiveError>): CR()
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
    is ApiChatItemInfo -> "chatItemInfo"
    is UserProtoServers -> "userProtoServers"
    is ServerTestResult -> "serverTestResult"
    is ChatItemTTL -> "chatItemTTL"
    is NetworkConfig -> "networkConfig"
    is ContactInfo -> "contactInfo"
    is GroupMemberInfo -> "groupMemberInfo"
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
    is ContactVerificationReset -> "contactVerificationReset"
    is GroupMemberVerificationReset -> "groupMemberVerificationReset"
    is ContactCode -> "contactCode"
    is GroupMemberCode -> "groupMemberCode"
    is ConnectionVerified -> "connectionVerified"
    is Invitation -> "invitation"
    is ConnectionIncognitoUpdated -> "connectionIncognitoUpdated"
    is CRConnectionPlan -> "connectionPlan"
    is SentConfirmation -> "sentConfirmation"
    is SentInvitation -> "sentInvitation"
    is SentInvitationToContact -> "sentInvitationToContact"
    is ContactAlreadyExists -> "contactAlreadyExists"
    is ContactRequestAlreadyAccepted -> "contactRequestAlreadyAccepted"
    is ContactDeleted -> "contactDeleted"
    is ContactDeletedByContact -> "contactDeletedByContact"
    is ChatCleared -> "chatCleared"
    is UserProfileNoChange -> "userProfileNoChange"
    is UserProfileUpdated -> "userProfileUpdated"
    is UserPrivacy -> "userPrivacy"
    is ContactAliasUpdated -> "contactAliasUpdated"
    is ConnectionAliasUpdated -> "connectionAliasUpdated"
    is ContactPrefsUpdated -> "contactPrefsUpdated"
    is UserContactLink -> "userContactLink"
    is UserContactLinkUpdated -> "userContactLinkUpdated"
    is UserContactLinkCreated -> "userContactLinkCreated"
    is UserContactLinkDeleted -> "userContactLinkDeleted"
    is ContactConnected -> "contactConnected"
    is ContactConnecting -> "contactConnecting"
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
    is GroupSubscribed -> "groupSubscribed"
    is MemberSubErrors -> "memberSubErrors"
    is GroupEmpty -> "groupEmpty"
    is UserContactLinkSubscribed -> "userContactLinkSubscribed"
    is NewChatItem -> "newChatItem"
    is ChatItemStatusUpdated -> "chatItemStatusUpdated"
    is ChatItemUpdated -> "chatItemUpdated"
    is ChatItemNotChanged -> "chatItemNotChanged"
    is ChatItemReaction -> "chatItemReaction"
    is ChatItemDeleted -> "chatItemDeleted"
    is GroupCreated -> "groupCreated"
    is SentGroupInvitation -> "sentGroupInvitation"
    is UserAcceptedGroupSent -> "userAcceptedGroupSent"
    is GroupLinkConnecting -> "groupLinkConnecting"
    is UserDeletedMember -> "userDeletedMember"
    is LeftMemberUser -> "leftMemberUser"
    is GroupMembers -> "groupMembers"
    is ReceivedGroupInvitation -> "receivedGroupInvitation"
    is GroupDeletedUser -> "groupDeletedUser"
    is JoinedGroupMemberConnecting -> "joinedGroupMemberConnecting"
    is MemberRole -> "memberRole"
    is MemberRoleUser -> "memberRoleUser"
    is DeletedMemberUser -> "deletedMemberUser"
    is DeletedMember -> "deletedMember"
    is LeftMember -> "leftMember"
    is GroupDeleted -> "groupDeleted"
    is ContactsMerged -> "contactsMerged"
    is GroupInvitation -> "groupInvitation"
    is UserJoinedGroup -> "userJoinedGroup"
    is JoinedGroupMember -> "joinedGroupMember"
    is ConnectedToGroupMember -> "connectedToGroupMember"
    is GroupRemoved -> "groupRemoved"
    is GroupUpdated -> "groupUpdated"
    is GroupLinkCreated -> "groupLinkCreated"
    is GroupLink -> "groupLink"
    is GroupLinkDeleted -> "groupLinkDeleted"
    is NewMemberContact -> "newMemberContact"
    is NewMemberContactSentInv -> "newMemberContactSentInv"
    is NewMemberContactReceivedInv -> "newMemberContactReceivedInv"
    is RcvFileAcceptedSndCancelled -> "rcvFileAcceptedSndCancelled"
    is RcvFileAccepted -> "rcvFileAccepted"
    is RcvFileStart -> "rcvFileStart"
    is RcvFileComplete -> "rcvFileComplete"
    is RcvFileCancelled -> "rcvFileCancelled"
    is RcvFileSndCancelled -> "rcvFileSndCancelled"
    is RcvFileProgressXFTP -> "rcvFileProgressXFTP"
    is RcvFileError -> "rcvFileError"
    is SndFileCancelled -> "sndFileCancelled"
    is SndFileComplete -> "sndFileComplete"
    is SndFileRcvCancelled -> "sndFileRcvCancelled"
    is SndFileStart -> "sndFileStart"
    is SndFileProgressXFTP -> "sndFileProgressXFTP"
    is SndFileCompleteXFTP -> "sndFileCompleteXFTP"
    is SndFileError -> "sndFileError"
    is CallInvitation -> "callInvitation"
    is CallOffer -> "callOffer"
    is CallAnswer -> "callAnswer"
    is CallExtraInfo -> "callExtraInfo"
    is CallEnded -> "callEnded"
    is ContactConnectionDeleted -> "contactConnectionDeleted"
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
    is VersionInfo -> "versionInfo"
    is CmdOk -> "cmdOk"
    is ChatCmdError -> "chatCmdError"
    is ChatRespError -> "chatError"
    is ArchiveImported -> "archiveImported"
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
    is ApiChat -> withUser(user, json.encodeToString(chat))
    is ApiChatItemInfo -> withUser(user, "chatItem: ${json.encodeToString(AChatItem)}\n${json.encodeToString(chatItemInfo)}")
    is UserProtoServers -> withUser(user, "servers: ${json.encodeToString(servers)}")
    is ServerTestResult -> withUser(user, "server: $testServer\nresult: ${json.encodeToString(testFailure)}")
    is ChatItemTTL -> withUser(user, json.encodeToString(chatItemTTL))
    is NetworkConfig -> json.encodeToString(networkConfig)
    is ContactInfo -> withUser(user, "contact: ${json.encodeToString(contact)}\nconnectionStats: ${json.encodeToString(connectionStats_)}")
    is GroupMemberInfo -> withUser(user, "group: ${json.encodeToString(groupInfo)}\nmember: ${json.encodeToString(member)}\nconnectionStats: ${json.encodeToString(connectionStats_)}")
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
    is ContactVerificationReset -> withUser(user, "contact: ${json.encodeToString(contact)}")
    is GroupMemberVerificationReset -> withUser(user, "group: ${json.encodeToString(groupInfo)}\nmember: ${json.encodeToString(member)}")
    is ContactCode -> withUser(user, "contact: ${json.encodeToString(contact)}\nconnectionCode: $connectionCode")
    is GroupMemberCode -> withUser(user, "groupInfo: ${json.encodeToString(groupInfo)}\nmember: ${json.encodeToString(member)}\nconnectionCode: $connectionCode")
    is ConnectionVerified -> withUser(user, "verified: $verified\nconnectionCode: $expectedCode")
    is Invitation -> withUser(user, "connReqInvitation: $connReqInvitation\nconnection: $connection")
    is ConnectionIncognitoUpdated -> withUser(user, json.encodeToString(toConnection))
    is CRConnectionPlan -> withUser(user, json.encodeToString(connectionPlan))
    is SentConfirmation -> withUser(user, json.encodeToString(connection))
    is SentInvitation -> withUser(user, json.encodeToString(connection))
    is SentInvitationToContact -> withUser(user, json.encodeToString(contact))
    is ContactAlreadyExists -> withUser(user, json.encodeToString(contact))
    is ContactRequestAlreadyAccepted -> withUser(user, json.encodeToString(contact))
    is ContactDeleted -> withUser(user, json.encodeToString(contact))
    is ContactDeletedByContact -> withUser(user, json.encodeToString(contact))
    is ChatCleared -> withUser(user, json.encodeToString(chatInfo))
    is UserProfileNoChange -> withUser(user, noDetails())
    is UserProfileUpdated -> withUser(user, json.encodeToString(toProfile))
    is UserPrivacy -> withUser(user, json.encodeToString(updatedUser))
    is ContactAliasUpdated -> withUser(user, json.encodeToString(toContact))
    is ConnectionAliasUpdated -> withUser(user, json.encodeToString(toConnection))
    is ContactPrefsUpdated -> withUser(user, "fromContact: $fromContact\ntoContact: \n${json.encodeToString(toContact)}")
    is UserContactLink -> withUser(user, contactLink.responseDetails)
    is UserContactLinkUpdated -> withUser(user, contactLink.responseDetails)
    is UserContactLinkCreated -> withUser(user, connReqContact)
    is UserContactLinkDeleted -> withUser(user, noDetails())
    is ContactConnected -> withUser(user, json.encodeToString(contact))
    is ContactConnecting -> withUser(user, json.encodeToString(contact))
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
    is GroupSubscribed -> withUser(user, json.encodeToString(group))
    is MemberSubErrors -> withUser(user, json.encodeToString(memberSubErrors))
    is GroupEmpty -> withUser(user, json.encodeToString(group))
    is UserContactLinkSubscribed -> noDetails()
    is NewChatItem -> withUser(user, json.encodeToString(chatItem))
    is ChatItemStatusUpdated -> withUser(user, json.encodeToString(chatItem))
    is ChatItemUpdated -> withUser(user, json.encodeToString(chatItem))
    is ChatItemNotChanged -> withUser(user, json.encodeToString(chatItem))
    is ChatItemReaction -> withUser(user, "added: $added\n${json.encodeToString(reaction)}")
    is ChatItemDeleted -> withUser(user, "deletedChatItem:\n${json.encodeToString(deletedChatItem)}\ntoChatItem:\n${json.encodeToString(toChatItem)}\nbyUser: $byUser")
    is GroupCreated -> withUser(user, json.encodeToString(groupInfo))
    is SentGroupInvitation -> withUser(user, "groupInfo: $groupInfo\ncontact: $contact\nmember: $member")
    is UserAcceptedGroupSent -> json.encodeToString(groupInfo)
    is GroupLinkConnecting -> withUser(user, "groupInfo: $groupInfo\nhostMember: $hostMember")
    is UserDeletedMember -> withUser(user, "groupInfo: $groupInfo\nmember: $member")
    is LeftMemberUser -> withUser(user, json.encodeToString(groupInfo))
    is GroupMembers -> withUser(user, json.encodeToString(group))
    is ReceivedGroupInvitation -> withUser(user, "groupInfo: $groupInfo\ncontact: $contact\nmemberRole: $memberRole")
    is GroupDeletedUser -> withUser(user, json.encodeToString(groupInfo))
    is JoinedGroupMemberConnecting -> withUser(user, "groupInfo: $groupInfo\nhostMember: $hostMember\nmember: $member")
    is MemberRole -> withUser(user, "groupInfo: $groupInfo\nbyMember: $byMember\nmember: $member\nfromRole: $fromRole\ntoRole: $toRole")
    is MemberRoleUser -> withUser(user, "groupInfo: $groupInfo\nmember: $member\nfromRole: $fromRole\ntoRole: $toRole")
    is DeletedMemberUser -> withUser(user, "groupInfo: $groupInfo\nmember: $member")
    is DeletedMember -> withUser(user, "groupInfo: $groupInfo\nbyMember: $byMember\ndeletedMember: $deletedMember")
    is LeftMember -> withUser(user, "groupInfo: $groupInfo\nmember: $member")
    is GroupDeleted -> withUser(user, "groupInfo: $groupInfo\nmember: $member")
    is ContactsMerged -> withUser(user, "intoContact: $intoContact\nmergedContact: $mergedContact")
    is GroupInvitation -> withUser(user, json.encodeToString(groupInfo))
    is UserJoinedGroup -> withUser(user, json.encodeToString(groupInfo))
    is JoinedGroupMember -> withUser(user, "groupInfo: $groupInfo\nmember: $member")
    is ConnectedToGroupMember -> withUser(user, "groupInfo: $groupInfo\nmember: $member\nmemberContact: $memberContact")
    is GroupRemoved -> withUser(user, json.encodeToString(groupInfo))
    is GroupUpdated -> withUser(user, json.encodeToString(toGroup))
    is GroupLinkCreated -> withUser(user, "groupInfo: $groupInfo\nconnReqContact: $connReqContact\nmemberRole: $memberRole")
    is GroupLink -> withUser(user, "groupInfo: $groupInfo\nconnReqContact: $connReqContact\nmemberRole: $memberRole")
    is GroupLinkDeleted -> withUser(user, json.encodeToString(groupInfo))
    is NewMemberContact -> withUser(user, "contact: $contact\ngroupInfo: $groupInfo\nmember: $member")
    is NewMemberContactSentInv -> withUser(user, "contact: $contact\ngroupInfo: $groupInfo\nmember: $member")
    is NewMemberContactReceivedInv -> withUser(user, "contact: $contact\ngroupInfo: $groupInfo\nmember: $member")
    is RcvFileAcceptedSndCancelled -> withUser(user, noDetails())
    is RcvFileAccepted -> withUser(user, json.encodeToString(chatItem))
    is RcvFileStart -> withUser(user, json.encodeToString(chatItem))
    is RcvFileComplete -> withUser(user, json.encodeToString(chatItem))
    is RcvFileCancelled -> withUser(user, json.encodeToString(chatItem))
    is RcvFileSndCancelled -> withUser(user, json.encodeToString(chatItem))
    is RcvFileProgressXFTP -> withUser(user, "chatItem: ${json.encodeToString(chatItem)}\nreceivedSize: $receivedSize\ntotalSize: $totalSize")
    is RcvFileError -> withUser(user, json.encodeToString(chatItem))
    is SndFileCancelled -> json.encodeToString(chatItem)
    is SndFileComplete -> withUser(user, json.encodeToString(chatItem))
    is SndFileRcvCancelled -> withUser(user, json.encodeToString(chatItem))
    is SndFileStart -> withUser(user, json.encodeToString(chatItem))
    is SndFileProgressXFTP -> withUser(user, "chatItem: ${json.encodeToString(chatItem)}\nsentSize: $sentSize\ntotalSize: $totalSize")
    is SndFileCompleteXFTP -> withUser(user, json.encodeToString(chatItem))
    is SndFileError -> withUser(user, json.encodeToString(chatItem))
    is CallInvitation -> "contact: ${callInvitation.contact.id}\ncallType: $callInvitation.callType\nsharedKey: ${callInvitation.sharedKey ?: ""}"
    is CallOffer -> withUser(user, "contact: ${contact.id}\ncallType: $callType\nsharedKey: ${sharedKey ?: ""}\naskConfirmation: $askConfirmation\noffer: ${json.encodeToString(offer)}")
    is CallAnswer -> withUser(user, "contact: ${contact.id}\nanswer: ${json.encodeToString(answer)}")
    is CallExtraInfo -> withUser(user, "contact: ${contact.id}\nextraInfo: ${json.encodeToString(extraInfo)}")
    is CallEnded -> withUser(user, "contact: ${contact.id}")
    is ContactConnectionDeleted -> withUser(user, json.encodeToString(connection))
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
    is RemoteCtrlStopped -> noDetails()
    is VersionInfo -> "version ${json.encodeToString(versionInfo)}\n\n" +
        "chat migrations: ${json.encodeToString(chatMigrations.map { it.upName })}\n\n" +
        "agent migrations: ${json.encodeToString(agentMigrations.map { it.upName })}"
    is CmdOk -> withUser(user, noDetails())
    is ChatCmdError -> withUser(user_, chatError.string)
    is ChatRespError -> withUser(user_, chatError.string)
    is ArchiveImported -> "${archiveErrors.map { it.string } }"
    is Response -> json
    is Invalid -> str
  }

  fun noDetails(): String ="${responseType}: " + generalGetString(MR.strings.no_details)

  private fun withUser(u: UserLike?, s: String): String = if (u != null) "userId: ${u.userId}\n$s" else s
}

fun chatError(r: CR): ChatErrorType? {
  return (
      if (r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorChat) r.chatError.errorType
      else if (r is CR.ChatRespError && r.chatError is ChatError.ChatErrorChat) r.chatError.errorType
      else null
      )
}

@Serializable
sealed class ConnectionPlan {
  @Serializable @SerialName("invitationLink") class InvitationLink(val invitationLinkPlan: InvitationLinkPlan): ConnectionPlan()
  @Serializable @SerialName("contactAddress") class ContactAddress(val contactAddressPlan: ContactAddressPlan): ConnectionPlan()
  @Serializable @SerialName("groupLink") class GroupLink(val groupLinkPlan: GroupLinkPlan): ConnectionPlan()
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

  class Cmd(override val id: Long, override val remoteHostId: Long?, val cmd: CC): TerminalItem() {
    override val label get() = "> ${cmd.cmdString}"
    override val details get() = cmd.cmdString
  }

  class Resp(override val id: Long, override val remoteHostId: Long?, val resp: CR): TerminalItem() {
    override val label get() = "< ${resp.responseType}"
    override val details get() = resp.details
  }

  companion object {
    val sampleData = listOf(
        Cmd(0, null, CC.ShowActiveUser()),
        Resp(1, null, CR.ActiveUser(User.sampleData))
    )

    fun cmd(rhId: Long?, c: CC) = Cmd(System.currentTimeMillis(), rhId, c)
    fun resp(rhId: Long?, r: CR) = Resp(System.currentTimeMillis(), rhId, r)
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
class UserContactLinkRec(val connReqContact: String, val autoAccept: AutoAccept? = null) {
  val responseDetails: String get() = "connReqContact: ${connReqContact}\nautoAccept: ${AutoAccept.cmdString(autoAccept)}"
}

@Serializable
class AutoAccept(val acceptIncognito: Boolean, val autoReply: MsgContent?) {
  companion object {
    fun cmdString(autoAccept: AutoAccept?): String {
      if (autoAccept == null) return "off"
      val s = "on" + if (autoAccept.acceptIncognito) " incognito=on" else ""
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
      is ChatNotStarted -> "chatNotStarted"
      is ChatNotStopped -> "chatNotStopped"
      is ChatStoreChanged -> "chatStoreChanged"
      is ConnectionPlanChatError -> "connectionPlan"
      is InvalidConnReq -> "invalidConnReq"
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
      is GroupMemberIntroNotFound -> "groupMemberIntroNotFound"
      is GroupCantResendInvitation -> "groupCantResendInvitation"
      is GroupInternal -> "groupInternal"
      is FileNotFound -> "fileNotFound"
      is FileSize -> "fileSize"
      is FileAlreadyReceiving -> "fileAlreadyReceiving"
      is FileCancelled -> "fileCancelled"
      is FileCancel -> "fileCancel"
      is FileAlreadyExists -> "fileAlreadyExists"
      is FileRead -> "fileRead"
      is FileWrite -> "fileWrite"
      is FileSend -> "fileSend"
      is FileRcvChunk -> "fileRcvChunk"
      is FileInternal -> "fileInternal"
      is FileImageType -> "fileImageType"
      is FileImageSize -> "fileImageSize"
      is FileNotReceived -> "fileNotReceived"
      // is XFTPRcvFile -> "xftpRcvFile"
      // is XFTPSndFile -> "xftpSndFile"
      is FallbackToSMPProhibited -> "fallbackToSMPProhibited"
      is InlineFileProhibited -> "inlineFileProhibited"
      is InvalidQuote -> "invalidQuote"
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
  @Serializable @SerialName("chatNotStarted") object ChatNotStarted: ChatErrorType()
  @Serializable @SerialName("chatNotStopped") object ChatNotStopped: ChatErrorType()
  @Serializable @SerialName("chatStoreChanged") object ChatStoreChanged: ChatErrorType()
  @Serializable @SerialName("connectionPlan") class ConnectionPlanChatError(val connectionPlan: ConnectionPlan): ChatErrorType()
  @Serializable @SerialName("invalidConnReq") object InvalidConnReq: ChatErrorType()
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
  @Serializable @SerialName("groupMemberIntroNotFound") class GroupMemberIntroNotFound(val contactName: String): ChatErrorType()
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
  // @Serializable @SerialName("xFTPRcvFile") object XFTPRcvFile: ChatErrorType()
  // @Serializable @SerialName("xFTPSndFile") object XFTPSndFile: ChatErrorType()
  @Serializable @SerialName("fallbackToSMPProhibited") class FallbackToSMPProhibited(val fileId: Long): ChatErrorType()
  @Serializable @SerialName("inlineFileProhibited") class InlineFileProhibited(val fileId: Long): ChatErrorType()
  @Serializable @SerialName("invalidQuote") object InvalidQuote: ChatErrorType()
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
  @Serializable @SerialName("peerChatVRangeIncompatible") object PeerChatVRangeIncompatible: ChatErrorType()
  @Serializable @SerialName("internalError") class InternalError(val message: String): ChatErrorType()
  @Serializable @SerialName("exception") class CEException(val message: String): ChatErrorType()
}

@Serializable
sealed class StoreError {
  val string: String
    get() = when (this) {
      is DuplicateName -> "duplicateName"
      is UserNotFound -> "userNotFound"
      is UserNotFoundByName -> "userNotFoundByName"
      is UserNotFoundByContactId -> "userNotFoundByContactId"
      is UserNotFoundByGroupId -> "userNotFoundByGroupId"
      is UserNotFoundByFileId -> "userNotFoundByFileId"
      is UserNotFoundByContactRequestId -> "userNotFoundByContactRequestId"
      is ContactNotFound -> "contactNotFound"
      is ContactNotFoundByName -> "contactNotFoundByName"
      is ContactNotFoundByMemberId -> "contactNotFoundByMemberId"
      is ContactNotReady -> "contactNotReady"
      is DuplicateContactLink -> "duplicateContactLink"
      is UserContactLinkNotFound -> "userContactLinkNotFound"
      is ContactRequestNotFound -> "contactRequestNotFound"
      is ContactRequestNotFoundByName -> "contactRequestNotFoundByName"
      is GroupNotFound -> "groupNotFound"
      is GroupNotFoundByName -> "groupNotFoundByName"
      is GroupMemberNameNotFound -> "groupMemberNameNotFound"
      is GroupMemberNotFound -> "groupMemberNotFound"
      is GroupMemberNotFoundByMemberId -> "groupMemberNotFoundByMemberId"
      is MemberContactGroupMemberNotFound -> "memberContactGroupMemberNotFound"
      is GroupWithoutUser -> "groupWithoutUser"
      is DuplicateGroupMember -> "duplicateGroupMember"
      is GroupAlreadyJoined -> "groupAlreadyJoined"
      is GroupInvitationNotFound -> "groupInvitationNotFound"
      is SndFileNotFound -> "sndFileNotFound"
      is SndFileInvalid -> "sndFileInvalid"
      is RcvFileNotFound -> "rcvFileNotFound"
      is RcvFileDescrNotFound -> "rcvFileDescrNotFound"
      is FileNotFound -> "fileNotFound"
      is RcvFileInvalid -> "rcvFileInvalid"
      is RcvFileInvalidDescrPart -> "rcvFileInvalidDescrPart"
      is SharedMsgIdNotFoundByFileId -> "sharedMsgIdNotFoundByFileId"
      is FileIdNotFoundBySharedMsgId -> "fileIdNotFoundBySharedMsgId"
      is SndFileNotFoundXFTP -> "sndFileNotFoundXFTP"
      is RcvFileNotFoundXFTP -> "rcvFileNotFoundXFTP"
      is ConnectionNotFound -> "connectionNotFound"
      is ConnectionNotFoundById -> "connectionNotFoundById"
      is ConnectionNotFoundByMemberId -> "connectionNotFoundByMemberId"
      is PendingConnectionNotFound -> "pendingConnectionNotFound"
      is IntroNotFound -> "introNotFound"
      is UniqueID -> "uniqueID"
      is InternalError -> "internalError"
      is NoMsgDelivery -> "noMsgDelivery"
      is BadChatItem -> "badChatItem"
      is ChatItemNotFound -> "chatItemNotFound"
      is ChatItemNotFoundByText -> "chatItemNotFoundByText"
      is ChatItemSharedMsgIdNotFound -> "chatItemSharedMsgIdNotFound"
      is ChatItemNotFoundByFileId -> "chatItemNotFoundByFileId"
      is ChatItemNotFoundByGroupId -> "chatItemNotFoundByGroupId"
      is ProfileNotFound -> "profileNotFound"
      is DuplicateGroupLink -> "duplicateGroupLink"
      is GroupLinkNotFound -> "groupLinkNotFound"
      is HostMemberIdNotFound -> "hostMemberIdNotFound"
      is ContactNotFoundByFileId -> "contactNotFoundByFileId"
      is NoGroupSndStatus -> "noGroupSndStatus"
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
  @Serializable @SerialName("sndFileNotFound") class SndFileNotFound(val fileId: Long): StoreError()
  @Serializable @SerialName("sndFileInvalid") class SndFileInvalid(val fileId: Long): StoreError()
  @Serializable @SerialName("rcvFileNotFound") class RcvFileNotFound(val fileId: Long): StoreError()
  @Serializable @SerialName("rcvFileDescrNotFound") class RcvFileDescrNotFound(val fileId: Long): StoreError()
  @Serializable @SerialName("fileNotFound") class FileNotFound(val fileId: Long): StoreError()
  @Serializable @SerialName("rcvFileInvalid") class RcvFileInvalid(val fileId: Long): StoreError()
  @Serializable @SerialName("rcvFileInvalidDescrPart") object RcvFileInvalidDescrPart: StoreError()
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
  @Serializable @SerialName("internalError") class InternalError(val message: String): StoreError()
  @Serializable @SerialName("noMsgDelivery") class NoMsgDelivery(val connId: Long, val agentMsgId: String): StoreError()
  @Serializable @SerialName("badChatItem") class BadChatItem(val itemId: Long): StoreError()
  @Serializable @SerialName("chatItemNotFound") class ChatItemNotFound(val itemId: Long): StoreError()
  @Serializable @SerialName("chatItemNotFoundByText") class ChatItemNotFoundByText(val text: String): StoreError()
  @Serializable @SerialName("chatItemSharedMsgIdNotFound") class ChatItemSharedMsgIdNotFound(val sharedMsgId: String): StoreError()
  @Serializable @SerialName("chatItemNotFoundByFileId") class ChatItemNotFoundByFileId(val fileId: Long): StoreError()
  @Serializable @SerialName("chatItemNotFoundByGroupId") class ChatItemNotFoundByGroupId(val groupId: Long): StoreError()
  @Serializable @SerialName("profileNotFound") class ProfileNotFound(val profileId: Long): StoreError()
  @Serializable @SerialName("duplicateGroupLink") class DuplicateGroupLink(val groupInfo: GroupInfo): StoreError()
  @Serializable @SerialName("groupLinkNotFound") class GroupLinkNotFound(val groupInfo: GroupInfo): StoreError()
  @Serializable @SerialName("hostMemberIdNotFound") class HostMemberIdNotFound(val groupId: Long): StoreError()
  @Serializable @SerialName("contactNotFoundByFileId") class ContactNotFoundByFileId(val fileId: Long): StoreError()
  @Serializable @SerialName("noGroupSndStatus") class NoGroupSndStatus(val itemId: Long, val groupMemberId: Long): StoreError()
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
  @Serializable @SerialName("error") class Error(val error: String): SQLiteError()
}

@Serializable
sealed class AgentErrorType {
  val string: String get() = when (this) {
    is CMD -> "CMD ${cmdErr.string}"
    is CONN -> "CONN ${connErr.string}"
    is SMP -> "SMP ${smpErr.string}"
    // is NTF -> "NTF ${ntfErr.string}"
    is XFTP -> "XFTP ${xftpErr.string}"
    is RCP -> "RCP ${rcpErr.string}"
    is BROKER -> "BROKER ${brokerErr.string}"
    is AGENT -> "AGENT ${agentErr.string}"
    is INTERNAL -> "INTERNAL $internalErr"
    is INACTIVE -> "INACTIVE"
  }
  @Serializable @SerialName("CMD") class CMD(val cmdErr: CommandErrorType): AgentErrorType()
  @Serializable @SerialName("CONN") class CONN(val connErr: ConnectionErrorType): AgentErrorType()
  @Serializable @SerialName("SMP") class SMP(val smpErr: SMPErrorType): AgentErrorType()
  // @Serializable @SerialName("NTF") class NTF(val ntfErr: SMPErrorType): AgentErrorType()
  @Serializable @SerialName("XFTP") class XFTP(val xftpErr: XFTPErrorType): AgentErrorType()
  @Serializable @SerialName("RCP") class RCP(val rcpErr: RCErrorType): AgentErrorType()
  @Serializable @SerialName("BROKER") class BROKER(val brokerAddress: String, val brokerErr: BrokerErrorType): AgentErrorType()
  @Serializable @SerialName("AGENT") class AGENT(val agentErr: SMPAgentError): AgentErrorType()
  @Serializable @SerialName("INTERNAL") class INTERNAL(val internalErr: String): AgentErrorType()
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

@Serializable
sealed class SMPErrorType {
  val string: String get() = when (this) {
    is BLOCK -> "BLOCK"
    is SESSION -> "SESSION"
    is CMD -> "CMD ${cmdErr.string}"
    is AUTH -> "AUTH"
    is QUOTA -> "QUOTA"
    is NO_MSG -> "NO_MSG"
    is LARGE_MSG -> "LARGE_MSG"
    is INTERNAL -> "INTERNAL"
  }
  @Serializable @SerialName("BLOCK") class BLOCK: SMPErrorType()
  @Serializable @SerialName("SESSION") class SESSION: SMPErrorType()
  @Serializable @SerialName("CMD") class CMD(val cmdErr: ProtocolCommandError): SMPErrorType()
  @Serializable @SerialName("AUTH") class AUTH: SMPErrorType()
  @Serializable @SerialName("QUOTA") class QUOTA: SMPErrorType()
  @Serializable @SerialName("NO_MSG") class NO_MSG: SMPErrorType()
  @Serializable @SerialName("LARGE_MSG") class LARGE_MSG: SMPErrorType()
  @Serializable @SerialName("INTERNAL") class INTERNAL: SMPErrorType()
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
    is LargeMsg -> "largeMsg"
    is BadSession -> "badSession"
    is Handshake -> "handshake ${handshakeErr.string}"
  }
  @Serializable @SerialName("badBlock") class BadBlock: SMPTransportError()
  @Serializable @SerialName("largeMsg") class LargeMsg: SMPTransportError()
  @Serializable @SerialName("badSession") class BadSession: SMPTransportError()
  @Serializable @SerialName("handshake") class Handshake(val handshakeErr: SMPHandshakeError): SMPTransportError()
}

@Serializable
sealed class SMPHandshakeError {
  val string: String get() = when (this) {
    is PARSE -> "PARSE"
    is VERSION -> "VERSION"
    is IDENTITY -> "IDENTITY"
  }
  @Serializable @SerialName("PARSE") class PARSE: SMPHandshakeError()
  @Serializable @SerialName("VERSION") class VERSION: SMPHandshakeError()
  @Serializable @SerialName("IDENTITY") class IDENTITY: SMPHandshakeError()
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
    is SIZE -> "SIZE"
    is QUOTA -> "QUOTA"
    is DIGEST -> "DIGEST"
    is CRYPTO -> "CRYPTO"
    is NO_FILE -> "NO_FILE"
    is HAS_FILE -> "HAS_FILE"
    is FILE_IO -> "FILE_IO"
    is INTERNAL -> "INTERNAL"
  }
  @Serializable @SerialName("BLOCK") object BLOCK: XFTPErrorType()
  @Serializable @SerialName("SESSION") object SESSION: XFTPErrorType()
  @Serializable @SerialName("CMD") class CMD(val cmdErr: ProtocolCommandError): XFTPErrorType()
  @Serializable @SerialName("AUTH") object AUTH: XFTPErrorType()
  @Serializable @SerialName("SIZE") object SIZE: XFTPErrorType()
  @Serializable @SerialName("QUOTA") object QUOTA: XFTPErrorType()
  @Serializable @SerialName("DIGEST") object DIGEST: XFTPErrorType()
  @Serializable @SerialName("CRYPTO") object CRYPTO: XFTPErrorType()
  @Serializable @SerialName("NO_FILE") object NO_FILE: XFTPErrorType()
  @Serializable @SerialName("HAS_FILE") object HAS_FILE: XFTPErrorType()
  @Serializable @SerialName("FILE_IO") object FILE_IO: XFTPErrorType()
  @Serializable @SerialName("INTERNAL") object INTERNAL: XFTPErrorType()
}

@Serializable
sealed class RCErrorType {
  val string: String get() = when (this) {
    is INTERNAL -> "INTERNAL $internalErr"
    is IDENTITY -> "IDENTITY"
    is NO_LOCAL_ADDRESS -> "NO_LOCAL_ADDRESS"
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
    is ArchiveErrorImport -> "import ${chatError.string}"
    is ArchiveErrorImportFile -> "importFile $file ${chatError.string}"
  }
  @Serializable @SerialName("import") class ArchiveErrorImport(val chatError: ChatError): ArchiveError()
  @Serializable @SerialName("importFile") class ArchiveErrorImportFile(val file: String, val chatError: ChatError): ArchiveError()
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
    is Disconnected -> "disconnected"
    is BadInvitation -> "badInvitation"
    is BadVersion -> "badVersion"
  }
  @Serializable @SerialName("inactive") object Inactive: RemoteCtrlError()
  @Serializable @SerialName("badState") object BadState: RemoteCtrlError()
  @Serializable @SerialName("busy") object Busy: RemoteCtrlError()
  @Serializable @SerialName("timeout") object Timeout: RemoteCtrlError()
  @Serializable @SerialName("disconnected") class Disconnected(val remoteCtrlId: Long, val reason: String): RemoteCtrlError()
  @Serializable @SerialName("badInvitation") object BadInvitation: RemoteCtrlError()
  @Serializable @SerialName("badVersion") data class BadVersion(val appVersion: String): RemoteCtrlError()
  //@Serializable @SerialName("protocolError") data class ProtocolError(val protocolError: RemoteProtocolError): RemoteCtrlError()
}

enum class NotificationsMode() {
  OFF, PERIODIC, SERVICE, /*INSTANT - for Firebase notifications */;

  companion object {
    val default: NotificationsMode = SERVICE
  }
}
