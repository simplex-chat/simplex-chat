package chat.simplex.app.model

import android.annotation.SuppressLint
import android.app.Application
import android.content.*
import android.net.Uri
import android.os.PowerManager
import android.provider.Settings
import android.util.Log
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.toArgb
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.fragment.app.FragmentActivity
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.call.*
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.newchat.ConnectViaLinkTab
import chat.simplex.app.views.onboarding.OnboardingStage
import chat.simplex.app.views.usersettings.*
import kotlinx.coroutines.*
import kotlinx.datetime.Clock
import kotlinx.datetime.Instant
import kotlinx.serialization.*
import kotlinx.serialization.json.*
import java.util.Date

typealias ChatCtrl = Long

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
    val default = SimplexLinkMode.DESCRIPTION
  }
}

class AppPreferences(val context: Context) {
  private val sharedPreferences: SharedPreferences = context.getSharedPreferences(SHARED_PREFS_ID, Context.MODE_PRIVATE)

  // deprecated, remove in 2024
  private val runServiceInBackground = mkBoolPreference(SHARED_PREFS_RUN_SERVICE_IN_BACKGROUND, true)
  val notificationsMode = mkStrPreference(SHARED_PREFS_NOTIFICATIONS_MODE,
    if (!runServiceInBackground.get()) NotificationsMode.OFF.name else NotificationsMode.default.name
  )
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
  val privacyFullBackup = mkBoolPreference(SHARED_PREFS_PRIVACY_FULL_BACKUP, false)
  val experimentalCalls = mkBoolPreference(SHARED_PREFS_EXPERIMENTAL_CALLS, false)
  val chatArchiveName = mkStrPreference(SHARED_PREFS_CHAT_ARCHIVE_NAME, null)
  val chatArchiveTime = mkDatePreference(SHARED_PREFS_CHAT_ARCHIVE_TIME, null)
  val chatLastStart = mkDatePreference(SHARED_PREFS_CHAT_LAST_START, null)
  val developerTools = mkBoolPreference(SHARED_PREFS_DEVELOPER_TOOLS, false)
  val networkUseSocksProxy = mkBoolPreference(SHARED_PREFS_NETWORK_USE_SOCKS_PROXY, false)
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
  val networkSMPPingInterval = mkLongPreference(SHARED_PREFS_NETWORK_SMP_PING_INTERVAL, NetCfg.defaults.smpPingInterval)
  val networkSMPPingCount = mkIntPreference(SHARED_PREFS_NETWORK_SMP_PING_COUNT, NetCfg.defaults.smpPingCount)
  val networkEnableKeepAlive = mkBoolPreference(SHARED_PREFS_NETWORK_ENABLE_KEEP_ALIVE, NetCfg.defaults.enableKeepAlive)
  val networkTCPKeepIdle = mkIntPreference(SHARED_PREFS_NETWORK_TCP_KEEP_IDLE, KeepAliveOpts.defaults.keepIdle)
  val networkTCPKeepIntvl = mkIntPreference(SHARED_PREFS_NETWORK_TCP_KEEP_INTVL, KeepAliveOpts.defaults.keepIntvl)
  val networkTCPKeepCnt = mkIntPreference(SHARED_PREFS_NETWORK_TCP_KEEP_CNT, KeepAliveOpts.defaults.keepCnt)
  val incognito = mkBoolPreference(SHARED_PREFS_INCOGNITO, false)
  val connectViaLinkTab = mkStrPreference(SHARED_PREFS_CONNECT_VIA_LINK_TAB, ConnectViaLinkTab.SCAN.name)
  val liveMessageAlertShown = mkBoolPreference(SHARED_PREFS_LIVE_MESSAGE_ALERT_SHOWN, false)

  val storeDBPassphrase = mkBoolPreference(SHARED_PREFS_STORE_DB_PASSPHRASE, true)
  val initialRandomDBPassphrase = mkBoolPreference(SHARED_PREFS_INITIAL_RANDOM_DB_PASSPHRASE, false)
  val encryptedDBPassphrase = mkStrPreference(SHARED_PREFS_ENCRYPTED_DB_PASSPHRASE, null)
  val initializationVectorDBPassphrase = mkStrPreference(SHARED_PREFS_INITIALIZATION_VECTOR_DB_PASSPHRASE, null)
  val encryptionStartedAt = mkDatePreference(SHARED_PREFS_ENCRYPTION_STARTED_AT, null, true)

  val currentTheme = mkStrPreference(SHARED_PREFS_CURRENT_THEME, DefaultTheme.SYSTEM.name)
  val primaryColor = mkIntPreference(SHARED_PREFS_PRIMARY_COLOR, LightColorPalette.primary.toArgb())

  val whatsNewVersion = mkStrPreference(SHARED_PREFS_WHATS_NEW_VERSION, null)

  private fun mkIntPreference(prefName: String, default: Int) =
    SharedPreference(
      get = fun() = sharedPreferences.getInt(prefName, default),
      set = fun(value) = sharedPreferences.edit().putInt(prefName, value).apply()
    )

  private fun mkLongPreference(prefName: String, default: Long) =
    SharedPreference(
      get = fun() = sharedPreferences.getLong(prefName, default),
      set = fun(value) = sharedPreferences.edit().putLong(prefName, value).apply()
    )

  private fun mkTimeoutPreference(prefName: String, default: Long, proxyDefault: Long): SharedPreference<Long> {
    val d = if (networkUseSocksProxy.get()) proxyDefault else default
    return SharedPreference(
      get = fun() = sharedPreferences.getLong(prefName, d),
      set = fun(value) = sharedPreferences.edit().putLong(prefName, value).apply()
    )
  }

  private fun mkBoolPreference(prefName: String, default: Boolean) =
    SharedPreference(
      get = fun() = sharedPreferences.getBoolean(prefName, default),
      set = fun(value) = sharedPreferences.edit().putBoolean(prefName, value).apply()
    )

  private fun mkStrPreference(prefName: String, default: String?): SharedPreference<String?> =
    SharedPreference(
      get = fun() = sharedPreferences.getString(prefName, default),
      set = fun(value) = sharedPreferences.edit().putString(prefName, value).apply()
    )

  /**
  * Provide `[commit] = true` to save preferences right now, not after some unknown period of time.
  * So in case of a crash this value will be saved 100%
  * */
  private fun mkDatePreference(prefName: String, default: Instant?, commit: Boolean = false): SharedPreference<Instant?> =
    SharedPreference(
      get = {
        val pref = sharedPreferences.getString(prefName, default?.toEpochMilliseconds()?.toString())
        pref?.let { Instant.fromEpochMilliseconds(pref.toLong()) }
      },
      set = fun(value) = sharedPreferences.edit().putString(prefName, value?.toEpochMilliseconds()?.toString()).let {
        if (commit) it.commit() else it.apply()
      }
    )

  companion object {
    internal const val SHARED_PREFS_ID = "chat.simplex.app.SIMPLEX_APP_PREFS"
    private const val SHARED_PREFS_AUTO_RESTART_WORKER_VERSION = "AutoRestartWorkerVersion"
    private const val SHARED_PREFS_RUN_SERVICE_IN_BACKGROUND = "RunServiceInBackground"
    private const val SHARED_PREFS_NOTIFICATIONS_MODE = "NotificationsMode"
    private const val SHARED_PREFS_NOTIFICATION_PREVIEW_MODE = "NotificationPreviewMode"
    private const val SHARED_PREFS_SERVICE_NOTICE_SHOWN = "BackgroundServiceNoticeShown"
    private const val SHARED_PREFS_SERVICE_BATTERY_NOTICE_SHOWN = "BackgroundServiceBatteryNoticeShown"
    private const val SHARED_PREFS_WEBRTC_POLICY_RELAY = "WebrtcPolicyRelay"
    private const val SHARED_PREFS_WEBRTC_CALLS_ON_LOCK_SCREEN = "CallsOnLockScreen"
    private const val SHARED_PREFS_PERFORM_LA = "PerformLA"
    private const val SHARED_PREFS_LA_NOTICE_SHOWN = "LANoticeShown"
    private const val SHARED_PREFS_WEBRTC_ICE_SERVERS = "WebrtcICEServers"
    private const val SHARED_PREFS_PRIVACY_PROTECT_SCREEN = "PrivacyProtectScreen"
    private const val SHARED_PREFS_PRIVACY_ACCEPT_IMAGES = "PrivacyAcceptImages"
    private const val SHARED_PREFS_PRIVACY_TRANSFER_IMAGES_INLINE = "PrivacyTransferImagesInline"
    private const val SHARED_PREFS_PRIVACY_LINK_PREVIEWS = "PrivacyLinkPreviews"
    private const val SHARED_PREFS_PRIVACY_SIMPLEX_LINK_MODE = "PrivacySimplexLinkMode"
    internal const val SHARED_PREFS_PRIVACY_FULL_BACKUP = "FullBackup"
    private const val SHARED_PREFS_EXPERIMENTAL_CALLS = "ExperimentalCalls"
    private const val SHARED_PREFS_CHAT_ARCHIVE_NAME = "ChatArchiveName"
    private const val SHARED_PREFS_CHAT_ARCHIVE_TIME = "ChatArchiveTime"
    private const val SHARED_PREFS_CHAT_LAST_START = "ChatLastStart"
    private const val SHARED_PREFS_DEVELOPER_TOOLS = "DeveloperTools"
    private const val SHARED_PREFS_NETWORK_USE_SOCKS_PROXY = "NetworkUseSocksProxy"
    private const val SHARED_PREFS_NETWORK_SESSION_MODE = "NetworkSessionMode"
    private const val SHARED_PREFS_NETWORK_HOST_MODE = "NetworkHostMode"
    private const val SHARED_PREFS_NETWORK_REQUIRED_HOST_MODE = "NetworkRequiredHostMode"
    private const val SHARED_PREFS_NETWORK_TCP_CONNECT_TIMEOUT = "NetworkTCPConnectTimeout"
    private const val SHARED_PREFS_NETWORK_TCP_TIMEOUT = "NetworkTCPTimeout"
    private const val SHARED_PREFS_NETWORK_SMP_PING_INTERVAL = "NetworkSMPPingInterval"
    private const val SHARED_PREFS_NETWORK_SMP_PING_COUNT = "NetworkSMPPingCount"
    private const val SHARED_PREFS_NETWORK_ENABLE_KEEP_ALIVE = "NetworkEnableKeepAlive"
    private const val SHARED_PREFS_NETWORK_TCP_KEEP_IDLE = "NetworkTCPKeepIdle"
    private const val SHARED_PREFS_NETWORK_TCP_KEEP_INTVL = "NetworkTCPKeepIntvl"
    private const val SHARED_PREFS_NETWORK_TCP_KEEP_CNT = "NetworkTCPKeepCnt"
    private const val SHARED_PREFS_INCOGNITO = "Incognito"
    private const val SHARED_PREFS_CONNECT_VIA_LINK_TAB = "ConnectViaLinkTab"
    private const val SHARED_PREFS_LIVE_MESSAGE_ALERT_SHOWN = "LiveMessageAlertShown"
    private const val SHARED_PREFS_STORE_DB_PASSPHRASE = "StoreDBPassphrase"
    private const val SHARED_PREFS_INITIAL_RANDOM_DB_PASSPHRASE = "InitialRandomDBPassphrase"
    private const val SHARED_PREFS_ENCRYPTED_DB_PASSPHRASE = "EncryptedDBPassphrase"
    private const val SHARED_PREFS_INITIALIZATION_VECTOR_DB_PASSPHRASE = "InitializationVectorDBPassphrase"
    private const val SHARED_PREFS_ENCRYPTION_STARTED_AT = "EncryptionStartedAt"
    private const val SHARED_PREFS_CURRENT_THEME = "CurrentTheme"
    private const val SHARED_PREFS_PRIMARY_COLOR = "PrimaryColor"
    private const val SHARED_PREFS_WHATS_NEW_VERSION = "WhatsNewVersion"
  }
}

private const val MESSAGE_TIMEOUT: Int = 15_000_000

open class ChatController(var ctrl: ChatCtrl?, val ntfManager: NtfManager, val appContext: Context, val appPrefs: AppPreferences) {
  val chatModel = ChatModel(this)
  private var receiverStarted = false
  var lastMsgReceivedTimestamp: Long = System.currentTimeMillis()
    private set

  init {
    chatModel.notificationsMode.value =
      kotlin.runCatching { NotificationsMode.valueOf(appPrefs.notificationsMode.get()!!) }.getOrDefault(NotificationsMode.default)
    chatModel.notificationPreviewMode.value =
      kotlin.runCatching { NotificationPreviewMode.valueOf(appPrefs.notificationPreviewMode.get()!!) }.getOrDefault(NotificationPreviewMode.default)
    chatModel.performLA.value = appPrefs.performLA.get()
    chatModel.incognito.value = appPrefs.incognito.get()
  }

  suspend fun startChat(user: User) {
    Log.d(TAG, "user: $user")
    try {
      if (chatModel.chatRunning.value == true) return
      apiSetNetworkConfig(getNetCfg())
      val justStarted = apiStartChat()
      val users = listUsers()
      chatModel.users.clear()
      chatModel.users.addAll(users)
      if (justStarted) {
        chatModel.currentUser.value = user
        chatModel.userCreated.value = true
        apiSetFilesFolder(getAppFilesDirectory(appContext))
        apiSetIncognito(chatModel.incognito.value)
        getUserChatData()
        chatModel.onboardingStage.value = OnboardingStage.OnboardingComplete
        chatModel.controller.appPrefs.chatLastStart.set(Clock.System.now())
        chatModel.chatRunning.value = true
        startReceiver()
        Log.d(TAG, "startChat: started")
      } else {
        val chats = apiGetChats()
        chatModel.updateChats(chats)
        Log.d(TAG, "startChat: running")
      }
    } catch (e: Error) {
      Log.e(TAG, "failed starting chat $e")
      throw e
    }
  }

  suspend fun changeActiveUser(toUserId: Long) {
    try {
      changeActiveUser_(toUserId)
    } catch (e: Exception) {
      Log.e(TAG, "Unable to set active user: ${e.stackTraceToString()}")
      AlertManager.shared.showAlertMsg(generalGetString(R.string.failed_to_active_user_title), e.stackTraceToString())
    }
  }

  suspend fun changeActiveUser_(toUserId: Long) {
    chatModel.currentUser.value = apiSetActiveUser(toUserId)
    val users = listUsers()
    chatModel.users.clear()
    chatModel.users.addAll(users)
    getUserChatData()
  }

  suspend fun getUserChatData() {
    chatModel.userAddress.value = apiGetUserAddress()
    val smpServers = getUserSMPServers()
    chatModel.userSMPServers.value = smpServers?.first
    chatModel.presetSMPServers.value = smpServers?.second
    chatModel.chatItemTTL.value = getChatItemTTL()
    val chats = apiGetChats()
    chatModel.updateChats(chats)
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
        val msg = recvMsg(ctrl)
        if (msg != null) processReceivedMsg(msg)
      }
    }
  }

  suspend fun sendCmd(cmd: CC): CR {
    val ctrl = ctrl ?: throw Exception("Controller is not initialized")

    return withContext(Dispatchers.IO) {
      val c = cmd.cmdString
      if (cmd !is CC.ApiParseMarkdown) {
        chatModel.addTerminalItem(TerminalItem.cmd(cmd.obfuscated))
        Log.d(TAG, "sendCmd: ${cmd.cmdType}")
      }
      val json = chatSendCmd(ctrl, c)
      val r = APIResponse.decodeStr(json)
      Log.d(TAG, "sendCmd response type ${r.resp.responseType}")
      if (r.resp is CR.Response || r.resp is CR.Invalid) {
        Log.d(TAG, "sendCmd response json $json")
      }
      if (r.resp !is CR.ParsedMarkdown) {
        chatModel.addTerminalItem(TerminalItem.resp(r.resp))
      }
      r.resp
    }
  }

  private fun recvMsg(ctrl: ChatCtrl): CR? {
    val json = chatRecvMsgWait(ctrl, MESSAGE_TIMEOUT)
    return if (json == "") {
      null
    } else {
      val r = APIResponse.decodeStr(json).resp
      Log.d(TAG, "chatRecvMsg: ${r.responseType}")
      if (r is CR.Response || r is CR.Invalid) Log.d(TAG, "chatRecvMsg json: $json")
      r
    }
  }

  suspend fun apiGetActiveUser(): User? {
    val r = sendCmd(CC.ShowActiveUser())
    if (r is CR.ActiveUser) return r.user
    Log.d(TAG, "apiGetActiveUser: ${r.responseType} ${r.details}")
    chatModel.userCreated.value = false
    return null
  }

  suspend fun apiCreateActiveUser(p: Profile): User? {
    val r = sendCmd(CC.CreateActiveUser(p))
    if (r is CR.ActiveUser) return r.user
    else if (
      r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorStore && r.chatError.storeError is StoreError.DuplicateName ||
      r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorChat && r.chatError.errorType is ChatErrorType.UserExists
    ) {
      AlertManager.shared.showAlertMsg(generalGetString(R.string.failed_to_create_user_duplicate_title), generalGetString(R.string.failed_to_create_user_duplicate_desc))
    } else {
      AlertManager.shared.showAlertMsg(generalGetString(R.string.failed_to_create_user_title), r.details)
    }
    Log.d(TAG, "apiCreateActiveUser: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun listUsers(): List<UserInfo> {
    val r = sendCmd(CC.ListUsers())
    if (r is CR.UsersList) return r.users.sortedBy { it.user.chatViewName }
    Log.d(TAG, "listUsers: ${r.responseType} ${r.details}")
    throw Exception("failed to list users ${r.responseType} ${r.details}")
  }

  suspend fun apiSetActiveUser(userId: Long): User {
    val r = sendCmd(CC.ApiSetActiveUser(userId))
    if (r is CR.ActiveUser) return r.user
    Log.d(TAG, "apiSetActiveUser: ${r.responseType} ${r.details}")
    throw Exception("failed to set the user as active ${r.responseType} ${r.details}")
  }

  suspend fun apiDeleteUser(userId: Long, delSMPQueues: Boolean) {
    val r = sendCmd(CC.ApiDeleteUser(userId, delSMPQueues))
    if (r is CR.CmdOk) return
    Log.d(TAG, "apiDeleteUser: ${r.responseType} ${r.details}")
    throw Exception("failed to delete the user ${r.responseType} ${r.details}")
  }

  suspend fun apiStartChat(): Boolean {
    val r = sendCmd(CC.StartChat(expire = true))
    when (r) {
      is CR.ChatStarted -> return true
      is CR.ChatRunning -> return false
      else -> throw Error("failed starting chat: ${r.responseType} ${r.details}")
    }
  }

  suspend fun apiStopChat(): Boolean {
    val r = sendCmd(CC.ApiStopChat())
    when (r) {
      is CR.ChatStopped -> return true
      else -> throw Error("failed stopping chat: ${r.responseType} ${r.details}")
    }
  }

  private suspend fun apiSetFilesFolder(filesFolder: String) {
    val r = sendCmd(CC.SetFilesFolder(filesFolder))
    if (r is CR.CmdOk) return
    throw Error("failed to set files folder: ${r.responseType} ${r.details}")
  }

  suspend fun apiSetIncognito(incognito: Boolean) {
    val r = sendCmd(CC.SetIncognito(incognito))
    if (r is CR.CmdOk) return
    throw Exception("failed to set incognito: ${r.responseType} ${r.details}")
  }

  suspend fun apiExportArchive(config: ArchiveConfig) {
    val r = sendCmd(CC.ApiExportArchive(config))
    if (r is CR.CmdOk) return
    throw Error("failed to export archive: ${r.responseType} ${r.details}")
  }

  suspend fun apiImportArchive(config: ArchiveConfig) {
    val r = sendCmd(CC.ApiImportArchive(config))
    if (r is CR.CmdOk) return
    throw Error("failed to import archive: ${r.responseType} ${r.details}")
  }

  suspend fun apiDeleteStorage() {
    val r = sendCmd(CC.ApiDeleteStorage())
    if (r is CR.CmdOk) return
    throw Error("failed to delete storage: ${r.responseType} ${r.details}")
  }

  suspend fun apiStorageEncryption(currentKey: String = "", newKey: String = ""): CR.ChatCmdError? {
    val r = sendCmd(CC.ApiStorageEncryption(DBEncryptionConfig(currentKey, newKey)))
    if (r is CR.CmdOk) return null
    else if (r is CR.ChatCmdError) return r
    throw Exception("failed to set storage encryption: ${r.responseType} ${r.details}")
  }

  suspend fun apiGetChats(): List<Chat> {
    val userId = chatModel.currentUser.value?.userId ?: run {
      Log.e(TAG, "apiGetChats: no current user")
      return emptyList()
    }
    val r = sendCmd(CC.ApiGetChats(userId))
    if (r is CR.ApiChats) return r.chats
    Log.e(TAG, "failed getting the list of chats: ${r.responseType} ${r.details}")
    AlertManager.shared.showAlertMsg(generalGetString(R.string.failed_to_parse_chats_title), generalGetString(R.string.contact_developers))
    return emptyList()
  }

  suspend fun apiGetChat(type: ChatType, id: Long, pagination: ChatPagination = ChatPagination.Last(ChatPagination.INITIAL_COUNT), search: String = ""): Chat? {
    val r = sendCmd(CC.ApiGetChat(type, id, pagination, search))
    if (r is CR.ApiChat) return r.chat
    Log.e(TAG, "apiGetChat bad response: ${r.responseType} ${r.details}")
    AlertManager.shared.showAlertMsg(generalGetString(R.string.failed_to_parse_chat_title), generalGetString(R.string.contact_developers))
    return null
  }

  suspend fun apiSendMessage(type: ChatType, id: Long, file: String? = null, quotedItemId: Long? = null, mc: MsgContent, live: Boolean = false): AChatItem? {
    val cmd = CC.ApiSendMessage(type, id, file, quotedItemId, mc, live)
    val r = sendCmd(cmd)
    return when (r) {
      is CR.NewChatItem -> r.chatItem
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiSendMessage", generalGetString(R.string.error_sending_message), r)
        }
        null
      }
    }
  }

  suspend fun apiUpdateChatItem(type: ChatType, id: Long, itemId: Long, mc: MsgContent, live: Boolean = false): AChatItem? {
    val r = sendCmd(CC.ApiUpdateChatItem(type, id, itemId, mc, live))
    if (r is CR.ChatItemUpdated) return r.chatItem
    Log.e(TAG, "apiUpdateChatItem bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiDeleteChatItem(type: ChatType, id: Long, itemId: Long, mode: CIDeleteMode): CR.ChatItemDeleted? {
    val r = sendCmd(CC.ApiDeleteChatItem(type, id, itemId, mode))
    if (r is CR.ChatItemDeleted) return r
    Log.e(TAG, "apiDeleteChatItem bad response: ${r.responseType} ${r.details}")
    return null
  }

  private suspend fun getUserSMPServers(): Pair<List<ServerCfg>, List<String>>? {
    val userId = chatModel.currentUser.value?.userId ?: run {
      Log.e(TAG, "getUserSMPServers: no current user")
      return null
    }
    val r = sendCmd(CC.APIGetUserSMPServers(userId))
    if (r is CR.UserSMPServers) return r.smpServers to r.presetSMPServers
    Log.e(TAG, "getUserSMPServers bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun setUserSMPServers(smpServers: List<ServerCfg>): Boolean {
    val userId = chatModel.currentUser.value?.userId ?: run {
      Log.e(TAG, "setUserSMPServers: no current user")
      return false
    }
    val r = sendCmd(CC.APISetUserSMPServers(userId, smpServers))
    return when (r) {
      is CR.CmdOk -> true
      else -> {
        Log.e(TAG, "setUserSMPServers bad response: ${r.responseType} ${r.details}")
        AlertManager.shared.showAlertMsg(
          generalGetString(R.string.error_saving_smp_servers),
          generalGetString(R.string.ensure_smp_server_address_are_correct_format_and_unique)
        )
        false
      }
    }
  }

  suspend fun testSMPServer(smpServer: String): SMPTestFailure? {
    val userId = chatModel.currentUser.value?.userId ?: run { throw Exception("testSMPServer: no current user") }
    val r = sendCmd(CC.TestSMPServer(userId, smpServer))
    return when (r) {
      is CR.SmpTestResult -> r.smpTestFailure
      else -> {
        Log.e(TAG, "testSMPServer bad response: ${r.responseType} ${r.details}")
        throw Exception("testSMPServer bad response: ${r.responseType} ${r.details}")
      }
    }
  }

  suspend fun getChatItemTTL(): ChatItemTTL {
    val userId = chatModel.currentUser.value?.userId ?: run { throw Exception("getChatItemTTL: no current user") }
    val r = sendCmd(CC.APIGetChatItemTTL(userId))
    if (r is CR.ChatItemTTL) return ChatItemTTL.fromSeconds(r.chatItemTTL)
    throw Exception("failed to get chat item TTL: ${r.responseType} ${r.details}")
  }

  suspend fun setChatItemTTL(chatItemTTL: ChatItemTTL) {
    val userId = chatModel.currentUser.value?.userId ?: run { throw Exception("setChatItemTTL: no current user") }
    val r = sendCmd(CC.APISetChatItemTTL(userId, chatItemTTL.seconds))
    if (r is CR.CmdOk) return
    throw Exception("failed to set chat item TTL: ${r.responseType} ${r.details}")
  }

  suspend fun apiGetNetworkConfig(): NetCfg? {
    val r = sendCmd(CC.APIGetNetworkConfig())
    if (r is CR.NetworkConfig) return r.networkConfig
    Log.e(TAG, "apiGetNetworkConfig bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiSetNetworkConfig(cfg: NetCfg): Boolean {
    val r = sendCmd(CC.APISetNetworkConfig(cfg))
    return when (r) {
      is CR.CmdOk -> true
      else -> {
        Log.e(TAG, "apiSetNetworkConfig bad response: ${r.responseType} ${r.details}")
        AlertManager.shared.showAlertMsg(
          generalGetString(R.string.error_setting_network_config),
          "${r.responseType}: ${r.details}"
        )
        false
      }
    }
  }

  suspend fun apiSetSettings(type: ChatType,id: Long, settings: ChatSettings): Boolean {
    val r = sendCmd(CC.APISetChatSettings(type, id, settings))
    return when (r) {
      is CR.CmdOk -> true
      else -> {
        Log.e(TAG, "apiSetSettings bad response: ${r.responseType} ${r.details}")
        false
      }
    }
  }

  suspend fun apiContactInfo(contactId: Long): Pair<ConnectionStats, Profile?>? {
    val r = sendCmd(CC.APIContactInfo(contactId))
    if (r is CR.ContactInfo) return r.connectionStats to r.customUserProfile
    Log.e(TAG, "apiContactInfo bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiGroupMemberInfo(groupId: Long, groupMemberId: Long): ConnectionStats? {
    val r = sendCmd(CC.APIGroupMemberInfo(groupId, groupMemberId))
    if (r is CR.GroupMemberInfo) return r.connectionStats_
    Log.e(TAG, "apiGroupMemberInfo bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiSwitchContact(contactId: Long) {
    return when (val r = sendCmd(CC.APISwitchContact(contactId))) {
      is CR.CmdOk -> {}
      else -> {
        apiErrorAlert("apiSwitchContact", generalGetString(R.string.connection_error), r)
      }
    }
  }

  suspend fun apiSwitchGroupMember(groupId: Long, groupMemberId: Long) {
    return when (val r = sendCmd(CC.APISwitchGroupMember(groupId, groupMemberId))) {
      is CR.CmdOk -> {}
      else -> {
        apiErrorAlert("apiSwitchGroupMember", generalGetString(R.string.error_changing_address), r)
      }
    }
  }

  suspend fun apiGetContactCode(contactId: Long): Pair<Contact, String> {
    val r = sendCmd(CC.APIGetContactCode(contactId))
    if (r is CR.ContactCode) return r.contact to r.connectionCode
    throw Exception("failed to get contact code: ${r.responseType} ${r.details}")
  }

  suspend fun apiGetGroupMemberCode(groupId: Long, groupMemberId: Long): Pair<GroupMember, String> {
    val r = sendCmd(CC.APIGetGroupMemberCode(groupId, groupMemberId))
    if (r is CR.GroupMemberCode) return r.member to r.connectionCode
    throw Exception("failed to get group member code: ${r.responseType} ${r.details}")
  }

  suspend fun apiVerifyContact(contactId: Long, connectionCode: String?): Pair<Boolean, String>? {
    return when (val r = sendCmd(CC.APIVerifyContact(contactId, connectionCode))) {
      is CR.ConnectionVerified -> r.verified to r.expectedCode
      else -> null
    }
  }

  suspend fun apiVerifyGroupMember(groupId: Long, groupMemberId: Long, connectionCode: String?): Pair<Boolean, String>? {
    return when (val r = sendCmd(CC.APIVerifyGroupMember(groupId, groupMemberId, connectionCode))) {
      is CR.ConnectionVerified -> r.verified to r.expectedCode
      else -> null
    }
  }



  suspend fun apiAddContact(): String? {
    val userId = chatModel.currentUser.value?.userId ?: run {
      Log.e(TAG, "apiAddContact: no current user")
      return null
    }
    val r = sendCmd(CC.APIAddContact(userId))
    return when (r) {
      is CR.Invitation -> r.connReqInvitation
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiAddContact", generalGetString(R.string.connection_error), r)
        }
        null
      }
    }
  }

  suspend fun apiConnect(connReq: String): Boolean  {
    val userId = chatModel.currentUser.value?.userId ?: run {
      Log.e(TAG, "apiConnect: no current user")
      return false
    }
    val r = sendCmd(CC.APIConnect(userId, connReq))
    when {
      r is CR.SentConfirmation || r is CR.SentInvitation -> return true
      r is CR.ContactAlreadyExists -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(R.string.contact_already_exists),
          String.format(generalGetString(R.string.you_are_already_connected_to_vName_via_this_link), r.contact.displayName)
        )
        return false
      }
      r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorChat
          && r.chatError.errorType is ChatErrorType.InvalidConnReq -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(R.string.invalid_connection_link),
          generalGetString(R.string.please_check_correct_link_and_maybe_ask_for_a_new_one)
        )
        return false
      }
      r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorAgent
          && r.chatError.agentError is AgentErrorType.SMP
          && r.chatError.agentError.smpErr is SMPErrorType.AUTH -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(R.string.connection_error_auth),
          generalGetString(R.string.connection_error_auth_desc)
        )
        return false
      }
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiConnect", generalGetString(R.string.connection_error), r)
        }
        return false
      }
    }
  }

  suspend fun apiDeleteChat(type: ChatType, id: Long): Boolean {
    val r = sendCmd(CC.ApiDeleteChat(type, id))
    when {
      r is CR.ContactDeleted && type == ChatType.Direct -> return true
      r is CR.ContactConnectionDeleted && type == ChatType.ContactConnection -> return true
      r is CR.GroupDeletedUser && type == ChatType.Group -> return true
      else -> {
        val titleId = when (type) {
          ChatType.Direct -> R.string.error_deleting_contact
          ChatType.Group -> R.string.error_deleting_group
          ChatType.ContactRequest -> R.string.error_deleting_contact_request
          ChatType.ContactConnection -> R.string.error_deleting_pending_contact_connection
        }
        apiErrorAlert("apiDeleteChat", generalGetString(titleId), r)
      }
    }
    return false
  }

  suspend fun apiClearChat(type: ChatType, id: Long): ChatInfo? {
    val r = sendCmd(CC.ApiClearChat(type, id))
    if (r is CR.ChatCleared) return r.chatInfo
    Log.e(TAG, "apiClearChat bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiListContacts(): List<Contact>? {
    val userId = chatModel.currentUser.value?.userId ?: run {
      Log.e(TAG, "apiListContacts: no current user")
      return null
    }
    val r = sendCmd(CC.ApiListContacts(userId))
    if (r is CR.ContactsList) return r.contacts
    Log.e(TAG, "apiListContacts bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiUpdateProfile(profile: Profile): Profile? {
    val userId = chatModel.currentUser.value?.userId ?: run {
      Log.e(TAG, "apiUpdateProfile: no current user")
      return null
    }
    val r = sendCmd(CC.ApiUpdateProfile(userId, profile))
    if (r is CR.UserProfileNoChange) return profile
    if (r is CR.UserProfileUpdated) return r.toProfile
    Log.e(TAG, "apiUpdateProfile bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiSetContactPrefs(contactId: Long, prefs: ChatPreferences): Contact? {
    val r = sendCmd(CC.ApiSetContactPrefs(contactId, prefs))
    if (r is CR.ContactPrefsUpdated) return r.toContact
    Log.e(TAG, "apiSetContactPrefs bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiSetContactAlias(contactId: Long, localAlias: String): Contact? {
    val r = sendCmd(CC.ApiSetContactAlias(contactId, localAlias))
    if (r is CR.ContactAliasUpdated) return r.toContact
    Log.e(TAG, "apiSetContactAlias bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiSetConnectionAlias(connId: Long, localAlias: String): PendingContactConnection? {
    val r = sendCmd(CC.ApiSetConnectionAlias(connId, localAlias))
    if (r is CR.ConnectionAliasUpdated) return r.toConnection
    Log.e(TAG, "apiSetConnectionAlias bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiCreateUserAddress(): String? {
    val userId = chatModel.currentUser.value?.userId ?: run {
      Log.e(TAG, "apiCreateUserAddress: no current user")
      return null
    }
    val r = sendCmd(CC.ApiCreateMyAddress(userId))
    return when (r) {
      is CR.UserContactLinkCreated -> r.connReqContact
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiCreateUserAddress", generalGetString(R.string.error_creating_address), r)
        }
        null
      }
    }
  }

  suspend fun apiDeleteUserAddress(): Boolean {
    val userId = chatModel.currentUser.value?.userId ?: run {
      Log.e(TAG, "apiDeleteUserAddress: no current user")
      return false
    }
    val r = sendCmd(CC.ApiDeleteMyAddress(userId))
    if (r is CR.UserContactLinkDeleted) return true
    Log.e(TAG, "apiDeleteUserAddress bad response: ${r.responseType} ${r.details}")
    return false
  }

  private suspend fun apiGetUserAddress(): UserContactLinkRec? {
    val userId = chatModel.currentUser.value?.userId ?: run {
      Log.e(TAG, "apiGetUserAddress: no current user")
      return null
    }
    val r = sendCmd(CC.ApiShowMyAddress(userId))
    if (r is CR.UserContactLink) return r.contactLink
    if (r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorStore
      && r.chatError.storeError is StoreError.UserContactLinkNotFound) {
      return null
    }
    Log.e(TAG, "apiGetUserAddress bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun userAddressAutoAccept(autoAccept: AutoAccept?): UserContactLinkRec? {
    val userId = chatModel.currentUser.value?.userId ?: run {
      Log.e(TAG, "userAddressAutoAccept: no current user")
      return null
    }
    val r = sendCmd(CC.ApiAddressAutoAccept(userId, autoAccept))
    if (r is CR.UserContactLinkUpdated) return r.contactLink
    if (r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorStore
      && r.chatError.storeError is StoreError.UserContactLinkNotFound) {
      return null
    }
    Log.e(TAG, "userAddressAutoAccept bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiAcceptContactRequest(contactReqId: Long): Contact? {
    val r = sendCmd(CC.ApiAcceptContact(contactReqId))
    return when {
      r is CR.AcceptingContactRequest -> r.contact
      r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorAgent
          && r.chatError.agentError is AgentErrorType.SMP
          && r.chatError.agentError.smpErr is SMPErrorType.AUTH -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(R.string.connection_error_auth),
          generalGetString(R.string.sender_may_have_deleted_the_connection_request)
        )
        null
      }
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiAcceptContactRequest", generalGetString(R.string.error_accepting_contact_request), r)
        }
        null
      }
    }
  }

  suspend fun apiRejectContactRequest(contactReqId: Long): Boolean {
    val r = sendCmd(CC.ApiRejectContact(contactReqId))
    if (r is CR.ContactRequestRejected) return true
    Log.e(TAG, "apiRejectContactRequest bad response: ${r.responseType} ${r.details}")
    return false
  }

  suspend fun apiSendCallInvitation(contact: Contact, callType: CallType): Boolean {
    val r = sendCmd(CC.ApiSendCallInvitation(contact, callType))
    return r is CR.CmdOk
  }

  suspend fun apiRejectCall(contact: Contact): Boolean {
    val r = sendCmd(CC.ApiRejectCall(contact))
    return r is CR.CmdOk
  }

  suspend fun apiSendCallOffer(contact: Contact, rtcSession: String, rtcIceCandidates: String, media: CallMediaType, capabilities: CallCapabilities): Boolean {
    val webRtcSession = WebRTCSession(rtcSession, rtcIceCandidates)
    val callOffer = WebRTCCallOffer(CallType(media, capabilities), webRtcSession)
    val r = sendCmd(CC.ApiSendCallOffer(contact, callOffer))
    return r is CR.CmdOk
  }

  suspend fun apiSendCallAnswer(contact: Contact, rtcSession: String, rtcIceCandidates: String): Boolean {
    val answer = WebRTCSession(rtcSession, rtcIceCandidates)
    val r = sendCmd(CC.ApiSendCallAnswer(contact, answer))
    return r is CR.CmdOk
  }

  suspend fun apiSendCallExtraInfo(contact: Contact, rtcIceCandidates: String): Boolean {
    val extraInfo = WebRTCExtraInfo(rtcIceCandidates)
    val r = sendCmd(CC.ApiSendCallExtraInfo(contact, extraInfo))
    return r is CR.CmdOk
  }

  suspend fun apiEndCall(contact: Contact): Boolean {
    val r = sendCmd(CC.ApiEndCall(contact))
    return r is CR.CmdOk
  }

  suspend fun apiCallStatus(contact: Contact, status: WebRTCCallStatus): Boolean {
    val r = sendCmd(CC.ApiCallStatus(contact, status))
    return r is CR.CmdOk
  }

  suspend fun apiChatRead(type: ChatType, id: Long, range: CC.ItemRange): Boolean {
    val r = sendCmd(CC.ApiChatRead(type, id, range))
    if (r is CR.CmdOk) return true
    Log.e(TAG, "apiChatRead bad response: ${r.responseType} ${r.details}")
    return false
  }

  suspend fun apiChatUnread(type: ChatType, id: Long, unreadChat: Boolean): Boolean {
    val r = sendCmd(CC.ApiChatUnread(type, id, unreadChat))
    if (r is CR.CmdOk) return true
    Log.e(TAG, "apiChatUnread bad response: ${r.responseType} ${r.details}")
    return false
  }

  suspend fun apiReceiveFile(fileId: Long, inline: Boolean? = null): AChatItem? {
    val r = sendCmd(CC.ReceiveFile(fileId, inline))
    return when (r) {
      is CR.RcvFileAccepted -> r.chatItem
      is CR.RcvFileAcceptedSndCancelled -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(R.string.cannot_receive_file),
          generalGetString(R.string.sender_cancelled_file_transfer)
        )
        null
      }
      else -> {
        if (!(networkErrorAlert(r))) {
          if (r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorChat
            && r.chatError.errorType is ChatErrorType.FileAlreadyReceiving
          ) {
            Log.d(TAG, "apiReceiveFile ignoring FileAlreadyReceiving error")
          } else {
            apiErrorAlert("apiReceiveFile", generalGetString(R.string.error_receiving_file), r)
          }
        }
        null
      }
    }
  }

  suspend fun apiNewGroup(p: GroupProfile): GroupInfo? {
    val userId = chatModel.currentUser.value?.userId ?: run {
      Log.e(TAG, "apiNewGroup: no current user")
      return null
    }
    val r = sendCmd(CC.ApiNewGroup(userId, p))
    if (r is CR.GroupCreated) return r.groupInfo
    Log.e(TAG, "apiNewGroup bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiAddMember(groupId: Long, contactId: Long, memberRole: GroupMemberRole): GroupMember? {
    val r = sendCmd(CC.ApiAddMember(groupId, contactId, memberRole))
    return when (r) {
      is CR.SentGroupInvitation -> r.member
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiAddMember", generalGetString(R.string.error_adding_members), r)
        }
        null
      }
    }
  }

  suspend fun apiJoinGroup(groupId: Long) {
    val r = sendCmd(CC.ApiJoinGroup(groupId))
    when (r) {
      is CR.UserAcceptedGroupSent ->
        chatModel.updateGroup(r.groupInfo)
      is CR.ChatCmdError -> {
        val e = r.chatError
        suspend fun deleteGroup() { if (apiDeleteChat(ChatType.Group, groupId)) { chatModel.removeChat("#$groupId") } }
        if (e is ChatError.ChatErrorAgent && e.agentError is AgentErrorType.SMP && e.agentError.smpErr is SMPErrorType.AUTH) {
          deleteGroup()
          AlertManager.shared.showAlertMsg(generalGetString(R.string.alert_title_group_invitation_expired), generalGetString(R.string.alert_message_group_invitation_expired))
        } else if (e is ChatError.ChatErrorStore && e.storeError is StoreError.GroupNotFound) {
          deleteGroup()
          AlertManager.shared.showAlertMsg(generalGetString(R.string.alert_title_no_group), generalGetString(R.string.alert_message_no_group))
        } else if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiJoinGroup", generalGetString(R.string.error_joining_group), r)
        }
      }
      else -> apiErrorAlert("apiJoinGroup", generalGetString(R.string.error_joining_group), r)
    }
  }

  suspend fun apiRemoveMember(groupId: Long, memberId: Long): GroupMember? =
    when (val r = sendCmd(CC.ApiRemoveMember(groupId, memberId))) {
      is CR.UserDeletedMember -> r.member
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiRemoveMember", generalGetString(R.string.error_removing_member), r)
        }
        null
      }
    }

  suspend fun apiMemberRole(groupId: Long, memberId: Long, memberRole: GroupMemberRole): GroupMember =
    when (val r = sendCmd(CC.ApiMemberRole(groupId, memberId, memberRole))) {
      is CR.MemberRoleUser -> r.member
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiMemberRole", generalGetString(R.string.error_changing_role), r)
        }
        throw Exception("failed to change member role: ${r.responseType} ${r.details}")
      }
    }

  suspend fun apiLeaveGroup(groupId: Long): GroupInfo? {
    val r = sendCmd(CC.ApiLeaveGroup(groupId))
    if (r is CR.LeftMemberUser) return r.groupInfo
    Log.e(TAG, "apiLeaveGroup bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiListMembers(groupId: Long): List<GroupMember> {
    val r = sendCmd(CC.ApiListMembers(groupId))
    if (r is CR.GroupMembers) return r.group.members
    Log.e(TAG, "apiListMembers bad response: ${r.responseType} ${r.details}")
    return emptyList()
  }

  suspend fun apiUpdateGroup(groupId: Long, groupProfile: GroupProfile): GroupInfo? {
    return when (val r = sendCmd(CC.ApiUpdateGroupProfile(groupId, groupProfile))) {
      is CR.GroupUpdated -> r.toGroup
      is CR.ChatCmdError -> {
        AlertManager.shared.showAlertMsg(generalGetString(R.string.error_saving_group_profile), "$r.chatError")
        null
      }
      else -> {
        Log.e(TAG, "apiUpdateGroup bad response: ${r.responseType} ${r.details}")
        AlertManager.shared.showAlertMsg(
          generalGetString(R.string.error_saving_group_profile),
          "${r.responseType}: ${r.details}"
        )
        null
      }
    }
  }

  suspend fun apiCreateGroupLink(groupId: Long): String? {
    return when (val r = sendCmd(CC.APICreateGroupLink(groupId))) {
      is CR.GroupLinkCreated -> r.connReqContact
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiCreateGroupLink", generalGetString(R.string.error_creating_link_for_group), r)
        }
        null
      }
    }
  }

  suspend fun apiDeleteGroupLink(groupId: Long): Boolean {
    return when (val r = sendCmd(CC.APIDeleteGroupLink(groupId))) {
      is CR.GroupLinkDeleted -> true
      else -> {
        if (!(networkErrorAlert(r))) {
          apiErrorAlert("apiDeleteGroupLink", generalGetString(R.string.error_deleting_link_for_group), r)
        }
        false
      }
    }
  }

  suspend fun apiGetGroupLink(groupId: Long): String? {
    return when (val r = sendCmd(CC.APIGetGroupLink(groupId))) {
      is CR.GroupLink -> r.connReqContact
      else -> {
        Log.e(TAG, "apiGetGroupLink bad response: ${r.responseType} ${r.details}")
        null
      }
    }
  }

  suspend fun allowFeatureToContact(contact: Contact, feature: ChatFeature, param: Int? = null) {
    val prefs = contact.mergedPreferences.toPreferences().setAllowed(feature, param = param)
    val toContact = apiSetContactPrefs(contact.contactId, prefs)
    if (toContact != null) {
      chatModel.updateContact(toContact)
    }
  }

  suspend fun apiGetVersion(): CoreVersionInfo? {
    val r = sendCmd(CC.ShowVersion())
    return if (r is CR.VersionInfo) {
      r.versionInfo
    } else {
      Log.e(TAG, "apiGetVersion bad response: ${r.responseType} ${r.details}")
      null
    }
  }

  suspend fun apiParseMarkdown(text: String): List<FormattedText>? {
    val r = sendCmd(CC.ApiParseMarkdown(text))
    if (r is CR.ParsedMarkdown) return r.formattedText
    Log.e(TAG, "apiParseMarkdown bad response: ${r.responseType} ${r.details}")
    return null
  }

  private fun networkErrorAlert(r: CR): Boolean {
    return when {
      r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorAgent
          && r.chatError.agentError is AgentErrorType.BROKER
          && r.chatError.agentError.brokerErr is BrokerErrorType.TIMEOUT -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(R.string.connection_timeout),
          String.format(generalGetString(R.string.network_error_desc), serverHostname(r.chatError.agentError.brokerAddress))
        )
        true
      }
      r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorAgent
          && r.chatError.agentError is AgentErrorType.BROKER
          && r.chatError.agentError.brokerErr is BrokerErrorType.NETWORK -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(R.string.connection_error),
          String.format(generalGetString(R.string.network_error_desc), serverHostname(r.chatError.agentError.brokerAddress))
        )
        true
      }
      else -> false
    }
  }

  fun apiErrorAlert(method: String, title: String, r: CR) {
    val errMsg = "${r.responseType}: ${r.details}"
    Log.e(TAG, "$method bad response: $errMsg")
    AlertManager.shared.showAlertMsg(title, errMsg)
  }

  suspend fun processReceivedMsg(r: CR) {
    lastMsgReceivedTimestamp = System.currentTimeMillis()
    chatModel.addTerminalItem(TerminalItem.resp(r))
    when (r) {
      is CR.NewContactConnection -> {
        if (active(r.user)) {
          chatModel.updateContactConnection(r.connection)
        }
      }
      is CR.ContactConnectionDeleted -> {
        if (active(r.user)) {
          chatModel.removeChat(r.connection.id)
        }
      }
      is CR.ContactConnected -> {
        if (active(r.user) && r.contact.directOrUsed) {
          chatModel.updateContact(r.contact)
          chatModel.dismissConnReqView(r.contact.activeConn.id)
          chatModel.removeChat(r.contact.activeConn.id)
        }
        if (r.contact.directOrUsed) {
          ntfManager.notifyContactConnected(r.user, r.contact)
        }
        chatModel.setContactNetworkStatus(r.contact, NetworkStatus.Connected())
      }
      is CR.ContactConnecting -> {
        if (active(r.user) && r.contact.directOrUsed) {
          chatModel.updateContact(r.contact)
          chatModel.dismissConnReqView(r.contact.activeConn.id)
          chatModel.removeChat(r.contact.activeConn.id)
        }
      }
      is CR.ReceivedContactRequest -> {
        val contactRequest = r.contactRequest
        val cInfo = ChatInfo.ContactRequest(contactRequest)
        if (active(r.user)) {
          chatModel.addChat(Chat(chatInfo = cInfo, chatItems = listOf()))
        }
        ntfManager.notifyContactRequestReceived(r.user, cInfo)
      }
      is CR.ContactUpdated -> {
        if (active(r.user) && chatModel.hasChat(r.toContact.id)) {
          val cInfo = ChatInfo.Direct(r.toContact)
          chatModel.updateChatInfo(cInfo)
        }
      }
      is CR.ContactsMerged -> {
        if (active(r.user) && chatModel.hasChat(r.mergedContact.id)) {
          if (chatModel.chatId.value == r.mergedContact.id) {
            chatModel.chatId.value = r.intoContact.id
          }
          chatModel.removeChat(r.mergedContact.id)
        }
      }
      is CR.ContactsSubscribed -> updateContactsStatus(r.contactRefs, NetworkStatus.Connected())
      is CR.ContactsDisconnected -> updateContactsStatus(r.contactRefs, NetworkStatus.Disconnected())
      is CR.ContactSubError -> {
        if (active(r.user)) {
          chatModel.updateContact(r.contact)
        }
        processContactSubError(r.contact, r.chatError)
      }
      is CR.ContactSubSummary -> {
        for (sub in r.contactSubscriptions) {
          if (active(r.user)) {
            chatModel.updateContact(sub.contact)
          }
          val err = sub.contactError
          if (err == null) {
            chatModel.setContactNetworkStatus(sub.contact, NetworkStatus.Connected())
          } else {
            processContactSubError(sub.contact, sub.contactError)
          }
        }
      }
      is CR.NewChatItem -> {
        val cInfo = r.chatItem.chatInfo
        val cItem = r.chatItem.chatItem
        if (active(r.user)) {
          chatModel.addChatItem(cInfo, cItem)
        } else if (cItem.isRcvNew && cInfo.ntfsEnabled) {
          chatModel.increaseUnreadCounter(r.user)
        }
        val file = cItem.file
        val mc = cItem.content.msgContent
        if (file != null && file.fileSize <= MAX_IMAGE_SIZE_AUTO_RCV) {
          val acceptImages = appPrefs.privacyAcceptImages.get()
          if ((mc is MsgContent.MCImage && acceptImages)
            || (mc is MsgContent.MCVoice && ((file.fileSize > MAX_VOICE_SIZE_FOR_SENDING && acceptImages) || cInfo is ChatInfo.Group))) {
            withApi { receiveFile(r.user, file.fileId) } // TODO check inlineFileMode != IFMSent
          }
        }
        if (cItem.showNotification && (!SimplexApp.context.isAppOnForeground || chatModel.chatId.value != cInfo.id)) {
          ntfManager.notifyMessageReceived(r.user, cInfo, cItem)
        }
      }
      is CR.ChatItemStatusUpdated -> {
        val cInfo = r.chatItem.chatInfo
        val cItem = r.chatItem.chatItem
        if (active(r.user) && !cItem.isDeletedContent && chatModel.upsertChatItem(cInfo, cItem)) {
          ntfManager.notifyMessageReceived(r.user, cInfo, cItem)
        }
        if (!active(r.user) && !cItem.isDeletedContent) {
          ntfManager.notifyMessageReceived(r.user, cInfo, cItem)
        }
      }
      is CR.ChatItemUpdated ->
        chatItemSimpleUpdate(r.user, r.chatItem)
      is CR.ChatItemDeleted -> {
        if (!active(r.user)) {
          if (r.toChatItem == null && r.deletedChatItem.chatItem.isRcvNew && r.deletedChatItem.chatInfo.ntfsEnabled) {
            chatModel.decreaseUnreadCounter(r.user)
          }
          return
        }

        val cInfo = r.deletedChatItem.chatInfo
        val cItem = r.deletedChatItem.chatItem
        AudioPlayer.stop(cItem)
        val isLastChatItem = chatModel.getChat(cInfo.id)?.chatItems?.lastOrNull()?.id == cItem.id
        if (isLastChatItem && ntfManager.hasNotificationsForChat(cInfo.id)) {
          ntfManager.cancelNotificationsForChat(cInfo.id)
          ntfManager.notifyMessageReceived(
            r.user,
            cInfo.id,
            cInfo.displayName,
            generalGetString(if (r.toChatItem != null) R.string.marked_deleted_description else R.string.deleted_description)
          )
        }
        if (r.toChatItem == null) {
          chatModel.removeChatItem(cInfo, cItem)
        } else {
          chatModel.upsertChatItem(cInfo, r.toChatItem.chatItem)
        }
      }
      is CR.ReceivedGroupInvitation -> {
        if (active(r.user)) {
          chatModel.updateGroup(r.groupInfo) // update so that repeat group invitations are not duplicated
          // TODO NtfManager.shared.notifyGroupInvitation
        }
      }
      is CR.UserAcceptedGroupSent -> {
        if (!active(r.user)) return

        chatModel.updateGroup(r.groupInfo)
        if (r.hostContact != null) {
          chatModel.dismissConnReqView(r.hostContact.activeConn.id)
          chatModel.removeChat(r.hostContact.activeConn.id)
        }
      }
      is CR.JoinedGroupMemberConnecting ->
        if (active(r.user)) {
          chatModel.upsertGroupMember(r.groupInfo, r.member)
        }
      is CR.DeletedMemberUser -> // TODO update user member
        if (active(r.user)) {
          chatModel.updateGroup(r.groupInfo)
        }
      is CR.DeletedMember ->
        if (active(r.user)) {
          chatModel.upsertGroupMember(r.groupInfo, r.deletedMember)
        }
      is CR.LeftMember ->
        if (active(r.user)) {
          chatModel.upsertGroupMember(r.groupInfo, r.member)
        }
      is CR.MemberRole ->
        if (active(r.user)) {
          chatModel.upsertGroupMember(r.groupInfo, r.member)
        }
      is CR.MemberRoleUser ->
        if (active(r.user)) {
          chatModel.upsertGroupMember(r.groupInfo, r.member)
        }
      is CR.GroupDeleted -> // TODO update user member
        if (active(r.user)) {
          chatModel.updateGroup(r.groupInfo)
        }
      is CR.UserJoinedGroup ->
        if (active(r.user)) {
          chatModel.updateGroup(r.groupInfo)
        }
      is CR.JoinedGroupMember ->
        if (active(r.user)) {
          chatModel.upsertGroupMember(r.groupInfo, r.member)
        }
      is CR.ConnectedToGroupMember ->
        if (active(r.user)) {
          chatModel.upsertGroupMember(r.groupInfo, r.member)
        }
      is CR.GroupUpdated ->
        if (active(r.user)) {
          chatModel.updateGroup(r.toGroup)
        }
      is CR.RcvFileStart ->
        chatItemSimpleUpdate(r.user, r.chatItem)
      is CR.RcvFileComplete ->
        chatItemSimpleUpdate(r.user, r.chatItem)
      is CR.SndFileStart ->
        chatItemSimpleUpdate(r.user, r.chatItem)
      is CR.SndFileComplete -> {
        chatItemSimpleUpdate(r.user, r.chatItem)
        val cItem = r.chatItem.chatItem
        val mc = cItem.content.msgContent
        val fileName = cItem.file?.fileName
        if (
          r.chatItem.chatInfo.chatType == ChatType.Direct
          && mc is MsgContent.MCFile
          && fileName != null
        ) {
          removeFile(appContext, fileName)
        }
      }
      is CR.CallInvitation ->
        chatModel.callManager.reportNewIncomingCall(r.callInvitation)
      is CR.CallOffer -> {
        // TODO askConfirmation?
        // TODO check encryption is compatible
        withCall(r, r.contact) { call ->
          chatModel.activeCall.value = call.copy(callState = CallState.OfferReceived, peerMedia = r.callType.media, sharedKey = r.sharedKey)
          val useRelay = chatModel.controller.appPrefs.webrtcPolicyRelay.get()
          val iceServers = getIceServers()
          Log.d(TAG, ".callOffer iceServers $iceServers")
          chatModel.callCommand.value = WCallCommand.Offer(
            offer = r.offer.rtcSession,
            iceCandidates = r.offer.rtcIceCandidates,
            media = r.callType.media,
            aesKey = r.sharedKey,
            iceServers = iceServers,
            relay = useRelay
          )
        }
      }
      is CR.CallAnswer -> {
        withCall(r, r.contact) { call ->
          chatModel.activeCall.value = call.copy(callState = CallState.AnswerReceived)
          chatModel.callCommand.value = WCallCommand.Answer(answer = r.answer.rtcSession, iceCandidates = r.answer.rtcIceCandidates)
        }
      }
      is CR.CallExtraInfo -> {
        withCall(r, r.contact) { _ ->
          chatModel.callCommand.value = WCallCommand.Ice(iceCandidates = r.extraInfo.rtcIceCandidates)
        }
      }
      is CR.CallEnded -> {
        val invitation = chatModel.callInvitations.remove(r.contact.id)
        if (invitation != null) {
          chatModel.callManager.reportCallRemoteEnded(invitation = invitation)
        }
        withCall(r, r.contact) { _ ->
          chatModel.callCommand.value = WCallCommand.End
          withApi {
            chatModel.activeCall.value = null
            chatModel.showCallView.value = false
          }
        }
      }
      else ->
        Log.d(TAG , "unsupported event: ${r.responseType}")
    }
  }

  private fun active(user: User): Boolean = user.userId == chatModel.currentUser.value?.userId

  private fun withCall(r: CR, contact: Contact, perform: (Call) -> Unit) {
    val call = chatModel.activeCall.value
    if (call != null && call.contact.apiId == contact.apiId) {
      perform(call)
    } else {
      Log.d(TAG, "processReceivedMsg: ignoring ${r.responseType}, not in call with the contact ${contact.id}")
    }
  }

  suspend fun receiveFile(user: User, fileId: Long) {
    val chatItem = apiReceiveFile(fileId)
    if (chatItem != null) {
      chatItemSimpleUpdate(user, chatItem)
    }
  }

  suspend fun leaveGroup(groupId: Long) {
    val groupInfo = apiLeaveGroup(groupId)
    if (groupInfo != null) {
      chatModel.updateGroup(groupInfo)
    }
  }

  private suspend fun chatItemSimpleUpdate(user: User, aChatItem: AChatItem) {
    val cInfo = aChatItem.chatInfo
    val cItem = aChatItem.chatItem
    val notify = { ntfManager.notifyMessageReceived(user, cInfo, cItem) }
    if (!active(user)) {
      notify()
    } else if (chatModel.upsertChatItem(cInfo, cItem)) {
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

  fun showBackgroundServiceNoticeIfNeeded() {
    val mode = NotificationsMode.valueOf(appPrefs.notificationsMode.get()!!)
    Log.d(TAG, "showBackgroundServiceNoticeIfNeeded")
    // Nothing to do if mode is OFF. Can be selected on on-boarding stage
    if (mode == NotificationsMode.OFF) return

    if (!appPrefs.backgroundServiceNoticeShown.get()) {
      // the branch for the new users who have never seen service notice
      if (!mode.requiresIgnoringBattery || isIgnoringBatteryOptimizations(appContext)) {
        showBGServiceNotice(mode)
      } else {
        showBGServiceNoticeIgnoreOptimization(mode)
      }
      // set both flags, so that if the user doesn't allow ignoring optimizations, the service will be disabled without additional notice
      appPrefs.backgroundServiceNoticeShown.set(true)
      appPrefs.backgroundServiceBatteryNoticeShown.set(true)
    } else if (mode.requiresIgnoringBattery && !isIgnoringBatteryOptimizations(appContext)) {
      // the branch for users who have app installed, and have seen the service notice,
      // but the battery optimization for the app is on (Android 12) AND the service is running
      if (appPrefs.backgroundServiceBatteryNoticeShown.get()) {
        // users have been presented with battery notice before - they did not allow ignoring optimizations -> disable service
        showDisablingServiceNotice(mode)
        appPrefs.notificationsMode.set(NotificationsMode.OFF.name)
        chatModel.notificationsMode.value = NotificationsMode.OFF
        SimplexService.StartReceiver.toggleReceiver(false)
        MessagesFetcherWorker.cancelAll()
        SimplexService.safeStopService(SimplexApp.context)
      } else {
        // show battery optimization notice
        showBGServiceNoticeIgnoreOptimization(mode)
        appPrefs.backgroundServiceBatteryNoticeShown.set(true)
      }
    } else {
      // service or periodic mode was chosen and battery optimization is disabled
      SimplexApp.context.schedulePeriodicServiceRestartWorker()
      SimplexApp.context.schedulePeriodicWakeUp()
    }
  }

  private fun showBGServiceNotice(mode: NotificationsMode) = AlertManager.shared.showAlert {
    AlertDialog(
      onDismissRequest = AlertManager.shared::hideAlert,
      title = {
        Row {
          Icon(
            Icons.Outlined.Bolt,
            contentDescription =
            if (mode == NotificationsMode.SERVICE) stringResource(R.string.icon_descr_instant_notifications) else stringResource(R.string.periodic_notifications),
          )
          Text(
            if (mode == NotificationsMode.SERVICE) stringResource(R.string.icon_descr_instant_notifications) else stringResource(R.string.periodic_notifications),
            fontWeight = FontWeight.Bold
          )
        }
      },
      text = {
        Column {
          Text(
            if (mode == NotificationsMode.SERVICE) annotatedStringResource(R.string.to_preserve_privacy_simplex_has_background_service_instead_of_push_notifications_it_uses_a_few_pc_battery) else annotatedStringResource(R.string.periodic_notifications_desc),
            Modifier.padding(bottom = 8.dp)
          )
          Text(
            annotatedStringResource(R.string.it_can_disabled_via_settings_notifications_still_shown)
          )
        }
      },
      confirmButton = {
        TextButton(onClick = AlertManager.shared::hideAlert) { Text(stringResource(R.string.ok)) }
      }
    )
  }

  private fun showBGServiceNoticeIgnoreOptimization(mode: NotificationsMode) = AlertManager.shared.showAlert {
    val ignoreOptimization = {
      AlertManager.shared.hideAlert()
      askAboutIgnoringBatteryOptimization(appContext)
    }
    AlertDialog(
      onDismissRequest = ignoreOptimization,
      title = {
        Row {
          Icon(
            Icons.Outlined.Bolt,
            contentDescription =
            if (mode == NotificationsMode.SERVICE) stringResource(R.string.icon_descr_instant_notifications) else stringResource(R.string.periodic_notifications),
          )
          Text(
            if (mode == NotificationsMode.SERVICE) stringResource(R.string.service_notifications) else stringResource(R.string.periodic_notifications),
            fontWeight = FontWeight.Bold
          )
        }
      },
      text = {
        Column {
          Text(
            if (mode == NotificationsMode.SERVICE) annotatedStringResource(R.string.to_preserve_privacy_simplex_has_background_service_instead_of_push_notifications_it_uses_a_few_pc_battery) else annotatedStringResource(R.string.periodic_notifications_desc),
            Modifier.padding(bottom = 8.dp)
          )
          Text(annotatedStringResource(R.string.turn_off_battery_optimization))
        }
      },
      confirmButton = {
        TextButton(onClick = ignoreOptimization) { Text(stringResource(R.string.ok)) }
      }
    )
  }

  private fun showDisablingServiceNotice(mode: NotificationsMode) = AlertManager.shared.showAlert {
    AlertDialog(
      onDismissRequest = AlertManager.shared::hideAlert,
      title = {
        Row {
          Icon(
            Icons.Outlined.Bolt,
            contentDescription =
            if (mode == NotificationsMode.SERVICE) stringResource(R.string.icon_descr_instant_notifications) else stringResource(R.string.periodic_notifications),
          )
          Text(
            if (mode == NotificationsMode.SERVICE) stringResource(R.string.service_notifications_disabled) else stringResource(R.string.periodic_notifications_disabled),
            fontWeight = FontWeight.Bold
          )
        }
      },
      text = {
        Column {
          Text(
            annotatedStringResource(R.string.turning_off_service_and_periodic),
            Modifier.padding(bottom = 8.dp)
          )
        }
      },
      confirmButton = {
        TextButton(onClick = AlertManager.shared::hideAlert) { Text(stringResource(R.string.ok)) }
      }
    )
  }

  fun showLANotice(activity: FragmentActivity) {
    Log.d(TAG, "showLANotice")
    if (!appPrefs.laNoticeShown.get()) {
      appPrefs.laNoticeShown.set(true)
      AlertManager.shared.showAlertDialog(
        title = generalGetString(R.string.la_notice_title_simplex_lock),
        text = generalGetString(R.string.la_notice_to_protect_your_information_turn_on_simplex_lock_you_will_be_prompted_to_complete_authentication_before_this_feature_is_enabled),
        confirmText = generalGetString(R.string.la_notice_turn_on),
        onConfirm = {
          authenticate(
            generalGetString(R.string.auth_enable_simplex_lock),
            generalGetString(R.string.auth_confirm_credential),
            activity,
            completed = { laResult ->
              when (laResult) {
                LAResult.Success -> {
                  chatModel.performLA.value = true
                  appPrefs.performLA.set(true)
                  laTurnedOnAlert()
                }
                is LAResult.Error, LAResult.Failed -> {
                  chatModel.performLA.value = false
                  appPrefs.performLA.set(false)
                }
                LAResult.Unavailable -> {
                  chatModel.performLA.value = false
                  appPrefs.performLA.set(false)
                  chatModel.showAdvertiseLAUnavailableAlert.value = true
                }
              }
            }
          )
        }
      )
    }
  }

  fun isIgnoringBatteryOptimizations(context: Context): Boolean {
    val powerManager = context.getSystemService(Application.POWER_SERVICE) as PowerManager
    return powerManager.isIgnoringBatteryOptimizations(context.packageName)
  }

  private fun askAboutIgnoringBatteryOptimization(context: Context) {
    Intent().apply {
      @SuppressLint("BatteryLife")
      action = Settings.ACTION_REQUEST_IGNORE_BATTERY_OPTIMIZATIONS
      data = Uri.parse("package:${context.packageName}")
      // This flag is needed when you start a new activity from non-Activity context
      addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
      context.startActivity(this)
    }
  }

  fun getNetCfg(): NetCfg {
    val useSocksProxy = appPrefs.networkUseSocksProxy.get()
    val socksProxy = if (useSocksProxy) ":9050" else null
    val hostMode = HostMode.valueOf(appPrefs.networkHostMode.get()!!)
    val requiredHostMode = appPrefs.networkRequiredHostMode.get()
    val sessionMode = appPrefs.networkSessionMode.get()
    val tcpConnectTimeout = appPrefs.networkTCPConnectTimeout.get()
    val tcpTimeout = appPrefs.networkTCPTimeout.get()
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
      tcpKeepAlive = tcpKeepAlive,
      smpPingInterval = smpPingInterval,
      smpPingCount = smpPingCount
    )
  }

  fun setNetCfg(cfg: NetCfg) {
    appPrefs.networkUseSocksProxy.set(cfg.useSocksProxy)
    appPrefs.networkHostMode.set(cfg.hostMode.name)
    appPrefs.networkRequiredHostMode.set(cfg.requiredHostMode)
    appPrefs.networkSessionMode.set(cfg.sessionMode)
    appPrefs.networkTCPConnectTimeout.set(cfg.tcpConnectTimeout)
    appPrefs.networkTCPTimeout.set(cfg.tcpTimeout)
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
  class CreateActiveUser(val profile: Profile): CC()
  class ListUsers: CC()
  class ApiSetActiveUser(val userId: Long): CC()
  class ApiDeleteUser(val userId: Long, val delSMPQueues: Boolean): CC()
  class StartChat(val expire: Boolean): CC()
  class ApiStopChat: CC()
  class SetFilesFolder(val filesFolder: String): CC()
  class SetIncognito(val incognito: Boolean): CC()
  class ApiExportArchive(val config: ArchiveConfig): CC()
  class ApiImportArchive(val config: ArchiveConfig): CC()
  class ApiDeleteStorage: CC()
  class ApiStorageEncryption(val config: DBEncryptionConfig): CC()
  class ApiGetChats(val userId: Long): CC()
  class ApiGetChat(val type: ChatType, val id: Long, val pagination: ChatPagination, val search: String = ""): CC()
  class ApiSendMessage(val type: ChatType, val id: Long, val file: String?, val quotedItemId: Long?, val mc: MsgContent, val live: Boolean): CC()
  class ApiUpdateChatItem(val type: ChatType, val id: Long, val itemId: Long, val mc: MsgContent, val live: Boolean): CC()
  class ApiDeleteChatItem(val type: ChatType, val id: Long, val itemId: Long, val mode: CIDeleteMode): CC()
  class ApiNewGroup(val userId: Long, val groupProfile: GroupProfile): CC()
  class ApiAddMember(val groupId: Long, val contactId: Long, val memberRole: GroupMemberRole): CC()
  class ApiJoinGroup(val groupId: Long): CC()
  class ApiMemberRole(val groupId: Long, val memberId: Long, val memberRole: GroupMemberRole): CC()
  class ApiRemoveMember(val groupId: Long, val memberId: Long): CC()
  class ApiLeaveGroup(val groupId: Long): CC()
  class ApiListMembers(val groupId: Long): CC()
  class ApiUpdateGroupProfile(val groupId: Long, val groupProfile: GroupProfile): CC()
  class APICreateGroupLink(val groupId: Long): CC()
  class APIDeleteGroupLink(val groupId: Long): CC()
  class APIGetGroupLink(val groupId: Long): CC()
  class APIGetUserSMPServers(val userId: Long): CC()
  class APISetUserSMPServers(val userId: Long, val smpServers: List<ServerCfg>): CC()
  class TestSMPServer(val userId: Long, val smpServer: String): CC()
  class APISetChatItemTTL(val userId: Long, val seconds: Long?): CC()
  class APIGetChatItemTTL(val userId: Long): CC()
  class APISetNetworkConfig(val networkConfig: NetCfg): CC()
  class APIGetNetworkConfig: CC()
  class APISetChatSettings(val type: ChatType, val id: Long, val chatSettings: ChatSettings): CC()
  class APIContactInfo(val contactId: Long): CC()
  class APIGroupMemberInfo(val groupId: Long, val groupMemberId: Long): CC()
  class APISwitchContact(val contactId: Long): CC()
  class APISwitchGroupMember(val groupId: Long, val groupMemberId: Long): CC()
  class APIGetContactCode(val contactId: Long): CC()
  class APIGetGroupMemberCode(val groupId: Long, val groupMemberId: Long): CC()
  class APIVerifyContact(val contactId: Long, val connectionCode: String?): CC()
  class APIVerifyGroupMember(val groupId: Long, val groupMemberId: Long, val connectionCode: String?): CC()
  class APIAddContact(val userId: Long): CC()
  class APIConnect(val userId: Long, val connReq: String): CC()
  class ApiDeleteChat(val type: ChatType, val id: Long): CC()
  class ApiClearChat(val type: ChatType, val id: Long): CC()
  class ApiListContacts(val userId: Long): CC()
  class ApiUpdateProfile(val userId: Long, val profile: Profile): CC()
  class ApiSetContactPrefs(val contactId: Long, val prefs: ChatPreferences): CC()
  class ApiParseMarkdown(val text: String): CC()
  class ApiSetContactAlias(val contactId: Long, val localAlias: String): CC()
  class ApiSetConnectionAlias(val connId: Long, val localAlias: String): CC()
  class ApiCreateMyAddress(val userId: Long): CC()
  class ApiDeleteMyAddress(val userId: Long): CC()
  class ApiShowMyAddress(val userId: Long): CC()
  class ApiAddressAutoAccept(val userId: Long, val autoAccept: AutoAccept?): CC()
  class ApiSendCallInvitation(val contact: Contact, val callType: CallType): CC()
  class ApiRejectCall(val contact: Contact): CC()
  class ApiSendCallOffer(val contact: Contact, val callOffer: WebRTCCallOffer): CC()
  class ApiSendCallAnswer(val contact: Contact, val answer: WebRTCSession): CC()
  class ApiSendCallExtraInfo(val contact: Contact, val extraInfo: WebRTCExtraInfo): CC()
  class ApiEndCall(val contact: Contact): CC()
  class ApiCallStatus(val contact: Contact, val callStatus: WebRTCCallStatus): CC()
  class ApiAcceptContact(val contactReqId: Long): CC()
  class ApiRejectContact(val contactReqId: Long): CC()
  class ApiChatRead(val type: ChatType, val id: Long, val range: ItemRange): CC()
  class ApiChatUnread(val type: ChatType, val id: Long, val unreadChat: Boolean): CC()
  class ReceiveFile(val fileId: Long, val inline: Boolean?): CC()
  class ShowVersion(): CC()

  val cmdString: String get() = when (this) {
    is Console -> cmd
    is ShowActiveUser -> "/u"
    is CreateActiveUser -> "/create user ${profile.displayName} ${profile.fullName}"
    is ListUsers -> "/users"
    is ApiSetActiveUser -> "/_user $userId"
    is ApiDeleteUser -> "/_delete user $userId del_smp=${onOff(delSMPQueues)}"
    is StartChat -> "/_start subscribe=on expire=${onOff(expire)}"
    is ApiStopChat -> "/_stop"
    is SetFilesFolder -> "/_files_folder $filesFolder"
    is SetIncognito -> "/incognito ${onOff(incognito)}"
    is ApiExportArchive -> "/_db export ${json.encodeToString(config)}"
    is ApiImportArchive -> "/_db import ${json.encodeToString(config)}"
    is ApiDeleteStorage -> "/_db delete"
    is ApiStorageEncryption -> "/_db encryption ${json.encodeToString(config)}"
    is ApiGetChats -> "/_get chats $userId pcc=on"
    is ApiGetChat -> "/_get chat ${chatRef(type, id)} ${pagination.cmdString}" + (if (search == "") "" else " search=$search")
    is ApiSendMessage -> "/_send ${chatRef(type, id)} live=${onOff(live)} json ${json.encodeToString(ComposedMessage(file, quotedItemId, mc))}"
    is ApiUpdateChatItem -> "/_update item ${chatRef(type, id)} $itemId live=${onOff(live)} ${mc.cmdString}"
    is ApiDeleteChatItem -> "/_delete item ${chatRef(type, id)} $itemId ${mode.deleteMode}"
    is ApiNewGroup -> "/_group $userId ${json.encodeToString(groupProfile)}"
    is ApiAddMember -> "/_add #$groupId $contactId ${memberRole.memberRole}"
    is ApiJoinGroup -> "/_join #$groupId"
    is ApiMemberRole -> "/_member role #$groupId $memberId ${memberRole.memberRole}"
    is ApiRemoveMember -> "/_remove #$groupId $memberId"
    is ApiLeaveGroup -> "/_leave #$groupId"
    is ApiListMembers -> "/_members #$groupId"
    is ApiUpdateGroupProfile -> "/_group_profile #$groupId ${json.encodeToString(groupProfile)}"
    is APICreateGroupLink -> "/_create link #$groupId"
    is APIDeleteGroupLink -> "/_delete link #$groupId"
    is APIGetGroupLink -> "/_get link #$groupId"
    is APIGetUserSMPServers -> "/_smp $userId"
    is APISetUserSMPServers -> "/_smp $userId ${smpServersStr(smpServers)}"
    is TestSMPServer -> "/smp test $userId $smpServer"
    is APISetChatItemTTL -> "/_ttl $userId ${chatItemTTLStr(seconds)}"
    is APIGetChatItemTTL -> "/_ttl $userId"
    is APISetNetworkConfig -> "/_network ${json.encodeToString(networkConfig)}"
    is APIGetNetworkConfig -> "/network"
    is APISetChatSettings -> "/_settings ${chatRef(type, id)} ${json.encodeToString(chatSettings)}"
    is APIContactInfo -> "/_info @$contactId"
    is APIGroupMemberInfo -> "/_info #$groupId $groupMemberId"
    is APISwitchContact -> "/_switch @$contactId"
    is APISwitchGroupMember -> "/_switch #$groupId $groupMemberId"
    is APIGetContactCode -> "/_get code @$contactId"
    is APIGetGroupMemberCode -> "/_get code #$groupId $groupMemberId"
    is APIVerifyContact -> "/_verify code @$contactId" + if (connectionCode != null) " $connectionCode" else ""
    is APIVerifyGroupMember -> "/_verify code #$groupId $groupMemberId" + if (connectionCode != null) " $connectionCode" else ""
    is APIAddContact -> "/_connect $userId"
    is APIConnect -> "/_connect $userId $connReq"
    is ApiDeleteChat -> "/_delete ${chatRef(type, id)}"
    is ApiClearChat -> "/_clear chat ${chatRef(type, id)}"
    is ApiListContacts -> "/_contacts $userId"
    is ApiUpdateProfile -> "/_profile $userId ${json.encodeToString(profile)}"
    is ApiSetContactPrefs -> "/_set prefs @$contactId ${json.encodeToString(prefs)}"
    is ApiParseMarkdown -> "/_parse $text"
    is ApiSetContactAlias -> "/_set alias @$contactId ${localAlias.trim()}"
    is ApiSetConnectionAlias -> "/_set alias :$connId ${localAlias.trim()}"
    is ApiCreateMyAddress -> "/_address $userId"
    is ApiDeleteMyAddress -> "/_delete_address $userId"
    is ApiShowMyAddress -> "/_show_address $userId"
    is ApiAddressAutoAccept -> "/_auto_accept $userId ${AutoAccept.cmdString(autoAccept)}"
    is ApiAcceptContact -> "/_accept $contactReqId"
    is ApiRejectContact -> "/_reject $contactReqId"
    is ApiSendCallInvitation -> "/_call invite @${contact.apiId} ${json.encodeToString(callType)}"
    is ApiRejectCall -> "/_call reject @${contact.apiId}"
    is ApiSendCallOffer -> "/_call offer @${contact.apiId} ${json.encodeToString(callOffer)}"
    is ApiSendCallAnswer -> "/_call answer @${contact.apiId} ${json.encodeToString(answer)}"
    is ApiSendCallExtraInfo -> "/_call extra @${contact.apiId} ${json.encodeToString(extraInfo)}"
    is ApiEndCall -> "/_call end @${contact.apiId}"
    is ApiCallStatus -> "/_call status @${contact.apiId} ${callStatus.value}"
    is ApiChatRead -> "/_read chat ${chatRef(type, id)} from=${range.from} to=${range.to}"
    is ApiChatUnread -> "/_unread chat ${chatRef(type, id)} ${onOff(unreadChat)}"
    is ReceiveFile -> if (inline == null) "/freceive $fileId" else "/freceive $fileId inline=${onOff(inline)}"
    is ShowVersion -> "/version"
  }

  val cmdType: String get() = when (this) {
    is Console -> "console command"
    is ShowActiveUser -> "showActiveUser"
    is CreateActiveUser -> "createActiveUser"
    is ListUsers -> "listUsers"
    is ApiSetActiveUser -> "apiSetActiveUser"
    is ApiDeleteUser -> "apiDeleteUser"
    is StartChat -> "startChat"
    is ApiStopChat -> "apiStopChat"
    is SetFilesFolder -> "setFilesFolder"
    is SetIncognito -> "setIncognito"
    is ApiExportArchive -> "apiExportArchive"
    is ApiImportArchive -> "apiImportArchive"
    is ApiDeleteStorage -> "apiDeleteStorage"
    is ApiStorageEncryption -> "apiStorageEncryption"
    is ApiGetChats -> "apiGetChats"
    is ApiGetChat -> "apiGetChat"
    is ApiSendMessage -> "apiSendMessage"
    is ApiUpdateChatItem -> "apiUpdateChatItem"
    is ApiDeleteChatItem -> "apiDeleteChatItem"
    is ApiNewGroup -> "apiNewGroup"
    is ApiAddMember -> "apiAddMember"
    is ApiJoinGroup -> "apiJoinGroup"
    is ApiMemberRole -> "apiMemberRole"
    is ApiRemoveMember -> "apiRemoveMember"
    is ApiLeaveGroup -> "apiLeaveGroup"
    is ApiListMembers -> "apiListMembers"
    is ApiUpdateGroupProfile -> "apiUpdateGroupProfile"
    is APICreateGroupLink -> "apiCreateGroupLink"
    is APIDeleteGroupLink -> "apiDeleteGroupLink"
    is APIGetGroupLink -> "apiGetGroupLink"
    is APIGetUserSMPServers -> "apiGetUserSMPServers"
    is APISetUserSMPServers -> "apiSetUserSMPServers"
    is TestSMPServer -> "testSMPServer"
    is APISetChatItemTTL -> "apiSetChatItemTTL"
    is APIGetChatItemTTL -> "apiGetChatItemTTL"
    is APISetNetworkConfig -> "/apiSetNetworkConfig"
    is APIGetNetworkConfig -> "/apiGetNetworkConfig"
    is APISetChatSettings -> "/apiSetChatSettings"
    is APIContactInfo -> "apiContactInfo"
    is APIGroupMemberInfo -> "apiGroupMemberInfo"
    is APISwitchContact -> "apiSwitchContact"
    is APISwitchGroupMember -> "apiSwitchGroupMember"
    is APIGetContactCode -> "apiGetContactCode"
    is APIGetGroupMemberCode -> "apiGetGroupMemberCode"
    is APIVerifyContact -> "apiVerifyContact"
    is APIVerifyGroupMember -> "apiVerifyGroupMember"
    is APIAddContact -> "apiAddContact"
    is APIConnect -> "apiConnect"
    is ApiDeleteChat -> "apiDeleteChat"
    is ApiClearChat -> "apiClearChat"
    is ApiListContacts -> "apiListContacts"
    is ApiUpdateProfile -> "apiUpdateProfile"
    is ApiSetContactPrefs -> "apiSetContactPrefs"
    is ApiParseMarkdown -> "apiParseMarkdown"
    is ApiSetContactAlias -> "apiSetContactAlias"
    is ApiSetConnectionAlias -> "apiSetConnectionAlias"
    is ApiCreateMyAddress -> "apiCreateMyAddress"
    is ApiDeleteMyAddress -> "apiDeleteMyAddress"
    is ApiShowMyAddress -> "apiShowMyAddress"
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
    is ApiChatRead -> "apiChatRead"
    is ApiChatUnread -> "apiChatUnread"
    is ReceiveFile -> "receiveFile"
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
      else -> this
    }

  private fun obfuscate(s: String): String = if (s.isEmpty()) "" else "***"

  private fun onOff(b: Boolean): String = if (b) "on" else "off"

  companion object {
    fun chatRef(chatType: ChatType, id: Long) = "${chatType.type}${id}"

    fun smpServersStr(smpServers: List<ServerCfg>) = if (smpServers.isEmpty()) "default" else json.encodeToString(SMPServersConfig(smpServers))
  }
}

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
class ComposedMessage(val filePath: String?, val quotedItemId: Long?, val msgContent: MsgContent)

@Serializable
class ArchiveConfig(val archivePath: String, val disableCompression: Boolean? = null, val parentTempDirectory: String? = null)

@Serializable
class DBEncryptionConfig(val currentKey: String, val newKey: String)

@Serializable
data class SMPServersConfig(
  val smpServers: List<ServerCfg>
)

@Serializable
data class ServerCfg(
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
    val empty = ServerCfg(server = "", preset = false, tested = null, enabled = true)

    class SampleData(
      val preset: ServerCfg,
      val custom: ServerCfg,
      val untested: ServerCfg
    )

    val sampleData = SampleData(
      preset = ServerCfg(
        server = "smp://abcd@smp8.simplex.im",
        preset = true,
        tested = true,
        enabled = true
      ),
      custom = ServerCfg(
        server = "smp://abcd@smp9.simplex.im",
        preset = false,
        tested = false,
        enabled = false
      ),
      untested = ServerCfg(
        server = "smp://abcd@smp10.simplex.im",
        preset = false,
        tested = null,
        enabled = true
      )
    )
  }
}

@Serializable
enum class SMPTestStep {
  @SerialName("connect") Connect,
  @SerialName("createQueue") CreateQueue,
  @SerialName("secureQueue") SecureQueue,
  @SerialName("deleteQueue") DeleteQueue,
  @SerialName("disconnect") Disconnect;

  val text: String get() = when (this) {
    Connect -> generalGetString(R.string.smp_server_test_connect)
    CreateQueue -> generalGetString(R.string.smp_server_test_create_queue)
    SecureQueue -> generalGetString(R.string.smp_server_test_secure_queue)
    DeleteQueue -> generalGetString(R.string.smp_server_test_delete_queue)
    Disconnect -> generalGetString(R.string.smp_server_test_disconnect)
  }
}

@Serializable
data class SMPTestFailure(
  val testStep: SMPTestStep,
  val testError: AgentErrorType
) {
  override fun equals(other: Any?): Boolean {
    if (other !is SMPTestFailure) return false
    return other.testStep == this.testStep
  }

  override fun hashCode(): Int {
    return testStep.hashCode()
  }

  val localizedDescription: String get() {
    val err = String.format(generalGetString(R.string.error_smp_test_failed_at_step), testStep.text)
    return when  {
      testError is AgentErrorType.SMP && testError.smpErr is SMPErrorType.AUTH ->
        err + " " + generalGetString(R.string.error_smp_test_server_auth)
      testError is AgentErrorType.BROKER && testError.brokerErr is BrokerErrorType.NETWORK ->
        err + " " + generalGetString(R.string.error_smp_test_certificate)
      else -> err
    }
  }
}

@Serializable
data class ServerAddress(
  val hostnames: List<String>,
  val port: String,
  val keyHash: String,
  val basicAuth: String = ""
) {
  val uri: String
    get() =
      "smp://${keyHash}${if (basicAuth.isEmpty()) "" else ":$basicAuth"}@${hostnames.joinToString(",")}"

  val valid: Boolean
    get() = hostnames.isNotEmpty() && hostnames.toSet().size == hostnames.size

  companion object {
    val empty = ServerAddress(
      hostnames = emptyList(),
      port = "",
      keyHash = "",
      basicAuth = ""
    )
    val sampleData = ServerAddress(
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
  val tcpKeepAlive: KeepAliveOpts?,
  val smpPingInterval: Long, // microseconds
  val smpPingCount: Int,
  val logTLSErrors: Boolean = false
) {
  val useSocksProxy: Boolean get() = socksProxy != null
  val enableKeepAlive: Boolean get() = tcpKeepAlive != null

  companion object {
    val defaults: NetCfg =
      NetCfg(
        socksProxy = null,
        hostMode = HostMode.OnionViaSocks,
        requiredHostMode = false,
        sessionMode = TransportSessionMode.User,
        tcpConnectTimeout = 10_000_000,
        tcpTimeout = 7_000_000,
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
        tcpConnectTimeout = 20_000_000,
        tcpTimeout = 15_000_000,
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
  val enableNtfs: Boolean
)

@Serializable
data class FullChatPreferences(
  val timedMessages: TimedMessagesPreference,
  val fullDelete: SimpleChatPreference,
  val voice: SimpleChatPreference,
) {
  fun toPreferences(): ChatPreferences = ChatPreferences(timedMessages = timedMessages, fullDelete = fullDelete, voice = voice)

  companion object {
    val sampleData = FullChatPreferences(
      timedMessages = TimedMessagesPreference(allow = FeatureAllowed.NO),
      fullDelete = SimpleChatPreference(allow = FeatureAllowed.NO),
      voice = SimpleChatPreference(allow = FeatureAllowed.YES)
    )
  }
}

@Serializable
data class ChatPreferences(
  val timedMessages: TimedMessagesPreference?,
  val fullDelete: SimpleChatPreference?,
  val voice: SimpleChatPreference?,
) {
  fun setAllowed(feature: ChatFeature, allowed: FeatureAllowed = FeatureAllowed.YES, param: Int? = null): ChatPreferences =
    when (feature) {
      ChatFeature.TimedMessages -> this.copy(timedMessages = TimedMessagesPreference(allow = allowed, ttl = param ?: this.timedMessages?.ttl))
      ChatFeature.FullDelete -> this.copy(fullDelete = SimpleChatPreference(allow = allowed))
      ChatFeature.Voice -> this.copy(voice = SimpleChatPreference(allow = allowed))
    }

  companion object {
    val sampleData = ChatPreferences(
      timedMessages = TimedMessagesPreference(allow = FeatureAllowed.NO),
      fullDelete = SimpleChatPreference(allow = FeatureAllowed.NO),
      voice = SimpleChatPreference(allow = FeatureAllowed.YES)
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
      get() = listOf(30, 300, 3600, 8 * 3600, 86400, 7 * 86400, 30 * 86400)

    fun ttlText(ttl: Int?): String {
      ttl ?: return generalGetString(R.string.feature_off)
      if (ttl == 0) return  String.format(generalGetString(R.string.ttl_sec), 0)
      val (m_, s) = divMod(ttl, 60)
      val (h_, m) = divMod(m_, 60)
      val (d_, h) = divMod(h_, 24)
      val (mm, d) = divMod(d_, 30)
      return maybe(mm, if (mm == 1) String.format(generalGetString(R.string.ttl_month), 1) else String.format(generalGetString(R.string.ttl_months), mm)) +
          maybe(d, if (d == 1) String.format(generalGetString(R.string.ttl_day), 1) else if (d == 7) String.format(generalGetString(R.string.ttl_week), 1) else if (d == 14) String.format(generalGetString(R.string.ttl_weeks), 2) else String.format(generalGetString(R.string.ttl_days), d)) +
          maybe(h, if (h == 1) String.format(generalGetString(R.string.ttl_hour), 1) else String.format(generalGetString(R.string.ttl_hours), h)) +
          maybe(m, String.format(generalGetString(R.string.ttl_min), m)) +
          maybe(s, String.format(generalGetString(R.string.ttl_sec), s))
    }

    fun shortTtlText(ttl: Int?): String {
      ttl ?: return generalGetString(R.string.feature_off)
      val m = ttl / 60
      if (m == 0) {
        return String.format(generalGetString(R.string.ttl_s), ttl)
      }
      val h = m / 60
      if (h == 0) {
        return String.format(generalGetString(R.string.ttl_m), m)
      }
      val d = h / 24
      if (d == 0) {
        return String.format(generalGetString(R.string.ttl_h), h)
      }
      val mm = d / 30
      if (mm > 0) {
        return String.format(generalGetString(R.string.ttl_mth), mm)
      }
      val w = d / 7
      return if (w == 0 || d % 7 != 0) String.format(generalGetString(R.string.ttl_d), d) else String.format(generalGetString(R.string.ttl_w), w)
    }

    fun divMod(n: Int, d: Int): Pair<Int, Int> =
      n / d to n % d

    fun maybe(n: Int, s: String): String =
      if (n == 0) "" else s
  }
}

@Serializable
data class ContactUserPreferences(
  val timedMessages: ContactUserPreferenceTimed,
  val fullDelete: ContactUserPreference,
  val voice: ContactUserPreference,
) {
  fun toPreferences(): ChatPreferences = ChatPreferences(
    timedMessages = timedMessages.userPreference.pref,
    fullDelete = fullDelete.userPreference.pref,
    voice = voice.userPreference.pref
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
      voice = ContactUserPreference(
        enabled = FeatureEnabled(forUser = true, forContact = true),
        userPreference = ContactUserPref.User(preference = SimpleChatPreference(allow = FeatureAllowed.YES)),
        contactPreference = SimpleChatPreference(allow = FeatureAllowed.YES)
      )
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
      forUser && forContact -> generalGetString(R.string.feature_enabled)
      forUser -> generalGetString(R.string.feature_enabled_for_you)
      forContact -> generalGetString(R.string.feature_enabled_for_contact)
      else -> generalGetString(R.string.feature_off)
    }

  val iconColor: Color
    get() = if (forUser) SimplexGreen else if (forContact) WarningYellow else HighOrLowlight

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

  val contactOverride: SimpleChatPreference?
    get() = when(this) {
      is Contact -> pref
      is User -> null
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

  val contactOverride: TimedMessagesPreference?
    get() = when(this) {
      is Contact -> pref
      is User -> null
    }
}

interface Feature {
//  val icon: ImageVector
  val text: String
  val iconFilled: ImageVector
  val hasParam: Boolean
}

@Serializable
enum class ChatFeature: Feature {
  @SerialName("timedMessages") TimedMessages,
  @SerialName("fullDelete") FullDelete,
  @SerialName("voice") Voice;

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
      TimedMessages -> generalGetString(R.string.timed_messages)
      FullDelete -> generalGetString(R.string.full_deletion)
      Voice -> generalGetString(R.string.voice_messages)
    }

  val icon: ImageVector
    get() = when(this) {
      TimedMessages -> Icons.Outlined.Timer
      FullDelete -> Icons.Outlined.DeleteForever
      Voice -> Icons.Outlined.KeyboardVoice
    }

  override val iconFilled: ImageVector
    get() = when(this) {
      TimedMessages -> Icons.Filled.Timer
      FullDelete -> Icons.Filled.DeleteForever
      Voice -> Icons.Filled.KeyboardVoice
    }

  fun allowDescription(allowed: FeatureAllowed): String =
    when (this) {
      TimedMessages -> when (allowed) {
        FeatureAllowed.ALWAYS -> generalGetString(R.string.allow_your_contacts_to_send_disappearing_messages)
        FeatureAllowed.YES -> generalGetString(R.string.allow_disappearing_messages_only_if)
        FeatureAllowed.NO -> generalGetString(R.string.prohibit_sending_disappearing_messages)
      }
      FullDelete -> when (allowed) {
        FeatureAllowed.ALWAYS -> generalGetString(R.string.allow_your_contacts_irreversibly_delete)
        FeatureAllowed.YES -> generalGetString(R.string.allow_irreversible_message_deletion_only_if)
        FeatureAllowed.NO -> generalGetString(R.string.contacts_can_mark_messages_for_deletion)
      }
      Voice -> when (allowed) {
        FeatureAllowed.ALWAYS -> generalGetString(R.string.allow_your_contacts_to_send_voice_messages)
        FeatureAllowed.YES -> generalGetString(R.string.allow_voice_messages_only_if)
        FeatureAllowed.NO -> generalGetString(R.string.prohibit_sending_voice_messages)
      }
    }

  fun enabledDescription(enabled: FeatureEnabled): String =
    when (this) {
      TimedMessages -> when {
        enabled.forUser && enabled.forContact -> generalGetString(R.string.both_you_and_your_contact_can_send_disappearing)
        enabled.forUser -> generalGetString(R.string.only_you_can_send_disappearing)
        enabled.forContact -> generalGetString(R.string.only_your_contact_can_send_disappearing)
        else -> generalGetString(R.string.disappearing_prohibited_in_this_chat)
      }
      FullDelete -> when {
        enabled.forUser && enabled.forContact -> generalGetString(R.string.both_you_and_your_contacts_can_delete)
        enabled.forUser -> generalGetString(R.string.only_you_can_delete_messages)
        enabled.forContact -> generalGetString(R.string.only_your_contact_can_delete)
        else -> generalGetString(R.string.message_deletion_prohibited)
      }
      Voice -> when {
        enabled.forUser && enabled.forContact -> generalGetString(R.string.both_you_and_your_contact_can_send_voice)
        enabled.forUser -> generalGetString(R.string.only_you_can_send_voice)
        enabled.forContact -> generalGetString(R.string.only_your_contact_can_send_voice)
        else -> generalGetString(R.string.voice_prohibited_in_this_chat)
      }
  }
}

@Serializable
enum class GroupFeature: Feature {
  @SerialName("timedMessages") TimedMessages,
  @SerialName("directMessages") DirectMessages,
  @SerialName("fullDelete") FullDelete,
  @SerialName("voice") Voice;

  override val hasParam: Boolean get() = when(this) {
    TimedMessages -> true
    else -> false
  }

  override val text: String
    get() = when(this) {
      TimedMessages -> generalGetString(R.string.timed_messages)
      DirectMessages -> generalGetString(R.string.direct_messages)
      FullDelete -> generalGetString(R.string.full_deletion)
      Voice -> generalGetString(R.string.voice_messages)
    }

  val icon: ImageVector
    get() = when(this) {
      TimedMessages -> Icons.Outlined.Timer
      DirectMessages -> Icons.Outlined.SwapHorizontalCircle
      FullDelete -> Icons.Outlined.DeleteForever
      Voice -> Icons.Outlined.KeyboardVoice
    }

  override val iconFilled: ImageVector
    get() = when(this) {
      TimedMessages -> Icons.Filled.Timer
      DirectMessages -> Icons.Filled.SwapHorizontalCircle
      FullDelete -> Icons.Filled.DeleteForever
      Voice -> Icons.Filled.KeyboardVoice
    }

  fun enableDescription(enabled: GroupFeatureEnabled, canEdit: Boolean): String =
    if (canEdit) {
      when(this) {
        TimedMessages -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(R.string.allow_to_send_disappearing)
          GroupFeatureEnabled.OFF -> generalGetString(R.string.prohibit_sending_disappearing)
        }
        DirectMessages -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(R.string.allow_direct_messages)
          GroupFeatureEnabled.OFF -> generalGetString(R.string.prohibit_direct_messages)
        }
        FullDelete -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(R.string.allow_to_delete_messages)
          GroupFeatureEnabled.OFF -> generalGetString(R.string.prohibit_message_deletion)
        }
        Voice -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(R.string.allow_to_send_voice)
          GroupFeatureEnabled.OFF -> generalGetString(R.string.prohibit_sending_voice)
        }
      }
    } else {
      when(this) {
        TimedMessages -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(R.string.group_members_can_send_disappearing)
          GroupFeatureEnabled.OFF -> generalGetString(R.string.disappearing_messages_are_prohibited)
        }
        DirectMessages -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(R.string.group_members_can_send_dms)
          GroupFeatureEnabled.OFF -> generalGetString(R.string.direct_messages_are_prohibited_in_chat)
        }
        FullDelete -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(R.string.group_members_can_delete)
          GroupFeatureEnabled.OFF -> generalGetString(R.string.message_deletion_prohibited_in_chat)
        }
        Voice -> when(enabled) {
          GroupFeatureEnabled.ON -> generalGetString(R.string.group_members_can_send_voice)
          GroupFeatureEnabled.OFF -> generalGetString(R.string.voice_messages_are_prohibited)
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
      is UserDefault -> String.format(generalGetString(R.string.chat_preferences_default), default.text)
      is Always -> generalGetString(R.string.chat_preferences_always)
      is Yes -> generalGetString(R.string.chat_preferences_yes)
      is No -> generalGetString(R.string.chat_preferences_no)
    }
}

@Serializable
data class ContactFeaturesAllowed(
  val timedMessagesAllowed: Boolean,
  val timedMessagesTTL: Int?,
  val fullDelete: ContactFeatureAllowed,
  val voice: ContactFeatureAllowed
) {
  companion object {
    val sampleData = ContactFeaturesAllowed(
      timedMessagesAllowed = false,
      timedMessagesTTL = null,
      fullDelete = ContactFeatureAllowed.UserDefault(FeatureAllowed.NO),
      voice = ContactFeatureAllowed.UserDefault(FeatureAllowed.YES)
    )
  }
}

fun contactUserPrefsToFeaturesAllowed(contactUserPreferences: ContactUserPreferences): ContactFeaturesAllowed {
  val pref = contactUserPreferences.timedMessages.userPreference
  val allow = pref.contactOverride?.allow
  return ContactFeaturesAllowed(
    timedMessagesAllowed = allow == FeatureAllowed.YES || allow == FeatureAllowed.ALWAYS,
    timedMessagesTTL = pref.pref.ttl,
    fullDelete = contactUserPrefToFeatureAllowed(contactUserPreferences.fullDelete),
    voice = contactUserPrefToFeatureAllowed(contactUserPreferences.voice)
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
    voice = contactFeatureAllowedToPref(contactFeaturesAllowed.voice)
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
      ALWAYS -> generalGetString(R.string.chat_preferences_always)
      YES -> generalGetString(R.string.chat_preferences_yes)
      NO -> generalGetString(R.string.chat_preferences_no)
    }
}

@Serializable
data class FullGroupPreferences(
  val timedMessages: TimedMessagesGroupPreference,
  val directMessages: GroupPreference,
  val fullDelete: GroupPreference,
  val voice: GroupPreference
) {
  fun toGroupPreferences(): GroupPreferences =
    GroupPreferences(timedMessages = timedMessages, directMessages = directMessages, fullDelete = fullDelete, voice = voice)

  companion object {
    val sampleData = FullGroupPreferences(
      timedMessages = TimedMessagesGroupPreference(GroupFeatureEnabled.OFF),
      directMessages = GroupPreference(GroupFeatureEnabled.OFF),
      fullDelete = GroupPreference(GroupFeatureEnabled.OFF),
      voice = GroupPreference(GroupFeatureEnabled.ON)
    )
  }
}

@Serializable
data class GroupPreferences(
  val timedMessages: TimedMessagesGroupPreference?,
  val directMessages: GroupPreference?,
  val fullDelete: GroupPreference?,
  val voice: GroupPreference?
) {
  companion object {
    val sampleData = GroupPreferences(
      timedMessages = TimedMessagesGroupPreference(GroupFeatureEnabled.OFF),
      directMessages = GroupPreference(GroupFeatureEnabled.OFF),
      fullDelete = GroupPreference(GroupFeatureEnabled.OFF),
      voice = GroupPreference(GroupFeatureEnabled.ON)
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
      ON -> generalGetString(R.string.chat_preferences_on)
      OFF -> generalGetString(R.string.chat_preferences_off)
    }

  val iconColor: Color
    get() = if (this == ON) SimplexGreen else HighOrLowlight

}

val json = Json {
  prettyPrint = true
  ignoreUnknownKeys = true
  encodeDefaults = true
  explicitNulls = false
}

@Serializable
class APIResponse(val resp: CR, val corr: String? = null) {
  companion object {
    fun decodeStr(str: String): APIResponse {
      return try {
        json.decodeFromString(str)
      } catch(e: Exception) {
        try {
          Log.d(TAG, e.localizedMessage ?: "")
          val data = json.parseToJsonElement(str).jsonObject
          val resp = data["resp"]!!.jsonObject
          val type = resp["type"]?.jsonPrimitive?.content ?: "invalid"
          try {
            if (type == "apiChats") {
              val user: User = json.decodeFromJsonElement(resp["user"]!!.jsonObject)
              val chats: List<Chat> = resp["chats"]!!.jsonArray.map {
                parseChatData(it)
              }
              return APIResponse(
                resp = CR.ApiChats(user, chats),
                corr = data["corr"]?.toString()
              )
            } else if (type == "apiChat") {
              val user: User = json.decodeFromJsonElement(resp["user"]!!.jsonObject)
              val chat = parseChatData(resp["chat"]!!)
              return APIResponse(
                resp = CR.ApiChat(user, chat),
                corr = data["corr"]?.toString()
              )
            }
          } catch (e: Exception) {
            Log.e(TAG, "Error while parsing chat(s): " + e.stackTraceToString())
          }
          APIResponse(
            resp = CR.Response(type, json.encodeToString(data)),
            corr = data["corr"]?.toString()
          )
        } catch(e: Exception) {
          APIResponse(CR.Invalid(str))
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
    decodeObject(ChatItem.serializer(), it) ?: ChatItem.invalidJSON(json.encodeToString(it))
  }
 return Chat(chatInfo, chatItems, chatStats)
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
  @Serializable @SerialName("apiChats") class ApiChats(val user: User, val chats: List<Chat>): CR()
  @Serializable @SerialName("apiChat") class ApiChat(val user: User, val chat: Chat): CR()
  @Serializable @SerialName("userSMPServers") class UserSMPServers(val user: User, val smpServers: List<ServerCfg>, val presetSMPServers: List<String>): CR()
  @Serializable @SerialName("smpTestResult") class SmpTestResult(val user: User, val smpTestFailure: SMPTestFailure? = null): CR()
  @Serializable @SerialName("chatItemTTL") class ChatItemTTL(val user: User, val chatItemTTL: Long? = null): CR()
  @Serializable @SerialName("networkConfig") class NetworkConfig(val networkConfig: NetCfg): CR()
  @Serializable @SerialName("contactInfo") class ContactInfo(val user: User, val contact: Contact, val connectionStats: ConnectionStats, val customUserProfile: Profile? = null): CR()
  @Serializable @SerialName("groupMemberInfo") class GroupMemberInfo(val user: User, val groupInfo: GroupInfo, val member: GroupMember, val connectionStats_: ConnectionStats?): CR()
  @Serializable @SerialName("contactCode") class ContactCode(val user: User, val contact: Contact, val connectionCode: String): CR()
  @Serializable @SerialName("groupMemberCode") class GroupMemberCode(val user: User, val groupInfo: GroupInfo, val member: GroupMember, val connectionCode: String): CR()
  @Serializable @SerialName("connectionVerified") class ConnectionVerified(val user: User, val verified: Boolean, val expectedCode: String): CR()
  @Serializable @SerialName("invitation") class Invitation(val user: User, val connReqInvitation: String): CR()
  @Serializable @SerialName("sentConfirmation") class SentConfirmation(val user: User): CR()
  @Serializable @SerialName("sentInvitation") class SentInvitation(val user: User): CR()
  @Serializable @SerialName("contactAlreadyExists") class ContactAlreadyExists(val user: User, val contact: Contact): CR()
  @Serializable @SerialName("contactDeleted") class ContactDeleted(val user: User, val contact: Contact): CR()
  @Serializable @SerialName("chatCleared") class ChatCleared(val user: User, val chatInfo: ChatInfo): CR()
  @Serializable @SerialName("userProfileNoChange") class UserProfileNoChange(val user: User): CR()
  @Serializable @SerialName("userProfileUpdated") class UserProfileUpdated(val user: User, val fromProfile: Profile, val toProfile: Profile): CR()
  @Serializable @SerialName("contactAliasUpdated") class ContactAliasUpdated(val user: User, val toContact: Contact): CR()
  @Serializable @SerialName("connectionAliasUpdated") class ConnectionAliasUpdated(val user: User, val toConnection: PendingContactConnection): CR()
  @Serializable @SerialName("contactPrefsUpdated") class ContactPrefsUpdated(val user: User, val fromContact: Contact, val toContact: Contact): CR()
  @Serializable @SerialName("userContactLink") class UserContactLink(val user: User, val contactLink: UserContactLinkRec): CR()
  @Serializable @SerialName("userContactLinkUpdated") class UserContactLinkUpdated(val user: User, val contactLink: UserContactLinkRec): CR()
  @Serializable @SerialName("userContactLinkCreated") class UserContactLinkCreated(val user: User, val connReqContact: String): CR()
  @Serializable @SerialName("userContactLinkDeleted") class UserContactLinkDeleted(val user: User): CR()
  @Serializable @SerialName("contactConnected") class ContactConnected(val user: User, val contact: Contact, val userCustomProfile: Profile? = null): CR()
  @Serializable @SerialName("contactConnecting") class ContactConnecting(val user: User, val contact: Contact): CR()
  @Serializable @SerialName("receivedContactRequest") class ReceivedContactRequest(val user: User, val contactRequest: UserContactRequest): CR()
  @Serializable @SerialName("acceptingContactRequest") class AcceptingContactRequest(val user: User, val contact: Contact): CR()
  @Serializable @SerialName("contactRequestRejected") class ContactRequestRejected(val user: User): CR()
  @Serializable @SerialName("contactUpdated") class ContactUpdated(val user: User, val toContact: Contact): CR()
  @Serializable @SerialName("contactsSubscribed") class ContactsSubscribed(val server: String, val contactRefs: List<ContactRef>): CR()
  @Serializable @SerialName("contactsDisconnected") class ContactsDisconnected(val server: String, val contactRefs: List<ContactRef>): CR()
  @Serializable @SerialName("contactSubError") class ContactSubError(val user: User, val contact: Contact, val chatError: ChatError): CR()
  @Serializable @SerialName("contactSubSummary") class ContactSubSummary(val user: User, val contactSubscriptions: List<ContactSubStatus>): CR()
  @Serializable @SerialName("groupSubscribed") class GroupSubscribed(val user: User, val group: GroupInfo): CR()
  @Serializable @SerialName("memberSubErrors") class MemberSubErrors(val user: User, val memberSubErrors: List<MemberSubError>): CR()
  @Serializable @SerialName("groupEmpty") class GroupEmpty(val user: User, val group: GroupInfo): CR()
  @Serializable @SerialName("userContactLinkSubscribed") class UserContactLinkSubscribed: CR()
  @Serializable @SerialName("newChatItem") class NewChatItem(val user: User, val chatItem: AChatItem): CR()
  @Serializable @SerialName("chatItemStatusUpdated") class ChatItemStatusUpdated(val user: User, val chatItem: AChatItem): CR()
  @Serializable @SerialName("chatItemUpdated") class ChatItemUpdated(val user: User, val chatItem: AChatItem): CR()
  @Serializable @SerialName("chatItemDeleted") class ChatItemDeleted(val user: User, val deletedChatItem: AChatItem, val toChatItem: AChatItem? = null, val byUser: Boolean): CR()
  @Serializable @SerialName("contactsList") class ContactsList(val user: User, val contacts: List<Contact>): CR()
  // group events
  @Serializable @SerialName("groupCreated") class GroupCreated(val user: User, val groupInfo: GroupInfo): CR()
  @Serializable @SerialName("sentGroupInvitation") class SentGroupInvitation(val user: User, val groupInfo: GroupInfo, val contact: Contact, val member: GroupMember): CR()
  @Serializable @SerialName("userAcceptedGroupSent") class UserAcceptedGroupSent (val user: User, val groupInfo: GroupInfo, val hostContact: Contact? = null): CR()
  @Serializable @SerialName("userDeletedMember") class UserDeletedMember(val user: User, val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("leftMemberUser") class LeftMemberUser(val user: User, val groupInfo: GroupInfo): CR()
  @Serializable @SerialName("groupMembers") class GroupMembers(val user: User, val group: Group): CR()
  @Serializable @SerialName("receivedGroupInvitation") class ReceivedGroupInvitation(val user: User, val groupInfo: GroupInfo, val contact: Contact, val memberRole: GroupMemberRole): CR()
  @Serializable @SerialName("groupDeletedUser") class GroupDeletedUser(val user: User, val groupInfo: GroupInfo): CR()
  @Serializable @SerialName("joinedGroupMemberConnecting") class JoinedGroupMemberConnecting(val user: User, val groupInfo: GroupInfo, val hostMember: GroupMember, val member: GroupMember): CR()
  @Serializable @SerialName("memberRole") class MemberRole(val user: User, val groupInfo: GroupInfo, val byMember: GroupMember, val member: GroupMember, val fromRole: GroupMemberRole, val toRole: GroupMemberRole): CR()
  @Serializable @SerialName("memberRoleUser") class MemberRoleUser(val user: User, val groupInfo: GroupInfo, val member: GroupMember, val fromRole: GroupMemberRole, val toRole: GroupMemberRole): CR()
  @Serializable @SerialName("deletedMemberUser") class DeletedMemberUser(val user: User, val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("deletedMember") class DeletedMember(val user: User, val groupInfo: GroupInfo, val byMember: GroupMember, val deletedMember: GroupMember): CR()
  @Serializable @SerialName("leftMember") class LeftMember(val user: User, val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("groupDeleted") class GroupDeleted(val user: User, val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("contactsMerged") class ContactsMerged(val user: User, val intoContact: Contact, val mergedContact: Contact): CR()
  @Serializable @SerialName("groupInvitation") class GroupInvitation(val user: User, val groupInfo: GroupInfo): CR() // unused
  @Serializable @SerialName("userJoinedGroup") class UserJoinedGroup(val user: User, val groupInfo: GroupInfo): CR()
  @Serializable @SerialName("joinedGroupMember") class JoinedGroupMember(val user: User, val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("connectedToGroupMember") class ConnectedToGroupMember(val user: User, val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("groupRemoved") class GroupRemoved(val user: User, val groupInfo: GroupInfo): CR() // unused
  @Serializable @SerialName("groupUpdated") class GroupUpdated(val user: User, val toGroup: GroupInfo): CR()
  @Serializable @SerialName("groupLinkCreated") class GroupLinkCreated(val user: User, val groupInfo: GroupInfo, val connReqContact: String): CR()
  @Serializable @SerialName("groupLink") class GroupLink(val user: User, val groupInfo: GroupInfo, val connReqContact: String): CR()
  @Serializable @SerialName("groupLinkDeleted") class GroupLinkDeleted(val user: User, val groupInfo: GroupInfo): CR()
  // receiving file events
  @Serializable @SerialName("rcvFileAccepted") class RcvFileAccepted(val user: User, val chatItem: AChatItem): CR()
  @Serializable @SerialName("rcvFileAcceptedSndCancelled") class RcvFileAcceptedSndCancelled(val user: User, val rcvFileTransfer: RcvFileTransfer): CR()
  @Serializable @SerialName("rcvFileStart") class RcvFileStart(val user: User, val chatItem: AChatItem): CR()
  @Serializable @SerialName("rcvFileComplete") class RcvFileComplete(val user: User, val chatItem: AChatItem): CR()
  // sending file events
  @Serializable @SerialName("sndFileStart") class SndFileStart(val user: User, val chatItem: AChatItem, val sndFileTransfer: SndFileTransfer): CR()
  @Serializable @SerialName("sndFileComplete") class SndFileComplete(val user: User, val chatItem: AChatItem, val sndFileTransfer: SndFileTransfer): CR()
  @Serializable @SerialName("sndFileCancelled") class SndFileCancelled(val chatItem: AChatItem, val sndFileTransfer: SndFileTransfer): CR()
  @Serializable @SerialName("sndFileRcvCancelled") class SndFileRcvCancelled(val user: User, val chatItem: AChatItem, val sndFileTransfer: SndFileTransfer): CR()
  @Serializable @SerialName("sndGroupFileCancelled") class SndGroupFileCancelled(val user: User, val chatItem: AChatItem, val fileTransferMeta: FileTransferMeta, val sndFileTransfers: List<SndFileTransfer>): CR()
  @Serializable @SerialName("callInvitation") class CallInvitation(val callInvitation: RcvCallInvitation): CR()
  @Serializable @SerialName("callOffer") class CallOffer(val user: User, val contact: Contact, val callType: CallType, val offer: WebRTCSession, val sharedKey: String? = null, val askConfirmation: Boolean): CR()
  @Serializable @SerialName("callAnswer") class CallAnswer(val user: User, val contact: Contact, val answer: WebRTCSession): CR()
  @Serializable @SerialName("callExtraInfo") class CallExtraInfo(val user: User, val contact: Contact, val extraInfo: WebRTCExtraInfo): CR()
  @Serializable @SerialName("callEnded") class CallEnded(val user: User, val contact: Contact): CR()
  @Serializable @SerialName("newContactConnection") class NewContactConnection(val user: User, val connection: PendingContactConnection): CR()
  @Serializable @SerialName("contactConnectionDeleted") class ContactConnectionDeleted(val user: User, val connection: PendingContactConnection): CR()
  @Serializable @SerialName("versionInfo") class VersionInfo(val versionInfo: CoreVersionInfo): CR()
  @Serializable @SerialName("apiParsedMarkdown") class ParsedMarkdown(val formattedText: List<FormattedText>? = null): CR()
  @Serializable @SerialName("cmdOk") class CmdOk(val user: User?): CR()
  @Serializable @SerialName("chatCmdError") class ChatCmdError(val user: User?, val chatError: ChatError): CR()
  @Serializable @SerialName("chatError") class ChatRespError(val user: User?, val chatError: ChatError): CR()
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
    is UserSMPServers -> "userSMPServers"
    is SmpTestResult -> "smpTestResult"
    is ChatItemTTL -> "chatItemTTL"
    is NetworkConfig -> "networkConfig"
    is ContactInfo -> "contactInfo"
    is GroupMemberInfo -> "groupMemberInfo"
    is ContactCode -> "contactCode"
    is GroupMemberCode -> "groupMemberCode"
    is ConnectionVerified -> "connectionVerified"
    is Invitation -> "invitation"
    is SentConfirmation -> "sentConfirmation"
    is SentInvitation -> "sentInvitation"
    is ContactAlreadyExists -> "contactAlreadyExists"
    is ContactDeleted -> "contactDeleted"
    is ChatCleared -> "chatCleared"
    is UserProfileNoChange -> "userProfileNoChange"
    is UserProfileUpdated -> "userProfileUpdated"
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
    is ContactsSubscribed -> "contactsSubscribed"
    is ContactsDisconnected -> "contactsDisconnected"
    is ContactSubError -> "contactSubError"
    is ContactSubSummary -> "contactSubSummary"
    is GroupSubscribed -> "groupSubscribed"
    is MemberSubErrors -> "memberSubErrors"
    is GroupEmpty -> "groupEmpty"
    is UserContactLinkSubscribed -> "userContactLinkSubscribed"
    is NewChatItem -> "newChatItem"
    is ChatItemStatusUpdated -> "chatItemStatusUpdated"
    is ChatItemUpdated -> "chatItemUpdated"
    is ChatItemDeleted -> "chatItemDeleted"
    is ContactsList -> "contactsList"
    is GroupCreated -> "groupCreated"
    is SentGroupInvitation -> "sentGroupInvitation"
    is UserAcceptedGroupSent -> "userAcceptedGroupSent"
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
    is RcvFileAcceptedSndCancelled -> "rcvFileAcceptedSndCancelled"
    is RcvFileAccepted -> "rcvFileAccepted"
    is RcvFileStart -> "rcvFileStart"
    is RcvFileComplete -> "rcvFileComplete"
    is SndFileCancelled -> "sndFileCancelled"
    is SndFileComplete -> "sndFileComplete"
    is SndFileRcvCancelled -> "sndFileRcvCancelled"
    is SndFileStart -> "sndFileStart"
    is SndGroupFileCancelled -> "sndGroupFileCancelled"
    is CallInvitation -> "callInvitation"
    is CallOffer -> "callOffer"
    is CallAnswer -> "callAnswer"
    is CallExtraInfo -> "callExtraInfo"
    is CallEnded -> "callEnded"
    is NewContactConnection -> "newContactConnection"
    is ContactConnectionDeleted -> "contactConnectionDeleted"
    is VersionInfo -> "versionInfo"
    is ParsedMarkdown -> "apiParsedMarkdown"
    is CmdOk -> "cmdOk"
    is ChatCmdError -> "chatCmdError"
    is ChatRespError -> "chatError"
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
    is UserSMPServers -> withUser(user, "$smpServers: ${json.encodeToString(smpServers)}\n$presetSMPServers: ${json.encodeToString(presetSMPServers)}")
    is SmpTestResult -> withUser(user, json.encodeToString(smpTestFailure))
    is ChatItemTTL -> withUser(user, json.encodeToString(chatItemTTL))
    is NetworkConfig -> json.encodeToString(networkConfig)
    is ContactInfo -> withUser(user, "contact: ${json.encodeToString(contact)}\nconnectionStats: ${json.encodeToString(connectionStats)}")
    is GroupMemberInfo -> withUser(user, "group: ${json.encodeToString(groupInfo)}\nmember: ${json.encodeToString(member)}\nconnectionStats: ${json.encodeToString(connectionStats_)}")
    is ContactCode -> withUser(user, "contact: ${json.encodeToString(contact)}\nconnectionCode: $connectionCode")
    is GroupMemberCode -> withUser(user, "groupInfo: ${json.encodeToString(groupInfo)}\nmember: ${json.encodeToString(member)}\nconnectionCode: $connectionCode")
    is ConnectionVerified -> withUser(user, "verified: $verified\nconnectionCode: $expectedCode")
    is Invitation -> withUser(user, connReqInvitation)
    is SentConfirmation -> withUser(user, noDetails())
    is SentInvitation -> withUser(user, noDetails())
    is ContactAlreadyExists -> withUser(user, json.encodeToString(contact))
    is ContactDeleted -> withUser(user, json.encodeToString(contact))
    is ChatCleared -> withUser(user, json.encodeToString(chatInfo))
    is UserProfileNoChange -> withUser(user, noDetails())
    is UserProfileUpdated -> withUser(user, json.encodeToString(toProfile))
    is ContactAliasUpdated -> withUser(user, json.encodeToString(toContact))
    is ConnectionAliasUpdated -> withUser(user, json.encodeToString(toConnection))
    is ContactPrefsUpdated -> withUser(user, "fromContact: $fromContact\ntoContact: \n${json.encodeToString(toContact)}")
    is ParsedMarkdown -> json.encodeToString(formattedText)
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
    is ContactsSubscribed -> "server: $server\ncontacts:\n${json.encodeToString(contactRefs)}"
    is ContactsDisconnected -> "server: $server\ncontacts:\n${json.encodeToString(contactRefs)}"
    is ContactSubError -> withUser(user, "error:\n${chatError.string}\ncontact:\n${json.encodeToString(contact)}")
    is ContactSubSummary -> withUser(user, json.encodeToString(contactSubscriptions))
    is GroupSubscribed -> withUser(user, json.encodeToString(group))
    is MemberSubErrors -> withUser(user, json.encodeToString(memberSubErrors))
    is GroupEmpty -> withUser(user, json.encodeToString(group))
    is UserContactLinkSubscribed -> noDetails()
    is NewChatItem -> withUser(user, json.encodeToString(chatItem))
    is ChatItemStatusUpdated -> withUser(user, json.encodeToString(chatItem))
    is ChatItemUpdated -> withUser(user, json.encodeToString(chatItem))
    is ChatItemDeleted -> withUser(user, "deletedChatItem:\n${json.encodeToString(deletedChatItem)}\ntoChatItem:\n${json.encodeToString(toChatItem)}\nbyUser: $byUser")
    is ContactsList -> withUser(user, json.encodeToString(contacts))
    is GroupCreated -> withUser(user, json.encodeToString(groupInfo))
    is SentGroupInvitation -> withUser(user, "groupInfo: $groupInfo\ncontact: $contact\nmember: $member")
    is UserAcceptedGroupSent -> json.encodeToString(groupInfo)
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
    is ConnectedToGroupMember -> withUser(user, "groupInfo: $groupInfo\nmember: $member")
    is GroupRemoved -> withUser(user, json.encodeToString(groupInfo))
    is GroupUpdated -> withUser(user, json.encodeToString(toGroup))
    is GroupLinkCreated -> withUser(user, "groupInfo: $groupInfo\nconnReqContact: $connReqContact")
    is GroupLink -> withUser(user, "groupInfo: $groupInfo\nconnReqContact: $connReqContact")
    is GroupLinkDeleted -> withUser(user, json.encodeToString(groupInfo))
    is RcvFileAcceptedSndCancelled -> withUser(user, noDetails())
    is RcvFileAccepted -> withUser(user, json.encodeToString(chatItem))
    is RcvFileStart -> withUser(user, json.encodeToString(chatItem))
    is RcvFileComplete -> withUser(user, json.encodeToString(chatItem))
    is SndFileCancelled -> json.encodeToString(chatItem)
    is SndFileComplete -> withUser(user, json.encodeToString(chatItem))
    is SndFileRcvCancelled -> withUser(user, json.encodeToString(chatItem))
    is SndFileStart -> withUser(user, json.encodeToString(chatItem))
    is SndGroupFileCancelled -> withUser(user, json.encodeToString(chatItem))
    is CallInvitation -> "contact: ${callInvitation.contact.id}\ncallType: $callInvitation.callType\nsharedKey: ${callInvitation.sharedKey ?: ""}"
    is CallOffer -> withUser(user, "contact: ${contact.id}\ncallType: $callType\nsharedKey: ${sharedKey ?: ""}\naskConfirmation: $askConfirmation\noffer: ${json.encodeToString(offer)}")
    is CallAnswer -> withUser(user, "contact: ${contact.id}\nanswer: ${json.encodeToString(answer)}")
    is CallExtraInfo -> withUser(user, "contact: ${contact.id}\nextraInfo: ${json.encodeToString(extraInfo)}")
    is CallEnded -> withUser(user, "contact: ${contact.id}")
    is NewContactConnection -> withUser(user, json.encodeToString(connection))
    is ContactConnectionDeleted -> withUser(user, json.encodeToString(connection))
    is VersionInfo -> json.encodeToString(versionInfo)
    is CmdOk -> withUser(user, noDetails())
    is ChatCmdError -> withUser(user, chatError.string)
    is ChatRespError -> withUser(user, chatError.string)
    is Response -> json
    is Invalid -> str
  }

  fun noDetails(): String ="${responseType}: " + generalGetString(R.string.no_details)

  private fun withUser(u: User?, s: String): String = if (u != null) "userId: ${u.userId}\n$s" else s
}

abstract class TerminalItem {
  abstract val id: Long
  val date: Instant = Clock.System.now()
  abstract val label: String
  abstract val details: String

  class Cmd(override val id: Long, val cmd: CC): TerminalItem() {
    override val label get() = "> ${cmd.cmdString}"
    override val details get() = cmd.cmdString
  }

  class Resp(override val id: Long, val resp: CR): TerminalItem() {
    override val label get() = "< ${resp.responseType}"
    override val details get() = resp.details
  }

  companion object {
    val sampleData = listOf(
        Cmd(0, CC.ShowActiveUser()),
        Resp(1, CR.ActiveUser(User.sampleData))
    )

    fun cmd(c: CC) = Cmd(System.currentTimeMillis(), c)
    fun resp(r: CR) = Resp(System.currentTimeMillis(), r)
  }
}

@Serializable
class ConnectionStats(val rcvServers: List<String>?, val sndServers: List<String>?)

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
  val buildTimestamp: String,
  val simplexmqVersion: String,
  val simplexmqCommit: String
)

@Serializable
sealed class ChatError {
  val string: String get() = when (this) {
    is ChatErrorChat -> "chat ${errorType.string}"
    is ChatErrorAgent -> "agent ${agentError.string}"
    is ChatErrorStore -> "store ${storeError.string}"
    is ChatErrorDatabase -> "database ${databaseError.string}"
  }
  @Serializable @SerialName("error") class ChatErrorChat(val errorType: ChatErrorType): ChatError()
  @Serializable @SerialName("errorAgent") class ChatErrorAgent(val agentError: AgentErrorType): ChatError()
  @Serializable @SerialName("errorStore") class ChatErrorStore(val storeError: StoreError): ChatError()
  @Serializable @SerialName("errorDatabase") class ChatErrorDatabase(val databaseError: DatabaseError): ChatError()
}

@Serializable
sealed class ChatErrorType {
  val string: String get() = when (this) {
    is NoActiveUser -> "noActiveUser"
    is DifferentActiveUser -> "differentActiveUser"
    is UserExists -> "userExists"
    is InvalidConnReq -> "invalidConnReq"
    is FileAlreadyReceiving -> "fileAlreadyReceiving"
    is СommandError -> "commandError $message"
  }
  @Serializable @SerialName("noActiveUser") class NoActiveUser: ChatErrorType()
  @Serializable @SerialName("differentActiveUser") class DifferentActiveUser: ChatErrorType()
  @Serializable @SerialName("userExists") class UserExists(val contactName: String): ChatErrorType()
  @Serializable @SerialName("invalidConnReq") class InvalidConnReq: ChatErrorType()
  @Serializable @SerialName("fileAlreadyReceiving") class FileAlreadyReceiving: ChatErrorType()
  @Serializable @SerialName("commandError") class СommandError(val message: String): ChatErrorType()
}

@Serializable
sealed class StoreError {
  val string: String get() = when (this) {
    is UserContactLinkNotFound -> "userContactLinkNotFound"
    is GroupNotFound -> "groupNotFound"
    is DuplicateName -> "duplicateName"
  }
  @Serializable @SerialName("userContactLinkNotFound") class UserContactLinkNotFound: StoreError()
  @Serializable @SerialName("groupNotFound") class GroupNotFound: StoreError()
  @Serializable @SerialName("duplicateName") class DuplicateName: StoreError()
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
    is BROKER -> "BROKER ${brokerErr.string}"
    is AGENT -> "AGENT ${agentErr.string}"
    is INTERNAL -> "INTERNAL $internalErr"
  }
  @Serializable @SerialName("CMD") class CMD(val cmdErr: CommandErrorType): AgentErrorType()
  @Serializable @SerialName("CONN") class CONN(val connErr: ConnectionErrorType): AgentErrorType()
  @Serializable @SerialName("SMP") class SMP(val smpErr: SMPErrorType): AgentErrorType()
  @Serializable @SerialName("BROKER") class BROKER(val brokerAddress: String, val brokerErr: BrokerErrorType): AgentErrorType()
  @Serializable @SerialName("AGENT") class AGENT(val agentErr: SMPAgentError): AgentErrorType()
  @Serializable @SerialName("INTERNAL") class INTERNAL(val internalErr: String): AgentErrorType()
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
    is RESPONSE -> "RESPONSE ${smpErr.string}"
    is UNEXPECTED -> "UNEXPECTED"
    is NETWORK -> "NETWORK"
    is TRANSPORT -> "TRANSPORT ${transportErr.string}"
    is TIMEOUT -> "TIMEOUT"
  }
  @Serializable @SerialName("RESPONSE") class RESPONSE(val smpErr: SMPErrorType): BrokerErrorType()
  @Serializable @SerialName("UNEXPECTED") class UNEXPECTED: BrokerErrorType()
  @Serializable @SerialName("NETWORK") class NETWORK: BrokerErrorType()
  @Serializable @SerialName("TRANSPORT") class TRANSPORT(val transportErr: SMPTransportError): BrokerErrorType()
  @Serializable @SerialName("TIMEOUT") class TIMEOUT: BrokerErrorType()
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
  @Serializable @SerialName("CMD") class CMD(val cmdErr: SMPCommandError): SMPErrorType()
  @Serializable @SerialName("AUTH") class AUTH: SMPErrorType()
  @Serializable @SerialName("QUOTA") class QUOTA: SMPErrorType()
  @Serializable @SerialName("NO_MSG") class NO_MSG: SMPErrorType()
  @Serializable @SerialName("LARGE_MSG") class LARGE_MSG: SMPErrorType()
  @Serializable @SerialName("INTERNAL") class INTERNAL: SMPErrorType()
}

@Serializable
sealed class SMPCommandError {
  val string: String get() = when (this) {
    is UNKNOWN -> "UNKNOWN"
    is SYNTAX -> "SYNTAX"
    is NO_AUTH -> "NO_AUTH"
    is HAS_AUTH -> "HAS_AUTH"
    is NO_QUEUE -> "NO_QUEUE"
  }
  @Serializable @SerialName("UNKNOWN") class UNKNOWN: SMPCommandError()
  @Serializable @SerialName("SYNTAX") class SYNTAX: SMPCommandError()
  @Serializable @SerialName("NO_AUTH") class NO_AUTH: SMPCommandError()
  @Serializable @SerialName("HAS_AUTH") class HAS_AUTH: SMPCommandError()
  @Serializable @SerialName("NO_QUEUE") class NO_QUEUE: SMPCommandError()
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
    is A_ENCRYPTION -> "A_ENCRYPTION"
  }
  @Serializable @SerialName("A_MESSAGE") class A_MESSAGE: SMPAgentError()
  @Serializable @SerialName("A_PROHIBITED") class A_PROHIBITED: SMPAgentError()
  @Serializable @SerialName("A_VERSION") class A_VERSION: SMPAgentError()
  @Serializable @SerialName("A_ENCRYPTION") class A_ENCRYPTION: SMPAgentError()
}
