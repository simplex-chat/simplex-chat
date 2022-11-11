package chat.simplex.app.model

import android.annotation.SuppressLint
import android.app.ActivityManager
import android.app.ActivityManager.RunningAppProcessInfo
import android.app.Application
import android.content.*
import android.net.Uri
import android.os.PowerManager
import android.provider.Settings
import android.util.Log
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Bolt
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.toArgb
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
import chat.simplex.app.views.usersettings.NotificationPreviewMode
import chat.simplex.app.views.usersettings.NotificationsMode
import kotlinx.coroutines.*
import kotlinx.datetime.Clock
import kotlinx.datetime.Instant
import kotlinx.serialization.*
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import kotlin.concurrent.thread

typealias ChatCtrl = Long

fun isAppOnForeground(context: Context): Boolean {
  val activityManager = context.getSystemService(Context.ACTIVITY_SERVICE) as ActivityManager
  val appProcesses = activityManager.runningAppProcesses ?: return false
  val packageName = context.packageName
  for (appProcess in appProcesses) {
    if (appProcess.importance == RunningAppProcessInfo.IMPORTANCE_FOREGROUND && appProcess.processName == packageName) {
      return true
    }
  }
  return false
}

enum class CallOnLockScreen {
  DISABLE,
  SHOW,
  ACCEPT;

  companion object {
    val default = SHOW
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
  val callOnLockScreen: Preference<CallOnLockScreen> = Preference(
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
  val privacyAcceptImages = mkBoolPreference(SHARED_PREFS_PRIVACY_ACCEPT_IMAGES, true)
  val privacyTransferImagesInline = mkBoolPreference(SHARED_PREFS_PRIVACY_TRANSFER_IMAGES_INLINE, false)
  val privacyLinkPreviews = mkBoolPreference(SHARED_PREFS_PRIVACY_LINK_PREVIEWS, true)
  val experimentalCalls = mkBoolPreference(SHARED_PREFS_EXPERIMENTAL_CALLS, false)
  val chatArchiveName = mkStrPreference(SHARED_PREFS_CHAT_ARCHIVE_NAME, null)
  val chatArchiveTime = mkDatePreference(SHARED_PREFS_CHAT_ARCHIVE_TIME, null)
  val chatLastStart = mkDatePreference(SHARED_PREFS_CHAT_LAST_START, null)
  val developerTools = mkBoolPreference(SHARED_PREFS_DEVELOPER_TOOLS, false)
  val networkUseSocksProxy = mkBoolPreference(SHARED_PREFS_NETWORK_USE_SOCKS_PROXY, false)
  val networkHostMode = mkStrPreference(SHARED_PREFS_NETWORK_HOST_MODE, HostMode.OnionViaSocks.name)
  val networkRequiredHostMode = mkBoolPreference(SHARED_PREFS_NETWORK_REQUIRED_HOST_MODE, false)
  val networkTCPConnectTimeout = mkTimeoutPreference(SHARED_PREFS_NETWORK_TCP_CONNECT_TIMEOUT, NetCfg.defaults.tcpConnectTimeout, NetCfg.proxyDefaults.tcpConnectTimeout)
  val networkTCPTimeout = mkTimeoutPreference(SHARED_PREFS_NETWORK_TCP_TIMEOUT, NetCfg.defaults.tcpTimeout, NetCfg.proxyDefaults.tcpTimeout)
  val networkSMPPingInterval = mkLongPreference(SHARED_PREFS_NETWORK_SMP_PING_INTERVAL, NetCfg.defaults.smpPingInterval)
  val networkEnableKeepAlive = mkBoolPreference(SHARED_PREFS_NETWORK_ENABLE_KEEP_ALIVE, NetCfg.defaults.enableKeepAlive)
  val networkTCPKeepIdle = mkIntPreference(SHARED_PREFS_NETWORK_TCP_KEEP_IDLE, KeepAliveOpts.defaults.keepIdle)
  val networkTCPKeepIntvl = mkIntPreference(SHARED_PREFS_NETWORK_TCP_KEEP_INTVL, KeepAliveOpts.defaults.keepIntvl)
  val networkTCPKeepCnt = mkIntPreference(SHARED_PREFS_NETWORK_TCP_KEEP_CNT, KeepAliveOpts.defaults.keepCnt)
  val incognito = mkBoolPreference(SHARED_PREFS_INCOGNITO, false)
  val connectViaLinkTab = mkStrPreference(SHARED_PREFS_CONNECT_VIA_LINK_TAB, ConnectViaLinkTab.SCAN.name)

  val storeDBPassphrase = mkBoolPreference(SHARED_PREFS_STORE_DB_PASSPHRASE, true)
  val initialRandomDBPassphrase = mkBoolPreference(SHARED_PREFS_INITIAL_RANDOM_DB_PASSPHRASE, false)
  val encryptedDBPassphrase = mkStrPreference(SHARED_PREFS_ENCRYPTED_DB_PASSPHRASE, null)
  val initializationVectorDBPassphrase = mkStrPreference(SHARED_PREFS_INITIALIZATION_VECTOR_DB_PASSPHRASE, null)
  val encryptionStartedAt = mkDatePreference(SHARED_PREFS_ENCRYPTION_STARTED_AT, null, true)

  val currentTheme = mkStrPreference(SHARED_PREFS_CURRENT_THEME, DefaultTheme.SYSTEM.name)
  val primaryColor = mkIntPreference(SHARED_PREFS_PRIMARY_COLOR, LightColorPalette.primary.toArgb())

  private fun mkIntPreference(prefName: String, default: Int) =
    Preference(
      get = fun() = sharedPreferences.getInt(prefName, default),
      set = fun(value) = sharedPreferences.edit().putInt(prefName, value).apply()
    )

  private fun mkLongPreference(prefName: String, default: Long) =
    Preference(
      get = fun() = sharedPreferences.getLong(prefName, default),
      set = fun(value) = sharedPreferences.edit().putLong(prefName, value).apply()
    )

  private fun mkTimeoutPreference(prefName: String, default: Long, proxyDefault: Long): Preference<Long> {
    val d = if (networkUseSocksProxy.get()) proxyDefault else default
    return Preference(
      get = fun() = sharedPreferences.getLong(prefName, d),
      set = fun(value) = sharedPreferences.edit().putLong(prefName, value).apply()
    )
  }

  private fun mkBoolPreference(prefName: String, default: Boolean) =
    Preference(
      get = fun() = sharedPreferences.getBoolean(prefName, default),
      set = fun(value) = sharedPreferences.edit().putBoolean(prefName, value).apply()
    )

  private fun mkStrPreference(prefName: String, default: String?): Preference<String?> =
    Preference(
      get = fun() = sharedPreferences.getString(prefName, default),
      set = fun(value) = sharedPreferences.edit().putString(prefName, value).apply()
    )

  /**
  * Provide `[commit] = true` to save preferences right now, not after some unknown period of time.
  * So in case of a crash this value will be saved 100%
  * */
  private fun mkDatePreference(prefName: String, default: Instant?, commit: Boolean = false): Preference<Instant?> =
    Preference(
      get = {
        val pref = sharedPreferences.getString(prefName, default?.toEpochMilliseconds()?.toString())
        pref?.let { Instant.fromEpochMilliseconds(pref.toLong()) }
      },
      set = fun(value) = sharedPreferences.edit().putString(prefName, value?.toEpochMilliseconds()?.toString()).let {
        if (commit) it.commit() else it.apply()
      }
    )

  companion object {
    private const val SHARED_PREFS_ID = "chat.simplex.app.SIMPLEX_APP_PREFS"
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
    private const val SHARED_PREFS_PRIVACY_ACCEPT_IMAGES = "PrivacyAcceptImages"
    private const val SHARED_PREFS_PRIVACY_TRANSFER_IMAGES_INLINE = "PrivacyTransferImagesInline"
    private const val SHARED_PREFS_PRIVACY_LINK_PREVIEWS = "PrivacyLinkPreviews"
    private const val SHARED_PREFS_EXPERIMENTAL_CALLS = "ExperimentalCalls"
    private const val SHARED_PREFS_CHAT_ARCHIVE_NAME = "ChatArchiveName"
    private const val SHARED_PREFS_CHAT_ARCHIVE_TIME = "ChatArchiveTime"
    private const val SHARED_PREFS_CHAT_LAST_START = "ChatLastStart"
    private const val SHARED_PREFS_DEVELOPER_TOOLS = "DeveloperTools"
    private const val SHARED_PREFS_NETWORK_USE_SOCKS_PROXY = "NetworkUseSocksProxy"
    private const val SHARED_PREFS_NETWORK_HOST_MODE = "NetworkHostMode"
    private const val SHARED_PREFS_NETWORK_REQUIRED_HOST_MODE = "NetworkRequiredHostMode"
    private const val SHARED_PREFS_NETWORK_TCP_CONNECT_TIMEOUT = "NetworkTCPConnectTimeout"
    private const val SHARED_PREFS_NETWORK_TCP_TIMEOUT = "NetworkTCPTimeout"
    private const val SHARED_PREFS_NETWORK_SMP_PING_INTERVAL = "NetworkSMPPingInterval"
    private const val SHARED_PREFS_NETWORK_ENABLE_KEEP_ALIVE = "NetworkEnableKeepAlive"
    private const val SHARED_PREFS_NETWORK_TCP_KEEP_IDLE = "NetworkTCPKeepIdle"
    private const val SHARED_PREFS_NETWORK_TCP_KEEP_INTVL = "NetworkTCPKeepIntvl"
    private const val SHARED_PREFS_NETWORK_TCP_KEEP_CNT = "NetworkTCPKeepCnt"
    private const val SHARED_PREFS_INCOGNITO = "Incognito"
    private const val SHARED_PREFS_CONNECT_VIA_LINK_TAB = "ConnectViaLinkTab"
    private const val SHARED_PREFS_STORE_DB_PASSPHRASE = "StoreDBPassphrase"
    private const val SHARED_PREFS_INITIAL_RANDOM_DB_PASSPHRASE = "InitialRandomDBPassphrase"
    private const val SHARED_PREFS_ENCRYPTED_DB_PASSPHRASE = "EncryptedDBPassphrase"
    private const val SHARED_PREFS_INITIALIZATION_VECTOR_DB_PASSPHRASE = "InitializationVectorDBPassphrase"
    private const val SHARED_PREFS_ENCRYPTION_STARTED_AT = "EncryptionStartedAt"
    private const val SHARED_PREFS_CURRENT_THEME = "CurrentTheme"
    private const val SHARED_PREFS_PRIMARY_COLOR = "PrimaryColor"
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
      if (justStarted) {
        apiSetFilesFolder(getAppFilesDirectory(appContext))
        apiSetIncognito(chatModel.incognito.value)
        chatModel.userAddress.value = apiGetUserAddress()
        chatModel.userSMPServers.value = getUserSMPServers()
        chatModel.chatItemTTL.value = getChatItemTTL()
        val chats = apiGetChats()
        chatModel.updateChats(chats)
        chatModel.currentUser.value = user
        chatModel.userCreated.value = true
        chatModel.onboardingStage.value = OnboardingStage.OnboardingComplete
        chatModel.controller.appPrefs.chatLastStart.set(Clock.System.now())
        chatModel.chatRunning.value = true
        chatModel.appOpenUrl.value?.let {
          chatModel.appOpenUrl.value = null
          connectIfOpenedViaUri(it, chatModel)
        }
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
        chatModel.terminalItems.add(TerminalItem.cmd(cmd.obfuscated))
        Log.d(TAG, "sendCmd: ${cmd.cmdType}")
      }
      val json = chatSendCmd(ctrl, c)
      val r = APIResponse.decodeStr(json)
      Log.d(TAG, "sendCmd response type ${r.resp.responseType}")
      if (r.resp is CR.Response || r.resp is CR.Invalid) {
        Log.d(TAG, "sendCmd response json $json")
      }
      if (r.resp !is CR.ParsedMarkdown) {
        chatModel.terminalItems.add(TerminalItem.resp(r.resp))
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

  suspend fun apiCreateActiveUser(p: Profile): User {
    val r = sendCmd(CC.CreateActiveUser(p))
    if (r is CR.ActiveUser) return r.user
    Log.d(TAG, "apiCreateActiveUser: ${r.responseType} ${r.details}")
    throw Error("user not created ${r.responseType} ${r.details}")
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
    val r = sendCmd(CC.ApiGetChats())
    if (r is CR.ApiChats ) return r.chats
    throw Exception("failed getting the list of chats: ${r.responseType} ${r.details}")
  }

  suspend fun apiGetChat(type: ChatType, id: Long, pagination: ChatPagination = ChatPagination.Last(ChatPagination.INITIAL_COUNT), search: String = ""): Chat? {
    val r = sendCmd(CC.ApiGetChat(type, id, pagination, search))
    if (r is CR.ApiChat ) return r.chat
    Log.e(TAG, "apiGetChat bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiSendMessage(type: ChatType, id: Long, file: String? = null, quotedItemId: Long? = null, mc: MsgContent): AChatItem? {
    val cmd = CC.ApiSendMessage(type, id, file, quotedItemId, mc)
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

  suspend fun apiUpdateChatItem(type: ChatType, id: Long, itemId: Long, mc: MsgContent): AChatItem? {
    val r = sendCmd(CC.ApiUpdateChatItem(type, id, itemId, mc))
    if (r is CR.ChatItemUpdated) return r.chatItem
    Log.e(TAG, "apiUpdateChatItem bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiDeleteChatItem(type: ChatType, id: Long, itemId: Long, mode: CIDeleteMode): AChatItem? {
    val r = sendCmd(CC.ApiDeleteChatItem(type, id, itemId, mode))
    if (r is CR.ChatItemDeleted) return r.toChatItem
    Log.e(TAG, "apiDeleteChatItem bad response: ${r.responseType} ${r.details}")
    return null
  }

  private suspend fun getUserSMPServers(): List<String>? {
    val r = sendCmd(CC.GetUserSMPServers())
    if (r is CR.UserSMPServers) return r.smpServers
    Log.e(TAG, "getUserSMPServers bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun setUserSMPServers(smpServers: List<String>): Boolean {
    val r = sendCmd(CC.SetUserSMPServers(smpServers))
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

  suspend fun getChatItemTTL(): ChatItemTTL {
    val r = sendCmd(CC.APIGetChatItemTTL())
    if (r is CR.ChatItemTTL) return ChatItemTTL.fromSeconds(r.chatItemTTL)
    throw Exception("failed to get chat item TTL: ${r.responseType} ${r.details}")
  }

  suspend fun setChatItemTTL(chatItemTTL: ChatItemTTL) {
    val r = sendCmd(CC.APISetChatItemTTL(chatItemTTL.seconds))
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

  suspend fun apiAddContact(): String? {
    val r = sendCmd(CC.AddContact())
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
    val r = sendCmd(CC.Connect(connReq))
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
    val r = sendCmd(CC.ListContacts())
    if (r is CR.ContactsList) return r.contacts
    Log.e(TAG, "apiListContacts bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiUpdateProfile(profile: Profile): Profile? {
    val r = sendCmd(CC.ApiUpdateProfile(profile))
    if (r is CR.UserProfileNoChange) return profile
    if (r is CR.UserProfileUpdated) return r.toProfile
    Log.e(TAG, "apiUpdateProfile bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiParseMarkdown(text: String): List<FormattedText>? {
    val r = sendCmd(CC.ApiParseMarkdown(text))
    if (r is CR.ParsedMarkdown) return r.formattedText
    Log.e(TAG, "apiParseMarkdown bad response: ${r.responseType} ${r.details}")
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

  suspend fun apiSetContactPrefs(contactId: Long, prefs: ChatPreferences): Contact? {
    val r = sendCmd(CC.ApiSetContactPrefs(contactId, prefs))
    if (r is CR.ContactPrefsUpdated) return r.toContact
    Log.e(TAG, "apiSetContactPrefs bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiCreateUserAddress(): String? {
    val r = sendCmd(CC.CreateMyAddress())
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
    val r = sendCmd(CC.DeleteMyAddress())
    if (r is CR.UserContactLinkDeleted) return true
    Log.e(TAG, "apiDeleteUserAddress bad response: ${r.responseType} ${r.details}")
    return false
  }

  private suspend fun apiGetUserAddress(): UserContactLinkRec? {
    val r = sendCmd(CC.ShowMyAddress())
    if (r is CR.UserContactLink) return r.contactLink
    if (r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorStore
      && r.chatError.storeError is StoreError.UserContactLinkNotFound) {
      return null
    }
    Log.e(TAG, "apiGetUserAddress bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun userAddressAutoAccept(autoAccept: AutoAccept?): UserContactLinkRec? {
    val r = sendCmd(CC.AddressAutoAccept(autoAccept))
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

  suspend fun apiReceiveFile(fileId: Long, inline: Boolean): AChatItem? {
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
          apiErrorAlert("apiReceiveFile", generalGetString(R.string.error_receiving_file), r)
        }
        null
      }
    }
  }

  suspend fun apiNewGroup(p: GroupProfile): GroupInfo? {
    val r = sendCmd(CC.NewGroup(p))
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

  private fun networkErrorAlert(r: CR): Boolean {
    return when {
      r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorAgent
          && r.chatError.agentError is AgentErrorType.BROKER
          && r.chatError.agentError.brokerErr is BrokerErrorType.TIMEOUT -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(R.string.connection_timeout),
          generalGetString(R.string.network_error_desc)
        )
        true
      }
      r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorAgent
          && r.chatError.agentError is AgentErrorType.BROKER
          && r.chatError.agentError.brokerErr is BrokerErrorType.NETWORK -> {
        AlertManager.shared.showAlertMsg(
          generalGetString(R.string.connection_error),
          generalGetString(R.string.network_error_desc)
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

  fun processReceivedMsg(r: CR) {
    lastMsgReceivedTimestamp = System.currentTimeMillis()
    chatModel.terminalItems.add(TerminalItem.resp(r))
    when (r) {
      is CR.NewContactConnection -> {
        chatModel.updateContactConnection(r.connection)
      }
      is CR.ContactConnectionDeleted -> {
        chatModel.removeChat(r.connection.id)
      }
      is CR.ContactConnected -> {
        if (!r.contact.viaGroupLink) {
          chatModel.updateContact(r.contact)
          chatModel.dismissConnReqView(r.contact.activeConn.id)
          chatModel.removeChat(r.contact.activeConn.id)
          chatModel.updateNetworkStatus(r.contact.id, Chat.NetworkStatus.Connected())
          ntfManager.notifyContactConnected(r.contact)
        }
      }
      is CR.ContactConnecting -> {
        if (!r.contact.viaGroupLink) {
          chatModel.updateContact(r.contact)
          chatModel.dismissConnReqView(r.contact.activeConn.id)
          chatModel.removeChat(r.contact.activeConn.id)
        }
      }
      is CR.ReceivedContactRequest -> {
        val contactRequest = r.contactRequest
        val cInfo = ChatInfo.ContactRequest(contactRequest)
        chatModel.addChat(Chat(chatInfo = cInfo, chatItems = listOf()))
        ntfManager.notifyContactRequestReceived(cInfo)
      }
      is CR.ContactUpdated -> {
        val cInfo = ChatInfo.Direct(r.toContact)
        if (chatModel.hasChat(r.toContact.id)) {
          chatModel.updateChatInfo(cInfo)
        }
      }
      is CR.ContactsMerged -> {
        if (chatModel.hasChat(r.mergedContact.id)) {
          if (chatModel.chatId.value == r.mergedContact.id) {
            chatModel.chatId.value = r.intoContact.id
          }
          chatModel.removeChat(r.mergedContact.id)
        }
      }
      is CR.ContactsSubscribed -> updateContactsStatus(r.contactRefs, Chat.NetworkStatus.Connected())
      is CR.ContactsDisconnected -> updateContactsStatus(r.contactRefs, Chat.NetworkStatus.Disconnected())
      is CR.ContactSubError -> processContactSubError(r.contact, r.chatError)
      is CR.ContactSubSummary -> {
        for (sub in r.contactSubscriptions) {
          val err = sub.contactError
          if (err == null) {
            chatModel.updateContact(sub.contact)
            chatModel.updateNetworkStatus(sub.contact.id, Chat.NetworkStatus.Connected())
          } else {
            processContactSubError(sub.contact, sub.contactError)
          }
        }
      }
      is CR.NewChatItem -> {
        val cInfo = r.chatItem.chatInfo
        val cItem = r.chatItem.chatItem
        chatModel.addChatItem(cInfo, cItem)
        val file = cItem.file
        if (cItem.content.msgContent is MsgContent.MCImage && file != null && file.fileSize <= MAX_IMAGE_SIZE_AUTO_RCV && appPrefs.privacyAcceptImages.get()) {
          withApi { receiveFile(file.fileId) }
        }
        if (!cItem.chatDir.sent && !cItem.isCall && !cItem.isMutedMemberEvent && (!isAppOnForeground(appContext) || chatModel.chatId.value != cInfo.id)) {
          ntfManager.notifyMessageReceived(cInfo, cItem)
        }
      }
      is CR.ChatItemStatusUpdated -> {
        val cInfo = r.chatItem.chatInfo
        val cItem = r.chatItem.chatItem
        var res = false
        if (!cItem.isDeletedContent) {
          res = chatModel.upsertChatItem(cInfo, cItem)
        }
        if (res) {
          ntfManager.notifyMessageReceived(cInfo, cItem)
        }
      }
      is CR.ChatItemUpdated ->
        chatItemSimpleUpdate(r.chatItem)
      is CR.ChatItemDeleted -> {
        val cInfo = r.toChatItem.chatInfo
        val cItem = r.toChatItem.chatItem
        if (cItem.meta.itemDeleted) {
          chatModel.removeChatItem(cInfo, cItem)
        } else {
          // currently only broadcast deletion of rcv message can be received, and only this case should happen
          chatModel.upsertChatItem(cInfo, cItem)
        }
      }
      is CR.ReceivedGroupInvitation -> {
        chatModel.updateGroup(r.groupInfo) // update so that repeat group invitations are not duplicated
        // TODO NtfManager.shared.notifyGroupInvitation
      }
      is CR.UserAcceptedGroupSent -> {
        chatModel.updateGroup(r.groupInfo)
        if (r.hostContact != null) {
          chatModel.dismissConnReqView(r.hostContact.activeConn.id)
          chatModel.removeChat(r.hostContact.activeConn.id)
        }
      }
      is CR.JoinedGroupMemberConnecting ->
        chatModel.upsertGroupMember(r.groupInfo, r.member)
      is CR.DeletedMemberUser -> // TODO update user member
        chatModel.updateGroup(r.groupInfo)
      is CR.DeletedMember ->
        chatModel.upsertGroupMember(r.groupInfo, r.deletedMember)
      is CR.LeftMember ->
        chatModel.upsertGroupMember(r.groupInfo, r.member)
      is CR.MemberRole ->
        chatModel.upsertGroupMember(r.groupInfo, r.member)
      is CR.MemberRoleUser ->
        chatModel.upsertGroupMember(r.groupInfo, r.member)
      is CR.GroupDeleted -> // TODO update user member
        chatModel.updateGroup(r.groupInfo)
      is CR.UserJoinedGroup ->
        chatModel.updateGroup(r.groupInfo)
      is CR.JoinedGroupMember ->
        chatModel.upsertGroupMember(r.groupInfo, r.member)
      is CR.ConnectedToGroupMember ->
        chatModel.upsertGroupMember(r.groupInfo, r.member)
      is CR.GroupUpdated ->
        chatModel.updateGroup(r.toGroup)
      is CR.RcvFileStart ->
        chatItemSimpleUpdate(r.chatItem)
      is CR.RcvFileComplete ->
        chatItemSimpleUpdate(r.chatItem)
      is CR.SndFileStart ->
        chatItemSimpleUpdate(r.chatItem)
      is CR.SndFileComplete -> {
        chatItemSimpleUpdate(r.chatItem)
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

  private fun withCall(r: CR, contact: Contact, perform: (Call) -> Unit) {
    val call = chatModel.activeCall.value
    if (call != null && call.contact.apiId == contact.apiId) {
      perform(call)
    } else {
      Log.d(TAG, "processReceivedMsg: ignoring ${r.responseType}, not in call with the contact ${contact.id}")
    }
  }

  suspend fun receiveFile(fileId: Long) {
    val inline = appPrefs.privacyTransferImagesInline.get()
    val chatItem = apiReceiveFile(fileId, inline)
    if (chatItem != null) {
      chatItemSimpleUpdate(chatItem)
    }
  }

  suspend fun leaveGroup(groupId: Long) {
    val groupInfo = apiLeaveGroup(groupId)
    if (groupInfo != null) {
      chatModel.updateGroup(groupInfo)
    }
  }

  private fun chatItemSimpleUpdate(aChatItem: AChatItem) {
    val cInfo = aChatItem.chatInfo
    val cItem = aChatItem.chatItem
    if (chatModel.upsertChatItem(cInfo, cItem)) {
      ntfManager.notifyMessageReceived(cInfo, cItem)
    }
  }

  fun updateContactsStatus(contactRefs: List<ContactRef>, status: Chat.NetworkStatus) {
    for (c in contactRefs) {
      chatModel.updateNetworkStatus(c.id, status)
    }
  }

  fun processContactSubError(contact: Contact, chatError: ChatError) {
    chatModel.updateContact(contact)
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
    chatModel.updateNetworkStatus(contact.id, Chat.NetworkStatus.Error(err))
  }

  fun showBackgroundServiceNoticeIfNeeded() {
    val mode = NotificationsMode.valueOf(appPrefs.notificationsMode.get()!!)
    Log.d(TAG, "showBackgroundServiceNoticeIfNeeded")
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
        SimplexService.stop(SimplexApp.context)
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
    val tcpConnectTimeout = appPrefs.networkTCPConnectTimeout.get()
    val tcpTimeout = appPrefs.networkTCPTimeout.get()
    val smpPingInterval = appPrefs.networkSMPPingInterval.get()
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
      tcpConnectTimeout = tcpConnectTimeout,
      tcpTimeout = tcpTimeout,
      tcpKeepAlive = tcpKeepAlive,
      smpPingInterval = smpPingInterval
    )
  }

  fun setNetCfg(cfg: NetCfg) {
    appPrefs.networkUseSocksProxy.set(cfg.useSocksProxy)
    appPrefs.networkHostMode.set(cfg.hostMode.name)
    appPrefs.networkRequiredHostMode.set(cfg.requiredHostMode)
    appPrefs.networkTCPConnectTimeout.set(cfg.tcpConnectTimeout)
    appPrefs.networkTCPTimeout.set(cfg.tcpTimeout)
    appPrefs.networkSMPPingInterval.set(cfg.smpPingInterval)
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

class Preference<T>(val get: () -> T, val set: (T) -> Unit)

// ChatCommand
sealed class CC {
  class Console(val cmd: String): CC()
  class ShowActiveUser: CC()
  class CreateActiveUser(val profile: Profile): CC()
  class StartChat(val expire: Boolean): CC()
  class ApiStopChat: CC()
  class SetFilesFolder(val filesFolder: String): CC()
  class SetIncognito(val incognito: Boolean): CC()
  class ApiExportArchive(val config: ArchiveConfig): CC()
  class ApiImportArchive(val config: ArchiveConfig): CC()
  class ApiDeleteStorage: CC()
  class ApiStorageEncryption(val config: DBEncryptionConfig): CC()
  class ApiGetChats: CC()
  class ApiGetChat(val type: ChatType, val id: Long, val pagination: ChatPagination, val search: String = ""): CC()
  class ApiSendMessage(val type: ChatType, val id: Long, val file: String?, val quotedItemId: Long?, val mc: MsgContent): CC()
  class ApiUpdateChatItem(val type: ChatType, val id: Long, val itemId: Long, val mc: MsgContent): CC()
  class ApiDeleteChatItem(val type: ChatType, val id: Long, val itemId: Long, val mode: CIDeleteMode): CC()
  class NewGroup(val groupProfile: GroupProfile): CC()
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
  class GetUserSMPServers: CC()
  class SetUserSMPServers(val smpServers: List<String>): CC()
  class APISetChatItemTTL(val seconds: Long?): CC()
  class APIGetChatItemTTL: CC()
  class APISetNetworkConfig(val networkConfig: NetCfg): CC()
  class APIGetNetworkConfig: CC()
  class APISetChatSettings(val type: ChatType, val id: Long, val chatSettings: ChatSettings): CC()
  class APIContactInfo(val contactId: Long): CC()
  class APIGroupMemberInfo(val groupId: Long, val groupMemberId: Long): CC()
  class APISwitchContact(val contactId: Long): CC()
  class APISwitchGroupMember(val groupId: Long, val groupMemberId: Long): CC()
  class AddContact: CC()
  class Connect(val connReq: String): CC()
  class ApiDeleteChat(val type: ChatType, val id: Long): CC()
  class ApiClearChat(val type: ChatType, val id: Long): CC()
  class ListContacts: CC()
  class ApiUpdateProfile(val profile: Profile): CC()
  class ApiParseMarkdown(val text: String): CC()
  class ApiSetContactAlias(val contactId: Long, val localAlias: String): CC()
  class ApiSetConnectionAlias(val connId: Long, val localAlias: String): CC()
  class ApiSetContactPrefs(val contactId: Long, val prefs: ChatPreferences): CC()
  class CreateMyAddress: CC()
  class DeleteMyAddress: CC()
  class ShowMyAddress: CC()
  class AddressAutoAccept(val autoAccept: AutoAccept?): CC()
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
  class ReceiveFile(val fileId: Long, val inline: Boolean): CC()

  val cmdString: String get() = when (this) {
    is Console -> cmd
    is ShowActiveUser -> "/u"
    is CreateActiveUser -> "/u ${profile.displayName} ${profile.fullName}"
    is StartChat -> "/_start subscribe=on expire=${onOff(expire)}"
    is ApiStopChat -> "/_stop"
    is SetFilesFolder -> "/_files_folder $filesFolder"
    is SetIncognito -> "/incognito ${onOff(incognito)}"
    is ApiExportArchive -> "/_db export ${json.encodeToString(config)}"
    is ApiImportArchive -> "/_db import ${json.encodeToString(config)}"
    is ApiDeleteStorage -> "/_db delete"
    is ApiStorageEncryption -> "/_db encryption ${json.encodeToString(config)}"
    is ApiGetChats -> "/_get chats pcc=on"
    is ApiGetChat -> "/_get chat ${chatRef(type, id)} ${pagination.cmdString}" + (if (search == "") "" else " search=$search")
    is ApiSendMessage -> "/_send ${chatRef(type, id)} json ${json.encodeToString(ComposedMessage(file, quotedItemId, mc))}"
    is ApiUpdateChatItem -> "/_update item ${chatRef(type, id)} $itemId ${mc.cmdString}"
    is ApiDeleteChatItem -> "/_delete item ${chatRef(type, id)} $itemId ${mode.deleteMode}"
    is NewGroup -> "/_group ${json.encodeToString(groupProfile)}"
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
    is GetUserSMPServers -> "/smp_servers"
    is SetUserSMPServers -> "/smp_servers ${smpServersStr(smpServers)}"
    is APISetChatItemTTL -> "/_ttl ${chatItemTTLStr(seconds)}"
    is APIGetChatItemTTL -> "/ttl"
    is APISetNetworkConfig -> "/_network ${json.encodeToString(networkConfig)}"
    is APIGetNetworkConfig -> "/network"
    is APISetChatSettings -> "/_settings ${chatRef(type, id)} ${json.encodeToString(chatSettings)}"
    is APIContactInfo -> "/_info @$contactId"
    is APIGroupMemberInfo -> "/_info #$groupId $groupMemberId"
    is APISwitchContact -> "/_switch @$contactId"
    is APISwitchGroupMember -> "/_switch #$groupId $groupMemberId"
    is AddContact -> "/connect"
    is Connect -> "/connect $connReq"
    is ApiDeleteChat -> "/_delete ${chatRef(type, id)}"
    is ApiClearChat -> "/_clear chat ${chatRef(type, id)}"
    is ListContacts -> "/contacts"
    is ApiUpdateProfile -> "/_profile ${json.encodeToString(profile)}"
    is ApiParseMarkdown -> "/_parse $text"
    is ApiSetContactAlias -> "/_set alias @$contactId ${localAlias.trim()}"
    is ApiSetConnectionAlias -> "/_set alias :$connId ${localAlias.trim()}"
    is ApiSetContactPrefs -> "/_set prefs @$contactId ${json.encodeToString(prefs)}"
    is CreateMyAddress -> "/address"
    is DeleteMyAddress -> "/delete_address"
    is ShowMyAddress -> "/show_address"
    is AddressAutoAccept -> "/auto_accept ${AutoAccept.cmdString(autoAccept)}"
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
    is ReceiveFile -> "/freceive $fileId inline=${onOff(inline)}"
  }

  val cmdType: String get() = when (this) {
    is Console -> "console command"
    is ShowActiveUser -> "showActiveUser"
    is CreateActiveUser -> "createActiveUser"
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
    is NewGroup -> "newGroup"
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
    is GetUserSMPServers -> "getUserSMPServers"
    is SetUserSMPServers -> "setUserSMPServers"
    is APISetChatItemTTL -> "apiSetChatItemTTL"
    is APIGetChatItemTTL -> "apiGetChatItemTTL"
    is APISetNetworkConfig -> "/apiSetNetworkConfig"
    is APIGetNetworkConfig -> "/apiGetNetworkConfig"
    is APISetChatSettings -> "/apiSetChatSettings"
    is APIContactInfo -> "apiContactInfo"
    is APIGroupMemberInfo -> "apiGroupMemberInfo"
    is APISwitchContact -> "apiSwitchContact"
    is APISwitchGroupMember -> "apiSwitchGroupMember"
    is AddContact -> "addContact"
    is Connect -> "connect"
    is ApiDeleteChat -> "apiDeleteChat"
    is ApiClearChat -> "apiClearChat"
    is ListContacts -> "listContacts"
    is ApiUpdateProfile -> "updateProfile"
    is ApiParseMarkdown -> "apiParseMarkdown"
    is ApiSetContactAlias -> "apiSetContactAlias"
    is ApiSetConnectionAlias -> "apiSetConnectionAlias"
    is ApiSetContactPrefs -> "apiSetContactPrefs"
    is CreateMyAddress -> "createMyAddress"
    is DeleteMyAddress -> "deleteMyAddress"
    is ShowMyAddress -> "showMyAddress"
    is AddressAutoAccept -> "addressAutoAccept"
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

    fun smpServersStr(smpServers: List<String>) = if (smpServers.isEmpty()) "default" else smpServers.joinToString(separator = ";")
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
data class NetCfg(
  val socksProxy: String? = null,
  val hostMode: HostMode = HostMode.OnionViaSocks,
  val requiredHostMode: Boolean = false,
  val tcpConnectTimeout: Long, // microseconds
  val tcpTimeout: Long, // microseconds
  val tcpKeepAlive: KeepAliveOpts?,
  val smpPingInterval: Long // microseconds
) {
  val useSocksProxy: Boolean get() = socksProxy != null
  val enableKeepAlive: Boolean get() = tcpKeepAlive != null

  companion object {
    val defaults: NetCfg =
      NetCfg(
        socksProxy = null,
        tcpConnectTimeout = 10_000_000,
        tcpTimeout = 7_000_000,
        tcpKeepAlive = KeepAliveOpts.defaults,
        smpPingInterval = 600_000_000
      )

    val proxyDefaults: NetCfg =
      NetCfg(
        socksProxy = ":9050",
        tcpConnectTimeout = 20_000_000,
        tcpTimeout = 15_000_000,
        tcpKeepAlive = KeepAliveOpts.defaults,
        smpPingInterval = 600_000_000
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
data class ChatPreferences(
  val voice: ChatPreference? = null
) {
  companion object {
    val default = ChatPreferences(
      voice = ChatPreference(allow = PrefAllowed.NO)
    )
    val empty = ChatPreferences(
      voice = null
    )
  }
}

@Serializable
data class ChatPreference(
  val allow: PrefAllowed
)

@Serializable
enum class PrefAllowed {
  @SerialName("always") ALWAYS,
  @SerialName("yes") YES,
  @SerialName("no") NO
}

val json = Json {
  prettyPrint = true
  ignoreUnknownKeys = true
  encodeDefaults = true
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
          APIResponse(
            resp = CR.Response(data["resp"]!!.jsonObject["type"]?.toString() ?: "invalid", json.encodeToString(data)),
            corr = data["corr"]?.toString()
          )
        } catch(e: Exception) {
          APIResponse(CR.Invalid(str))
        }
      }
    }
  }
}

// ChatResponse
@Serializable
sealed class CR {
  @Serializable @SerialName("activeUser") class ActiveUser(val user: User): CR()
  @Serializable @SerialName("chatStarted") class ChatStarted: CR()
  @Serializable @SerialName("chatRunning") class ChatRunning: CR()
  @Serializable @SerialName("chatStopped") class ChatStopped: CR()
  @Serializable @SerialName("apiChats") class ApiChats(val chats: List<Chat>): CR()
  @Serializable @SerialName("apiChat") class ApiChat(val chat: Chat): CR()
  @Serializable @SerialName("userSMPServers") class UserSMPServers(val smpServers: List<String>): CR()
  @Serializable @SerialName("chatItemTTL") class ChatItemTTL(val chatItemTTL: Long? = null): CR()
  @Serializable @SerialName("networkConfig") class NetworkConfig(val networkConfig: NetCfg): CR()
  @Serializable @SerialName("contactInfo") class ContactInfo(val contact: Contact, val connectionStats: ConnectionStats, val customUserProfile: Profile? = null): CR()
  @Serializable @SerialName("groupMemberInfo") class GroupMemberInfo(val groupInfo: GroupInfo, val member: GroupMember, val connectionStats_: ConnectionStats?): CR()
  @Serializable @SerialName("invitation") class Invitation(val connReqInvitation: String): CR()
  @Serializable @SerialName("sentConfirmation") class SentConfirmation: CR()
  @Serializable @SerialName("sentInvitation") class SentInvitation: CR()
  @Serializable @SerialName("contactAlreadyExists") class ContactAlreadyExists(val contact: Contact): CR()
  @Serializable @SerialName("contactDeleted") class ContactDeleted(val contact: Contact): CR()
  @Serializable @SerialName("chatCleared") class ChatCleared(val chatInfo: ChatInfo): CR()
  @Serializable @SerialName("userProfileNoChange") class UserProfileNoChange: CR()
  @Serializable @SerialName("userProfileUpdated") class UserProfileUpdated(val fromProfile: Profile, val toProfile: Profile): CR()
  @Serializable @SerialName("contactAliasUpdated") class ContactAliasUpdated(val toContact: Contact): CR()
  @Serializable @SerialName("connectionAliasUpdated") class ConnectionAliasUpdated(val toConnection: PendingContactConnection): CR()
  @Serializable @SerialName("contactPrefsUpdated") class ContactPrefsUpdated(val toContact: Contact): CR()
  @Serializable @SerialName("apiParsedMarkdown") class ParsedMarkdown(val formattedText: List<FormattedText>? = null): CR()
  @Serializable @SerialName("userContactLink") class UserContactLink(val contactLink: UserContactLinkRec): CR()
  @Serializable @SerialName("userContactLinkUpdated") class UserContactLinkUpdated(val contactLink: UserContactLinkRec): CR()
  @Serializable @SerialName("userContactLinkCreated") class UserContactLinkCreated(val connReqContact: String): CR()
  @Serializable @SerialName("userContactLinkDeleted") class UserContactLinkDeleted: CR()
  @Serializable @SerialName("contactConnected") class ContactConnected(val contact: Contact, val userCustomProfile: Profile? = null): CR()
  @Serializable @SerialName("contactConnecting") class ContactConnecting(val contact: Contact): CR()
  @Serializable @SerialName("receivedContactRequest") class ReceivedContactRequest(val contactRequest: UserContactRequest): CR()
  @Serializable @SerialName("acceptingContactRequest") class AcceptingContactRequest(val contact: Contact): CR()
  @Serializable @SerialName("contactRequestRejected") class ContactRequestRejected: CR()
  @Serializable @SerialName("contactUpdated") class ContactUpdated(val toContact: Contact): CR()
  @Serializable @SerialName("contactsSubscribed") class ContactsSubscribed(val server: String, val contactRefs: List<ContactRef>): CR()
  @Serializable @SerialName("contactsDisconnected") class ContactsDisconnected(val server: String, val contactRefs: List<ContactRef>): CR()
  @Serializable @SerialName("contactSubError") class ContactSubError(val contact: Contact, val chatError: ChatError): CR()
  @Serializable @SerialName("contactSubSummary") class ContactSubSummary(val contactSubscriptions: List<ContactSubStatus>): CR()
  @Serializable @SerialName("groupSubscribed") class GroupSubscribed(val group: GroupInfo): CR()
  @Serializable @SerialName("memberSubErrors") class MemberSubErrors(val memberSubErrors: List<MemberSubError>): CR()
  @Serializable @SerialName("groupEmpty") class GroupEmpty(val group: GroupInfo): CR()
  @Serializable @SerialName("userContactLinkSubscribed") class UserContactLinkSubscribed: CR()
  @Serializable @SerialName("newChatItem") class NewChatItem(val chatItem: AChatItem): CR()
  @Serializable @SerialName("chatItemStatusUpdated") class ChatItemStatusUpdated(val chatItem: AChatItem): CR()
  @Serializable @SerialName("chatItemUpdated") class ChatItemUpdated(val chatItem: AChatItem): CR()
  @Serializable @SerialName("chatItemDeleted") class ChatItemDeleted(val deletedChatItem: AChatItem, val toChatItem: AChatItem): CR()
  @Serializable @SerialName("contactsList") class ContactsList(val contacts: List<Contact>): CR()
  // group events
  @Serializable @SerialName("groupCreated") class GroupCreated(val groupInfo: GroupInfo): CR()
  @Serializable @SerialName("sentGroupInvitation") class SentGroupInvitation(val groupInfo: GroupInfo, val contact: Contact, val member: GroupMember): CR()
  @Serializable @SerialName("userAcceptedGroupSent") class UserAcceptedGroupSent (val groupInfo: GroupInfo, val hostContact: Contact? = null): CR()
  @Serializable @SerialName("userDeletedMember") class UserDeletedMember(val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("leftMemberUser") class LeftMemberUser(val groupInfo: GroupInfo): CR()
  @Serializable @SerialName("groupMembers") class GroupMembers(val group: Group): CR()
  @Serializable @SerialName("receivedGroupInvitation") class ReceivedGroupInvitation(val groupInfo: GroupInfo, val contact: Contact, val memberRole: GroupMemberRole): CR()
  @Serializable @SerialName("groupDeletedUser") class GroupDeletedUser(val groupInfo: GroupInfo): CR()
  @Serializable @SerialName("joinedGroupMemberConnecting") class JoinedGroupMemberConnecting(val groupInfo: GroupInfo, val hostMember: GroupMember, val member: GroupMember): CR()
  @Serializable @SerialName("memberRole") class MemberRole(val groupInfo: GroupInfo, val byMember: GroupMember, val member: GroupMember, val fromRole: GroupMemberRole, val toRole: GroupMemberRole): CR()
  @Serializable @SerialName("memberRoleUser") class MemberRoleUser(val groupInfo: GroupInfo, val member: GroupMember, val fromRole: GroupMemberRole, val toRole: GroupMemberRole): CR()
  @Serializable @SerialName("deletedMemberUser") class DeletedMemberUser(val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("deletedMember") class DeletedMember(val groupInfo: GroupInfo, val byMember: GroupMember, val deletedMember: GroupMember): CR()
  @Serializable @SerialName("leftMember") class LeftMember(val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("groupDeleted") class GroupDeleted(val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("contactsMerged") class ContactsMerged(val intoContact: Contact, val mergedContact: Contact): CR()
  @Serializable @SerialName("groupInvitation") class GroupInvitation(val groupInfo: GroupInfo): CR() // unused
  @Serializable @SerialName("userJoinedGroup") class UserJoinedGroup(val groupInfo: GroupInfo): CR()
  @Serializable @SerialName("joinedGroupMember") class JoinedGroupMember(val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("connectedToGroupMember") class ConnectedToGroupMember(val groupInfo: GroupInfo, val member: GroupMember): CR()
  @Serializable @SerialName("groupRemoved") class GroupRemoved(val groupInfo: GroupInfo): CR() // unused
  @Serializable @SerialName("groupUpdated") class GroupUpdated(val toGroup: GroupInfo): CR()
  @Serializable @SerialName("groupLinkCreated") class GroupLinkCreated(val groupInfo: GroupInfo, val connReqContact: String): CR()
  @Serializable @SerialName("groupLink") class GroupLink(val groupInfo: GroupInfo, val connReqContact: String): CR()
  @Serializable @SerialName("groupLinkDeleted") class GroupLinkDeleted(val groupInfo: GroupInfo): CR()
  // receiving file events
  @Serializable @SerialName("rcvFileAccepted") class RcvFileAccepted(val chatItem: AChatItem): CR()
  @Serializable @SerialName("rcvFileAcceptedSndCancelled") class RcvFileAcceptedSndCancelled(val rcvFileTransfer: RcvFileTransfer): CR()
  @Serializable @SerialName("rcvFileStart") class RcvFileStart(val chatItem: AChatItem): CR()
  @Serializable @SerialName("rcvFileComplete") class RcvFileComplete(val chatItem: AChatItem): CR()
  // sending file events
  @Serializable @SerialName("sndFileStart") class SndFileStart(val chatItem: AChatItem, val sndFileTransfer: SndFileTransfer): CR()
  @Serializable @SerialName("sndFileComplete") class SndFileComplete(val chatItem: AChatItem, val sndFileTransfer: SndFileTransfer): CR()
  @Serializable @SerialName("sndFileCancelled") class SndFileCancelled(val chatItem: AChatItem, val sndFileTransfer: SndFileTransfer): CR()
  @Serializable @SerialName("sndFileRcvCancelled") class SndFileRcvCancelled(val chatItem: AChatItem, val sndFileTransfer: SndFileTransfer): CR()
  @Serializable @SerialName("sndGroupFileCancelled") class SndGroupFileCancelled(val chatItem: AChatItem, val fileTransferMeta: FileTransferMeta, val sndFileTransfers: List<SndFileTransfer>): CR()
  @Serializable @SerialName("callInvitation") class CallInvitation(val callInvitation: RcvCallInvitation): CR()
  @Serializable @SerialName("callOffer") class CallOffer(val contact: Contact, val callType: CallType, val offer: WebRTCSession, val sharedKey: String? = null, val askConfirmation: Boolean): CR()
  @Serializable @SerialName("callAnswer") class CallAnswer(val contact: Contact, val answer: WebRTCSession): CR()
  @Serializable @SerialName("callExtraInfo") class CallExtraInfo(val contact: Contact, val extraInfo: WebRTCExtraInfo): CR()
  @Serializable @SerialName("callEnded") class CallEnded(val contact: Contact): CR()
  @Serializable @SerialName("newContactConnection") class NewContactConnection(val connection: PendingContactConnection): CR()
  @Serializable @SerialName("contactConnectionDeleted") class ContactConnectionDeleted(val connection: PendingContactConnection): CR()
  @Serializable @SerialName("cmdOk") class CmdOk: CR()
  @Serializable @SerialName("chatCmdError") class ChatCmdError(val chatError: ChatError): CR()
  @Serializable @SerialName("chatError") class ChatRespError(val chatError: ChatError): CR()
  @Serializable class Response(val type: String, val json: String): CR()
  @Serializable class Invalid(val str: String): CR()

  val responseType: String get() = when(this) {
    is ActiveUser -> "activeUser"
    is ChatStarted -> "chatStarted"
    is ChatRunning -> "chatRunning"
    is ChatStopped -> "chatStopped"
    is ApiChats -> "apiChats"
    is ApiChat -> "apiChat"
    is UserSMPServers -> "userSMPServers"
    is ChatItemTTL -> "chatItemTTL"
    is NetworkConfig -> "networkConfig"
    is ContactInfo -> "contactInfo"
    is GroupMemberInfo -> "groupMemberInfo"
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
    is ParsedMarkdown -> "apiParsedMarkdown"
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
    is CmdOk -> "cmdOk"
    is ChatCmdError -> "chatCmdError"
    is ChatRespError -> "chatError"
    is Response -> "* $type"
    is Invalid -> "* invalid json"
  }

  val details: String get() = when(this) {
    is ActiveUser -> json.encodeToString(user)
    is ChatStarted -> noDetails()
    is ChatRunning -> noDetails()
    is ChatStopped -> noDetails()
    is ApiChats -> json.encodeToString(chats)
    is ApiChat -> json.encodeToString(chat)
    is UserSMPServers -> json.encodeToString(smpServers)
    is ChatItemTTL -> json.encodeToString(chatItemTTL)
    is NetworkConfig -> json.encodeToString(networkConfig)
    is ContactInfo -> "contact: ${json.encodeToString(contact)}\nconnectionStats: ${json.encodeToString(connectionStats)}"
    is GroupMemberInfo -> "group: ${json.encodeToString(groupInfo)}\nmember: ${json.encodeToString(member)}\nconnectionStats: ${json.encodeToString(connectionStats_)}"
    is Invitation -> connReqInvitation
    is SentConfirmation -> noDetails()
    is SentInvitation -> noDetails()
    is ContactAlreadyExists -> json.encodeToString(contact)
    is ContactDeleted -> json.encodeToString(contact)
    is ChatCleared -> json.encodeToString(chatInfo)
    is UserProfileNoChange -> noDetails()
    is UserProfileUpdated -> json.encodeToString(toProfile)
    is ContactAliasUpdated -> json.encodeToString(toContact)
    is ConnectionAliasUpdated -> json.encodeToString(toConnection)
    is ContactPrefsUpdated -> json.encodeToString(toContact)
    is ParsedMarkdown -> json.encodeToString(formattedText)
    is UserContactLink -> contactLink.responseDetails
    is UserContactLinkUpdated -> contactLink.responseDetails
    is UserContactLinkCreated -> connReqContact
    is UserContactLinkDeleted -> noDetails()
    is ContactConnected -> json.encodeToString(contact)
    is ContactConnecting -> json.encodeToString(contact)
    is ReceivedContactRequest -> json.encodeToString(contactRequest)
    is AcceptingContactRequest -> json.encodeToString(contact)
    is ContactRequestRejected -> noDetails()
    is ContactUpdated -> json.encodeToString(toContact)
    is ContactsSubscribed -> "server: $server\ncontacts:\n${json.encodeToString(contactRefs)}"
    is ContactsDisconnected -> "server: $server\ncontacts:\n${json.encodeToString(contactRefs)}"
    is ContactSubError -> "error:\n${chatError.string}\ncontact:\n${json.encodeToString(contact)}"
    is ContactSubSummary -> json.encodeToString(contactSubscriptions)
    is GroupSubscribed -> json.encodeToString(group)
    is MemberSubErrors -> json.encodeToString(memberSubErrors)
    is GroupEmpty -> json.encodeToString(group)
    is UserContactLinkSubscribed -> noDetails()
    is NewChatItem -> json.encodeToString(chatItem)
    is ChatItemStatusUpdated -> json.encodeToString(chatItem)
    is ChatItemUpdated -> json.encodeToString(chatItem)
    is ChatItemDeleted -> "deletedChatItem:\n${json.encodeToString(deletedChatItem)}\ntoChatItem:\n${json.encodeToString(toChatItem)}"
    is ContactsList -> json.encodeToString(contacts)
    is GroupCreated -> json.encodeToString(groupInfo)
    is SentGroupInvitation -> "groupInfo: $groupInfo\ncontact: $contact\nmember: $member"
    is UserAcceptedGroupSent -> json.encodeToString(groupInfo)
    is UserDeletedMember -> "groupInfo: $groupInfo\nmember: $member"
    is LeftMemberUser -> json.encodeToString(groupInfo)
    is GroupMembers -> json.encodeToString(group)
    is ReceivedGroupInvitation -> "groupInfo: $groupInfo\ncontact: $contact\nmemberRole: $memberRole"
    is GroupDeletedUser -> json.encodeToString(groupInfo)
    is JoinedGroupMemberConnecting -> "groupInfo: $groupInfo\nhostMember: $hostMember\nmember: $member"
    is MemberRole -> "groupInfo: $groupInfo\nbyMember: $byMember\nmember: $member\nfromRole: $fromRole\ntoRole: $toRole"
    is MemberRoleUser -> "groupInfo: $groupInfo\nmember: $member\nfromRole: $fromRole\ntoRole: $toRole"
    is DeletedMemberUser -> "groupInfo: $groupInfo\nmember: $member"
    is DeletedMember -> "groupInfo: $groupInfo\nbyMember: $byMember\ndeletedMember: $deletedMember"
    is LeftMember -> "groupInfo: $groupInfo\nmember: $member"
    is GroupDeleted -> "groupInfo: $groupInfo\nmember: $member"
    is ContactsMerged -> "intoContact: $intoContact\nmergedContact: $mergedContact"
    is GroupInvitation -> json.encodeToString(groupInfo)
    is UserJoinedGroup -> json.encodeToString(groupInfo)
    is JoinedGroupMember -> "groupInfo: $groupInfo\nmember: $member"
    is ConnectedToGroupMember -> "groupInfo: $groupInfo\nmember: $member"
    is GroupRemoved -> json.encodeToString(groupInfo)
    is GroupUpdated -> json.encodeToString(toGroup)
    is GroupLinkCreated -> "groupInfo: $groupInfo\nconnReqContact: $connReqContact"
    is GroupLink -> "groupInfo: $groupInfo\nconnReqContact: $connReqContact"
    is GroupLinkDeleted -> json.encodeToString(groupInfo)
    is RcvFileAcceptedSndCancelled -> noDetails()
    is RcvFileAccepted -> json.encodeToString(chatItem)
    is RcvFileStart -> json.encodeToString(chatItem)
    is RcvFileComplete -> json.encodeToString(chatItem)
    is SndFileCancelled -> json.encodeToString(chatItem)
    is SndFileComplete -> json.encodeToString(chatItem)
    is SndFileRcvCancelled -> json.encodeToString(chatItem)
    is SndFileStart -> json.encodeToString(chatItem)
    is SndGroupFileCancelled -> json.encodeToString(chatItem)
    is CallInvitation -> "contact: ${callInvitation.contact.id}\ncallType: $callInvitation.callType\nsharedKey: ${callInvitation.sharedKey ?: ""}"
    is CallOffer -> "contact: ${contact.id}\ncallType: $callType\nsharedKey: ${sharedKey ?: ""}\naskConfirmation: $askConfirmation\noffer: ${json.encodeToString(offer)}"
    is CallAnswer -> "contact: ${contact.id}\nanswer: ${json.encodeToString(answer)}"
    is CallExtraInfo -> "contact: ${contact.id}\nextraInfo: ${json.encodeToString(extraInfo)}"
    is CallEnded -> "contact: ${contact.id}"
    is NewContactConnection -> json.encodeToString(connection)
    is ContactConnectionDeleted -> json.encodeToString(connection)
    is CmdOk -> noDetails()
    is ChatCmdError -> chatError.string
    is ChatRespError -> chatError.string
    is Response -> json
    is Invalid -> str
  }

  fun noDetails(): String ="${responseType}: " + generalGetString(R.string.no_details)
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
    is InvalidConnReq -> "invalidConnReq"
    is СommandError -> "commandError $message"
  }
  @Serializable @SerialName("noActiveUser") class NoActiveUser: ChatErrorType()
  @Serializable @SerialName("invalidConnReq") class InvalidConnReq: ChatErrorType()
  @Serializable @SerialName("commandError") class СommandError(val message: String): ChatErrorType()
}

@Serializable
sealed class StoreError {
  val string: String get() = when (this) {
    is UserContactLinkNotFound -> "userContactLinkNotFound"
    is GroupNotFound -> "groupNotFound"
  }
  @Serializable @SerialName("userContactLinkNotFound") class UserContactLinkNotFound: StoreError()
  @Serializable @SerialName("groupNotFound") class GroupNotFound: StoreError()
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
  @Serializable @SerialName("BROKER") class BROKER(val brokerErr: BrokerErrorType): AgentErrorType()
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
