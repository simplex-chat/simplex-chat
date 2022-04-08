package chat.simplex.app.model

import android.app.ActivityManager
import android.app.ActivityManager.RunningAppProcessInfo
import android.content.Context
import android.content.SharedPreferences
import android.util.Log
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Bolt
import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import chat.simplex.app.*
import chat.simplex.app.views.helpers.AlertManager
import chat.simplex.app.views.helpers.withApi
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import kotlinx.datetime.Clock
import kotlinx.datetime.Instant
import kotlinx.serialization.*
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import kotlin.concurrent.thread

typealias ChatCtrl = Long

open class ChatController(private val ctrl: ChatCtrl, private val ntfManager: NtfManager, val appContext: Context) {
  var chatModel = ChatModel(this)
  private val sharedPreferences: SharedPreferences  = appContext.getSharedPreferences(SHARED_PREFS_ID, Context.MODE_PRIVATE)

  init {
    chatModel.runServiceInBackground.value = getRunServiceInBackground()
  }

  suspend fun startChat(user: User) {
    Log.d(TAG, "user: $user")
    try {
      apiStartChat()
      chatModel.userAddress.value = apiGetUserAddress()
      chatModel.userSMPServers.value = getUserSMPServers()
      val chats = apiGetChats()
      chatModel.chats.clear()
      chatModel.chats.addAll(chats)
      chatModel.currentUser = mutableStateOf(user)
      chatModel.userCreated.value = true
      Log.d(TAG, "started chat")
    } catch(e: Error) {
      Log.e(TAG, "failed starting chat $e")
      throw e
    }
  }

  fun startReceiver() {
    Log.d(TAG, "ChatController startReceiver")
    thread(name="receiver") {
      withApi { recvMspLoop() }
    }
  }

  open fun isAppOnForeground(context: Context): Boolean {
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

  suspend fun sendCmd(cmd: CC): CR {
    return withContext(Dispatchers.IO) {
      val c = cmd.cmdString
      chatModel.terminalItems.add(TerminalItem.cmd(cmd))
      val json = chatSendCmd(ctrl, c)
      Log.d(TAG, "sendCmd: ${cmd.cmdType}")
      val r = APIResponse.decodeStr(json)
      Log.d(TAG, "sendCmd response type ${r.resp.responseType}")
      if (r.resp is CR.Response || r.resp is CR.Invalid) {
        Log.d(TAG, "sendCmd response json $json")
      }
      chatModel.terminalItems.add(TerminalItem.resp(r.resp))
      r.resp
    }
  }

  suspend fun recvMsg(): CR {
    return withContext(Dispatchers.IO) {
      val json = chatRecvMsg(ctrl)
      val r = APIResponse.decodeStr(json).resp
      Log.d(TAG, "chatRecvMsg: ${r.responseType}")
      if (r is CR.Response || r is CR.Invalid) Log.d(TAG, "chatRecvMsg json: $json")
      r
    }
  }

  suspend fun recvMspLoop() {
    processReceivedMsg(recvMsg())
    recvMspLoop()
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

  suspend fun apiStartChat() {
    val r = sendCmd(CC.StartChat())
    if (r is CR.ChatStarted || r is CR.ChatRunning) return
    throw Error("failed starting chat: ${r.responseType} ${r.details}")
  }

  suspend fun apiGetChats(): List<Chat> {
    val r = sendCmd(CC.ApiGetChats())
    if (r is CR.ApiChats ) return r.chats
    throw Error("failed getting the list of chats: ${r.responseType} ${r.details}")
  }

  suspend fun apiGetChat(type: ChatType, id: Long): Chat? {
    val r = sendCmd(CC.ApiGetChat(type, id))
    if (r is CR.ApiChat ) return r.chat
    Log.e(TAG, "apiGetChat bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiSendMessage(type: ChatType, id: Long, quotedItemId: Long? = null, mc: MsgContent): AChatItem? {
    val cmd = if (quotedItemId == null) CC.ApiSendMessage(type, id, mc)
              else CC.ApiSendMessageQuote(type, id, quotedItemId, mc)
    val r = sendCmd(cmd)
    if (r is CR.NewChatItem ) return r.chatItem
    Log.e(TAG, "apiSendMessage bad response: ${r.responseType} ${r.details}")
    return null
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

  suspend fun getUserSMPServers(): List<String>? {
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
          "Error saving SMP servers",
          "Make sure SMP server addresses are in correct format, line separated and are not duplicated."
        )
        false
      }
    }
  }

  suspend fun apiAddContact(): String? {
    val r = sendCmd(CC.AddContact())
    if (r is CR.Invitation) return r.connReqInvitation
    Log.e(TAG, "apiAddContact bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiConnect(connReq: String): Boolean  {
    val r = sendCmd(CC.Connect(connReq))
    when {
      r is CR.SentConfirmation || r is CR.SentInvitation -> return true
      r is CR.ContactAlreadyExists -> {
        AlertManager.shared.showAlertMsg("Contact already exists",
          "You are already connected to ${r.contact.displayName} via this link."
        )
        return false
      }
      r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorChat
          && r.chatError.errorType is ChatErrorType.InvalidConnReq -> {
        AlertManager.shared.showAlertMsg("Invalid connection link",
          "Please check that you used the correct link or ask your contact to send you another one."
        )
        return false
      }
      else -> {
        apiErrorAlert("apiConnect", "Connection error", r)
        return false
      }
    }
  }

  suspend fun apiDeleteChat(type: ChatType, id: Long): Boolean {
    val r = sendCmd(CC.ApiDeleteChat(type, id))
    when (r) {
      is CR.ContactDeleted -> return true // TODO groups
      is CR.ChatCmdError -> {
        val e = r.chatError
        if (e is ChatError.ChatErrorChat && e.errorType is ChatErrorType.ContactGroups) {
          AlertManager.shared.showAlertMsg(
            "Can't delete contact!",
            "Contact ${e.errorType.contact.displayName} cannot be deleted, it is a member of the group(s) ${e.errorType.groupNames}."
          )
        }
      }
      else -> apiErrorAlert("apiDeleteChat", "Error deleting ${type.chatTypeName}", r)
    }
    return false
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

  suspend fun apiCreateUserAddress(): String? {
    val r = sendCmd(CC.CreateMyAddress())
    if (r is CR.UserContactLinkCreated) return r.connReqContact
    Log.e(TAG, "apiCreateUserAddress bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiDeleteUserAddress(): Boolean {
    val r = sendCmd(CC.DeleteMyAddress())
    if (r is CR.UserContactLinkDeleted) return true
    Log.e(TAG, "apiDeleteUserAddress bad response: ${r.responseType} ${r.details}")
    return false
  }

  suspend fun apiGetUserAddress(): String? {
    val r = sendCmd(CC.ShowMyAddress())
    if (r is CR.UserContactLink) return r.connReqContact
    if (r is CR.ChatCmdError && r.chatError is ChatError.ChatErrorStore
      && r.chatError.storeError is StoreError.UserContactLinkNotFound) {
      return null
    }
    Log.e(TAG, "apiGetUserAddress bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiAcceptContactRequest(contactReqId: Long): Contact? {
    val r = sendCmd(CC.ApiAcceptContact(contactReqId))
    if (r is CR.AcceptingContactRequest) return r.contact
    Log.e(TAG, "apiAcceptContactRequest bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiRejectContactRequest(contactReqId: Long): Boolean {
    val r = sendCmd(CC.ApiRejectContact(contactReqId))
    if (r is CR.ContactRequestRejected) return true
    Log.e(TAG, "apiRejectContactRequest bad response: ${r.responseType} ${r.details}")
    return false
  }

  suspend fun apiChatRead(type: ChatType, id: Long, range: CC.ItemRange): Boolean {
    val r = sendCmd(CC.ApiChatRead(type, id, range))
    if (r is CR.CmdOk) return true
    Log.e(TAG, "apiChatRead bad response: ${r.responseType} ${r.details}")
    return false
  }

  fun apiErrorAlert(method: String, title: String, r: CR) {
    val errMsg = "${r.responseType}: ${r.details}"
    Log.e(TAG, "$method bad response: $errMsg")
    AlertManager.shared.showAlertMsg(title, errMsg)
  }

  fun processReceivedMsg(r: CR) {
    chatModel.terminalItems.add(TerminalItem.resp(r))
    when (r) {
      is CR.ContactConnected -> {
        chatModel.updateContact(r.contact)
        chatModel.updateNetworkStatus(r.contact, Chat.NetworkStatus.Connected())
//        NtfManager.shared.notifyContactConnected(contact)
      }
      is CR.ReceivedContactRequest -> {
        val contactRequest = r.contactRequest
        val cInfo = ChatInfo.ContactRequest(contactRequest)
        chatModel.addChat(Chat(chatInfo = cInfo, chatItems = listOf()))
//        NtfManager.shared.notifyContactRequest(contactRequest)
      }
      is CR.ContactUpdated -> {
        val cInfo = ChatInfo.Direct(r.toContact)
        if (chatModel.hasChat(r.toContact.id)) {
          chatModel.updateChatInfo(cInfo)
        }
      }
      is CR.ContactSubscribed -> processContactSubscribed(r.contact)
      is CR.ContactDisconnected -> {
        chatModel.updateContact(r.contact)
        chatModel.updateNetworkStatus(r.contact, Chat.NetworkStatus.Disconnected())
      }
      is CR.ContactSubError -> processContactSubError(r.contact, r.chatError)
      is CR.ContactSubSummary -> {
        for (sub in r.contactSubscriptions) {
          val err = sub.contactError
          if (err == null) processContactSubscribed(sub.contact)
          else processContactSubError(sub.contact, sub.contactError)
        }
      }
      is CR.NewChatItem -> {
        val cInfo = r.chatItem.chatInfo
        val cItem = r.chatItem.chatItem
        chatModel.addChatItem(cInfo, cItem)
        if (!isAppOnForeground(appContext) || chatModel.chatId.value != cInfo.id) {
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
      is CR.ChatItemUpdated -> {
        val cInfo = r.chatItem.chatInfo
        val cItem = r.chatItem.chatItem
        if (chatModel.upsertChatItem(cInfo, cItem)) {
          ntfManager.notifyMessageReceived(cInfo, cItem)
        }
      }
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
      else ->
        Log.d(TAG , "unsupported event: ${r.responseType}")
    }
  }

  fun processContactSubscribed(contact: Contact) {
    chatModel.updateContact(contact)
    chatModel.updateNetworkStatus(contact, Chat.NetworkStatus.Connected())
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
    chatModel.updateNetworkStatus(contact, Chat.NetworkStatus.Error(err))
  }

  fun showBackgroundServiceNotice() {
    if (!getBackgroundServiceNoticeShown()) {
      AlertManager.shared.showAlert {
        AlertDialog(
          onDismissRequest = AlertManager.shared::hideAlert,
          title = {
            Row {
              Icon(
                Icons.Outlined.Bolt,
                contentDescription = "Instant notifications",
              )
              Text("Private instant notifications!", fontWeight = FontWeight.Bold)
            }
          },
          text = {
            Column {
              Text(
                buildAnnotatedString {
                  append("To preserve your privacy, instead of push notifications the app has a ")
                  withStyle(SpanStyle(fontWeight = FontWeight.Medium)) {
                    append("SimpleX background service")
                  }
                  append(" – it uses a few percent of the battery per day.")
                },
                Modifier.padding(bottom = 8.dp)
              )
              Text(
                buildAnnotatedString {
                  withStyle(SpanStyle(fontWeight = FontWeight.Medium)) {
                    append("It can be disabled via settings")
                  }
                  append(" – notifications will still be shown while the app is running.")
                }
              )
            }
          },
          confirmButton = {
            Button(onClick = AlertManager.shared::hideAlert) { Text("Ok") }
          }
        )
      }
      setBackgroundServiceNoticeShown()
    }
  }

  fun getAutoRestartWorkerVersion(): Int = sharedPreferences.getInt(SHARED_PREFS_AUTO_RESTART_WORKER_VERSION, 0)

  fun setAutoRestartWorkerVersion(version: Int) =
    sharedPreferences.edit()
      .putInt(SHARED_PREFS_AUTO_RESTART_WORKER_VERSION, version)
      .apply()

  fun getRunServiceInBackground(): Boolean = sharedPreferences.getBoolean(SHARED_PREFS_RUN_SERVICE_IN_BACKGROUND, true)

  fun setRunServiceInBackground(runService: Boolean) =
    sharedPreferences.edit()
      .putBoolean(SHARED_PREFS_RUN_SERVICE_IN_BACKGROUND, runService)
      .apply()

  fun getBackgroundServiceNoticeShown(): Boolean = sharedPreferences.getBoolean(SHARED_PREFS_SERVICE_NOTICE_SHOWN, false)

  fun setBackgroundServiceNoticeShown() =
    sharedPreferences.edit()
      .putBoolean(SHARED_PREFS_SERVICE_NOTICE_SHOWN, true)
      .apply()

  companion object {
    private const val SHARED_PREFS_ID = "chat.simplex.app.SIMPLEX_APP_PREFS"
    private const val SHARED_PREFS_AUTO_RESTART_WORKER_VERSION = "AutoRestartWorkerVersion"
    private const val SHARED_PREFS_RUN_SERVICE_IN_BACKGROUND = "RunServiceInBackground"
    private const val SHARED_PREFS_SERVICE_NOTICE_SHOWN = "BackgroundServiceNoticeShown"
  }
}

// ChatCommand
sealed class CC {
  class Console(val cmd: String): CC()
  class ShowActiveUser: CC()
  class CreateActiveUser(val profile: Profile): CC()
  class StartChat: CC()
  class ApiGetChats: CC()
  class ApiGetChat(val type: ChatType, val id: Long): CC()
  class ApiSendMessage(val type: ChatType, val id: Long, val mc: MsgContent): CC()
  class ApiSendMessageQuote(val type: ChatType, val id: Long, val itemId: Long, val mc: MsgContent): CC()
  class ApiUpdateChatItem(val type: ChatType, val id: Long, val itemId: Long, val mc: MsgContent): CC()
  class ApiDeleteChatItem(val type: ChatType, val id: Long, val itemId: Long, val mode: CIDeleteMode): CC()
  class GetUserSMPServers(): CC()
  class SetUserSMPServers(val smpServers: List<String>): CC()
  class AddContact: CC()
  class Connect(val connReq: String): CC()
  class ApiDeleteChat(val type: ChatType, val id: Long): CC()
  class ApiUpdateProfile(val profile: Profile): CC()
  class ApiParseMarkdown(val text: String): CC()
  class CreateMyAddress: CC()
  class DeleteMyAddress: CC()
  class ShowMyAddress: CC()
  class ApiAcceptContact(val contactReqId: Long): CC()
  class ApiRejectContact(val contactReqId: Long): CC()
  class ApiChatRead(val type: ChatType, val id: Long, val range: ItemRange): CC()

  val cmdString: String get() = when (this) {
    is Console -> cmd
    is ShowActiveUser -> "/u"
    is CreateActiveUser -> "/u ${profile.displayName} ${profile.fullName}"
    is StartChat -> "/_start"
    is ApiGetChats -> "/_get chats"
    is ApiGetChat -> "/_get chat ${chatRef(type, id)} count=100"
    is ApiSendMessage -> "/_send ${chatRef(type, id)} ${mc.cmdString}"
    is ApiSendMessageQuote -> "/_send_quote ${chatRef(type, id)} $itemId ${mc.cmdString}"
    is ApiUpdateChatItem -> "/_update item ${chatRef(type, id)} $itemId ${mc.cmdString}"
    is ApiDeleteChatItem -> "/_delete item ${chatRef(type, id)} $itemId ${mode.deleteMode}"
    is GetUserSMPServers -> "/smp_servers"
    is SetUserSMPServers -> "/smp_servers ${smpServersStr(smpServers)}"
    is AddContact -> "/connect"
    is Connect -> "/connect $connReq"
    is ApiDeleteChat -> "/_delete ${chatRef(type, id)}"
    is ApiUpdateProfile -> "/_profile ${json.encodeToString(profile)}"
    is ApiParseMarkdown -> "/_parse $text"
    is CreateMyAddress -> "/address"
    is DeleteMyAddress -> "/delete_address"
    is ShowMyAddress -> "/show_address"
    is ApiAcceptContact -> "/_accept $contactReqId"
    is ApiRejectContact -> "/_reject $contactReqId"
    is ApiChatRead -> "/_read chat ${chatRef(type, id)} from=${range.from} to=${range.to}"
  }

  val cmdType: String get() = when (this) {
    is Console -> "console command"
    is ShowActiveUser -> "showActiveUser"
    is CreateActiveUser -> "createActiveUser"
    is StartChat -> "startChat"
    is ApiGetChats -> "apiGetChats"
    is ApiGetChat -> "apiGetChat"
    is ApiSendMessage -> "apiSendMessage"
    is ApiSendMessageQuote -> "apiSendMessageQuote"
    is ApiUpdateChatItem -> "apiUpdateChatItem"
    is ApiDeleteChatItem -> "apiDeleteChatItem"
    is GetUserSMPServers -> "getUserSMPServers"
    is SetUserSMPServers -> "setUserSMPServers"
    is AddContact -> "addContact"
    is Connect -> "connect"
    is ApiDeleteChat -> "apiDeleteChat"
    is ApiUpdateProfile -> "updateProfile"
    is ApiParseMarkdown -> "apiParseMarkdown"
    is CreateMyAddress -> "createMyAddress"
    is DeleteMyAddress -> "deleteMyAddress"
    is ShowMyAddress -> "showMyAddress"
    is ApiAcceptContact -> "apiAcceptContact"
    is ApiRejectContact -> "apiRejectContact"
    is ApiChatRead -> "apiChatRead"
  }

  class ItemRange(val from: Long, val to: Long)

  companion object {
    fun chatRef(chatType: ChatType, id: Long) = "${chatType.type}${id}"

    fun smpServersStr(smpServers: List<String>) = if (smpServers.isEmpty()) "default" else smpServers.joinToString(separator = ",")
  }
}

val json = Json {
  prettyPrint = true
  ignoreUnknownKeys = true
}

@Serializable
class APIResponse(val resp: CR, val corr: String? = null) {
  companion object {
    fun decodeStr(str: String): APIResponse {
      return try {
        json.decodeFromString(str)
      } catch(e: Exception) {
        try {
          Log.d(TAG, e.localizedMessage)
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
  @Serializable @SerialName("apiChats") class ApiChats(val chats: List<Chat>): CR()
  @Serializable @SerialName("apiChat") class ApiChat(val chat: Chat): CR()
  @Serializable @SerialName("userSMPServers") class UserSMPServers(val smpServers: List<String>): CR()
  @Serializable @SerialName("invitation") class Invitation(val connReqInvitation: String): CR()
  @Serializable @SerialName("sentConfirmation") class SentConfirmation: CR()
  @Serializable @SerialName("sentInvitation") class SentInvitation: CR()
  @Serializable @SerialName("contactAlreadyExists") class ContactAlreadyExists(val contact: Contact): CR()
  @Serializable @SerialName("contactDeleted") class ContactDeleted(val contact: Contact): CR()
  @Serializable @SerialName("userProfileNoChange") class UserProfileNoChange: CR()
  @Serializable @SerialName("userProfileUpdated") class UserProfileUpdated(val fromProfile: Profile, val toProfile: Profile): CR()
  @Serializable @SerialName("parsedMarkdown") class ParsedMarkdown(val formattedText: List<FormattedText>?): CR()
  @Serializable @SerialName("userContactLink") class UserContactLink(val connReqContact: String): CR()
  @Serializable @SerialName("userContactLinkCreated") class UserContactLinkCreated(val connReqContact: String): CR()
  @Serializable @SerialName("userContactLinkDeleted") class UserContactLinkDeleted: CR()
  @Serializable @SerialName("contactConnected") class ContactConnected(val contact: Contact): CR()
  @Serializable @SerialName("receivedContactRequest") class ReceivedContactRequest(val contactRequest: UserContactRequest): CR()
  @Serializable @SerialName("acceptingContactRequest") class AcceptingContactRequest(val contact: Contact): CR()
  @Serializable @SerialName("contactRequestRejected") class ContactRequestRejected: CR()
  @Serializable @SerialName("contactUpdated") class ContactUpdated(val toContact: Contact): CR()
  @Serializable @SerialName("contactSubscribed") class ContactSubscribed(val contact: Contact): CR()
  @Serializable @SerialName("contactDisconnected") class ContactDisconnected(val contact: Contact): CR()
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
  @Serializable @SerialName("cmdOk") class CmdOk: CR()
  @Serializable @SerialName("chatCmdError") class ChatCmdError(val chatError: ChatError): CR()
  @Serializable @SerialName("chatError") class ChatRespError(val chatError: ChatError): CR()
  @Serializable class Response(val type: String, val json: String): CR()
  @Serializable class Invalid(val str: String): CR()

  val responseType: String get() = when(this) {
    is ActiveUser -> "activeUser"
    is ChatStarted -> "chatStarted"
    is ChatRunning -> "chatRunning"
    is ApiChats -> "apiChats"
    is ApiChat -> "apiChat"
    is UserSMPServers -> "userSMPServers"
    is Invitation -> "invitation"
    is SentConfirmation -> "sentConfirmation"
    is SentInvitation -> "sentInvitation"
    is ContactAlreadyExists -> "contactAlreadyExists"
    is ContactDeleted -> "contactDeleted"
    is UserProfileNoChange -> "userProfileNoChange"
    is UserProfileUpdated -> "userProfileUpdated"
    is ParsedMarkdown -> "parsedMarkdown"
    is UserContactLink -> "userContactLink"
    is UserContactLinkCreated -> "userContactLinkCreated"
    is UserContactLinkDeleted -> "userContactLinkDeleted"
    is ContactConnected -> "contactConnected"
    is ReceivedContactRequest -> "receivedContactRequest"
    is AcceptingContactRequest -> "acceptingContactRequest"
    is ContactRequestRejected -> "contactRequestRejected"
    is ContactUpdated -> "contactUpdated"
    is ContactSubscribed -> "contactSubscribed"
    is ContactDisconnected -> "contactDisconnected"
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
    is ApiChats -> json.encodeToString(chats)
    is ApiChat -> json.encodeToString(chat)
    is UserSMPServers -> json.encodeToString(smpServers)
    is Invitation -> connReqInvitation
    is SentConfirmation -> noDetails()
    is SentInvitation -> noDetails()
    is ContactAlreadyExists -> json.encodeToString(contact)
    is ContactDeleted -> json.encodeToString(contact)
    is UserProfileNoChange -> noDetails()
    is UserProfileUpdated -> json.encodeToString(toProfile)
    is ParsedMarkdown -> json.encodeToString(formattedText)
    is UserContactLink -> connReqContact
    is UserContactLinkCreated -> connReqContact
    is UserContactLinkDeleted -> noDetails()
    is ContactConnected -> json.encodeToString(contact)
    is ReceivedContactRequest -> json.encodeToString(contactRequest)
    is AcceptingContactRequest -> json.encodeToString(contact)
    is ContactRequestRejected -> noDetails()
    is ContactUpdated -> json.encodeToString(toContact)
    is ContactSubscribed -> json.encodeToString(contact)
    is ContactDisconnected -> json.encodeToString(contact)
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
    is CmdOk -> noDetails()
    is ChatCmdError -> chatError.string
    is ChatRespError -> chatError.string
    is Response -> json
    is Invalid -> str
  }

  fun noDetails(): String ="${responseType}: no details"
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
sealed class ChatError {
  val string: String get() = when (this) {
    is ChatErrorChat -> "chat ${errorType.string}"
    is ChatErrorAgent -> "agent ${agentError.string}"
    is ChatErrorStore -> "store ${storeError.string}"
  }
  @Serializable @SerialName("error") class ChatErrorChat(val errorType: ChatErrorType): ChatError()
  @Serializable @SerialName("errorAgent") class ChatErrorAgent(val agentError: AgentErrorType): ChatError()
  @Serializable @SerialName("errorStore") class ChatErrorStore(val storeError: StoreError): ChatError()
}

@Serializable
sealed class ChatErrorType {
  val string: String get() = when (this) {
    is InvalidConnReq -> "invalidConnReq"
    is ContactGroups -> "groupNames $groupNames"
  }
  @Serializable @SerialName("invalidConnReq") class InvalidConnReq: ChatErrorType()
  @Serializable @SerialName("contactGroups") class ContactGroups(val contact: Contact, val groupNames: List<String>): ChatErrorType()
}

@Serializable
sealed class StoreError {
  val string: String get() = when (this) {
    is UserContactLinkNotFound -> "userContactLinkNotFound"
  }
  @Serializable @SerialName("userContactLinkNotFound") class UserContactLinkNotFound: StoreError()
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
