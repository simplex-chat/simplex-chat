package chat.simplex.app.model

import android.util.Log
import androidx.compose.runtime.mutableStateOf
import chat.simplex.app.chatRecvMsg
import chat.simplex.app.chatSendCmd
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import kotlinx.serialization.*
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import java.lang.Exception
import java.util.*
import kotlin.concurrent.thread

typealias ChatCtrl = Long

open class ChatController(val ctrl: ChatCtrl) {
  var chatModel = ChatModel(this)

  suspend fun startChat(u: User) {
    chatModel.currentUser = mutableStateOf(u)
    Log.d("SIMPLEX (user)", u.toString())
    try {
      apiStartChat()
      chatModel.chats.addAll(apiGetChats())
      startReceiver()
      Log.d("SIMPLEX", "started chat")
    } catch(e: Error) {
      Log.d("SIMPLEX", "failed starting chat $e")
      throw e
    }
  }

  fun startReceiver() {
    thread(name="receiver") {
//            val chatlog = FifoQueue<String>(500)
      while(true) {
        val json = chatRecvMsg(ctrl)
        val r = APIResponse.decodeStr(json).resp
        Log.d("SIMPLEX", "chatRecvMsg: ${r.responseType}")
        if (r is CR.Response || r is CR.Invalid) Log.d("SIMPLEX", "chatRecvMsg json: $json")
        processReceivedMsg(r)
      }
    }
  }

  suspend fun sendCmd(cmd: CC): CR {
    return withContext(Dispatchers.IO) {
      val c = cmd.cmdString
      chatModel.terminalItems.add(TerminalItem.cmd(cmd))
      val json = chatSendCmd(ctrl, c)
      Log.d("SIMPLEX", "sendCmd: ${cmd.cmdType}")
      val r = APIResponse.decodeStr(json)
      Log.d("SIMPLEX", "sendCmd response type ${r.resp.responseType}")
      if (r.resp is CR.Response || r.resp is CR.Invalid) {
        Log.d("SIMPLEX", "sendCmd response json $json")
      }
      chatModel.terminalItems.add(TerminalItem.resp(r.resp))
      r.resp
    }
  }

  suspend fun apiGetActiveUser(): User? {
    val r = sendCmd(CC.ShowActiveUser())
    if (r is CR.ActiveUser) return r.user
    Log.d("SIMPLEX", "apiGetActiveUser: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiCreateActiveUser(p: Profile): User {
    val r = sendCmd(CC.CreateActiveUser(p))
    if (r is CR.ActiveUser) return r.user
    Log.d("SIMPLEX", "apiCreateActiveUser: ${r.responseType} ${r.details}")
    throw Error("user not created ${r.responseType} ${r.details}")
  }

  suspend fun apiStartChat() {
    val r = sendCmd(CC.StartChat())
    if (r is CR.ChatStarted ) return
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
    Log.d("SIMPLEX", "apiGetChat bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiSendMessage(type: ChatType, id: Long, mc: MsgContent): AChatItem? {
    val r = sendCmd(CC.ApiSendMessage(type, id, mc))
    if (r is CR.NewChatItem ) return r.chatItem
    Log.d("SIMPLEX", "apiSendMessage bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiAddContact(): String? {
    val r = sendCmd(CC.AddContact())
    if (r is CR.Invitation) return r.connReqInvitation
    Log.d("SIMPLEX", "apiAddContact bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiConnect(connReq: String): Boolean  {
    val r = sendCmd(CC.Connect(connReq))
    if (r is CR.SentConfirmation || r is CR.SentInvitation) return true
    Log.d("SIMPLEX", "apiConnect bad response: ${r.responseType} ${r.details}")
    return false
  }

  suspend fun apiDeleteChat(type: ChatType, id: Long): Boolean {
    val r = sendCmd(CC.ApiDeleteChat(type, id))
    if (r is CR.ContactDeleted) return true // TODO groups
    Log.d("SIMPLEX", "apiDeleteChat bad response: ${r.responseType} ${r.details}")
    return false
  }

  suspend fun apiUpdateProfile(profile: Profile): Profile? {
    val r = sendCmd(CC.UpdateProfile(profile))
    if (r is CR.UserProfileNoChange) return profile
    if (r is CR.UserProfileUpdated) return r.toProfile
    Log.d("SIMPLEX", "apiUpdateProfile bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiCreateUserAddress(): String? {
    val r = sendCmd(CC.CreateMyAddress())
    if (r is CR.UserContactLinkCreated) return r.connReqContact
    Log.d("SIMPLEX", "apiCreateUserAddress bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiDeleteUserAddress(): Boolean {
    val r = sendCmd(CC.DeleteMyAddress())
    if (r is CR.UserContactLinkDeleted) return true
    Log.d("SIMPLEX", "apiDeleteUserAddress bad response: ${r.responseType} ${r.details}")
    return false
  }

  suspend fun apiGetUserAddress(): String? {
    val r = sendCmd(CC.ShowMyAddress())
    if (r is CR.UserContactLink) return r.connReqContact
    if (r is CR.ChatCmdError && r.chatError is ChatError.ErrorStore
      && r.chatError.storeError is StoreError.UserContactLinkNotFound) {
      return null
    }
    Log.d("SIMPLEX", "apiGetUserAddress bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiAcceptContactRequest(contactReqId: Long): Contact? {
    val r = sendCmd(CC.ApiAcceptContact(contactReqId))
    if (r is CR.AcceptingContactRequest) return r.contact
    Log.d("SIMPLEX", "apiAcceptContactRequest bad response: ${r.responseType} ${r.details}")
    return null
  }

  suspend fun apiRejectContactRequest(contactReqId: Long): Boolean {
    val r = sendCmd(CC.ApiRejectContact(contactReqId))
    if (r is CR.ContactRequestRejected) return true
    Log.d("SIMPLEX", "apiRejectContactRequest bad response: ${r.responseType} ${r.details}")
    return false
  }

  suspend fun apiChatRead(type: ChatType, id: Long, range: CC.ItemRange): Boolean {
    val r = sendCmd(CC.ApiChatRead(type, id, range))
    if (r is CR.CmdOk) return true
    Log.d("SIMPLEX", "apiChatRead bad response: ${r.responseType} ${r.details}")
    return false
  }

  fun processReceivedMsg(r: CR) {
    chatModel.terminalItems.add(TerminalItem.resp(r))
    when {
      r is CR.ContactConnected -> chatModel.updateContact(r.contact)
//      r is CR.UpdateNetworkStatus -> return
//      r is CR.ReceivedContactRequest -> return
//      r is CR.ContactUpdated -> return
//      r is CR.ContactSubscribed -> return
//      r is CR.ContactSubError -> return
//      r is CR.UpdateContact -> return
      r is CR.NewChatItem -> {
        val cInfo = r.chatItem.chatInfo
        val cItem = r.chatItem.chatItem
        chatModel.addChatItem(cInfo, cItem)
      }
//        NtfManager.shared.notifyMessageReceived(cInfo, cItem)

//      switch res {
//        chatModel.updateNetworkStatus(contact, .connected)
//          NtfManager.shared.notifyContactConnected(contact)
//        case let .receivedContactRequest(contactRequest):
  //        chatModel.addChat(Chat(
  //          chatInfo: ChatInfo.contactRequest(contactRequest: contactRequest),
  //        chatItems: []
  //        ))
//          NtfManager.shared.notifyContactRequest(contactRequest)
//        case let .contactUpdated(toContact):
  //        let cInfo = ChatInfo.direct(contact: toContact)
  //        if chatModel.hasChat(toContact.id) {
  //          chatModel.updateChatInfo(cInfo)
  //        }
//        case let .contactSubscribed(contact):
  //        chatModel.updateContact(contact)
  //        chatModel.updateNetworkStatus(contact, .connected)
  //        case let .contactDisconnected(contact):
  //        chatModel.updateContact(contact)
  //        chatModel.updateNetworkStatus(contact, .disconnected)
//        case let .contactSubError(contact, chatError):
//        chatModel.updateContact(contact)
////        var err: String
////        switch chatError {
////          case .errorAgent(agentError: .BROKER(brokerErr: .NETWORK)): err = "network"
////          case .errorAgent(agentError: .SMP(smpErr: .AUTH)): err = "contact deleted"
////          default: err = String(describing: chatError)
////        }
////        chatModel.updateNetworkStatus(contact, .error(err))
//        case let .newChatItem(aChatItem):
  //        let cInfo = aChatItem.chatInfo
  //            let cItem = aChatItem.chatItem
  //            chatModel.addChatItem(cInfo, cItem)
  //        NtfManager.shared.notifyMessageReceived(cInfo, cItem)
//        case let .chatItemUpdated(aChatItem):
  //        let cInfo = aChatItem.chatInfo
  //            let cItem = aChatItem.chatItem
  //            if chatModel.upsertChatItem(cInfo, cItem) {
  //              NtfManager.shared.notifyMessageReceived(cInfo, cItem)
  //            }
//        default:
//        logger.debug("unsupported event: \(res.responseType)")
//      }
    }
  }

  class Mock: ChatController(0) {}
}

// ChatCommand
abstract class CC {
  abstract val cmdString: String
  abstract val cmdType: String

  class Console(val cmd: String): CC() {
    override val cmdString get() = cmd
    override val cmdType get() = "console command"
  }

  class ShowActiveUser: CC() {
    override val cmdString get() = "/u"
    override val cmdType get() = "showActiveUser"
  }

  class CreateActiveUser(val profile: Profile): CC() {
    override val cmdString get() = "/u ${profile.displayName} ${profile.fullName}"
    override val cmdType get() = "createActiveUser"
  }

  class StartChat: CC() {
    override val cmdString get() = "/_start"
    override val cmdType get() = "startChat"
  }

  class ApiGetChats: CC() {
    override val cmdString get() = "/_get chats"
    override val cmdType get() = "apiGetChats"
  }

  class ApiGetChat(val type: ChatType, val id: Long): CC() {
    override val cmdString get() = "/_get chat ${chatRef(type, id)} count=100"
    override val cmdType get() = "apiGetChat"
  }

  class ApiSendMessage(val type: ChatType, val id: Long, val mc: MsgContent): CC() {
    override val cmdString get() = "/_send ${chatRef(type, id)} ${mc.cmdString}"
    override val cmdType get() = "apiGetChat"
  }

  class AddContact: CC() {
    override val cmdString get() = "/connect"
    override val cmdType get() = "addContact"
  }

  class Connect(val connReq: String): CC() {
    override val cmdString get() = "/connect $connReq"
    override val cmdType get() = "connect"
  }

  class ApiDeleteChat(val type: ChatType, val id: Long): CC() {
    override val cmdString get() = "/_delete ${chatRef(type, id)}"
    override val cmdType get() = "apiDeleteChat"
  }

  class UpdateProfile(val profile: Profile): CC() {
    override val cmdString get() = "/profile ${profile.displayName} ${profile.fullName}"
    override val cmdType get() = "updateProfile"
  }

  class CreateMyAddress: CC() {
    override val cmdString get() = "/address"
    override val cmdType get() = "createMyAddress"
  }

  class DeleteMyAddress: CC() {
    override val cmdString get() = "/delete_address"
    override val cmdType get() = "deleteMyAddress"
  }

  class ShowMyAddress: CC() {
    override val cmdString get() = "/show_address"
    override val cmdType get() = "showMyAddress"
  }

  class ApiAcceptContact(val contactReqId: Long): CC() {
    override val cmdString get() = "/_accept $contactReqId"
    override val cmdType get() = "apiAcceptContact"
  }

  class ApiRejectContact(val contactReqId: Long): CC() {
    override val cmdString get() = "/_reject $contactReqId"
    override val cmdType get() = "apiRejectContact"
  }

  class ApiChatRead(val type: ChatType, val id: Long, val range: ItemRange): CC() {
    override val cmdString get() = "/_read chat ${chatRef(type, id)} from=${range.from} to=${range.to}"
    override val cmdType get() = "apiDeleteChat"
  }

  data class ItemRange(val from: Long, val to: Long)

  companion object {
    fun chatRef(chatType: ChatType, id: Long) = "${chatType.type}${id}"
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
      try {
        return json.decodeFromString(str)
      } catch(e: Exception) {
        try {
          val data = json.parseToJsonElement(str).jsonObject
          return APIResponse(
            resp = CR.Response(data["resp"]!!.jsonObject["type"]?.toString() ?: "invalid", json.encodeToString(data)),
            corr = data["corr"]?.toString()
          )
        } catch(e: Exception) {
          return APIResponse(CR.Invalid(str))
        }
      }
    }
  }
}

// ChatResponse
@Serializable
sealed class CR {
  abstract val responseType: String
  abstract val details: String

  @Serializable @SerialName("activeUser")
  class ActiveUser(val user: User): CR() {
    override val responseType get() = "activeUser"
    override val details get() = user.toString()
  }

  @Serializable @SerialName("chatStarted")
  class ChatStarted: CR() {
    override val responseType get() = "chatStarted"
    override val details get() = noDetails()
  }

  @Serializable @SerialName("apiChats")
  class ApiChats(val chats: List<Chat>): CR() {
    override val responseType get() = "apiChats"
    override val details get() = chats.toString()
  }

  @Serializable @SerialName("apiChat")
  class ApiChat(val chat: Chat): CR() {
    override val responseType get() = "apiChats"
    override val details get() = chat.toString()
  }

  @Serializable @SerialName("invitation")
  class Invitation(val connReqInvitation: String): CR() {
    override val responseType get() = "invitation"
    override val details get() = connReqInvitation
  }

  @Serializable @SerialName("sentConfirmation")
  class SentConfirmation: CR() {
    override val responseType get() = "sentConfirmation"
    override val details get() = noDetails()
  }

  @Serializable @SerialName("sentInvitation")
  class SentInvitation: CR() {
    override val responseType get() = "sentInvitation"
    override val details get() = noDetails()
  }

  @Serializable @SerialName("contactDeleted")
  class ContactDeleted(val contact: Contact): CR() {
    override val responseType get() = "contactDeleted"
    override val details get() = contact.toString()
  }

  @Serializable @SerialName("userProfileNoChange")
  class UserProfileNoChange: CR() {
    override val responseType get() = "userProfileNoChange"
    override val details get() = noDetails()
  }

  @Serializable @SerialName("userProfileUpdated")
  class UserProfileUpdated(val fromProfile: Profile, val toProfile: Profile): CR() {
    override val responseType get() = "userProfileUpdated"
    override val details get() = toProfile.toString()
  }

  @Serializable @SerialName("userContactLink")
  class UserContactLink(val connReqContact: String): CR() {
    override val responseType get() = "userContactLink"
    override val details get() = connReqContact
  }

  @Serializable @SerialName("userContactLinkCreated")
  class UserContactLinkCreated(val connReqContact: String): CR() {
    override val responseType get() = "userContactLinkCreated"
    override val details get() = connReqContact
  }

  @Serializable @SerialName("userContactLinkDeleted")
  class UserContactLinkDeleted: CR() {
    override val responseType get() = "userContactLinkDeleted"
    override val details get() = noDetails()
  }

  @Serializable @SerialName("contactConnected")
  class ContactConnected(val contact: Contact): CR() {
    override val responseType get() = "contactConnected"
    override val details get() = contact.toString()
  }

  @Serializable @SerialName("receivedContactRequest")
  class ReceivedContactRequest(val contactRequest: UserContactRequest): CR() {
    override val responseType get() = "receivedContactRequest"
    override val details get() = contactRequest.toString()
  }

  @Serializable @SerialName("acceptingContactRequest")
  class AcceptingContactRequest(val contact: Contact): CR() {
    override val responseType get() = "acceptingContactRequest"
    override val details get() = contact.toString()
  }

  @Serializable @SerialName("contactRequestRejected")
  class ContactRequestRejected: CR() {
    override val responseType get() = "contactRequestRejected"
    override val details get() = noDetails()
  }

  @Serializable @SerialName("contactUpdated")
  class ContactUpdated(val toContact: Contact): CR() {
    override val responseType get() = "contactUpdated"
    override val details get() = toContact.toString()
  }

  @Serializable @SerialName("contactSubscribed")
  class ContactSubscribed(val contact: Contact): CR() {
    override val responseType get() = "contactSubscribed"
    override val details get() = contact.toString()
  }

  @Serializable @SerialName("contactDisconnected")
  class ContactDisconnected(val contact: Contact): CR() {
    override val responseType get() = "contactDisconnected"
    override val details get() = contact.toString()
  }

  @Serializable @SerialName("contactSubError")
  class ContactSubError(val contact: Contact, val chatError: ChatError): CR() {
    override val responseType get() = "contactSubError"
    override val details get() = "contact:\n${contact.toString()}\nerror:\n${chatError.toString()}"
  }

  @Serializable @SerialName("groupSubscribed")
  class GroupSubscribed(val group: GroupInfo): CR() {
    override val responseType get() = "groupSubscribed"
    override val details get() = group.toString()
  }

  @Serializable @SerialName("groupEmpty")
  class GroupEmpty(val group: GroupInfo): CR() {
    override val responseType get() = "groupEmpty"
    override val details get() = group.toString()
  }

  @Serializable @SerialName("userContactLinkSubscribed")
  class UserContactLinkSubscribed: CR() {
    override val responseType get() = "userContactLinkSubscribed"
    override val details get() = noDetails()
  }

  @Serializable @SerialName("newChatItem")
  class NewChatItem(val chatItem: AChatItem): CR() {
    override val responseType get() = "newChatItem"
    override val details get() = chatItem.toString()
  }

  @Serializable @SerialName("chatItemUpdated")
  class ChatItemUpdated(val chatItem: AChatItem): CR() {
    override val responseType get() = "chatItemUpdated"
    override val details get() = chatItem.toString()
  }

  @Serializable @SerialName("cmdOk")
  class CmdOk: CR() {
    override val responseType get() = "cmdOk"
    override val details get() = noDetails()
  }

  @Serializable @SerialName("chatCmdError")
  class ChatCmdError(val chatError: ChatError): CR() {
    override val responseType get() = "chatCmdError"
    override val details get() = chatError.toString()
  }

  @Serializable @SerialName("chatError")
  class ChatRespError(val chatError: ChatError): CR() {
    override val responseType get() = "chatError"
    override val details get() = chatError.toString()
  }

  @Serializable
  class Response(val type: String, val json: String): CR() {
    override val responseType get() = "* $type"
    override val details get() = json
  }

  @Serializable
  class Invalid(val str: String): CR() {
    override val responseType get() = "* invalid json"
    override val details get() = str
  }

  fun noDetails(): String ="${responseType}: no details"
}

abstract class TerminalItem {
  abstract val id: Long
  val date = Date()
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
  @Serializable @SerialName("errorStore")
  class ErrorStore(val storeError: StoreError): ChatError()
}

@Serializable
sealed class StoreError {
  @Serializable @SerialName("userContactLinkNotFound")
  class UserContactLinkNotFound: StoreError()
}