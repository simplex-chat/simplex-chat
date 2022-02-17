package chat.simplex.app.model

import android.util.Log
import androidx.compose.runtime.mutableStateListOf
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.toMutableStateList
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
  private var chatModel: ChatModel? = null
  private val callbacks = mutableMapOf<String, (Error?, CR?) -> Unit>()

  fun setModel(m: ChatModel) {
    chatModel = m
  }

  suspend fun startChat(u: User) {
    chatModel!!.currentUser = mutableStateOf(u)
    Log.d("SIMPLEX (user)", u.toString())
    apiStartChat()
    try {
      Log.d("SIMPLEX", "started chat")
      chatModel!!.chats = apiGetChats().toMutableStateList()
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
        Log.d("SIMPLEX chatRecvMsg: ", json)
        val r = APIResponse.decodeStr(json)
        if (r.corr != null) {
          val cb = callbacks[r.corr]
          if (cb != null) cb(null, r.resp)
        }
        chatModel?.terminalItems?.add(TerminalItem.Resp(System.currentTimeMillis(), r.resp))
      }
    }
  }

  suspend fun sendCmd(cmd: CC): CR {
    return withContext(Dispatchers.IO) {
      val c = cmd.cmdString
      chatModel?.terminalItems?.add(TerminalItem.Cmd(System.currentTimeMillis(), cmd))
      val json = chatSendCmd(ctrl, c)
      Log.d("SIMPLEX", "sendCmd: ${cmd.cmdType}")
      Log.d("SIMPLEX", "sendCmd response json $json")
      val r = APIResponse.decodeStr(json)
      Log.d("SIMPLEX", "sendCmd response type ${r.resp.responseType}")
      chatModel?.terminalItems?.add(TerminalItem.Resp(System.currentTimeMillis(), r.resp))
      r.resp
    }
  }

  suspend fun apiGetActiveUser(): User? {
    val r = sendCmd(CC.ShowActiveUser())
    if (r is CR.ActiveUser) return r.user
    Log.d("SIMPLEX", "apiGetActiveUser: ${r.toString()}")
    return null
  }

  suspend fun apiCreateActiveUser(p: Profile): User {
    val r = sendCmd(CC.CreateActiveUser(p))
    if (r is CR.ActiveUser) return r.user
    Log.d("SIMPLEX", "apiCreateActiveUser: ${r.toString()}")
    throw Error("user not created ${r.toString()}")
  }

  suspend fun apiStartChat() {
    val r = sendCmd(CC.StartChat())
    if (r is CR.ChatStarted ) return
    throw Error("failed starting chat: ${r.toString()}")
  }

  suspend fun apiGetChats(): List<Chat> {
    val r = sendCmd(CC.ApiGetChats())
    if (r is CR.ApiChats ) return r.chats
    throw Error("failed getting the list of chats: ${r.toString()}")
  }

  suspend fun apiGetChat(type: ChatType, id: Long): Chat? {
    val r = sendCmd(CC.ApiGetChat(type, id))
    if (r is CR.ApiChat ) return r.chat
    Log.d("SIMPLEX", "apiGetChat bad response: ${r.toString()}")
    return null
  }

  suspend fun apiSendMessage(type: ChatType, id: Long, mc: MsgContent): AChatItem? {
    val r = sendCmd(CC.ApiSendMessage(type, id, mc))
    if (r is CR.NewChatItem ) return r.chatItem
    Log.d("SIMPLEX", "apiSendMessage bad response: ${r.toString()}")
    return null
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
    override val cmdString get() = "/_get chat ${CC.chatRef(type, id)} count=100"
    override val cmdType get() = "apiGetChat"
  }

  class ApiSendMessage(val type: ChatType, val id: Long, val mc: MsgContent): CC() {
    override val cmdString get() = "/_send ${CC.chatRef(type, id)} ${mc.cmdString}"
    override val cmdType get() = "apiGetChat"
  }

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
    override val details get() = CR.noDetails(this)
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

  @Serializable @SerialName("contactSubscribed")
  class ContactSubscribed(val contact: Contact): CR() {
    override val responseType get() = "contactSubscribed"
    override val details get() = contact.toString()
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

  @Serializable @SerialName("cmdAccepted")
  class CmdAccepted(val corr: String): CR() {
    override val responseType get() = "cmdAccepted"
    override val details get() = "corrId: $corr"
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

  companion object {
    fun noDetails(r: CR): String ="${r.responseType}: no details"
  }
}

abstract class TerminalItem {
  abstract val id: Long
  val date = Date()
  abstract val label: String
  abstract val details: String

  class Cmd(id: Long, val cmd: CC): TerminalItem() {
    override val id = id
    override val label get() = "> ${cmd.cmdString}"
    override val details get() = cmd.cmdString
  }

  class Resp(id: Long, val resp: CR): TerminalItem() {
    override val id = id
    override val label get() = "< ${resp.responseType}"
    override val details get() = resp.details
  }

  companion object {
    val sampleData = listOf<TerminalItem>(
        TerminalItem.Cmd(0, CC.ShowActiveUser()),
        TerminalItem.Resp(1, CR.ActiveUser(User.sampleData))
    )
  }
}
