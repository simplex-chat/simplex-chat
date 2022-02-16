package chat.simplex.app.model

import android.util.Log
import chat.simplex.app.chatRecvMsg
import chat.simplex.app.chatSendCmd
import kotlinx.serialization.*
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import kotlinx.serialization.json.jsonPrimitive
import java.lang.Exception
import java.util.*
import kotlin.concurrent.thread

typealias Controller = Long

open class ChatController(val ctrl: Controller) {
  private lateinit var chatModel: ChatModel

  fun setModel(m: ChatModel) {
    chatModel = m
  }

  fun startReceiver() {
    thread(name="receiver") {
//            val chatlog = FifoQueue<String>(500)
      while(true) {
        val json = chatRecvMsg(ctrl)
        Log.d("SIMPLEX chatRecvMsg: ", json)
        chatModel.terminalItems.add(TerminalItem.Resp(APIResponse.decodeStr(json)))
      }
    }
  }

  fun sendCmd(cmd: String) {
    val json = chatSendCmd(ctrl, cmd)
    Log.d("SIMPLEX chatSendCmd: ", cmd)
    Log.d("SIMPLEX chatSendCmd response: ", json)
    chatModel.terminalItems.add(TerminalItem.Resp(APIResponse.decodeStr(json)))
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
    override val cmdType get() = "ShowActiveUser"
  }

  class CreateActiveUser(val profile: Profile): CC() {
    override val cmdString get() = "/u ${profile.displayName} ${profile.fullName}"
    override val cmdType get() = "CreateActiveUser"
  }

  class StartChat: CC() {
    override val cmdString get() = "/_start"
    override val cmdType get() = "StartChat"
  }

  class ApiGetChats: CC() {
    override val cmdString get() = "/_get chats"
    override val cmdType get() = "ApiGetChats"
  }

  companion object {
    fun chatRef(type: ChatType, id: String) = "${type}${id}"
  }
}

val json = Json {
  prettyPrint = true
  ignoreUnknownKeys = true
}

@Serializable
class APIResponse(val resp: CR) {
  companion object {
    fun decodeStr(str: String): CR {
      try {
        return json.decodeFromString<APIResponse>(str).resp
      } catch(e: Exception) {
        try {
          val data = json.parseToJsonElement(str)
          return CR.Response(data.jsonObject["resp"]!!.jsonObject["type"]?.toString() ?: "invalid", json.encodeToString(data))
        } catch(e: Exception) {
          return CR.Invalid(str)
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

  @Serializable
  @SerialName("activeUser")
  class ActiveUser(val user: User): CR() {
    override val responseType get() = "activeUser"
    override val details get() = user.toString()
  }

  @Serializable
  @SerialName("contactSubscribed")
  class ContactSubscribed(val contact: Contact): CR() {
    override val responseType get() = "contactSubscribed"
    override val details get() = contact.toString()
  }

  @Serializable
  class Response(val type: String, val json: String): CR() {
    override val responseType get() = "* ${type}"
    override val details get() = json
  }

  @Serializable
  class Invalid(val str: String): CR() {
    override val responseType get() = "* invalid json"
    override val details get() = str
  }

  // {"resp": {"activeUser": {"user": {<user>}}}}
  // {"resp": {"anythingElse": <json> }} -> Unknown(type = "anythingElse", json = "<the whole thing including resp>")

  // {"type": "activeUser", "user": <user>}
}

abstract class TerminalItem {
  val date = Date()
  abstract val label: String
  abstract val details: String

  class Cmd(val cmd: CC): TerminalItem() {
    override val label get() = "> ${cmd.cmdString.substring(0, 30)}"
    override val details get() = cmd.cmdString
  }

  class Resp(val resp: CR): TerminalItem() {
    override val label get() = "< ${resp.responseType}"
    override val details get() = resp.details
  }

  companion object {
    val sampleData = listOf<TerminalItem>(
        TerminalItem.Cmd(CC.ShowActiveUser()),
        TerminalItem.Resp(CR.ActiveUser(User.sampleData))
    )
  }
}
