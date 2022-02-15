package chat.simplex.app.model

import android.util.Log
import chat.simplex.app.chatRecvMsg
import chat.simplex.app.chatSendCmd
import kotlinx.serialization.Serializable
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
        Log.d("SIMPLEX RECV", json)
        chatModel.terminalItems.add(TerminalItem.Resp(CR.Unknown(type = "Unknown", json = json)))
      }
    }
  }

  fun sendCmd(cmd: String) {
    val response = chatSendCmd(ctrl, cmd)
    Log.d("SIMPLEX SEND", response)
    chatModel.terminalItems.add(TerminalItem.Resp(CR.Unknown(type = "Unknown", json = response)))
  }

  class Mock: ChatController(0) {}
}

// Chat Command
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

// chat response
@Serializable
abstract class CR {
  abstract val responseType: String
  abstract val details: String
//}
  class Unknown(val type: String, val json: String): CR() {
    override val responseType get() = "* ${type}"
    override val details get() = json
  }

  class ActiveUser(val user: User): CR() {
    override val responseType get() = "ActiveUser"
    override val details get() = user.toString()
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
