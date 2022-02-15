package chat.simplex.app.model

import android.util.Log
import chat.simplex.app.chatRecvMsg
import chat.simplex.app.chatSendCmd
import java.util.*
import kotlin.concurrent.thread

typealias Controller = Long

class ChatController(val ctrl: Controller) {
  private lateinit var chatModel: ChatModel

  fun setModel(m: ChatModel) {
    chatModel = m
  }

  fun startReceiver() {
    thread(name="receiver") {
//            val chatlog = FifoQueue<String>(500)
      while(true) {
        val msg = chatRecvMsg(ctrl)
        Log.d("SIMPLEX RECV", msg)
        chatModel.terminalItems.add(msg)
//                val currentText = chatlog.joinToString("\n")
//                weakActivity.get()?.runOnUiThread {
//                    val log = weakActivity.get()?.findViewById<TextView>(R.id.chatlog)
//                    val scroll = weakActivity.get()?.findViewById<ScrollView>(R.id.scroller)
//                    log?.text = currentText
//                    scroll?.scrollTo(0, scroll.getChildAt(0).height)
//                }
      }
    }
  }

  fun sendCmd(cmd: String) {
    val response = chatSendCmd(ctrl, cmd)
    Log.d("SIMPLEX SEND", response)
    chatModel.terminalItems.add(response)
  }
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
abstract class CR {
  abstract val responseType: String
  abstract val details: String

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

abstract class TerminalItem(val date: Date) {
  abstract val label: String
  abstract val details: String

  class Cmd(val cmd: CC): TerminalItem(date = Date()) {
    override val label get() = cmd.cmdString
    override val details get() = cmd.cmdString
  }

  class Resp(val resp: CR): TerminalItem(date = Date()) {
    override val label get() = resp.responseType
    override val details get() = resp.details
  }
}
