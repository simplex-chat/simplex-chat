package chat.simplex.app.model

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
}

enum class ChatType(val type: String) {
  Direct("@"),
  Group("#"),
  ContactRequest("<@")
}

private fun chatRef(type: ChatType, id: String) = "${type}${id}"

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
}
