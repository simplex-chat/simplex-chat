package chat.simplex.app.model

// Chat Command
abstract class CC {
  abstract val cmdString: String
  abstract val cmdType: String

  class Console(val cmd: String): CC() {
    override val cmdString = cmd
    override val cmdType = "console command"
  }

  class ShowActiveUser: CC() {
    override val cmdString = "/u"
    override val cmdType = "ShowActiveUser"
  }

  class CreateActiveUser(val profile: Profile): CC() {
    override val cmdString = "/u ${profile.displayName} ${profile.fullName}"
    override val cmdType = "CreateActiveUser"
  }

  class StartChat: CC() {
    override val cmdString = "/_start"
    override val cmdType = "StartChat"
  }

  class ApiGetChats: CC() {
    override val cmdString = "/_get chats"
    override val cmdType = "ApiGetChats"
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
  abstract fun responseType(): String
  abstract fun details(): String

  class Unknown(val type: String, val json: String): CR() {
    override fun responseType() = "* ${type}"
    override fun details() = json
  }

  class ActiveUser(val user: User): CR() {
    override fun responseType() = "ActiveUser"
    override fun details() = user.toString()
  }
}
