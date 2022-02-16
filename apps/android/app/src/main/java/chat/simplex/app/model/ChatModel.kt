package chat.simplex.app.model

import androidx.compose.runtime.mutableStateListOf
import kotlinx.serialization.Serializable
import java.util.*

class ChatModel(val controller: ChatController) {
  val currentUser: User? = null
  var terminalItems = mutableStateListOf<TerminalItem>()

  companion object {
    val sampleData: ChatModel get() {
      val m = ChatModel(ChatController.Mock())
      m.terminalItems = mutableStateListOf(
        TerminalItem.Cmd(CC.ShowActiveUser()),
        TerminalItem.Resp(CR.ActiveUser(User.sampleData))
      )
      return m
    }
  }
}

enum class ChatType(val type: String) {
  Direct("@"),
  Group("#"),
  ContactRequest("<@")
}

@Serializable
class User (
  val userId: Int,
  val userContactId: Int,
  val localDisplayName: String,
  val profile: Profile,
  val activeUser: Boolean
) : NamedChat {
  override val displayName: String get() = profile.displayName
  override val fullName: String get() = profile.fullName

  companion object {
    val sampleData = User(
      userId = 1,
      userContactId = 1,
      localDisplayName = "alice",
      profile = Profile.sampleData,
      activeUser = true
    )
  }
}

typealias ChatId = String

@Serializable
class Contact(
  val contactId: Int,
  val localDisplayName: String,
  val profile: Profile,
  val activeConn: Connection,
  val viaGroup: Int? = null,
// no serializer for type Date?
//  val createdAt: Date
): NamedChat {
  val id: ChatId get() = "@$contactId"
  val apiId: Int get() = contactId
  val ready: Boolean get() = activeConn.connStatus == "ready" || activeConn.connStatus == "snd-ready"
  override val displayName: String get() = profile.displayName
  override val fullName: String get() = profile.fullName

  companion object {
    val sampleData = Contact(
      contactId = 1,
      localDisplayName = "alice",
      profile = Profile.sampleData,
      activeConn = Connection.sampleData
//      createdAt = Date()
    )
  }
}

@Serializable
class Connection(val connStatus: String) {
  companion object {
    val sampleData = Connection(connStatus = "ready")
  }
}

@Serializable
class Profile(
  val displayName: String,
  val fullName: String
  ) {
  companion object {
    val sampleData = Profile(
      displayName = "alice",
      fullName = "Alice"
    )
  }
}

interface NamedChat {
  abstract val displayName: String
  abstract val fullName: String
  val chatViewName: String
    get() = displayName + (if (fullName == "" || fullName == displayName) "" else " / $fullName")
}
