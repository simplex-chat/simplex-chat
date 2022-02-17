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

interface NamedChat {
  val displayName: String
  val fullName: String
  val chatViewName: String
    get() = displayName + (if (fullName == "" || fullName == displayName) "" else " / $fullName")
}

interface SomeChat {
  val localDisplayName: String
  val id: ChatId
  val apiId: Int
  val ready: Boolean
}

@Serializable
class Chat (
  val chatInfo: ChatInfo,
  val chatItems: List<ChatItem>,
  val chatStats: ChatStats,
  val serverInfo: ServerInfo = ServerInfo(NetworkStatus.Unknown())
) {

  @Serializable
  class ChatStats {

  }

  @Serializable
  class ServerInfo(val networkStatus: NetworkStatus)

  @Serializable
  sealed class NetworkStatus {
    abstract val statusString: String
    abstract val statusExplanation: String
    abstract val imageName: String

    @Serializable
    class Unknown: NetworkStatus() {
      override val statusString get() = "Server connected"
      override val statusExplanation get() = "You are connected to the server you use to receve messages from this contact."
      override val imageName get() = "circle.dotted" // ?
    }
  }
}

@Serializable
sealed class ChatInfo: SomeChat, NamedChat {
  @Serializable
  class DirectChat(val contact: Contact): ChatInfo() {
    override val localDisplayName get() = contact.localDisplayName
    override val id get() = contact.id
    override val apiId get() = contact.apiId
    override val ready get() = contact.ready
    override val displayName get() = contact.displayName
    override val fullName get() = contact.displayName

    companion object {
      val sampleData = DirectChat(Contact.sampleData)
    }
  }

  @Serializable
  class GroupGhat(val groupInfo: GroupInfo): ChatInfo() {
    override val localDisplayName get() = groupInfo.localDisplayName
    override val id get() = groupInfo.id
    override val apiId get() = groupInfo.apiId
    override val ready get() = groupInfo.ready
    override val displayName get() = groupInfo.displayName
    override val fullName get() = groupInfo.displayName

    companion object {
      val sampleData = GroupGhat(GroupInfo.sampleData)
    }
  }

  @Serializable
  class ContactRequest(val contactRequest: UserContactRequest): ChatInfo() {
    override val localDisplayName get() = contactRequest.localDisplayName
    override val id get() = contactRequest.id
    override val apiId get() = contactRequest.apiId
    override val ready get() = contactRequest.ready
    override val displayName get() = contactRequest.displayName
    override val fullName get() = contactRequest.displayName

    companion object {
      val sampleData = ContactRequest(UserContactRequest.sampleData)
    }
  }
}

@Serializable
class Contact(
  val contactId: Int,
  override val localDisplayName: String,
  val profile: Profile,
  val activeConn: Connection,
  val viaGroup: Int? = null,
// no serializer for type Date?
//  val createdAt: Date
): SomeChat, NamedChat {
  override val id get() = "@$contactId"
  override val apiId get() = contactId
  override val ready get() = activeConn.connStatus == "ready" || activeConn.connStatus == "snd-ready"
  override val displayName get() = profile.displayName
  override val fullName get() = profile.fullName

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

@Serializable
class GroupInfo (
  val groupId: Int,
  override val localDisplayName: String,
  val groupProfile: GroupProfile,
//  var createdAt: Date
): SomeChat, NamedChat {
  override val id get() = "#$groupId"
  override val apiId get() = groupId
  override val ready get() = true
  override val displayName get() = groupProfile.displayName
  override val fullName get() = groupProfile.fullName

  companion object {
    val sampleData = GroupInfo(
      groupId = 1,
      localDisplayName = "team",
      groupProfile = GroupProfile.sampleData,
//      createdAt: Date()
    )
  }
}

@Serializable
class GroupProfile (
  override val displayName: String,
  override val fullName: String
): NamedChat {
  companion object {
    val sampleData = GroupProfile(
      displayName = "team",
      fullName = "My Team"
    )
  }
}

@Serializable
class GroupMember (
  val groupMemberId: Int,
  val memberId: String,
//    var memberRole: GroupMemberRole
//    var memberCategory: GroupMemberCategory
//    var memberStatus: GroupMemberStatus
//    var invitedBy: InvitedBy
  val localDisplayName: String,
  val memberProfile: Profile,
  val memberContactId: Int?
//    var activeConn: Connection?
) {
  companion object {
    val sampleData = GroupMember(
      groupMemberId = 1,
      memberId = "abcd",
      localDisplayName = "alice",
      memberProfile = Profile.sampleData,
      memberContactId = 1
    )
  }
}

@Serializable
class UserContactRequest (
  val contactRequestId: Int,
  override val localDisplayName: String,
  val profile: Profile
//  val createdAt: Date
): SomeChat, NamedChat {
  override val id get() = "<@$contactRequestId"
  override val apiId get() = contactRequestId
  override val ready get() = true
  override val displayName get() = profile.displayName
  override val fullName get() = profile.fullName

  companion object {
    val sampleData = UserContactRequest(
      contactRequestId = 1,
      localDisplayName = "alice",
      profile = Profile.sampleData,
//      createdAt: Date()
    )
  }
}

@Serializable
class ChatItem {

}
