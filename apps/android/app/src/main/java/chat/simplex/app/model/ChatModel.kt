package chat.simplex.app.model

import androidx.compose.runtime.*
import kotlinx.serialization.*
import java.util.*

class ChatModel(val controller: ChatController) {
  var currentUser = mutableStateOf<User?>(null)
  var chats = mutableStateListOf<Chat>()
  var chatId = mutableStateOf<String?>(null)
  var chatItems = mutableStateListOf<ChatItem>()

  var terminalItems = mutableStateListOf<TerminalItem>()

  companion object {
    val sampleData: ChatModel get() {
      val m = ChatModel(ChatController.Mock())
      m.terminalItems = mutableStateListOf(
        TerminalItem.Cmd(0, CC.ShowActiveUser()),
        TerminalItem.Resp(1, CR.ActiveUser(User.sampleData))
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
  val userId: Long,
  val userContactId: Long,
  val localDisplayName: String,
  val profile: Profile,
  val activeUser: Boolean
): NamedChat {
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
  val chatType: ChatType
  val localDisplayName: String
  val id: ChatId
  val apiId: Long
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
  @Serializable @SerialName("direct")
  class Direct(val contact: Contact): ChatInfo() {
    override val chatType get() = ChatType.Direct
    override val localDisplayName get() = contact.localDisplayName
    override val id get() = contact.id
    override val apiId get() = contact.apiId
    override val ready get() = contact.ready
    override val displayName get() = contact.displayName
    override val fullName get() = contact.displayName

    companion object {
      val sampleData = Direct(Contact.sampleData)
    }
  }

  @Serializable @SerialName("group")
  class Group(val groupInfo: GroupInfo): ChatInfo() {
    override val chatType get() = ChatType.Group
    override val localDisplayName get() = groupInfo.localDisplayName
    override val id get() = groupInfo.id
    override val apiId get() = groupInfo.apiId
    override val ready get() = groupInfo.ready
    override val displayName get() = groupInfo.displayName
    override val fullName get() = groupInfo.displayName

    companion object {
      val sampleData = Group(GroupInfo.sampleData)
    }
  }

  @Serializable @SerialName("contactRequest")
  class ContactRequest(val contactRequest: UserContactRequest): ChatInfo() {
    override val chatType get() = ChatType.ContactRequest
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
  val contactId: Long,
  override val localDisplayName: String,
  val profile: Profile,
  val activeConn: Connection,
  val viaGroup: Long? = null,
// no serializer for type Date?
//  val createdAt: Date
): SomeChat, NamedChat {
  override val chatType get() = ChatType.Direct
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
  val groupId: Long,
  override val localDisplayName: String,
  val groupProfile: GroupProfile,
//  var createdAt: Date
): SomeChat, NamedChat {
  override val chatType get() = ChatType.Group
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
  val groupMemberId: Long,
  val memberId: String,
//    var memberRole: GroupMemberRole
//    var memberCategory: GroupMemberCategory
//    var memberStatus: GroupMemberStatus
//    var invitedBy: InvitedBy
  val localDisplayName: String,
  val memberProfile: Profile,
  val memberContactId: Long?
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
  val contactRequestId: Long,
  override val localDisplayName: String,
  val profile: Profile
//  val createdAt: Date
): SomeChat, NamedChat {
  override val chatType get() = ChatType.ContactRequest
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
class AChatItem (
  val chatInfo: ChatInfo,
  val chatItem: ChatItem
)

@Serializable
class ChatItem (
  val chatDir: CIDirection,
  val meta: CIMeta,
  val content: CIContent
) {
  val id: Long get() = meta.itemId
  //  val timestampText: String get() =  meta.timestampText
  val isRcvNew: Boolean get() = meta.itemStatus is CIStatus.RcvNew

  companion object {
    fun getSampleData(id: Long, dir: CIDirection, ts: Date, text: String,status: CIStatus = CIStatus.SndNew()) =
      ChatItem(
        chatDir = dir,
        meta = CIMeta.getSample(id, ts, text, status),
        content = CIContent.SndMsgContent(msgContent = MsgContent.MCText(text))
      )
  }
}

@Serializable
sealed class CIDirection {
  abstract val sent: Boolean

  @Serializable @SerialName("directSnd")
  class DirectSnd: CIDirection() {
    override val sent get() = true
  }

  @Serializable @SerialName("directRcv")
  class DirectRcv: CIDirection() {
    override val sent get() = false
  }

  @Serializable @SerialName("groupSnd")
  class GroupSnd: CIDirection() {
    override val sent get() = true
  }

  @Serializable @SerialName("groupRcv")
  class GroupRcv(val groupMember: GroupMember): CIDirection() {
    override val sent get() = false
  }
}

@Serializable
class CIMeta (
  val itemId: Long,
//  val itemTs: Date,
  val itemText: String,
  val itemStatus: CIStatus,
//  val createdAt: Date
) {
//  val timestampText: String get() = getTimestampText(itemTs)

  companion object {
    fun getSample(id: Long, ts: Date, text: String, status: CIStatus = CIStatus.SndNew()): CIMeta =
      CIMeta(
        itemId = id,
//        itemTs = ts,
        itemText = text,
        itemStatus = status,
//        createdAt = ts
      )
  }
}

// TODO
fun getTimestampText(d: Date): String = ""

@Serializable
sealed class CIStatus {
  @Serializable @SerialName("sndNew")
  class SndNew: CIStatus()

  @Serializable @SerialName("sndSent")
  class SndSent: CIStatus()

  @Serializable @SerialName("sndErrorAuth")
  class SndErrorAuth: CIStatus()

  @Serializable @SerialName("sndError")
  class SndError(val agentError: AgentErrorType): CIStatus()

  @Serializable @SerialName("rcvNew")
  class RcvNew: CIStatus()

  @Serializable @SerialName("rcvRead")
  class RcvRead: CIStatus()
}

@Serializable
sealed class CIContent {
  abstract val text: String

  @Serializable @SerialName("sndMsgContent")
  class SndMsgContent(val msgContent: MsgContent): CIContent() {
    override val text get() = msgContent.text
  }

  @Serializable @SerialName("rcvMsgContent")
  class RcvMsgContent(val msgContent: MsgContent): CIContent() {
    override val text get() = msgContent.text
  }

  @Serializable @SerialName("sndFileInvitation")
  class SndFileInvitation(val fileId: Long, val filePath: String): CIContent() {
    override val text get() = "sending files is not supported yet"
  }

  @Serializable @SerialName("rcvFileInvitation")
  class RcvFileInvitation(val rcvFileTransfer: RcvFileTransfer): CIContent() {
    override val text get() = "receiving files is not supported yet"
  }
}

@Serializable
sealed class MsgContent {
  abstract val text: String
  abstract val cmdString: String

  @Serializable @SerialName("text")
  class MCText(override val text: String): MsgContent() {
    override val cmdString get() = "text $text"
  }
}

@Serializable
class RcvFileTransfer {

}

@Serializable
class AgentErrorType {

}
