package chat.simplex.app.model

import android.net.Uri
import androidx.compose.runtime.mutableStateListOf
import androidx.compose.runtime.mutableStateOf
import chat.simplex.app.SimplexApp
import kotlinx.datetime.*
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlin.Boolean
import kotlin.Int
import kotlin.Long
import kotlin.String

class ChatModel(val controller: ChatController, val alertManager: SimplexApp.AlertManager) {
  var currentUser = mutableStateOf<User?>(null)
  var chats = mutableStateListOf<Chat>()
  var chatId = mutableStateOf<String?>(null)
  var chatItems = mutableStateListOf<ChatItem>()

  var connReqInvitation: String? = null
  var terminalItems = mutableStateListOf<TerminalItem>()
  // set when app is opened via contact or invitation URI
  var appOpenUrl = mutableStateOf<Uri?>(null)

  fun hasChat(id: String): Boolean = chats.firstOrNull() { it.id == id } != null
  fun getChat(id: String): Chat? = chats.firstOrNull { it.id == id }
  private fun getChatIndex(id: String): Int = chats.indexOfFirst { it.id == id }
  fun addChat(chat: Chat) = chats.add(index = 0, chat)

  fun updateChatInfo(cInfo: ChatInfo) {
    val i = getChatIndex(cInfo.id)
    if (i >= 0) chats[i] = chats[i].copy(chatInfo = cInfo)
  }

  fun updateContact(contact: Contact) {
    val cInfo = ChatInfo.Direct(contact)
    if (hasChat(contact.id)) {
      updateChatInfo(cInfo)
    } else {
      addChat(Chat(chatInfo = cInfo, chatItems = arrayListOf()))
    }
  }

  fun updateNetworkStatus(contact: Contact, status: Chat.NetworkStatus) {
    val i = getChatIndex(contact.id)
    if (i >= 0) {
      val chat = chats[i]
      chats[i] = chat.copy(serverInfo = chat.serverInfo.copy(networkStatus = status))
    }
  }

//  func replaceChat(_ id: String, _ chat: Chat) {
//    if let i = getChatIndex(id) {
//      chats[i] = chat
//    } else {
//      // invalid state, correcting
//      chats.insert(chat, at: 0)
//    }
//  }

  fun addChatItem(cInfo: ChatInfo, cItem: ChatItem) {
    // update previews
    val i = getChatIndex(cInfo.id)
    if (i >= 0) {
      val chat = chats[i]
      chats[i] = chat.copy(
        chatItems = arrayListOf(cItem),
        chatStats =
          if (cItem.meta.itemStatus is CIStatus.RcvNew)
            chat.chatStats.copy(unreadCount = chat.chatStats.unreadCount + 1)
          else
            chat.chatStats
      )
      if (i > 0) {
        popChat_(i)
      }
    } else {
      addChat(Chat(chatInfo = cInfo, chatItems = arrayListOf(cItem)))
    }
    // add to current chat
    if (chatId.value == cInfo.id) {
      chatItems.add(cItem)
      if (cItem.meta.itemStatus is CIStatus.RcvNew) {
        // TODO mark item read via api and model
//        DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
//          if self.chatId == cInfo.id {
//            SimpleX.markChatItemRead(cInfo, cItem)
//          }
//        }
      }
    }
  }
//
//  func upsertChatItem(_ cInfo: ChatInfo, _ cItem: ChatItem) -> Bool {
//    // update previews
//    var res: Bool
//    if let chat = getChat(cInfo.id) {
//      if let pItem = chat.chatItems.last, pItem.id == cItem.id {
//      chat.chatItems = [cItem]
//    }
//      res = false
//    } else {
//      addChat(Chat(chatInfo: cInfo, chatItems: [cItem]))
//      res = true
//    }
//    // update current chat
//    if chatId == cInfo.id {
//      if let i = chatItems.firstIndex(where: { $0.id == cItem.id }) {
//      withAnimation(.default) {
//      self.chatItems[i] = cItem
//    }
//      return false
//    } else {
//      withAnimation { chatItems.append(cItem) }
//      return true
//    }
//    } else {
//      return res
//    }
//  }
//
//  func markChatItemsRead(_ cInfo: ChatInfo) {
//    // update preview
//    if let chat = getChat(cInfo.id) {
//      chat.chatStats = ChatStats()
//    }
//    // update current chat
//    if chatId == cInfo.id {
//      var i = 0
//      while i < chatItems.count {
//        if case .rcvNew = chatItems[i].meta.itemStatus {
//          chatItems[i].meta.itemStatus = .rcvRead
//        }
//        i = i + 1
//      }
//    }
//  }
//
//  func markChatItemRead(_ cInfo: ChatInfo, _ cItem: ChatItem) {
//    // update preview
//    if let i = getChatIndex(cInfo.id) {
//      chats[i].chatStats.unreadCount = chats[i].chatStats.unreadCount - 1
//    }
//    // update current chat
//    if chatId == cInfo.id, let j = chatItems.firstIndex(where: { $0.id == cItem.id }) {
//      chatItems[j].meta.itemStatus = .rcvRead
//    }
//  }
//
//  func popChat(_ id: String) {
//    if let i = getChatIndex(id) {
//      popChat_(i)
//    }
//  }
//
  private fun popChat_(i: Int) {
    val chat = chats.removeAt(i)
    chats.add(index = 0, chat)
  }

  fun removeChat(id: String) {
    chats.removeAll { it.id == id }
  }
}

enum class ChatType(val type: String) {
  Direct("@"),
  Group("#"),
  ContactRequest("<@");

  val chatTypeName: String get () =
    when (this) {
      Direct -> "contact"
      Group -> "group"
      ContactRequest -> "contact request"
    }
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
  val createdAt: Instant
}

@Serializable
data class Chat (
  val chatInfo: ChatInfo,
  val chatItems: List<ChatItem>,
  val chatStats: ChatStats = ChatStats(),
  val serverInfo: ServerInfo = ServerInfo(NetworkStatus.Unknown())
) {
  val id: String get() = chatInfo.id

  @Serializable
  data class ChatStats(val unreadCount: Int = 0, val minUnreadItemId: Long = 0)

  @Serializable
  data class ServerInfo(val networkStatus: NetworkStatus)

  @Serializable
  sealed class NetworkStatus {
    val statusString: String get() = if (this is Connected) "Server connected" else "Connecting server…"
    val statusExplanation: String get() =
      when {
        this is Connected -> "You are connected to the server you use to receve messages from this contact."
        this is Error -> "Trying to connect to the server you use to receve messages from this contact (error: $error)."
        else -> "Trying to connect to the server you use to receve messages from this contact."
      }

    @Serializable @SerialName("unknown") class Unknown: NetworkStatus()
    @Serializable @SerialName("connected") class Connected: NetworkStatus()
    @Serializable @SerialName("disconnected") class Disconnected: NetworkStatus()
    @Serializable @SerialName("error") class Error(val error: String): NetworkStatus()
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
    override val createdAt get() = contact.createdAt
    override val displayName get() = contact.displayName
    override val fullName get() = contact.fullName

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
    override val createdAt get() = groupInfo.createdAt
    override val displayName get() = groupInfo.displayName
    override val fullName get() = groupInfo.fullName

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
    override val createdAt get() = contactRequest.createdAt
    override val displayName get() = contactRequest.displayName
    override val fullName get() = contactRequest.fullName

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
  override val createdAt: Instant
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
      activeConn = Connection.sampleData,
      createdAt = Clock.System.now()
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
  override val createdAt: Instant
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
      createdAt = Clock.System.now()
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
  val profile: Profile,
  override val createdAt: Instant
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
      createdAt = Clock.System.now()
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
  val timestampText: String get() = meta.timestampText
  val isRcvNew: Boolean get() = meta.itemStatus is CIStatus.RcvNew

  companion object {
    fun getSampleData(id: Long, dir: CIDirection, ts: Instant, text: String,status: CIStatus = CIStatus.SndNew()) =
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
  val itemTs: Instant,
  val itemText: String,
  val itemStatus: CIStatus,
  val createdAt: Instant
) {
  val timestampText: String get() = getTimestampText(itemTs)

  companion object {
    fun getSample(id: Long, ts: Instant, text: String, status: CIStatus = CIStatus.SndNew()): CIMeta =
      CIMeta(
        itemId = id,
        itemTs = ts,
        itemText = text,
        itemStatus = status,
        createdAt = ts
      )
  }
}

fun getTimestampText(t: Instant): String {
  val tz = TimeZone.currentSystemDefault()
  val now: LocalDateTime = Clock.System.now().toLocalDateTime(tz)
  val time: LocalDateTime = t.toLocalDateTime(tz)
  val recent = now.date == time.date ||
      (now.date.minus(time.date).days == 1 && now.hour < 12 && time.hour >= 18 )
  return if (recent) String.format("%02d:%02d", time.hour, time.minute)
                else String.format("%02d/%02d", time.dayOfMonth, time.monthNumber)
}

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
