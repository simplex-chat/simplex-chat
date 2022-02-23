package chat.simplex.app.model

import android.net.Uri
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.*
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.SpanStyle
import androidx.compose.ui.text.font.*
import androidx.compose.ui.text.style.TextDecoration
import chat.simplex.app.SimplexApp
import chat.simplex.app.ui.theme.HighOrLowlight
import kotlinx.datetime.*
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

class ChatModel(val controller: ChatController, val alertManager: SimplexApp.AlertManager) {
  var currentUser = mutableStateOf<User?>(null)
  var userCreated = mutableStateOf<Boolean?>(null)
  var chats = mutableStateListOf<Chat>()
  var chatsLoaded = mutableStateOf<Boolean?>(null)
  var chatId = mutableStateOf<String?>(null)
  var chatItems = mutableStateListOf<ChatItem>()

  var connReqInvitation: String? = null
  var terminalItems = mutableStateListOf<TerminalItem>()
  // set when app is opened via contact or invitation URI
  var appOpenUrl = mutableStateOf<Uri?>(null)

  fun updateUserProfile(profile: Profile) {
    val user = currentUser.value
    if (user != null) {
      currentUser.value = user.copy(profile = profile)
    }
  }

  fun hasChat(id: String): Boolean = chats.firstOrNull { it.id == id } != null
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
    val chat: Chat
    if (i >= 0) {
      chat = chats[i]
      chats[i] = chat.copy(
        chatItems = arrayListOf(cItem),
        chatStats =
          if (cItem.meta.itemStatus is CIStatus.RcvNew) {
            val minUnreadId = if(chat.chatStats.minUnreadItemId == 0L) cItem.id else chat.chatStats.minUnreadItemId
            chat.chatStats.copy(unreadCount = chat.chatStats.unreadCount + 1, minUnreadItemId = minUnreadId)
          }
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
    }
  }

  fun markChatItemsRead(cInfo: ChatInfo) {
    val chatIdx = getChatIndex(cInfo.id)
    // update current chat
    if (chatId.value == cInfo.id) {
      var i = 0
      while (i < chatItems.count()) {
        val item = chatItems[i]
        if (item.meta.itemStatus is CIStatus.RcvNew) {
          chatItems[i] = item.copy(meta=item.meta.copy(itemStatus = CIStatus.RcvRead()))
        }
        i += 1
      }
      val chat = chats[chatIdx]
      chats[chatIdx] = chat.copy(
        chatItems = chatItems,
        chatStats = chat.chatStats.copy(unreadCount = 0, minUnreadItemId = chat.chatItems.last().id + 1)
      )
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
data class User(
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
data class ChatItem (
  val chatDir: CIDirection,
  val meta: CIMeta,
  val content: CIContent,
  val formattedText: List<FormattedText>? = null
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
data class CIMeta (
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
class FormattedText(val text: String, val format: Format? = null) {
  val link: String? = when (format) {
    is Format.Uri -> text
    is Format.Email -> "mailto:$text"
    is Format.Phone -> "tel:$text"
    else -> null
  }
}

@Serializable
sealed class Format {
  @Serializable @SerialName("bold") class Bold: Format()
  @Serializable @SerialName("italic") class Italic: Format()
  @Serializable @SerialName("underline") class Underline: Format()
  @Serializable @SerialName("strikeThrough") class StrikeThrough: Format()
  @Serializable @SerialName("snippet") class Snippet: Format()
  @Serializable @SerialName("secret") class Secret: Format()
  @Serializable @SerialName("colored") class Colored(val formatColor: FormatColor): Format()
  @Serializable @SerialName("uri") class Uri: Format()
  @Serializable @SerialName("email") class Email: Format()
  @Serializable @SerialName("phone") class Phone: Format()

  val style: SpanStyle @Composable get() = when (this) {
    is Bold -> SpanStyle(fontWeight = FontWeight.Bold)
    is Italic -> SpanStyle(fontStyle = FontStyle.Italic)
    is Underline -> SpanStyle(textDecoration = TextDecoration.Underline)
    is StrikeThrough -> SpanStyle(textDecoration = TextDecoration.LineThrough)
    is Snippet -> SpanStyle(fontFamily = FontFamily.Monospace)
    is Secret -> SpanStyle(color = HighOrLowlight, background = HighOrLowlight)
    is Colored -> SpanStyle(color = this.formatColor.uiColor)
    is Uri -> linkStyle
    is Email -> linkStyle
    is Phone -> linkStyle
  }

  companion object {
    val linkStyle @Composable get() = SpanStyle(color = MaterialTheme.colors.primary, textDecoration = TextDecoration.Underline)
  }
}

@Serializable
enum class FormatColor(val color: String) {
  red("red"),
  green("green"),
  blue("blue"),
  yellow("yellow"),
  cyan("cyan"),
  magenta("magenta"),
  black("black"),
  white("white");

  val uiColor: Color @Composable get() = when (this) {
    red -> Color.Red
    green -> Color.Green
    blue -> Color.Blue
    yellow -> Color.Yellow
    cyan -> Color.Cyan
    magenta -> Color.Magenta
    black -> MaterialTheme.colors.onBackground
    white -> MaterialTheme.colors.onBackground
  }
}

@Serializable
class RcvFileTransfer
