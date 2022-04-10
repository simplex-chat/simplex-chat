package chat.simplex.app.model

import android.net.Uri
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.*
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.SpanStyle
import androidx.compose.ui.text.font.*
import androidx.compose.ui.text.style.TextDecoration
import chat.simplex.app.ui.theme.SecretColor
import chat.simplex.app.ui.theme.SimplexBlue
import kotlinx.datetime.*
import kotlinx.serialization.*
import kotlinx.serialization.descriptors.*
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.json.*

class ChatModel(val controller: ChatController) {
  var currentUser = mutableStateOf<User?>(null)
  var userCreated = mutableStateOf<Boolean?>(null)
  var chats = mutableStateListOf<Chat>()
  var chatId = mutableStateOf<String?>(null)
  var chatItems = mutableStateListOf<ChatItem>()

  var connReqInvitation: String? = null
  var terminalItems = mutableStateListOf<TerminalItem>()
  var userAddress = mutableStateOf<String?>(null)
  var userSMPServers = mutableStateOf<(List<String>)?>(null)
  // set when app is opened via contact or invitation URI
  var appOpenUrl = mutableStateOf<Uri?>(null)
  var runServiceInBackground = mutableStateOf(true)

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

  fun replaceChat(id: String, chat: Chat) {
    val i = getChatIndex(id)
    if (i >= 0) {
      chats[i] = chat
    } else {
      // invalid state, correcting
      chats.add(index = 0, chat)
    }
  }

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

  fun upsertChatItem(cInfo: ChatInfo, cItem: ChatItem): Boolean {
    // update previews
    val i = getChatIndex(cInfo.id)
    val chat: Chat
    val res: Boolean
    if (i >= 0) {
      chat = chats[i]
      val pItem = chat.chatItems.last()
      if (pItem.id == cItem.id) {
        chats[i] = chat.copy(chatItems = arrayListOf(cItem))
      }
      res = false
    } else {
      addChat(Chat(chatInfo = cInfo, chatItems = arrayListOf(cItem)))
      res = true
    }
    // update current chat
    if (chatId.value == cInfo.id) {
      val itemIndex = chatItems.indexOfFirst { it.id == cItem.id }
      if (itemIndex >= 0) {
        chatItems[itemIndex] = cItem
        return false
      } else {
        chatItems.add(cItem)
        return true
      }
    } else {
      return res
    }
  }

  fun removeChatItem(cInfo: ChatInfo, cItem: ChatItem) {
    // update previews
    val i = getChatIndex(cInfo.id)
    val chat: Chat
    if (i >= 0) {
      chat = chats[i]
      val pItem = chat.chatItems.last()
      if (pItem.id == cItem.id) {
        chats[i] = chat.copy(chatItems = arrayListOf(cItem))
      }
    }
    // remove from current chat
    if (chatId.value == cInfo.id) {
      val itemIndex = chatItems.indexOfFirst { it.id == cItem.id }
      if (itemIndex >= 0) {
        chatItems.removeAt(itemIndex)
      }
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

//  func popChat(_ id: String) {
//    if let i = getChatIndex(id) {
//      popChat_(i)
//    }
//  }

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
  override val image: String? get() = profile.image

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
  val image: String?
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
        this is Connected -> "You are connected to the server used to receive messages from this contact."
        this is Error -> "Trying to connect to the server used to receive messages from this contact (error: $error)."
        else -> "Trying to connect to the server used to receive messages from this contact."
      }

    @Serializable @SerialName("unknown") class Unknown: NetworkStatus()
    @Serializable @SerialName("connected") class Connected: NetworkStatus()
    @Serializable @SerialName("disconnected") class Disconnected: NetworkStatus()
    @Serializable @SerialName("error") class Error(val error: String): NetworkStatus()
  }

  companion object {
    val sampleData = Chat(
      chatInfo = ChatInfo.Direct.sampleData,
      chatItems = arrayListOf(ChatItem.getSampleData())
    )
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
    override val image get() = contact.image

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
    override val image get() = groupInfo.image

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
    override val image get() = contactRequest.image

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
  override val image get() = profile.image

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
class ContactSubStatus(
  val contact: Contact,
  val contactError: ChatError? = null
)

@Serializable
class Connection(val connStatus: String) {
  companion object {
    val sampleData = Connection(connStatus = "ready")
  }
}

@Serializable
class Profile(
  val displayName: String,
  val fullName: String,
  val image: String? = null
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
  override val image get() = groupProfile.image

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
  override val fullName: String,
  override val image: String? = null
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
class LinkPreview (
  val uri: String,
  val title: String,
  val description: String,
  val image: String
) {
  companion object {
    val sampleData = LinkPreview(
      uri = "https://www.duckduckgo.com",
      title = "Privacy, simplified.",
      description = "The Internet privacy company that empowers you to seamlessly take control of your personal information online, without any tradeoffs.",
      image = "data:image/jpg;base64,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"
    )
  }
}

@Serializable
class MemberSubError (
  val member: GroupMember,
  val memberError: ChatError
)

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
  override val image get() = profile.image

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
  val formattedText: List<FormattedText>? = null,
  val quotedItem: CIQuote? = null
) {
  val id: Long get() = meta.itemId
  val timestampText: String get() = meta.timestampText
  val isRcvNew: Boolean get() = meta.itemStatus is CIStatus.RcvNew

  val memberDisplayName: String? get() =
    if (chatDir is CIDirection.GroupRcv) chatDir.groupMember.memberProfile.displayName
    else null

  val isMsgContent: Boolean get() =
    when (content) {
      is CIContent.SndMsgContent -> true
      is CIContent.RcvMsgContent -> true
      else -> false
    }

  val isDeletedContent: Boolean get() =
    when (content) {
      is CIContent.SndDeleted -> true
      is CIContent.RcvDeleted -> true
      else -> false
    }

  companion object {
    fun getSampleData(
      id: Long = 1,
      dir: CIDirection = CIDirection.DirectSnd(),
      ts: Instant = Clock.System.now(),
      text: String = "hello\nthere",
      status: CIStatus = CIStatus.SndNew(),
      quotedItem: CIQuote? = null,
      itemDeleted: Boolean = false,
      itemEdited: Boolean = false,
      editable: Boolean = true
    ) =
      ChatItem(
        chatDir = dir,
        meta = CIMeta.getSample(id, ts, text, status, itemDeleted, itemEdited, editable),
        content = CIContent.SndMsgContent(msgContent = MsgContent.MCText(text)),
        quotedItem = quotedItem
      )

    fun getDeletedContentSampleData(
      id: Long = 1,
      dir: CIDirection = CIDirection.DirectRcv(),
      ts: Instant = Clock.System.now(),
      text: String = "this item is deleted",
      status: CIStatus = CIStatus.RcvRead()
    ) =
      ChatItem(
        chatDir = dir,
        meta = CIMeta.getSample(id, ts, text, status, false, false, false),
        content = CIContent.RcvDeleted(deleteMode = CIDeleteMode.cidmBroadcast),
        quotedItem = null
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
  val createdAt: Instant,
  val itemDeleted: Boolean,
  val itemEdited: Boolean,
  val editable: Boolean
) {
  val timestampText: String get() = getTimestampText(itemTs)

  companion object {
    fun getSample(
      id: Long, ts: Instant, text: String, status: CIStatus = CIStatus.SndNew(),
      itemDeleted: Boolean = false, itemEdited: Boolean = false, editable: Boolean = true
    ): CIMeta =
      CIMeta(
        itemId = id,
        itemTs = ts,
        itemText = text,
        itemStatus = status,
        createdAt = ts,
        itemDeleted = itemDeleted,
        itemEdited = itemEdited,
        editable = editable
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
enum class CIDeleteMode(val deleteMode: String) {
  @SerialName("internal") cidmInternal("internal"),
  @SerialName("broadcast") cidmBroadcast("broadcast");
}

interface ItemContent {
  val text: String
}

@Serializable
sealed class CIContent: ItemContent {
  abstract override val text: String
  abstract val msgContent: MsgContent?

  @Serializable @SerialName("sndMsgContent")
  class SndMsgContent(override val msgContent: MsgContent): CIContent() {
    override val text get() = msgContent.text
  }

  @Serializable @SerialName("rcvMsgContent")
  class RcvMsgContent(override val msgContent: MsgContent): CIContent() {
    override val text get() = msgContent.text
  }

  @Serializable @SerialName("sndDeleted")
  class SndDeleted(val deleteMode: CIDeleteMode): CIContent() {
    override val text get() = "deleted"
    override val msgContent get() = null
  }

  @Serializable @SerialName("rcvDeleted")
  class RcvDeleted(val deleteMode: CIDeleteMode): CIContent() {
    override val text get() = "deleted"
    override val msgContent get() = null
  }

  @Serializable @SerialName("sndFileInvitation")
  class SndFileInvitation(val fileId: Long, val filePath: String): CIContent() {
    override val text get() = "sending files is not supported yet"
    override val msgContent get() = null
  }

  @Serializable @SerialName("rcvFileInvitation")
  class RcvFileInvitation(val rcvFileTransfer: RcvFileTransfer): CIContent() {
    override val text get() = "receiving files is not supported yet"
    override val msgContent get() = null
  }
}

@Serializable
class CIQuote (
  val chatDir: CIDirection? = null,
  val itemId: Long? = null,
  val sharedMsgId: String? = null,
  val sentAt: Instant,
  val content: MsgContent,
  val formattedText: List<FormattedText>? = null
): ItemContent {
  override val text: String get() = content.text

  fun sender(user: User): String? = when (chatDir) {
    is CIDirection.DirectSnd -> "you"
    is CIDirection.DirectRcv -> null
    is CIDirection.GroupSnd -> user.displayName
    is CIDirection.GroupRcv -> chatDir.groupMember.memberProfile.displayName
    null -> null
  }

  companion object {
    fun getSample(itemId: Long?, sentAt: Instant, text: String, chatDir: CIDirection?): CIQuote =
      CIQuote(chatDir = chatDir, itemId = itemId, sentAt = sentAt, content = MsgContent.MCText(text))
  }
}

@Suppress("SERIALIZER_TYPE_INCOMPATIBLE")
@Serializable(with = MsgContentSerializer::class)
sealed class MsgContent {
  abstract val text: String

  @Serializable(with = MsgContentSerializer::class)
  class MCText(override val text: String): MsgContent()

  @Serializable(with = MsgContentSerializer::class)
  class MCLink(override val text: String, val preview: LinkPreview): MsgContent()

  @Serializable(with = MsgContentSerializer::class)
  class MCUnknown(val type: String? = null, override val text: String, val json: JsonElement): MsgContent()

  val cmdString: String get() = when (this) {
    is MCText -> "text $text"
    is MCLink -> "json ${json.encodeToString(this)}"
    is MCUnknown -> "json $json"
  }
}

object MsgContentSerializer : KSerializer<MsgContent> {
  @OptIn(InternalSerializationApi::class)
  override val descriptor: SerialDescriptor = buildSerialDescriptor("MsgContent", PolymorphicKind.SEALED) {
    element("MCText", buildClassSerialDescriptor("MCText") {
      element<String>("text")
    })
    element("MCLink", buildClassSerialDescriptor("MCLink") {
      element<String>("text")
      element<String>("preview")
    })
    element("MCUnknown", buildClassSerialDescriptor("MCUnknown"))
  }

  override fun deserialize(decoder: Decoder): MsgContent {
    require(decoder is JsonDecoder)
    val json = decoder.decodeJsonElement()
    return if (json is JsonObject) {
      if ("type" in json) {
        val t = json["type"]?.jsonPrimitive?.content ?: ""
        val text = json["text"]?.jsonPrimitive?.content ?: "unknown message format"
        when (t) {
          "text" -> MsgContent.MCText(text)
          "link" -> {
            val preview = Json.decodeFromString<LinkPreview>(json["preview"].toString())
            MsgContent.MCLink(text, preview)
          }
          else -> MsgContent.MCUnknown(t, text, json)
        }
      } else {
        MsgContent.MCUnknown(text = "invalid message format", json = json)
      }
    } else {
      MsgContent.MCUnknown(text = "invalid message format", json = json)
    }
  }

  override fun serialize(encoder: Encoder, value: MsgContent) {
    require(encoder is JsonEncoder)
    val json = when (value) {
      is MsgContent.MCText ->
        buildJsonObject {
          put("type", "text")
          put("text", value.text)
        }
      is MsgContent.MCLink ->
        buildJsonObject {
          put("type", "link")
          put("text", value.text)
          put("preview", json.encodeToJsonElement(value.preview))
        }
      is MsgContent.MCUnknown -> value.json
    }
    encoder.encodeJsonElement(json)
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
  @Serializable @SerialName("strikeThrough") class StrikeThrough: Format()
  @Serializable @SerialName("snippet") class Snippet: Format()
  @Serializable @SerialName("secret") class Secret: Format()
  @Serializable @SerialName("colored") class Colored(val color: FormatColor): Format()
  @Serializable @SerialName("uri") class Uri: Format()
  @Serializable @SerialName("email") class Email: Format()
  @Serializable @SerialName("phone") class Phone: Format()

  val style: SpanStyle @Composable get() = when (this) {
    is Bold -> SpanStyle(fontWeight = FontWeight.Bold)
    is Italic -> SpanStyle(fontStyle = FontStyle.Italic)
    is StrikeThrough -> SpanStyle(textDecoration = TextDecoration.LineThrough)
    is Snippet -> SpanStyle(fontFamily = FontFamily.Monospace)
    is Secret -> SpanStyle(color = Color.Transparent, background = SecretColor)
    is Colored -> SpanStyle(color = this.color.uiColor)
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
    blue -> SimplexBlue
    yellow -> Color.Yellow
    cyan -> Color.Cyan
    magenta -> Color.Magenta
    black -> MaterialTheme.colors.onBackground
    white -> MaterialTheme.colors.onBackground
  }
}

@Serializable
class RcvFileTransfer
