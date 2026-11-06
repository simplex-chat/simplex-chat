package chat.simplex.common.model

import chat.simplex.common.views.chatlist.DirectoryFooter
import chat.simplex.common.views.newchat.ConnectTarget
import chat.simplex.common.views.newchat.strConnectTarget
import kotlinx.serialization.SerialName
import kotlinx.serialization.SerializationException
import kotlinx.serialization.Serializable
import kotlinx.serialization.Transient
import kotlinx.serialization.json.*

const val DIRECTORY_SERVICE_LINK = "https://smp4.simplex.im/a#lXUjJW5vHYQzoLYgmi8GbxkGP41_kjefFvBrdwg-0Ok"

const val DIRECTORY_SEARCH_TIMEOUT_SEC = 10.0

const val DIRECTORY_SEARCH_MAX_TEXT_LENGTH = 100

const val DIRECTORY_SEARCH_MAX_IMAGE_LENGTH = 12500

@Serializable
data class DirectoryPublicLink(
  val connFullLink: String? = null,
  val connShortLink: String? = null,
)

@Serializable
data class DirectoryEntryType(
  val groupType: GroupType? = null,
  val summary: GroupSummary,
)

@Serializable
data class DirectorySearchEntry(
  val entryType: DirectoryEntryType,
  val displayName: String,
  val simplexName: String? = null,
  private val groupLink: DirectoryPublicLink,
  val shortDescr: String? = null,
  @SerialName("image") private val imageData: String? = null,
) {
  val id: String get() = link

  // untrusted input: only group and channel links are kept
  @Transient
  val link: String = run {
    val link = groupLink.connShortLink ?: groupLink.connFullLink
    val linkType = if (link != null) simplexLinkType(link) else null
    if (link == null || linkType == null || !(linkType == SimplexLinkType.group || linkType == SimplexLinkType.channel)) {
      throw SerializationException("not a group or channel link")
    }
    link
  }

  @Transient
  val image: String? = imageData?.takeIf(::isSafeImageData)
}

fun simplexLinkType(s: String): SimplexLinkType? {
  val t = strConnectTarget(s)
  return if (t is ConnectTarget.Link && t.text == s) t.linkType else null
}

private fun isSafeImageData(s: String): Boolean =
  s.length <= DIRECTORY_SEARCH_MAX_IMAGE_LENGTH
    && (s.startsWith("data:image/jpg;base64,/9j/") || s.startsWith("data:image/png;base64,iVBORw0KGgo"))

data class DirectorySearchResults(
  val entries: List<DirectorySearchEntry>,
  val cursor: JsonObject?,
  val moreCount: Int?,
)

fun directorySearchRequest(text: String, cursor: JsonObject?): JsonObject = buildJsonObject {
  put("type", JsonPrimitive("search"))
  put("searchText", JsonPrimitive(text.takeCodePoints(DIRECTORY_SEARCH_MAX_TEXT_LENGTH)))
  if (cursor != null) put("searchCursor", cursor)
}

private fun String.takeCodePoints(n: Int): String {
  var end = 0
  var count = 0
  while (end < length && count < n) {
    end += if (this[end].isHighSurrogate() && getOrNull(end + 1)?.isLowSurrogate() == true) 2 else 1
    count++
  }
  return substring(0, end)
}

fun parseDirectorySearchResponse(resp: JsonObject): DirectorySearchResults? =
  when ((resp["type"] as? JsonPrimitive)?.contentOrNull) {
    "searchResults" -> {
      val entries = (resp["entries"] as? JsonArray)?.mapNotNull {
        runCatching { json.decodeFromJsonElement<DirectorySearchEntry>(it) }.getOrNull()
      } ?: emptyList()
      val moreCount = (resp["moreCount"] as? JsonPrimitive)?.takeIf { !it.isString }?.intOrNull?.takeIf { it >= 0 }
      DirectorySearchResults(entries, resp["searchCursor"] as? JsonObject, moreCount)
    }
    else -> null
  }

sealed class DirectoryResultState {
  data class YourChat(val chatId: ChatId): DirectoryResultState()
  object Joining: DirectoryResultState()
  object Open: DirectoryResultState()
}

fun directoryResultState(plan: ConnectionPlan?): DirectoryResultState =
  when (val groupLinkPlan = (plan as? ConnectionPlan.GroupLink)?.groupLinkPlan) {
    is GroupLinkPlan.Known -> DirectoryResultState.YourChat(groupLinkPlan.groupInfo.id)
    is GroupLinkPlan.OwnLink -> DirectoryResultState.YourChat(groupLinkPlan.groupInfo.id)
    is GroupLinkPlan.ConnectingProhibit -> groupLinkPlan.groupInfo_?.let { DirectoryResultState.YourChat(it.id) } ?: DirectoryResultState.Joining
    GroupLinkPlan.ConnectingConfirmReconnect -> DirectoryResultState.Joining
    is GroupLinkPlan.Ok, is GroupLinkPlan.NoRelays, is GroupLinkPlan.UpdateRequired, null -> DirectoryResultState.Open
  }

data class DirectoryResult(
  val entry: DirectorySearchEntry,
  val state: DirectoryResultState,
) {
  val id: String get() = entry.id
}

data class OnlineSearchRows(
  val yourChats: List<ChatId>,
  val results: List<DirectoryResult>,
  val footer: DirectoryFooter?,
) {
  val isEmpty: Boolean get() = yourChats.isEmpty() && results.isEmpty() && footer == null
}
