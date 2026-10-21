package chat.simplex.common.model

import chat.simplex.common.views.chatlist.DirectoryFooter
import chat.simplex.common.views.newchat.ConnectTarget
import chat.simplex.common.views.newchat.strConnectTarget
import kotlinx.datetime.Instant
import kotlinx.serialization.SerializationException
import kotlinx.serialization.Serializable
import kotlinx.serialization.Transient
import kotlinx.serialization.json.*

// The directory's address, as published in docs/DIRECTORY.md. It must be the short link form -
// only that one carries the keys a service request needs; a full link fails every time.
const val DIRECTORY_SERVICE_LINK = "https://smp4.simplex.im/a#lXUjJW5vHYQzoLYgmi8GbxkGP41_kjefFvBrdwg-0Ok"

// A search sets up an encrypted connection, so it takes seconds, not milliseconds.
const val DIRECTORY_SEARCH_TIMEOUT_SEC = 10.0

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
  val image: String? = null,
  val activeAt: Instant? = null,
  val createdAt: Instant? = null,
) {
  // the link is stable and unique per entry, so it also de-duplicates across pages
  val id: String get() = link

  // untrusted input: only group and channel links are kept, as the core fetches an invitation link even in a store-only plan
  // @Transient: the decoder always runs this check, and a "link" key in the JSON cannot replace it
  @Transient
  val link: String = run {
    val link = groupLink.connShortLink ?: groupLink.connFullLink
    val linkType = if (link != null) simplexLinkType(link) else null
    if (link == null || linkType == null || !(linkType == SimplexLinkType.group || linkType == SimplexLinkType.channel)) {
      throw SerializationException("not a group or channel link")
    }
    link
  }
}

fun simplexLinkType(s: String): SimplexLinkType? {
  val t = strConnectTarget(s)
  return if (t is ConnectTarget.Link && t.text == s) t.linkType else null
}

data class DirectorySearchResults(
  val entries: List<DirectorySearchEntry>,
  // opaque: stored and echoed back on the next request, never inspected
  val cursor: JsonObject?,
  val moreCount: Int?,
)

fun directorySearchRequest(text: String, cursor: JsonObject?): JsonObject = buildJsonObject {
  put("type", JsonPrimitive("search"))
  put("searchText", JsonPrimitive(text))
  if (cursor != null) put("searchCursor", cursor)
}

// Anything but a well-formed results response is a failure, not something to salvage: it comes
// from another party, not from our own core.
fun parseDirectorySearchResponse(resp: JsonObject): DirectorySearchResults? =
  when ((resp["type"] as? JsonPrimitive)?.contentOrNull) {
    "searchResults" -> {
      val entries = (resp["entries"] as? JsonArray)?.mapNotNull {
        runCatching { json.decodeFromJsonElement<DirectorySearchEntry>(it) }.getOrNull()
      } ?: emptyList()
      // only shown as text, so a count out of range is dropped rather than failing the response
      val moreCount = (resp["moreCount"] as? JsonPrimitive)?.takeIf { !it.isString }?.intOrNull?.takeIf { it >= 0 }
      DirectorySearchResults(entries, resp["searchCursor"] as? JsonObject, moreCount)
    }
    else -> null
  }

fun planLocalChatId(plan: ConnectionPlan): ChatId? =
  when (plan) {
    is ConnectionPlan.ContactAddress -> when (val contactAddressPlan = plan.contactAddressPlan) {
      is ContactAddressPlan.Known -> contactAddressPlan.contact.id
      is ContactAddressPlan.ConnectingProhibit -> contactAddressPlan.contact.id
      is ContactAddressPlan.ContactViaAddress -> contactAddressPlan.contact.id
      is ContactAddressPlan.Ok, ContactAddressPlan.OwnLink, ContactAddressPlan.ConnectingConfirmReconnect -> null
    }
    is ConnectionPlan.GroupLink -> when (val groupLinkPlan = plan.groupLinkPlan) {
      is GroupLinkPlan.Known -> groupLinkPlan.groupInfo.id
      is GroupLinkPlan.OwnLink -> groupLinkPlan.groupInfo.id
      is GroupLinkPlan.ConnectingProhibit -> groupLinkPlan.groupInfo_?.id
      is GroupLinkPlan.Ok, GroupLinkPlan.ConnectingConfirmReconnect, is GroupLinkPlan.NoRelays, is GroupLinkPlan.UpdateRequired -> null
    }
    is ConnectionPlan.InvitationLink -> when (val invitationLinkPlan = plan.invitationLinkPlan) {
      is InvitationLinkPlan.Known -> invitationLinkPlan.contact.id
      is InvitationLinkPlan.Connecting -> invitationLinkPlan.contact_?.id
      is InvitationLinkPlan.Ok, InvitationLinkPlan.OwnLink -> null
    }
    is ConnectionPlan.Error -> null
  }

sealed class DirectoryResultState {
  data class YourChat(val chatId: ChatId): DirectoryResultState()
  object Joining: DirectoryResultState()
  object Open: DirectoryResultState()
}

// no plan means the store doesn't know the link, or the plan failed and Open's online plan will show why
fun directoryResultState(plan: ConnectionPlan?): DirectoryResultState {
  val chatId = plan?.let { planLocalChatId(it) }
  if (chatId != null) return DirectoryResultState.YourChat(chatId)
  val groupLinkPlan = (plan as? ConnectionPlan.GroupLink)?.groupLinkPlan
  return if (groupLinkPlan == GroupLinkPlan.ConnectingConfirmReconnect || (groupLinkPlan is GroupLinkPlan.ConnectingProhibit && groupLinkPlan.groupInfo_ == null)) {
    DirectoryResultState.Joining
  } else {
    DirectoryResultState.Open
  }
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
  val pageInChats: Boolean,
  val footer: DirectoryFooter?,
) {
  val isEmpty: Boolean get() = yourChats.isEmpty() && results.isEmpty() && !pageInChats && footer == null
}
