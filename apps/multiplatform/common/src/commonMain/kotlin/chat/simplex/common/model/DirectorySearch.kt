package chat.simplex.common.model

import chat.simplex.common.views.chatlist.DirectoryFooter
import chat.simplex.common.views.newchat.ConnectTarget
import chat.simplex.common.views.newchat.strConnectTarget
import chat.simplex.res.*
import dev.icerock.moko.resources.ImageResource
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
      DirectorySearchResults(entries, resp["searchCursor"] as? JsonObject)
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

class NameSearchResult private constructor(
  val target: String,
  val simplexName: String,
  val profileName: String?,
  val descr: String?,
  val image: String?,
  val iconName: ImageResource,
  val localChatId: ChatId?,
  // the link the core resolved the name to, never a profile field the name's owner wrote
  val resolvedLink: String?,
) {
  val id: String get() = target

  fun withLocalChatId(localChatId: ChatId?): NameSearchResult =
    NameSearchResult(target, simplexName, profileName, descr, image, iconName, localChatId, resolvedLink)

  companion object {
    operator fun invoke(target: String, result: ConnectionPlanResult): NameSearchResult? {
      val profile: NamedChat?
      val iconName: ImageResource
      val resolvedLink: String?
      when (val connectionPlan = result.connectionPlan) {
        is ConnectionPlan.ContactAddress -> {
          profile = when (val contactAddressPlan = connectionPlan.contactAddressPlan) {
            is ContactAddressPlan.Ok -> contactAddressPlan.contactSLinkData_?.profile
            is ContactAddressPlan.ContactViaAddress -> contactAddressPlan.contact.profile
            is ContactAddressPlan.ConnectingProhibit -> contactAddressPlan.contact.profile
            is ContactAddressPlan.Known -> contactAddressPlan.contact.profile
            ContactAddressPlan.ConnectingConfirmReconnect -> null
            ContactAddressPlan.OwnLink -> return null
          }
          iconName = MR.images.ic_account_circle_filled
          resolvedLink = null
        }
        is ConnectionPlan.GroupLink -> {
          val groupProfile: GroupProfile? = when (val groupLinkPlan = connectionPlan.groupLinkPlan) {
            is GroupLinkPlan.Ok -> groupLinkPlan.groupSLinkData_?.groupProfile
            is GroupLinkPlan.NoRelays -> groupLinkPlan.groupSLinkData_?.groupProfile
            is GroupLinkPlan.UpdateRequired -> groupLinkPlan.groupSLinkData_?.groupProfile
            is GroupLinkPlan.Known -> groupLinkPlan.groupInfo.groupProfile
            is GroupLinkPlan.OwnLink -> groupLinkPlan.groupInfo.groupProfile
            is GroupLinkPlan.ConnectingProhibit -> groupLinkPlan.groupInfo_?.groupProfile
            GroupLinkPlan.ConnectingConfirmReconnect -> null
          }
          profile = groupProfile
          iconName = if (groupProfile?.isChannel == true) MR.images.ic_bigtop_updates_circle_filled else MR.images.ic_supervised_user_circle_filled
          resolvedLink = result.connLink.connShortLink
        }
        is ConnectionPlan.InvitationLink, is ConnectionPlan.Error -> return null
      }
      return NameSearchResult(
        target = target,
        simplexName = result.planSimplexName?.shortStr ?: target,
        profileName = profile?.displayName,
        descr = profile?.shortDescr,
        image = profile?.image,
        iconName = iconName,
        localChatId = planLocalChatId(result.connectionPlan),
        resolvedLink = resolvedLink,
      )
    }
  }
}

data class DirectoryResult(
  val entry: DirectorySearchEntry,
  val localChatId: ChatId?,
) {
  val id: String get() = entry.id
}

data class OnlineSearchRows(
  val names: List<NameSearchResult>,
  val directory: List<DirectoryResult>,
  val pageInChats: Boolean,
  val footer: DirectoryFooter?,
) {
  val isEmpty: Boolean get() = names.isEmpty() && directory.isEmpty() && !pageInChats && footer == null
}
