package chat.simplex.common.model

import kotlinx.datetime.Instant
import kotlinx.serialization.Serializable
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
  val groupLink: DirectoryPublicLink,
  val shortDescr: String? = null,
  val image: String? = null,
  val activeAt: Instant? = null,
  val createdAt: Instant? = null,
) {
  // the directory drops entries with no link, but the response is untrusted input
  val connectLink: String? get() = groupLink.connShortLink ?: groupLink.connFullLink
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
