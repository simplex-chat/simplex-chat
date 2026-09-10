package chat.simplex.common.model

import kotlinx.datetime.Instant
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.*

// The directory's contact address, as published in docs/DIRECTORY.md. It must be the short
// link: only that form carries the address DR keys that service requests require, so the full
// links on the What's New cards cannot be substituted here.
const val DIRECTORY_SERVICE_LINK = "https://smp4.simplex.im/a#lXUjJW5vHYQzoLYgmi8GbxkGP41_kjefFvBrdwg-0Ok"

// A service request is a full DR handshake, so it is slower than a local API call; the user
// gets a cancellable spinner while it runs and a retry row if it times out.
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

// The response is a tagged object: searchResults or error. Anything else is treated as a failure
// rather than parsed leniently - it comes from outside the app.
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
