package chat.simplex.common.platform

import java.net.URI

internal val videoFileExtensions = listOf("mov", "avi", "mp4", "mpg", "mpeg", "mkv", "webm")

fun isVideo(uri: URI): Boolean = hasFileExtension(uri.toFile().path, videoFileExtensions)

internal fun hasFileExtension(path: String, extensions: List<String>): Boolean {
  val lowercased = path.lowercase()
  return extensions.any { lowercased.endsWith(".$it") }
}
