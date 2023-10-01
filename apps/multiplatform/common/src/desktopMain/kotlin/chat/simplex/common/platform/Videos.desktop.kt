package chat.simplex.common.platform

import java.net.URI

fun isVideo(uri: URI): Boolean {
  val path = uri.path.lowercase()
  return path.endsWith(".mov") ||
      path.endsWith(".avi") ||
      path.endsWith(".mp4") ||
      path.endsWith(".mpg") ||
      path.endsWith(".mpeg") ||
      path.endsWith(".mkv")
}
