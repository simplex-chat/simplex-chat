package chat.simplex.app.views.helpers

import android.net.Uri

sealed class SharedContent {
  data class Text(val text: String): SharedContent()
  data class Image(val uri: Uri): SharedContent()
  data class File(val uri: Uri): SharedContent()
}

enum class ImageType {
  SIMPLE, ANIMATED
}
