package chat.simplex.app.views.helpers

import android.graphics.Bitmap
import android.net.Uri
import androidx.compose.runtime.saveable.Saver
import kotlinx.coroutines.flow.MutableStateFlow

sealed class SharedContent {
  data class Text(val text: String): SharedContent()
  data class Images(val text: String, val uris: List<Uri>): SharedContent()
  data class File(val text: String, val uri: Uri): SharedContent()
}

enum class NewChatSheetState {
  VISIBLE, HIDING, GONE;
  fun isVisible(): Boolean {
    return this == VISIBLE
  }
  fun isHiding(): Boolean {
    return this == HIDING
  }
  fun isGone(): Boolean {
    return this == GONE
  }
  companion object {
    fun saver(): Saver<MutableStateFlow<NewChatSheetState>, *> = Saver(
      save = { it.value.toString() },
      restore = {
        MutableStateFlow(valueOf(it))
      }
    )
  }
}

sealed class UploadContent {
  data class SimpleImage(val uri: Uri): UploadContent()
  data class AnimatedImage(val uri: Uri): UploadContent()
}
