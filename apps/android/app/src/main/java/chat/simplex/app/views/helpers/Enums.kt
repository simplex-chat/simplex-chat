package chat.simplex.app.views.helpers

import android.net.Uri
import androidx.compose.runtime.saveable.Saver
import kotlinx.coroutines.flow.MutableStateFlow

sealed class SharedContent {
  data class Text(val text: String): SharedContent()
  data class Image(val uri: Uri): SharedContent()
  data class File(val uri: Uri): SharedContent()
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

enum class ImageType {
  SIMPLE, ANIMATED
}
