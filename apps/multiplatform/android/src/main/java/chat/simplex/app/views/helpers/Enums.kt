@file:UseSerializers(UriSerializer::class)
package chat.simplex.app.views.helpers

import android.net.Uri
import androidx.compose.runtime.saveable.Saver
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.serialization.*
import kotlinx.serialization.descriptors.*
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder

sealed class SharedContent {
  data class Text(val text: String): SharedContent()
  data class Media(val text: String, val uris: List<Uri>): SharedContent()
  data class File(val text: String, val uri: Uri): SharedContent()
}

enum class AnimatedViewState {
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
    fun saver(): Saver<MutableStateFlow<AnimatedViewState>, *> = Saver(
      save = { it.value.toString() },
      restore = {
        MutableStateFlow(valueOf(it))
      }
    )
  }
}


@Serializer(forClass = Uri::class)
object UriSerializer : KSerializer<Uri> {
  override val descriptor: SerialDescriptor = PrimitiveSerialDescriptor("Uri", PrimitiveKind.STRING)
  override fun serialize(encoder: Encoder, value: Uri) = encoder.encodeString(value.toString())
  override fun deserialize(decoder: Decoder): Uri = Uri.parse(decoder.decodeString())
}

@Serializable
sealed class UploadContent {
  @Serializable data class SimpleImage(val uri: Uri): UploadContent()
  @Serializable data class AnimatedImage(val uri: Uri): UploadContent()
  @Serializable data class Video(val uri: Uri, val duration: Int): UploadContent()
}
