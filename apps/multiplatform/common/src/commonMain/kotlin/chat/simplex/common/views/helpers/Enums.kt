@file:UseSerializers(UriSerializer::class)
package chat.simplex.common.views.helpers

import androidx.compose.runtime.saveable.Saver
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.serialization.*
import kotlinx.serialization.descriptors.*
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import java.net.URI

sealed class SharedContent {
  data class Text(val text: String): SharedContent()
  data class Media(val text: String, val uris: List<URI>): SharedContent()
  data class File(val text: String, val uri: URI): SharedContent()
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


@Serializer(forClass = URI::class)
object UriSerializer : KSerializer<URI> {
  override val descriptor: SerialDescriptor = PrimitiveSerialDescriptor("URI", PrimitiveKind.STRING)
  override fun serialize(encoder: Encoder, value: URI) = encoder.encodeString(value.toString())
  override fun deserialize(decoder: Decoder): URI = URI(decoder.decodeString())
}

@Serializable
sealed class UploadContent {
  @Serializable data class SimpleImage(val uri: URI): UploadContent()
  @Serializable data class AnimatedImage(val uri: URI): UploadContent()
  @Serializable data class Video(val uri: URI, val duration: Int): UploadContent()
}
