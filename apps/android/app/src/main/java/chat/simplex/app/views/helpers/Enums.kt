package chat.simplex.app.views.helpers

import android.net.Uri
import android.os.Parcel
import android.os.Parcelable
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

/**
 * [Parcelable] implementation is needed to prevent a crash. Without it this situation produce a crash:
 * - select images in SendMsgView
 * - do this again without sending or dismissing previous selection
 * */
sealed class UploadContent: Parcelable {
  data class SimpleImage(val uri: Uri): UploadContent(), Parcelable {
    constructor(parcel: Parcel): this(parcel.readParcelable<Uri>(Uri::class.java.classLoader) as Uri)
    override fun describeContents(): Int = 0
    override fun writeToParcel(dest: Parcel?, flags: Int) {
      dest?.writeParcelable(uri, 0)
    }
    companion object CREATOR: Parcelable.Creator<SimpleImage> {
      override fun createFromParcel(parcel: Parcel): SimpleImage {
        return SimpleImage(parcel)
      }
      override fun newArray(size: Int): Array<SimpleImage?> {
        return arrayOfNulls(size)
      }
    }
  }
  data class AnimatedImage(val uri: Uri): UploadContent(), Parcelable {
    constructor(parcel: Parcel): this(parcel.readParcelable<Uri>(Uri::class.java.classLoader) as Uri)
    override fun describeContents(): Int = 0
    override fun writeToParcel(dest: Parcel?, flags: Int) {
      dest?.writeParcelable(uri, 0)
    }
    companion object CREATOR: Parcelable.Creator<AnimatedImage> {
      override fun createFromParcel(parcel: Parcel): AnimatedImage {
        return AnimatedImage(parcel)
      }
      override fun newArray(size: Int): Array<AnimatedImage?> {
        return arrayOfNulls(size)
      }
    }
  }
}
