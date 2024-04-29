package chat.simplex.common.views.helpers

import androidx.compose.material.MaterialTheme
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.drawscope.DrawScope
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.unit.IntOffset
import androidx.compose.ui.unit.IntSize
import chat.simplex.res.MR
import chat.simplex.common.ui.theme.ThemeManager.colorFromReadableHex
import chat.simplex.common.ui.theme.ThemeManager.toReadableHex
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.StringResource
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlin.math.max
import kotlin.math.roundToInt

@Serializable
enum class PredefinedBackgroundImage(val res: ImageResource, val filename: String, val text: StringResource, val type: BackgroundImageType) {
  @SerialName("cat") CAT(MR.images.background_cat, "background_cat", MR.strings.background_cat, BackgroundImageType.Repeated(false, "background_cat", 1f, null));

  companion object {
    fun from(filename: String): PredefinedBackgroundImage? =
      entries.firstOrNull { it.filename == filename }
  }
}

@Serializable
enum class BackgroundImageScale(val contentScale: ContentScale) {
  @SerialName("crop") CROP(ContentScale.Crop),
  @SerialName("fit") FIT(ContentScale.Fit),
  @SerialName("fillWidth") FILL_WIDTH(ContentScale.FillWidth),
  @SerialName("fillHeight") FILL_HEIGHT(ContentScale.FillHeight),
  @SerialName("fillBounds") FILL_BOUNDS((ContentScale.FillBounds))
}

@Serializable
sealed class BackgroundImageType {
  abstract val custom: Boolean
  abstract val filename: String
  @Serializable @SerialName("repeated") data class Repeated(override val custom: Boolean = true, override val filename: String, val scale: Float, val tint: String?): BackgroundImageType()
  @Serializable @SerialName("static") data class Static(override val custom: Boolean = true, override val filename: String, val scale: BackgroundImageScale, val tint: String?): BackgroundImageType()

  val tintColor: Color? by lazy {
    when (this) {
      is Repeated -> tint?.colorFromReadableHex()
      is Static -> tint?.colorFromReadableHex()
    }
  }

  fun toPredefined(): PredefinedBackgroundImage? =
    when (this) {
      is Repeated -> if (!custom) PredefinedBackgroundImage.from(filename) else null
      is Static -> if (!custom) PredefinedBackgroundImage.from(filename) else null
    }

  companion object {
    val default: BackgroundImageType =
      Repeated(custom = false, PredefinedBackgroundImage.CAT.filename, 1f, null)
  }
}

fun DrawScope.chatViewBackground(image: ImageBitmap, imageType: BackgroundImageType, defaultTint: Color) {
  if (imageType is BackgroundImageType.Repeated) {
    val scale = imageType.scale
    for (h in 0..(size.height / image.height / scale).roundToInt()) {
      for (w in 0..(size.width / image.width / scale).roundToInt()) {
        drawImage(
          image,
          dstOffset = IntOffset(x = (w * image.width * scale).roundToInt(), y = (h * image.height * scale).roundToInt()),
          dstSize = IntSize((image.width * scale).roundToInt(), (image.height * scale).roundToInt()),
          colorFilter = ColorFilter.tint(imageType.tintColor ?: defaultTint)
        )
      }
    }
  } else if (imageType is BackgroundImageType.Static) {
    val scale = imageType.scale.contentScale.computeScaleFactor(Size(image.width.toFloat(), image.height.toFloat()), Size(size.width, size.height))
    val scaledWidth = (image.width * scale.scaleX).roundToInt()
    val scaledHeight = (image.height * scale.scaleY).roundToInt()
    drawImage(image, dstOffset = IntOffset(x = (max(0f, size.width - scaledWidth) / 2).roundToInt(), y = (max(0f, size.height - scaledHeight) / 2).roundToInt()), dstSize = IntSize(scaledWidth, scaledHeight))
  }
}