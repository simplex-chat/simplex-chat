package chat.simplex.common.views.helpers

import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.drawscope.DrawScope
import androidx.compose.ui.graphics.drawscope.clipRect
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.unit.IntOffset
import androidx.compose.ui.unit.IntSize
import chat.simplex.common.ui.theme.CurrentColors
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.StringResource
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlin.math.*

@Serializable
enum class PredefinedBackgroundImage(val res: ImageResource, val filename: String, val text: StringResource, val type: BackgroundImageType) {
  @SerialName("cat") CAT(MR.images.background_cat, "simplex_cat", MR.strings.background_cat, BackgroundImageType.Repeated("simplex_cat", 0.5f)),
  @SerialName("hearts") HEARTS(MR.images.background_hearts, "simplex_hearts", MR.strings.background_hearts, BackgroundImageType.Repeated("simplex_hearts", 0.5f)),
  @SerialName("school") SCHOOL(MR.images.background_school, "simplex_school", MR.strings.background_school, BackgroundImageType.Repeated("simplex_school", 0.5f)),
  @SerialName("internet") INTERNET(MR.images.background_internet, "simplex_internet", MR.strings.background_internet, BackgroundImageType.Repeated("simplex_internet", 0.5f)),
  @SerialName("space") SPACE(MR.images.background_space, "simplex_space", MR.strings.background_space, BackgroundImageType.Repeated("simplex_space", 0.5f)),
  @SerialName("pets") PETS(MR.images.background_pets, "simplex_pets", MR.strings.background_pets, BackgroundImageType.Repeated("simplex_pets", 0.5f)),
  @SerialName("rabbit") RABBIT(MR.images.background_rabbit, "simplex_rabbit", MR.strings.background_rabbit, BackgroundImageType.Repeated("simplex_rabbit", 0.5f));

  companion object {
    fun from(filename: String): PredefinedBackgroundImage? =
      entries.firstOrNull { it.filename == filename }
  }
}

@Serializable
enum class BackgroundImageScale(val contentScale: ContentScale, val text: StringResource) {
  @SerialName("crop") CROP(ContentScale.Crop, MR.strings.background_image_scale_crop),
  @SerialName("fit") FIT(ContentScale.Fit, MR.strings.background_image_scale_fit),
  @SerialName("fillWidth") FILL_WIDTH(ContentScale.FillWidth, MR.strings.background_image_scale_fill_width),
  @SerialName("fillHeight") FILL_HEIGHT(ContentScale.FillHeight, MR.strings.background_image_scale_fill_height),
  @SerialName("fillBounds") FILL_BOUNDS(ContentScale.FillBounds, MR.strings.background_image_scale_fill_bounds)
}

@Serializable
sealed class BackgroundImageType {
  abstract val filename: String
  @Serializable @SerialName("repeated") data class Repeated(
    override val filename: String,
    val scale: Float,
  ): BackgroundImageType()

  @Serializable @SerialName("static") data class Static(
    override val filename: String,
    val scale: BackgroundImageScale,
  ): BackgroundImageType()

  val background: Color?
    get() = CurrentColors.value.appColors.wallpaperBackground

  val tint: Color?
    get() = CurrentColors.value.appColors.wallpaperTint

  fun toPredefined(): PredefinedBackgroundImage? = PredefinedBackgroundImage.from(filename)

  val defaultBackgroundColor: Color
    @Composable get() = if (this is Static) MaterialTheme.colors.background else MaterialTheme.colors.background

  val defaultTintColor: Color
    @Composable get() = if (this is Static) MaterialTheme.colors.background.copy(0.9f) else MaterialTheme.colors.primary


  companion object {
    val default: BackgroundImageType =
      Repeated(PredefinedBackgroundImage.CAT.filename, 1f)
  }
}

fun DrawScope.chatViewBackground(image: ImageBitmap, imageType: BackgroundImageType, defaultBackground: Color, defaultTint: Color) = clipRect {
  drawRect(imageType.background ?: defaultBackground)
  if (imageType is BackgroundImageType.Repeated) {
    val scale = imageType.scale * density
    for (h in 0..(size.height / image.height / scale).roundToInt()) {
      for (w in 0..(size.width / image.width / scale).roundToInt()) {
        drawImage(
          image,
          dstOffset = IntOffset(x = (w * image.width * scale).roundToInt(), y = (h * image.height * scale).roundToInt()),
          dstSize = IntSize((image.width * scale).roundToInt(), (image.height * scale).roundToInt()),
          colorFilter = ColorFilter.tint(imageType.tint ?: defaultTint)
        )
      }
    }
  } else if (imageType is BackgroundImageType.Static) {
    val scale = imageType.scale.contentScale.computeScaleFactor(Size(image.width.toFloat(), image.height.toFloat()), Size(size.width, size.height))
    val scaledWidth = (image.width * scale.scaleX).roundToInt()
    val scaledHeight = (image.height * scale.scaleY).roundToInt()
    drawImage(image, dstOffset = IntOffset(x = ((size.width - scaledWidth) / 2).roundToInt(), y = ((size.height - scaledHeight) / 2).roundToInt()), dstSize = IntSize(scaledWidth, scaledHeight))
    drawRect(imageType.tint ?: defaultTint)
  }
}
