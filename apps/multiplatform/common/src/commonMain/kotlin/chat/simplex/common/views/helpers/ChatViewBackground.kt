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
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.CurrentColors
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.StringResource
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import java.io.File
import kotlin.math.*

@Serializable
enum class PredefinedBackgroundImage(val res: ImageResource, val filename: String, val scale: Float, val text: StringResource) {
  @SerialName("cat") CAT(MR.images.background_cat, "simplex_cat", 0.5f, MR.strings.background_cat),
  @SerialName("hearts") HEARTS(MR.images.background_hearts, "simplex_hearts", 0.5f, MR.strings.background_hearts),
  @SerialName("school") SCHOOL(MR.images.background_school, "simplex_school", 0.5f, MR.strings.background_school),
  @SerialName("internet") INTERNET(MR.images.background_internet, "simplex_internet", 0.5f, MR.strings.background_internet),
  @SerialName("space") SPACE(MR.images.background_space, "simplex_space", 0.5f, MR.strings.background_space),
  @SerialName("pets") PETS(MR.images.background_pets, "simplex_pets", 0.5f, MR.strings.background_pets),
  @SerialName("rabbit") RABBIT(MR.images.background_rabbit, "simplex_rabbit", 0.5f, MR.strings.background_rabbit);

  fun toType(): BackgroundImageType =
    BackgroundImageType.Repeated(filename, scale)

  companion object {
    fun from(filename: String): PredefinedBackgroundImage? =
      entries.firstOrNull { it.filename == filename }
  }
}

@Serializable
enum class BackgroundImageScaleType(val contentScale: ContentScale, val text: StringResource) {
  @SerialName("fill") FILL(ContentScale.Crop, MR.strings.background_image_scale_fill),
  @SerialName("fit") FIT(ContentScale.Fit, MR.strings.background_image_scale_fit),
  @SerialName("repeat") REPEAT(ContentScale.Fit, MR.strings.background_image_scale_repeat),
}

@Serializable
sealed class BackgroundImageType {
  abstract val filename: String

  val image by lazy {
    val cache = cachedImage
    if (cache != null && cache.first == filename) {
      cache.second
    } else {
      val res = if (this is Repeated) {
        PredefinedBackgroundImage.from(filename)!!.res.toComposeImageBitmap()!!
      } else {
        File(getBackgroundImageFilePath(filename)).inputStream().use { loadImageBitmap(it) }
      }
      cachedImage = filename to res
      res
    }
  }

  @Serializable @SerialName("repeated") data class Repeated(
    override val filename: String,
    val scale: Float,
  ): BackgroundImageType()

  @Serializable @SerialName("static") data class Static(
    override val filename: String,
    val scale: Float,
    val scaleType: BackgroundImageScaleType,
  ): BackgroundImageType()

  val background: Color?
    get() = CurrentColors.value.wallpaper.background

  val tint: Color?
    get() = CurrentColors.value.wallpaper.tint

  val defaultBackgroundColor: Color
    @Composable get() = if (this is Static)
      MaterialTheme.colors.background
    else
      MaterialTheme.colors.background

  val defaultTintColor: Color
    @Composable get() = if (this is Repeated || (this is Static && this.scaleType == BackgroundImageScaleType.REPEAT))
      MaterialTheme.colors.primary
    else
      MaterialTheme.colors.background.copy(0.9f)


  companion object {
    val default: BackgroundImageType
      get() = PredefinedBackgroundImage.CAT.toType()

    private var cachedImage: Pair<String, ImageBitmap>? = null
  }
}

fun DrawScope.chatViewBackground(image: ImageBitmap, imageType: BackgroundImageType, defaultBackground: Color, defaultTint: Color) = clipRect {
  fun repeat(imageScale: Float) {
    val scale = imageScale * density
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
  }

  drawRect(imageType.background ?: defaultBackground)
  if (imageType is BackgroundImageType.Repeated) {
    repeat(imageType.scale)
  } else if (imageType is BackgroundImageType.Static && imageType.scaleType == BackgroundImageScaleType.REPEAT) {
    repeat(imageType.scale)
  } else if (imageType is BackgroundImageType.Static) {
    val scale = imageType.scaleType.contentScale.computeScaleFactor(Size(image.width.toFloat(), image.height.toFloat()), Size(size.width, size.height))
    val scaledWidth = (image.width * scale.scaleX).roundToInt()
    val scaledHeight = (image.height * scale.scaleY).roundToInt()
    drawImage(image, dstOffset = IntOffset(x = ((size.width - scaledWidth) / 2).roundToInt(), y = ((size.height - scaledHeight) / 2).roundToInt()), dstSize = IntSize(scaledWidth, scaledHeight))
    drawRect(imageType.tint ?: defaultTint)
  }
}
