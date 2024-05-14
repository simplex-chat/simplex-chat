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
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.StringResource
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import java.io.File
import kotlin.math.*

@Serializable
enum class PredefinedBackgroundImage(
  val res: ImageResource,
  val filename: String,
  val text: StringResource,
  val scale: Float,
  val background: Map<DefaultTheme, Color>,
  val tint: Map<DefaultTheme, Color>,
  val colors: Map<DefaultTheme, ThemeColors>,
) {
  @SerialName("cats") CATS(MR.images.background_cats, "cats", MR.strings.background_cats, 0.5f,
    mapOf(DefaultTheme.LIGHT to Color.White, DefaultTheme.DARK to Color.Black, DefaultTheme.SIMPLEX to Color.Black),
    mapOf(DefaultTheme.LIGHT to Color.Blue, DefaultTheme.DARK to Color.Blue, DefaultTheme.SIMPLEX to Color.Blue),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
//       primary = "#ff000000"
      ),
      DefaultTheme.DARK to ThemeColors(
//        primary = "#ff000000"
      ),
      DefaultTheme.SIMPLEX to ThemeColors(
//        primary = "#ff000000"
      )
    )
  ),
  @SerialName("hearts") HEARTS(MR.images.background_hearts, "hearts", MR.strings.background_hearts, 0.5f,
    mapOf(DefaultTheme.LIGHT to Color.White, DefaultTheme.DARK to Color.Black, DefaultTheme.SIMPLEX to Color.Black),
    mapOf(DefaultTheme.LIGHT to Color.Blue, DefaultTheme.DARK to Color.Blue, DefaultTheme.SIMPLEX to Color.Blue),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
      ),
      DefaultTheme.DARK to ThemeColors(
      ),
      DefaultTheme.SIMPLEX to ThemeColors(
      )
    )
  ),
  @SerialName("school") SCHOOL(MR.images.background_school, "school",  MR.strings.background_school, 0.5f,
  mapOf(DefaultTheme.LIGHT to Color.White, DefaultTheme.DARK to Color.Black, DefaultTheme.SIMPLEX to Color.Black),
  mapOf(DefaultTheme.LIGHT to Color.Blue, DefaultTheme.DARK to Color.Blue, DefaultTheme.SIMPLEX to Color.Blue),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
      ),
      DefaultTheme.DARK to ThemeColors(
      ),
      DefaultTheme.SIMPLEX to ThemeColors(
      )
    )
  ),
  @SerialName("internet") INTERNET(MR.images.background_internet, "internet", MR.strings.background_internet, 0.5f,
  mapOf(DefaultTheme.LIGHT to Color.White, DefaultTheme.DARK to Color.Black, DefaultTheme.SIMPLEX to Color.Black),
  mapOf(DefaultTheme.LIGHT to Color.Blue, DefaultTheme.DARK to Color.Blue, DefaultTheme.SIMPLEX to Color.Blue),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
      ),
      DefaultTheme.DARK to ThemeColors(
      ),
      DefaultTheme.SIMPLEX to ThemeColors(
      )
    )
  ),
  @SerialName("travel") TRAVEL(MR.images.background_travel, "travel", MR.strings.background_travel, 0.5f,
  mapOf(DefaultTheme.LIGHT to Color.White, DefaultTheme.DARK to Color.Black, DefaultTheme.SIMPLEX to Color.Black),
  mapOf(DefaultTheme.LIGHT to Color.Blue, DefaultTheme.DARK to Color.Blue, DefaultTheme.SIMPLEX to Color.Blue),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
      ),
      DefaultTheme.DARK to ThemeColors(
      ),
      DefaultTheme.SIMPLEX to ThemeColors(
      )
    )
  ),
  @SerialName("pets") PETS(MR.images.background_pets, "pets", MR.strings.background_pets, 0.5f,
  mapOf(DefaultTheme.LIGHT to Color.White, DefaultTheme.DARK to Color.Black, DefaultTheme.SIMPLEX to Color.Black),
  mapOf(DefaultTheme.LIGHT to Color.Blue, DefaultTheme.DARK to Color.Blue, DefaultTheme.SIMPLEX to Color.Blue),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
      ),
      DefaultTheme.DARK to ThemeColors(
      ),
      DefaultTheme.SIMPLEX to ThemeColors(
      )
    )
  ),
  @SerialName("space") SPACE(MR.images.background_space, "space", MR.strings.background_space, 0.5f,
  mapOf(DefaultTheme.LIGHT to Color.White, DefaultTheme.DARK to Color.Black, DefaultTheme.SIMPLEX to Color.Black),
  mapOf(DefaultTheme.LIGHT to Color.Blue, DefaultTheme.DARK to Color.Blue, DefaultTheme.SIMPLEX to Color.Blue),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
      ),
      DefaultTheme.DARK to ThemeColors(
      ),
      DefaultTheme.SIMPLEX to ThemeColors(
      )
    )
  ),
  @SerialName("flowers") FLOWERS(MR.images.background_flowers, "flowers", MR.strings.background_flowers, 0.5f,
    mapOf(DefaultTheme.LIGHT to Color.White, DefaultTheme.DARK to Color.Black, DefaultTheme.SIMPLEX to Color.Black),
    mapOf(DefaultTheme.LIGHT to Color.Blue, DefaultTheme.DARK to Color.Blue, DefaultTheme.SIMPLEX to Color.Blue),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
      ),
      DefaultTheme.DARK to ThemeColors(
      ),
      DefaultTheme.SIMPLEX to ThemeColors(
      )
    )
  ),
  @SerialName("kids") KIDS(MR.images.background_kids, "kids", MR.strings.background_kids, 0.5f,
    mapOf(DefaultTheme.LIGHT to Color.White, DefaultTheme.DARK to Color.Black, DefaultTheme.SIMPLEX to Color.Black),
    mapOf(DefaultTheme.LIGHT to Color.Blue, DefaultTheme.DARK to Color.Blue, DefaultTheme.SIMPLEX to Color.Blue),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
      ),
      DefaultTheme.DARK to ThemeColors(
      ),
      DefaultTheme.SIMPLEX to ThemeColors(
      )
    )
  ),
  @SerialName("social") SOCIAL(MR.images.background_social, "social", MR.strings.background_social, 0.5f,
    mapOf(DefaultTheme.LIGHT to Color.White, DefaultTheme.DARK to Color.Black, DefaultTheme.SIMPLEX to Color.Black),
    mapOf(DefaultTheme.LIGHT to Color.Blue, DefaultTheme.DARK to Color.Blue, DefaultTheme.SIMPLEX to Color.Blue),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
      ),
      DefaultTheme.DARK to ThemeColors(
      ),
      DefaultTheme.SIMPLEX to ThemeColors(
      )
    )
  );

  fun toType(scale: Float? = null): BackgroundImageType =
    BackgroundImageType.Repeated(
      filename,
      scale ?: appPrefs.themeOverrides.get().firstOrNull { it.wallpaper != null && it.wallpaper.preset == filename && it.base == CurrentColors.value.base }?.wallpaper?.scale ?: 1f
    )

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
  abstract val scale: Float

  val image by lazy {
    val cache = cachedImage
    if (cache != null && cache.first == filename) {
      cache.second
    } else {
      val res = if (this is Repeated) {
        (PredefinedBackgroundImage.from(filename) ?: PredefinedBackgroundImage.CATS).res.toComposeImageBitmap()!!
      } else {
        try {
          // In case of unintentional image deletion don't crash the app
          File(getBackgroundImageFilePath(filename)).inputStream().use { loadImageBitmap(it) }
        } catch (e: Exception) {
          Log.e(TAG, "Error while loading background image: ${e.stackTraceToString()}")
          null
        }
      }
      cachedImage = if (res != null) filename to res else null
      res
    }
  }

  @Serializable @SerialName("repeated") data class Repeated(
    override val filename: String,
    override val scale: Float,
  ): BackgroundImageType() {
    val predefinedImageScale = PredefinedBackgroundImage.from(filename)?.scale ?: 1f
  }

  @Serializable @SerialName("static") data class Static(
    override val filename: String,
    override val scale: Float,
    val scaleType: BackgroundImageScaleType,
  ): BackgroundImageType()

  @Composable
  fun defaultBackgroundColor(theme: DefaultTheme): Color =
    if (this is Repeated) {
      (PredefinedBackgroundImage.from(filename) ?: PredefinedBackgroundImage.CATS).background[theme]!!
    } else {
      MaterialTheme.colors.background
    }

  @Composable
  fun defaultTintColor(theme: DefaultTheme): Color =
    if (this is Repeated) {
      (PredefinedBackgroundImage.from(filename) ?: PredefinedBackgroundImage.CATS).tint[theme]!!
    } else if (this is Static && scaleType == BackgroundImageScaleType.REPEAT) {
      Color.Transparent
    } else {
      Color.Transparent
    }

  companion object {
    val default: BackgroundImageType
      get() = PredefinedBackgroundImage.CATS.toType()

    private var cachedImage: Pair<String, ImageBitmap>? = null

    fun from(wallpaper: ThemeWallpaper?): BackgroundImageType? {
      return if (wallpaper?.preset != null) {
        Repeated(wallpaper.preset, wallpaper.scale ?: 1f)
      } else {
        Static(wallpaper?.imageFile ?: return null, wallpaper.scale ?: 1f, wallpaper.scaleType ?: BackgroundImageScaleType.FILL)
      }
    }
  }
}

fun DrawScope.chatViewBackground(image: ImageBitmap, imageType: BackgroundImageType, background: Color, tint: Color) = clipRect {
  fun repeat(imageScale: Float) {
    val scale = imageScale * density
    for (h in 0..(size.height / image.height / scale).roundToInt()) {
      for (w in 0..(size.width / image.width / scale).roundToInt()) {
        drawImage(
          image,
          dstOffset = IntOffset(x = (w * image.width * scale).roundToInt(), y = (h * image.height * scale).roundToInt()),
          dstSize = IntSize((image.width * scale).roundToInt(), (image.height * scale).roundToInt()),
          colorFilter = ColorFilter.tint(tint, BlendMode.SrcIn)
        )
      }
    }
  }

  drawRect(background)
  when (imageType) {
    is BackgroundImageType.Repeated -> repeat(imageType.scale * imageType.predefinedImageScale)
    is BackgroundImageType.Static -> when (imageType.scaleType) {
      BackgroundImageScaleType.REPEAT -> repeat(imageType.scale)
      BackgroundImageScaleType.FILL, BackgroundImageScaleType.FIT -> {
        val scale = imageType.scaleType.contentScale.computeScaleFactor(Size(image.width.toFloat(), image.height.toFloat()), Size(size.width, size.height))
        val scaledWidth = (image.width * scale.scaleX).roundToInt()
        val scaledHeight = (image.height * scale.scaleY).roundToInt()
        drawImage(image, dstOffset = IntOffset(x = ((size.width - scaledWidth) / 2).roundToInt(), y = ((size.height - scaledHeight) / 2).roundToInt()), dstSize = IntSize(scaledWidth, scaledHeight))
        if (imageType.scaleType == BackgroundImageScaleType.FIT) {
          if (scaledWidth < size.width) {
            // has black lines at left and right sides
            var x = (size.width - scaledWidth) / 2
            while (x > 0) {
              drawImage(image, dstOffset = IntOffset(x = (x - scaledWidth).roundToInt(), y = ((size.height - scaledHeight) / 2).roundToInt()), dstSize = IntSize(scaledWidth, scaledHeight))
              x -= scaledWidth
            }
            x = size.width - (size.width - scaledWidth) / 2
            while (x < size.width) {
              drawImage(image, dstOffset = IntOffset(x = x.roundToInt(), y = ((size.height - scaledHeight) / 2).roundToInt()), dstSize = IntSize(scaledWidth, scaledHeight))
              x += scaledWidth
            }
          } else {
            // has black lines at top and bottom sides
            var y = (size.height - scaledHeight) / 2
            while (y > 0) {
              drawImage(image, dstOffset = IntOffset(x = ((size.width - scaledWidth) / 2).roundToInt(), y = (y - scaledHeight).roundToInt()), dstSize = IntSize(scaledWidth, scaledHeight))
              y -= scaledHeight
            }
            y = size.height - (size.height - scaledHeight) / 2
            while (y < size.height) {
              drawImage(image, dstOffset = IntOffset(x = ((size.width - scaledWidth) / 2).roundToInt(), y = y.roundToInt()), dstSize = IntSize(scaledWidth, scaledHeight))
              y += scaledHeight
            }
          }
        }
        drawRect(tint)
      }
    }
  }
}
