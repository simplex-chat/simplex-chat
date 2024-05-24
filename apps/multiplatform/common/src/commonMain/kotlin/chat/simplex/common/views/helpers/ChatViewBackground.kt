package chat.simplex.common.views.helpers

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
import chat.simplex.common.ui.theme.ThemeManager.colorFromReadableHex
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
  @SerialName("cats") CATS(MR.images.background_cats, "cats", MR.strings.background_cats, 0.63f,
    mapOf(DefaultTheme.LIGHT to "#ffF8F6EA".colorFromReadableHex(), DefaultTheme.DARK to "#ff121212".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff111528".colorFromReadableHex(), DefaultTheme.BLACK to "#ff121212".colorFromReadableHex()),
    mapOf(DefaultTheme.LIGHT to "#ffEFDCA6".colorFromReadableHex(), DefaultTheme.DARK to "#ff4B0E3A".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff4A104A".colorFromReadableHex(), DefaultTheme.BLACK to "#ff4B0E3A".colorFromReadableHex()),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
        sentMessage = "#ffFFF9EA",
        sentQuote = "#ffFAF0D6",
        receivedMessage = "#ffF8F7F4",
        receivedQuote = "#ffEFEDE7",
      ),
      DefaultTheme.DARK to ThemeColors(
        sentMessage = "#ff321027",
        sentQuote = "#ff4D0E3C",
        receivedMessage = "#ff2A2628",
        receivedQuote = "#ff42393E",
      ),
      DefaultTheme.SIMPLEX to ThemeColors(
        sentMessage = "#ff4C1F4E",
        sentQuote = "#ff782870",
        receivedMessage = "#ff29283B",
        receivedQuote = "#ff413B4E",
      ),
      DefaultTheme.BLACK to ThemeColors(
        sentMessage = "#ff4C1F4E",
        sentQuote = "#ff782870",
        receivedMessage = "#ff29283B",
        receivedQuote = "#ff413B4E",
      ),
    )
  ),
  @SerialName("flowers") FLOWERS(MR.images.background_flowers, "flowers", MR.strings.background_flowers, 0.53f,
    mapOf(DefaultTheme.LIGHT to "#ffE2FFE4".colorFromReadableHex(), DefaultTheme.DARK to "#ff121212".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff111528".colorFromReadableHex(), DefaultTheme.BLACK to "#ff121212".colorFromReadableHex()),
    mapOf(DefaultTheme.LIGHT to "#ff9CEA59".colorFromReadableHex(), DefaultTheme.DARK to "#ff31560D".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff31570D".colorFromReadableHex(), DefaultTheme.BLACK to "#ff31560D".colorFromReadableHex()),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
        sentMessage = "#ffEBFEDC",
        sentQuote = "#ffD7F6BE",
        receivedMessage = "#ffF4F8F2",
        receivedQuote = "#ffE7EEE3",
      ),
      DefaultTheme.DARK to ThemeColors(
        sentMessage = "#ff163521",
        sentQuote = "#ff1B5330",
        receivedMessage = "#ff242523",
        receivedQuote = "#ff353733",
      ),
      DefaultTheme.SIMPLEX to ThemeColors(
        sentMessage = "#ff184739",
        sentQuote = "#ff1F6F4B",
        receivedMessage = "#ff232736",
        receivedQuote = "#ff343944",
      ),
      DefaultTheme.BLACK to ThemeColors(
        sentMessage = "#ff184739",
        sentQuote = "#ff1F6F4B",
        receivedMessage = "#ff232736",
        receivedQuote = "#ff343944",
      ),
    )
  ),
  @SerialName("hearts") HEARTS(MR.images.background_hearts, "hearts", MR.strings.background_hearts, 0.59f,
    mapOf(DefaultTheme.LIGHT to "#ffFDECEC".colorFromReadableHex(), DefaultTheme.DARK to "#ff121212".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff111528".colorFromReadableHex(), DefaultTheme.BLACK to "#ff121212".colorFromReadableHex()),
    mapOf(DefaultTheme.LIGHT to "#ffFCDADA".colorFromReadableHex(), DefaultTheme.DARK to "#ff3C0F0F".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff381221".colorFromReadableHex(), DefaultTheme.BLACK to "#ff3C0F0F".colorFromReadableHex()),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
        sentMessage = "#ffFFF1F1",
        sentQuote = "#ffFBE3E3",
        receivedMessage = "#ffF8F5F5",
        receivedQuote = "#ffEFE9E9",
      ),
      DefaultTheme.DARK to ThemeColors(
        sentMessage = "#ff301515",
        sentQuote = "#ff4C1818",
        receivedMessage = "#ff292626",
        receivedQuote = "#ff403939",
      ),
      DefaultTheme.SIMPLEX to ThemeColors(
        sentMessage = "#ff491A28",
        sentQuote = "#ff761F29",
        receivedMessage = "#ff282839",
        receivedQuote = "#ff3F3B4A",
      ),
      DefaultTheme.BLACK to ThemeColors(
        sentMessage = "#ff491A28",
        sentQuote = "#ff761F29",
        receivedMessage = "#ff282839",
        receivedQuote = "#ff3F3B4A",
      ),
    )
  ),
  @SerialName("kids") KIDS(MR.images.background_kids, "kids", MR.strings.background_kids, 0.53f,
    mapOf(DefaultTheme.LIGHT to "#ffD1FDFC".colorFromReadableHex(), DefaultTheme.DARK to "#ff121212".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff111528".colorFromReadableHex(), DefaultTheme.BLACK to "#ff121212".colorFromReadableHex()),
    mapOf(DefaultTheme.LIGHT to "#ff90E4F3".colorFromReadableHex(), DefaultTheme.DARK to "#ff16404B".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff143047".colorFromReadableHex(), DefaultTheme.BLACK to "#ff16404B".colorFromReadableHex()),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
        sentMessage = "#ffDCFEFF",
        sentQuote = "#ffBEF6F8",
        receivedMessage = "#ffF3FAFA",
        receivedQuote = "#ffE7F2F2",
      ),
      DefaultTheme.DARK to ThemeColors(
        sentMessage = "#ff16302F",
        sentQuote = "#ff1A4A49",
        receivedMessage = "#ff252626",
        receivedQuote = "#ff373A39",
      ),
      DefaultTheme.SIMPLEX to ThemeColors(
        sentMessage = "#ff174855",
        sentQuote = "#ff1D707A",
        receivedMessage = "#ff242839",
        receivedQuote = "#ff363C4A",
      ),
      DefaultTheme.BLACK to ThemeColors(
        sentMessage = "#ff174855",
        sentQuote = "#ff1D707A",
        receivedMessage = "#ff242839",
        receivedQuote = "#ff363C4A",
      ),
    )
  ),
  @SerialName("school") SCHOOL(MR.images.background_school, "school",  MR.strings.background_school, 0.53f,
  mapOf(DefaultTheme.LIGHT to "#ffE7F5FF".colorFromReadableHex(), DefaultTheme.DARK to "#ff121212".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff111528".colorFromReadableHex(), DefaultTheme.BLACK to "#ff121212".colorFromReadableHex()),
  mapOf(DefaultTheme.LIGHT to "#ffCEEBFF".colorFromReadableHex(), DefaultTheme.DARK to "#ff0F293B".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff0E2B4D".colorFromReadableHex(), DefaultTheme.BLACK to "#ff0F293B".colorFromReadableHex()),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
        sentMessage = "#ffEAF7FF",
        sentQuote = "#ffD6EDFA",
        receivedMessage = "#ffF3F5F9",
        receivedQuote = "#ffE6E9F0",
      ),
      DefaultTheme.DARK to ThemeColors(
        sentMessage = "#ff172833",
        sentQuote = "#ff1C3E4F",
        receivedMessage = "#ff282A2E",
        receivedQuote = "#ff3E434A",
      ),
      DefaultTheme.SIMPLEX to ThemeColors(
        sentMessage = "#ff1A3C5D",
        sentQuote = "#ff225E85",
        receivedMessage = "#ff282D41",
        receivedQuote = "#ff3E455B",
      ),
      DefaultTheme.BLACK to ThemeColors(
        sentMessage = "#ff1A3C5D",
        sentQuote = "#ff225E85",
        receivedMessage = "#ff282D41",
        receivedQuote = "#ff3E455B",
      ),
    )
  ),
  @SerialName("travel") TRAVEL(MR.images.background_travel, "travel", MR.strings.background_travel, 0.68f,
    mapOf(DefaultTheme.LIGHT to "#ffF7E9FF".colorFromReadableHex(), DefaultTheme.DARK to "#ff121212".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff111528".colorFromReadableHex(), DefaultTheme.BLACK to "#ff121212".colorFromReadableHex()),
    mapOf(DefaultTheme.LIGHT to "#ffEDD8FF".colorFromReadableHex(), DefaultTheme.DARK to "#ff311E48".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff34225E".colorFromReadableHex(), DefaultTheme.BLACK to "#ff311E48".colorFromReadableHex()),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
        sentMessage = "#ffF8ECFF",
        sentQuote = "#ffEEDAFA",
        receivedMessage = "#ffF6F4F7",
        receivedQuote = "#ffEBE7ED",
      ),
      DefaultTheme.DARK to ThemeColors(
        sentMessage = "#ff33263B",
        sentQuote = "#ff53385E",
        receivedMessage = "#ff272528",
        receivedQuote = "#ff3B373E",
      ),
      DefaultTheme.SIMPLEX to ThemeColors(
        sentMessage = "#ff3C255D",
        sentQuote = "#ff623485",
        receivedMessage = "#ff26273B",
        receivedQuote = "#ff3A394F",
      ),
      DefaultTheme.BLACK to ThemeColors(
        sentMessage = "#ff3C255D",
        sentQuote = "#ff623485",
        receivedMessage = "#ff26273B",
        receivedQuote = "#ff3A394F",
      ),
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
  abstract val scale: Float?

  val image by lazy {
    val filename = when (this) {
      is Repeated -> filename
      is Static -> filename
      else -> return@lazy null
    }
    if (filename == "") return@lazy null
    if (cachedImages[filename] != null) {
      cachedImages[filename]
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
      cachedImages[filename] = res ?: return@lazy null
      res
    }
  }

  fun sameType(other: BackgroundImageType?): Boolean =
    if (this is Repeated && other is Repeated) this.filename == other.filename
    else this.javaClass == other?.javaClass

  fun samePreset(other: PredefinedBackgroundImage?): Boolean = this is Repeated && filename == other?.filename

  @Serializable @SerialName("repeated") data class Repeated(
    val filename: String,
    override val scale: Float?,
  ): BackgroundImageType() {
    val predefinedImageScale = PredefinedBackgroundImage.from(filename)?.scale ?: 1f
  }

  @Serializable @SerialName("static") data class Static(
    val filename: String,
    override val scale: Float?,
    val scaleType: BackgroundImageScaleType?,
  ): BackgroundImageType()

  @Serializable @SerialName("empty") object Empty: BackgroundImageType() {
    override val scale: Float?
      get() = null
  }

  fun defaultBackgroundColor(theme: DefaultTheme, materialBackground: Color): Color =
    if (this is Repeated) {
      (PredefinedBackgroundImage.from(filename) ?: PredefinedBackgroundImage.CATS).background[theme]!!
    } else {
      materialBackground
    }

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

    var cachedImages: MutableMap<String, ImageBitmap> = mutableMapOf()

    fun from(wallpaper: ThemeWallpaper?): BackgroundImageType? {
      return if (wallpaper == null) {
        null
      } else if (wallpaper.preset != null) {
        Repeated(wallpaper.preset, wallpaper.scale)
      } else if (wallpaper.imageFile != null) {
        Static(wallpaper.imageFile, wallpaper.scale, wallpaper.scaleType)
      } else {
        Empty
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
    is BackgroundImageType.Repeated -> repeat((imageType.scale ?: 1f) * imageType.predefinedImageScale)
    is BackgroundImageType.Static -> when (val scaleType = imageType.scaleType ?: BackgroundImageScaleType.FILL) {
      BackgroundImageScaleType.REPEAT -> repeat(imageType.scale ?: 1f)
      BackgroundImageScaleType.FILL, BackgroundImageScaleType.FIT -> {
        val scale = scaleType.contentScale.computeScaleFactor(Size(image.width.toFloat(), image.height.toFloat()), Size(size.width, size.height))
        val scaledWidth = (image.width * scale.scaleX).roundToInt()
        val scaledHeight = (image.height * scale.scaleY).roundToInt()
        drawImage(image, dstOffset = IntOffset(x = ((size.width - scaledWidth) / 2).roundToInt(), y = ((size.height - scaledHeight) / 2).roundToInt()), dstSize = IntSize(scaledWidth, scaledHeight))
        if (scaleType == BackgroundImageScaleType.FIT) {
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
    is BackgroundImageType.Empty -> {}
  }
}
