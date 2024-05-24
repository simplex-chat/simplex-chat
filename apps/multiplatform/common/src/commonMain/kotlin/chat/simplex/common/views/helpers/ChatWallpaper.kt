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
enum class PresetWallpaper(
  val res: ImageResource,
  val filename: String,
  val text: StringResource,
  val scale: Float,
  val background: Map<DefaultTheme, Color>,
  val tint: Map<DefaultTheme, Color>,
  val colors: Map<DefaultTheme, ThemeColors>,
) {
  @SerialName("cats") CATS(MR.images.wallpaper_cats, "cats", MR.strings.wallpaper_cats, 0.63f,
    mapOf(DefaultTheme.LIGHT to "#ffF8F6EA".colorFromReadableHex(), DefaultTheme.DARK to "#ff121212".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff111528".colorFromReadableHex(), DefaultTheme.BLACK to "#ff000000".colorFromReadableHex()),
//    mapOf(DefaultTheme.LIGHT to "#ffF8F6EA".colorFromReadableHex()),
    mapOf(DefaultTheme.LIGHT to "#ffefdca6".colorFromReadableHex(), DefaultTheme.DARK to "#ff4b3b0e".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff4b3b0e".colorFromReadableHex(), DefaultTheme.BLACK to "#ff4b3b0e".colorFromReadableHex()),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
        sentMessage = "#fffffaed",
        sentQuote = "#fffaf0d6",
        receivedMessage = "#ffF8F7F4",
        receivedQuote = "#ffefede9",
      ),
      DefaultTheme.DARK to ThemeColors(
        sentMessage = "#ff2f2919",
        sentQuote = "#ff473a1d",
        receivedMessage = "#ff272624",
        receivedQuote = "#ff373633",
      ),
      DefaultTheme.SIMPLEX to ThemeColors(
        sentMessage = "#ff41371b",
        sentQuote = "#ff654f1c",
        receivedMessage = "#ff272624",
        receivedQuote = "#ff373633",
      ),
      DefaultTheme.BLACK to ThemeColors(
        sentMessage = "#ff41371b",
        sentQuote = "#ff654f1c",
        receivedMessage = "#ff272624",
        receivedQuote = "#ff373633",
      ),
    )
  ),
  @SerialName("flowers") FLOWERS(MR.images.wallpaper_flowers, "flowers", MR.strings.wallpaper_flowers, 0.53f,
    mapOf(DefaultTheme.LIGHT to "#ffE2FFE4".colorFromReadableHex(), DefaultTheme.DARK to "#ff121212".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff111528".colorFromReadableHex(), DefaultTheme.BLACK to "#ff000000".colorFromReadableHex()),
    mapOf(DefaultTheme.LIGHT to "#ff9CEA59".colorFromReadableHex(), DefaultTheme.DARK to "#ff31560D".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff31570D".colorFromReadableHex(), DefaultTheme.BLACK to "#ff31560D".colorFromReadableHex()),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
        sentMessage = "#fff1ffe5",
        sentQuote = "#ffdcf9c4",
        receivedMessage = "#ffF4F8F2",
        receivedQuote = "#ffe7ece7",
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
        receivedMessage = "#ff242523",
        receivedQuote = "#ff353733",
      ),
      DefaultTheme.BLACK to ThemeColors(
        sentMessage = "#ff184739",
        sentQuote = "#ff1F6F4B",
        receivedMessage = "#ff242523",
        receivedQuote = "#ff353733",
      ),
    )
  ),
  @SerialName("hearts") HEARTS(MR.images.wallpaper_hearts, "hearts", MR.strings.wallpaper_hearts, 0.59f,
    mapOf(DefaultTheme.LIGHT to "#ffFDECEC".colorFromReadableHex(), DefaultTheme.DARK to "#ff121212".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff111528".colorFromReadableHex(), DefaultTheme.BLACK to "#ff000000".colorFromReadableHex()),
    mapOf(DefaultTheme.LIGHT to "#fffde0e0".colorFromReadableHex(), DefaultTheme.DARK to "#ff3C0F0F".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff381221".colorFromReadableHex(), DefaultTheme.BLACK to "#ff3C0F0F".colorFromReadableHex()),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
        sentMessage = "#fffff4f4",
        sentQuote = "#ffffdfdf",
        receivedMessage = "#fff8f6f6",
        receivedQuote = "#ffefebeb",
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
        receivedMessage = "#ff292626",
        receivedQuote = "#ff403939",
      ),
      DefaultTheme.BLACK to ThemeColors(
        sentMessage = "#ff491A28",
        sentQuote = "#ff761F29",
        receivedMessage = "#ff292626",
        receivedQuote = "#ff403939",
      ),
    )
  ),
  @SerialName("kids") KIDS(MR.images.wallpaper_kids, "kids", MR.strings.wallpaper_kids, 0.53f,
    mapOf(DefaultTheme.LIGHT to "#ffdbfdfb".colorFromReadableHex(), DefaultTheme.DARK to "#ff121212".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff111528".colorFromReadableHex(), DefaultTheme.BLACK to "#ff000000".colorFromReadableHex()),
    mapOf(DefaultTheme.LIGHT to "#ffadeffc".colorFromReadableHex(), DefaultTheme.DARK to "#ff16404B".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff143047".colorFromReadableHex(), DefaultTheme.BLACK to "#ff16404B".colorFromReadableHex()),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
        sentMessage = "#ffeafeff",
        sentQuote = "#ffcbf4f7",
        receivedMessage = "#fff3fafa",
        receivedQuote = "#ffe4efef",
      ),
      DefaultTheme.DARK to ThemeColors(
        sentMessage = "#ff16302F",
        sentQuote = "#ff1a4a49",
        receivedMessage = "#ff252626",
        receivedQuote = "#ff373A39",
      ),
      DefaultTheme.SIMPLEX to ThemeColors(
        sentMessage = "#ff1a4745",
        sentQuote = "#ff1d6b69",
        receivedMessage = "#ff252626",
        receivedQuote = "#ff373a39",
      ),
      DefaultTheme.BLACK to ThemeColors(
        sentMessage = "#ff1a4745",
        sentQuote = "#ff1d6b69",
        receivedMessage = "#ff252626",
        receivedQuote = "#ff373A39",
      ),
    )
  ),
  @SerialName("school") SCHOOL(MR.images.wallpaper_school, "school",  MR.strings.wallpaper_school, 0.53f,
  mapOf(DefaultTheme.LIGHT to "#ffE7F5FF".colorFromReadableHex(), DefaultTheme.DARK to "#ff121212".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff111528".colorFromReadableHex(), DefaultTheme.BLACK to "#ff000000".colorFromReadableHex()),
  mapOf(DefaultTheme.LIGHT to "#ffCEEBFF".colorFromReadableHex(), DefaultTheme.DARK to "#ff0F293B".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff0E2B4D".colorFromReadableHex(), DefaultTheme.BLACK to "#ff0F293B".colorFromReadableHex()),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
        sentMessage = "#ffeef9ff",
        sentQuote = "#ffD6EDFA",
        receivedMessage = "#ffF3F5F9",
        receivedQuote = "#ffe4e8ee",
      ),
      DefaultTheme.DARK to ThemeColors(
        sentMessage = "#ff172833",
        sentQuote = "#ff1C3E4F",
        receivedMessage = "#ff282A2E",
        receivedQuote = "#ff3E434A",
      ),
      DefaultTheme.SIMPLEX to ThemeColors(
        sentMessage = "#ff1A3C5D",
        sentQuote = "#ff235b80",
        receivedMessage = "#ff282D41",
        receivedQuote = "#ff3E455B",
      ),
      DefaultTheme.BLACK to ThemeColors(
        sentMessage = "#ff1A3C5D",
        sentQuote = "#ff235b80",
        receivedMessage = "#ff282D41",
        receivedQuote = "#ff3E455B",
      ),
    )
  ),
  @SerialName("travel") TRAVEL(MR.images.wallpaper_travel, "travel", MR.strings.wallpaper_travel, 0.68f,
    mapOf(DefaultTheme.LIGHT to "#fff9eeff".colorFromReadableHex(), DefaultTheme.DARK to "#ff121212".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff111528".colorFromReadableHex(), DefaultTheme.BLACK to "#ff000000".colorFromReadableHex()),
    mapOf(DefaultTheme.LIGHT to "#ffeedbfe".colorFromReadableHex(), DefaultTheme.DARK to "#ff311E48".colorFromReadableHex(), DefaultTheme.SIMPLEX to "#ff34225E".colorFromReadableHex(), DefaultTheme.BLACK to "#ff311E48".colorFromReadableHex()),
    mapOf(
      DefaultTheme.LIGHT to ThemeColors(
        sentMessage = "#fffcf6ff",
        sentQuote = "#fff2e0fc",
        receivedMessage = "#ffF6F4F7",
        receivedQuote = "#ffede9ee",
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

  fun toType(base: DefaultTheme, scale: Float? = null): WallpaperType =
    WallpaperType.Preset(
      filename,
      scale ?: appPrefs.themeOverrides.get().firstOrNull { it.wallpaper != null && it.wallpaper.preset == filename && it.base == base }?.wallpaper?.scale ?: 1f
    )

  companion object {
    fun from(filename: String): PresetWallpaper? =
      entries.firstOrNull { it.filename == filename }
  }
}

@Serializable
enum class WallpaperScaleType(val contentScale: ContentScale, val text: StringResource) {
  @SerialName("fill") FILL(ContentScale.Crop, MR.strings.wallpaper_scale_fill),
  @SerialName("fit") FIT(ContentScale.Fit, MR.strings.wallpaper_scale_fit),
  @SerialName("repeat") REPEAT(ContentScale.Fit, MR.strings.wallpaper_scale_repeat),
}

sealed class WallpaperType {
  abstract val scale: Float?

  val image by lazy {
    val filename = when (this) {
      is Preset -> filename
      is Image -> filename
      else -> return@lazy null
    }
    if (filename == "") return@lazy null
    if (cachedImages[filename] != null) {
      cachedImages[filename]
    } else {
      val res = if (this is Preset) {
        (PresetWallpaper.from(filename) ?: PresetWallpaper.CATS).res.toComposeImageBitmap()!!
      } else {
        try {
          // In case of unintentional image deletion don't crash the app
          File(getWallpaperFilePath(filename)).inputStream().use { loadImageBitmap(it) }
        } catch (e: Exception) {
          Log.e(TAG, "Error while loading wallpaper file: ${e.stackTraceToString()}")
          null
        }
      }
      res?.prepareToDraw()
      cachedImages[filename] = res ?: return@lazy null
      res
    }
  }

  fun sameType(other: WallpaperType?): Boolean =
    if (this is Preset && other is Preset) this.filename == other.filename
    else this.javaClass == other?.javaClass

  fun samePreset(other: PresetWallpaper?): Boolean = this is Preset && filename == other?.filename

  data class Preset(
    val filename: String,
    override val scale: Float?,
  ): WallpaperType() {
    val predefinedImageScale = PresetWallpaper.from(filename)?.scale ?: 1f
  }

  data class Image(
    val filename: String,
    override val scale: Float?,
    val scaleType: WallpaperScaleType?,
  ): WallpaperType()

  object Empty: WallpaperType() {
    override val scale: Float?
      get() = null
  }

  fun defaultBackgroundColor(theme: DefaultTheme, materialBackground: Color): Color =
    if (this is Preset) {
      (PresetWallpaper.from(filename) ?: PresetWallpaper.CATS).background[theme]!!
    } else {
      materialBackground
    }

  fun defaultTintColor(theme: DefaultTheme): Color =
    if (this is Preset) {
      (PresetWallpaper.from(filename) ?: PresetWallpaper.CATS).tint[theme]!!
    } else if (this is Image && scaleType == WallpaperScaleType.REPEAT) {
      Color.Transparent
    } else {
      Color.Transparent
    }

  companion object {
    var cachedImages: MutableMap<String, ImageBitmap> = mutableMapOf()

    fun from(wallpaper: ThemeWallpaper?): WallpaperType? {
      return if (wallpaper == null) {
        null
      } else if (wallpaper.preset != null) {
        Preset(wallpaper.preset, wallpaper.scale)
      } else if (wallpaper.imageFile != null) {
        Image(wallpaper.imageFile, wallpaper.scale, wallpaper.scaleType)
      } else {
        Empty
      }
    }
  }
}

fun DrawScope.chatViewBackground(image: ImageBitmap, imageType: WallpaperType, background: Color, tint: Color) = clipRect {
  val quality = FilterQuality.High
  fun repeat(imageScale: Float) {
    val scale = imageScale * density
    for (h in 0..(size.height / image.height / scale).roundToInt()) {
      for (w in 0..(size.width / image.width / scale).roundToInt()) {
        drawImage(
          image,
          dstOffset = IntOffset(x = (w * image.width * scale).roundToInt(), y = (h * image.height * scale).roundToInt()),
          dstSize = IntSize((image.width * scale).roundToInt(), (image.height * scale).roundToInt()),
          colorFilter = ColorFilter.tint(tint, BlendMode.SrcIn),
          filterQuality = quality
        )
      }
    }
  }

  drawRect(background)
  when (imageType) {
    is WallpaperType.Preset -> repeat((imageType.scale ?: 1f) * imageType.predefinedImageScale)
    is WallpaperType.Image -> when (val scaleType = imageType.scaleType ?: WallpaperScaleType.FILL) {
      WallpaperScaleType.REPEAT -> repeat(imageType.scale ?: 1f)
      WallpaperScaleType.FILL, WallpaperScaleType.FIT -> {
        val scale = scaleType.contentScale.computeScaleFactor(Size(image.width.toFloat(), image.height.toFloat()), Size(size.width, size.height))
        val scaledWidth = (image.width * scale.scaleX).roundToInt()
        val scaledHeight = (image.height * scale.scaleY).roundToInt()
        drawImage(image, dstOffset = IntOffset(x = ((size.width - scaledWidth) / 2).roundToInt(), y = ((size.height - scaledHeight) / 2).roundToInt()), dstSize = IntSize(scaledWidth, scaledHeight), filterQuality = quality)
        if (scaleType == WallpaperScaleType.FIT) {
          if (scaledWidth < size.width) {
            // has black lines at left and right sides
            var x = (size.width - scaledWidth) / 2
            while (x > 0) {
              drawImage(image, dstOffset = IntOffset(x = (x - scaledWidth).roundToInt(), y = ((size.height - scaledHeight) / 2).roundToInt()), dstSize = IntSize(scaledWidth, scaledHeight), filterQuality = quality)
              x -= scaledWidth
            }
            x = size.width - (size.width - scaledWidth) / 2
            while (x < size.width) {
              drawImage(image, dstOffset = IntOffset(x = x.roundToInt(), y = ((size.height - scaledHeight) / 2).roundToInt()), dstSize = IntSize(scaledWidth, scaledHeight), filterQuality = quality)
              x += scaledWidth
            }
          } else {
            // has black lines at top and bottom sides
            var y = (size.height - scaledHeight) / 2
            while (y > 0) {
              drawImage(image, dstOffset = IntOffset(x = ((size.width - scaledWidth) / 2).roundToInt(), y = (y - scaledHeight).roundToInt()), dstSize = IntSize(scaledWidth, scaledHeight), filterQuality = quality)
              y -= scaledHeight
            }
            y = size.height - (size.height - scaledHeight) / 2
            while (y < size.height) {
              drawImage(image, dstOffset = IntOffset(x = ((size.width - scaledWidth) / 2).roundToInt(), y = y.roundToInt()), dstSize = IntSize(scaledWidth, scaledHeight), filterQuality = quality)
              y += scaledHeight
            }
          }
        }
        drawRect(tint)
      }
    }
    is WallpaperType.Empty -> {}
  }
}
