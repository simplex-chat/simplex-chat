package chat.simplex.common.views.helpers

import androidx.compose.runtime.*
import androidx.compose.ui.draw.CacheDrawScope
import androidx.compose.ui.draw.DrawResult
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.drawscope.*
import androidx.compose.ui.graphics.layer.GraphicsLayer
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.unit.*
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

enum class PresetWallpaper(
  val res: ImageResource,
  val filename: String,
  val scale: Float,
  val background: Map<DefaultTheme, Color>,
  val tint: Map<DefaultTheme, Color>,
  val colors: Map<DefaultTheme, ThemeColors>,
) {
  CATS(MR.images.wallpaper_cats, "cats", 0.63f,
    wallpaperBackgrounds(light = "#ffF8F6EA"),
    tint = mapOf(
      DefaultTheme.LIGHT to "#ffefdca6".colorFromReadableHex(),
      DefaultTheme.DARK to "#ff4b3b0e".colorFromReadableHex(),
      DefaultTheme.SIMPLEX to "#ff51400f".colorFromReadableHex(),
      DefaultTheme.BLACK to "#ff4b3b0e".colorFromReadableHex()
    ),
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
        receivedMessage = "#ff1f1e1b",
        receivedQuote = "#ff2f2d27",
      ),
    )
  ),
  FLOWERS(MR.images.wallpaper_flowers, "flowers", 0.53f,
    wallpaperBackgrounds(light = "#ffE2FFE4"),
    tint = mapOf(
      DefaultTheme.LIGHT to "#ff9CEA59".colorFromReadableHex(),
      DefaultTheme.DARK to "#ff31560D".colorFromReadableHex(),
      DefaultTheme.SIMPLEX to "#ff36600f".colorFromReadableHex(),
      DefaultTheme.BLACK to "#ff31560D".colorFromReadableHex()
    ),
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
        receivedMessage = "#ff1c1f1a",
        receivedQuote = "#ff282b25",
      ),
    )
  ),
  HEARTS(MR.images.wallpaper_hearts, "hearts", 0.59f,
    wallpaperBackgrounds(light = "#ffFDECEC"),
    tint = mapOf(
      DefaultTheme.LIGHT to "#fffde0e0".colorFromReadableHex(),
      DefaultTheme.DARK to "#ff3c0f0f".colorFromReadableHex(),
      DefaultTheme.SIMPLEX to "#ff411010".colorFromReadableHex(),
      DefaultTheme.BLACK to "#ff3C0F0F".colorFromReadableHex()
    ),
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
        receivedMessage = "#ff242121",
        receivedQuote = "#ff3b3535",
      ),
      DefaultTheme.SIMPLEX to ThemeColors(
        sentMessage = "#ff491A28",
        sentQuote = "#ff761F29",
        receivedMessage = "#ff242121",
        receivedQuote = "#ff3b3535",
      ),
      DefaultTheme.BLACK to ThemeColors(
        sentMessage = "#ff491A28",
        sentQuote = "#ff761F29",
        receivedMessage = "#ff1f1b1b",
        receivedQuote = "#ff2e2626",
      ),
    )
  ),
  KIDS(MR.images.wallpaper_kids, "kids", 0.53f,
    wallpaperBackgrounds(light = "#ffdbfdfb"),
    tint = mapOf(
      DefaultTheme.LIGHT to "#ffadeffc".colorFromReadableHex(),
      DefaultTheme.DARK to "#ff16404B".colorFromReadableHex(),
      DefaultTheme.SIMPLEX to "#ff184753".colorFromReadableHex(),
      DefaultTheme.BLACK to "#ff16404B".colorFromReadableHex()
    ),
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
        receivedMessage = "#ff1e1f1f",
        receivedQuote = "#ff262b29",
      ),
    )
  ),
  SCHOOL(MR.images.wallpaper_school, "school", 0.53f,
    wallpaperBackgrounds(light = "#ffE7F5FF"),
    tint = mapOf(
      DefaultTheme.LIGHT to "#ffCEEBFF".colorFromReadableHex(),
      DefaultTheme.DARK to "#ff0F293B".colorFromReadableHex(),
      DefaultTheme.SIMPLEX to "#ff112f43".colorFromReadableHex(),
      DefaultTheme.BLACK to "#ff0F293B".colorFromReadableHex()
    ),
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
        receivedMessage = "#ff26282c",
        receivedQuote = "#ff393c40",
      ),
      DefaultTheme.SIMPLEX to ThemeColors(
        sentMessage = "#ff1A3C5D",
        sentQuote = "#ff235b80",
        receivedMessage = "#ff26282c",
        receivedQuote = "#ff393c40",
      ),
      DefaultTheme.BLACK to ThemeColors(
        sentMessage = "#ff1A3C5D",
        sentQuote = "#ff235b80",
        receivedMessage = "#ff1d1e22",
        receivedQuote = "#ff292b2f",
      ),
    )
  ),
  TRAVEL(MR.images.wallpaper_travel, "travel", 0.68f,
    wallpaperBackgrounds(light = "#fff9eeff"),
    tint = mapOf(
      DefaultTheme.LIGHT to "#ffeedbfe".colorFromReadableHex(),
      DefaultTheme.DARK to "#ff311E48".colorFromReadableHex(),
      DefaultTheme.SIMPLEX to "#ff35204e".colorFromReadableHex(),
      DefaultTheme.BLACK to "#ff311E48".colorFromReadableHex()
    ),
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
        receivedMessage = "#ff231f23",
        receivedQuote = "#ff2c2931",
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

fun wallpaperBackgrounds(light: String): Map<DefaultTheme, Color> =
  mapOf(
    DefaultTheme.LIGHT to light.colorFromReadableHex(),
    DefaultTheme.DARK to "#ff121212".colorFromReadableHex(),
    DefaultTheme.SIMPLEX to "#ff111528".colorFromReadableHex(),
    DefaultTheme.BLACK to "#ff070707".colorFromReadableHex()
  )

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

private fun drawToBitmap(image: ImageBitmap, imageScale: Float, tint: Color, size: Size, density: Float, layoutDirection: LayoutDirection): ImageBitmap {
  val quality = if (appPlatform.isAndroid) FilterQuality.High else FilterQuality.Low
  val drawScope = CanvasDrawScope()
  // Don't allow to make zero size because it crashes the app when reducing a size of a window on desktop
  val bitmap = ImageBitmap(size.width.toInt().coerceAtLeast(1), size.height.toInt().coerceAtLeast(1))
  val canvas = Canvas(bitmap)
  drawScope.draw(
    density = Density(density),
    layoutDirection = layoutDirection,
    canvas = canvas,
    size = size,
  ) {
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
  return bitmap
}

fun CacheDrawScope.chatViewBackground(
  image: ImageBitmap,
  imageType: WallpaperType,
  background: Color,
  tint: Color,
  graphicsLayerSize: MutableState<IntSize>? = null,
  backgroundGraphicsLayer: GraphicsLayer? = null
): DrawResult {
  val imageScale = if (imageType is WallpaperType.Preset) {
    (imageType.scale ?: 1f) * imageType.predefinedImageScale
  } else if (imageType is WallpaperType.Image && imageType.scaleType == WallpaperScaleType.REPEAT) {
    imageType.scale ?: 1f
  } else {
    1f
  }
  val image = if (imageType is WallpaperType.Preset || (imageType is WallpaperType.Image && imageType.scaleType == WallpaperScaleType.REPEAT)) {
    drawToBitmap(image, imageScale, tint, size, density, layoutDirection)
  } else {
    image
  }

  return onDrawBehind {
    copyBackgroundToAppBar(graphicsLayerSize, backgroundGraphicsLayer) {
      val quality = if (appPlatform.isAndroid) FilterQuality.High else FilterQuality.Low
      drawRect(background)
      when (imageType) {
        is WallpaperType.Preset -> drawImage(image)
        is WallpaperType.Image -> when (val scaleType = imageType.scaleType ?: WallpaperScaleType.FILL) {
          WallpaperScaleType.REPEAT -> drawImage(image)
          WallpaperScaleType.FILL, WallpaperScaleType.FIT -> {
            clipRect {
              val scale = scaleType.contentScale.computeScaleFactor(Size(image.width.toFloat(), image.height.toFloat()), Size(size.width, size.height))
              val scaledWidth = (image.width * scale.scaleX).roundToInt()
              val scaledHeight = (image.height * scale.scaleY).roundToInt()
              // Large image will cause freeze
              if (image.width > 4320 || image.height > 4320) return@clipRect

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
            }
            drawRect(tint)
          }
        }
        is WallpaperType.Empty -> {}
      }
    }
  }
}
