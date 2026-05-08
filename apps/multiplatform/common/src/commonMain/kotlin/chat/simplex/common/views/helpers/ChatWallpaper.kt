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
  val hue: Float,
  val cScale: Float,
  private val _background: Map<DefaultTheme, Color>,
  private val _tint: Map<DefaultTheme, Color>,
  private val _colors: Map<DefaultTheme, ResolvedColors>,
) {
  CATS(MR.images.wallpaper_cats, "cats", 0.5f, 77f, 0.8172f,
    wallpaperBackgrounds(
      light = oklch(0.9800f, 0.0530f, 77f),
      dark = oklch(0.1800f, 0.0250f, 77f),
    ),
    _tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.8800f, 0.0800f, 77f),
      DefaultTheme.DARK to oklch(0.2940f, 0.0567f, 77f),
      DefaultTheme.SIMPLEX to oklch(0.3797781f, 0.06842897f, 88.88896f), // #ff51400f
      DefaultTheme.BLACK to oklch(0.3600f, 0.0866f, 77f)
    ),
    _colors = mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9850f, 0.0396f, 77f),
        sentQuote = oklch(0.9650f, 0.0614f, 77f),
        receivedMessage = oklch(0.9950f, 0.0051f, 77f),
        receivedQuote = oklch(0.9850f, 0.0396f, 77f),
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.3130f, 0.0630f, 77f),
        sentQuote = oklch(0.3700f, 0.0725f, 77f),
        receivedMessage = oklch(0.2560f, 0.0278f, 77f),
        receivedQuote = oklch(0.2940f, 0.0320f, 77f),
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.3402031f, 0.04537511f, 90.2498f), // #ff41371b
        sentQuote = oklch(0.4398707f, 0.0737883f, 85.23908f), // #ff654f1c
        receivedMessage = oklch(0.2689313f, 0.003935312f, 84.58291f), // #ff272624
        receivedQuote = oklch(0.332832f, 0.005361989f, 91.54412f), // #ff373633
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.24f, 0.0596f, 70f),
        sentQuote = oklch(0.33f, 0.0815f, 70f),
        receivedMessage = oklch(0.12f, 0f, 0f),
        receivedQuote = oklch(0.22f, 0f, 0f),
      ),
    )
  ),
  FLOWERS(MR.images.wallpaper_flowers, "flowers", 0.5f, 130f, 1.3553f,
    wallpaperBackgrounds(
      light = oklch(0.95f, 0.035f, 130f),
      dark = oklch(0.1800f, 0.0250f, 130f),
    ),
    _tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.8450f, 0.0931f, 130f),
      DefaultTheme.DARK to oklch(0.3130f, 0.0567f, 130f),
      DefaultTheme.SIMPLEX to oklch(0.4415422f, 0.1170956f, 133.8571f), // #ff36600f
      DefaultTheme.BLACK to oklch(0.3600f, 0.1133f, 130f)
    ),
    _colors = mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9617f, 0.0259f, 130f),
        sentQuote = oklch(0.9150f, 0.0757f, 130f),
        receivedMessage = oklch(0.9950f, 0.0114f, 130f),
        receivedQuote = oklch(0.9650f, 0.0234f, 130f),
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.3130f, 0.0630f, 130f),
        sentQuote = oklch(0.3700f, 0.0725f, 130f),
        receivedMessage = oklch(0.2560f, 0.0278f, 130f),
        receivedQuote = oklch(0.2940f, 0.0320f, 130f),
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.3611755f, 0.05678164f, 170.3752f), // #ff184739
        sentQuote = oklch(0.484029f, 0.09629127f, 159.5568f), // #ff1F6F4B
        receivedMessage = oklch(0.2626721f, 0.003936427f, 128.6285f), // #ff242523
        receivedQuote = oklch(0.3334174f, 0.007411477f, 128.7105f), // #ff353733
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.24f, 0.0756f, 130f),
        sentQuote = oklch(0.33f, 0.1029f, 130f),
        receivedMessage = oklch(0.12f, 0f, 0f),
        receivedQuote = oklch(0.22f, 0f, 0f),
      ),
    )
  ),
  HEARTS(MR.images.wallpaper_hearts, "hearts", 0.5f, 15f, 1.0504f,
    wallpaperBackgrounds(
      light = oklch(0.96f, 0.0259f, 15f),
      dark = oklch(0.1800f, 0.0250f, 5f),
    ),
    _tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.8970f, 0.0706f, 15f),
      DefaultTheme.DARK to oklch(0.2940f, 0.0567f, 5f),
      DefaultTheme.SIMPLEX to oklch(0.2574974f, 0.07614605f, 24.19117f), // #ff411010
      DefaultTheme.BLACK to oklch(0.3600f, 0.1630f, 5f)
    ),
    _colors = mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9700f, 0.0193f, 15f),
        sentQuote = oklch(0.9300f, 0.0344f, 15f),
        receivedMessage = oklch(0.9950f, 0.0031f, 15f),
        receivedQuote = oklch(0.9700f, 0.0193f, 15f),
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.3130f, 0.0630f, 5f),
        sentQuote = oklch(0.3700f, 0.0725f, 5f),
        receivedMessage = oklch(0.2560f, 0.0278f, 5f),
        receivedQuote = oklch(0.2940f, 0.0320f, 5f),
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.2941874f, 0.07322977f, 4.102547f), // #ff491A28
        sentQuote = oklch(0.3831088f, 0.1201278f, 18.61089f), // #ff761F29
        receivedMessage = oklch(0.2510736f, 0.004554155f, 17.46058f), // #ff242121
        receivedQuote = oklch(0.3352158f, 0.008515606f, 17.58481f), // #ff3b3535
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.24f, 0.1087f, 5f),
        sentQuote = oklch(0.33f, 0.1480f, 5f),
        receivedMessage = oklch(0.12f, 0f, 0f),
        receivedQuote = oklch(0.22f, 0f, 0f),
      ),
    )
  ),
  KIDS(MR.images.wallpaper_kids, "kids", 0.5f, 200f, 0.7723f,
    wallpaperBackgrounds(
      light = oklch(0.9625f, 0.0371f, 200f),
      dark = oklch(0.1800f, 0.0250f, 200f),
    ),
    _tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.8828f, 0.0700f, 200f),
      DefaultTheme.DARK to oklch(0.2560f, 0.0567f, 200f),
      DefaultTheme.SIMPLEX to oklch(0.3716418f, 0.05389406f, 217.7104f), // #ff184753
      DefaultTheme.BLACK to oklch(0.3000f, 0.0684f, 200f)
    ),
    _colors = mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9717f, 0.0280f, 200f),
        sentQuote = oklch(0.9350f, 0.0511f, 200f),
        receivedMessage = oklch(0.9950f, 0.0069f, 200f),
        receivedQuote = oklch(0.9725f, 0.0272f, 200f),
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.3130f, 0.0630f, 200f),
        sentQuote = oklch(0.3700f, 0.0725f, 200f),
        receivedMessage = oklch(0.2560f, 0.0278f, 200f),
        receivedQuote = oklch(0.2940f, 0.0320f, 200f),
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.3662882f, 0.04909204f, 191.2229f), // #ff1a4745
        sentQuote = oklch(0.4817563f, 0.07299667f, 192.4874f), // #ff1d6b69
        receivedMessage = oklch(0.2675764f, 0.001466786f, 197.0692f), // #ff252626
        receivedQuote = oklch(0.3451987f, 0.004436687f, 174.2088f), // #ff373a39
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.24f, 0.0555f, 192f),
        sentQuote = oklch(0.33f, 0.0756f, 192f),
        receivedMessage = oklch(0.12f, 0f, 0f),
        receivedQuote = oklch(0.22f, 0f, 0f),
      ),
    )
  ),
  SCHOOL(MR.images.wallpaper_school, "school", 0.5f, 239f, 0.7950f,
    wallpaperBackgrounds(
      light = oklch(0.9625f, 0.0296f, 239f),
      dark = oklch(0.1800f, 0.0250f, 249f),
    ),
    _tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.8910f, 0.0600f, 239f),
      DefaultTheme.DARK to oklch(0.2560f, 0.0567f, 249f),
      DefaultTheme.SIMPLEX to oklch(0.2929108f, 0.05102392f, 240.8139f), // #ff112f43
      DefaultTheme.BLACK to oklch(0.3000f, 0.1070f, 249f)
    ),
    _colors = mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9717f, 0.0223f, 239f),
        sentQuote = oklch(0.9350f, 0.0364f, 239f),
        receivedMessage = oklch(0.9950f, 0.0030f, 239f),
        receivedQuote = oklch(0.9725f, 0.0217f, 239f),
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.3130f, 0.0630f, 249f),
        sentQuote = oklch(0.3700f, 0.0725f, 249f),
        receivedMessage = oklch(0.2560f, 0.0278f, 249f),
        receivedQuote = oklch(0.2940f, 0.0320f, 249f),
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.4187f, 0.1388f, 262.50f), // #204797
        sentQuote = oklch(0.4766f, 0.1487f, 262.58f), // #2c57af
        receivedMessage = oklch(0.2314f, 0.0685f, 268.59f), // #101a3d
        receivedQuote = oklch(0.3023f, 0.0900f, 267.90f), // #1b2a5b
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.24f, 0.0856f, 249f),
        sentQuote = oklch(0.33f, 0.1166f, 249f),
        receivedMessage = oklch(0.12f, 0f, 0f),
        receivedQuote = oklch(0.22f, 0f, 0f),
      ),
    )
  ),
  TRAVEL(MR.images.wallpaper_travel, "travel", 0.5f, 315f, 1.2099f,
    wallpaperBackgrounds(
      light = oklch(0.9625f, 0.0389f, 315f),
      dark = oklch(0.1800f, 0.0250f, 315f),
    ),
    _tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.8938f, 0.0700f, 315f),
      DefaultTheme.DARK to oklch(0.2750f, 0.0567f, 315f),
      DefaultTheme.SIMPLEX to oklch(0.2948376f, 0.08277514f, 302.7197f), // #ff35204e
      DefaultTheme.BLACK to oklch(0.3000f, 0.1579f, 315f)
    ),
    _colors = mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9717f, 0.0294f, 315f),
        sentQuote = oklch(0.9350f, 0.0479f, 315f),
        receivedMessage = oklch(0.9950f, 0.0040f, 315f),
        receivedQuote = oklch(0.9725f, 0.0285f, 315f),
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.3130f, 0.0630f, 315f),
        sentQuote = oklch(0.3700f, 0.0725f, 315f),
        receivedMessage = oklch(0.2560f, 0.0278f, 315f),
        receivedQuote = oklch(0.2940f, 0.0320f, 315f),
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.3234681f, 0.09690244f, 299.9634f), // #ff3C255D
        sentQuote = oklch(0.4226042f, 0.1341495f, 307.8573f), // #ff623485
        receivedMessage = oklch(0.2812692f, 0.03669397f, 281.5485f), // #ff26273B
        receivedQuote = oklch(0.355058f, 0.03791292f, 286.3773f), // #ff3A394F
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.24f, 0.1263f, 315f),
        sentQuote = oklch(0.33f, 0.1721f, 315f),
        receivedMessage = oklch(0.12f, 0f, 0f),
        receivedQuote = oklch(0.22f, 0f, 0f),
      ),
    )
  );

  val background: Map<DefaultTheme, Color> get() = _background
  val tint: Map<DefaultTheme, Color> get() = _tint
  val colors: Map<DefaultTheme, ResolvedColors> get() = _colors

  /** Hue for a given theme. Most wallpapers use the same hue across all themes,
   *  but hearts (15→5) and school (239→249) shift hue in dark themes for perceptual correction on AMOLED. */
  fun hue(theme: DefaultTheme): Float = when (theme) {
    DefaultTheme.DARK, DefaultTheme.BLACK -> when (this) {
      HEARTS -> 5f
      SCHOOL -> 249f
      else -> hue
    }
    else -> hue
  }

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

fun wallpaperBackgrounds(
  light: Color,
  dark: Color = oklch(0.1822037f, 0f, 0f), // #ff121212
): Map<DefaultTheme, Color> =
  mapOf(
    DefaultTheme.LIGHT to light,
    DefaultTheme.DARK to dark,
    DefaultTheme.SIMPLEX to oklch(0.2024453f, 0.03849037f, 273.4875f), // #ff111528
    DefaultTheme.BLACK to oklch(0f, 0f, 0f) // #ff000000 — pure black for hyper-contrast theme
  )

// ===== Theme color formula =====
// L = mode.lBase + mode.lSpread * elem.lOffset
// C = mode.cFactor * elem.cFactor * theme.cScale
// H = theme.hue

private data class ModeParams(val lBase: Float, val lSpread: Float, val cFactor: Float)
private data class ElemParams(val lOffset: Float, val cFactor: Float)

private val MODE_PARAMS = mapOf(
  DefaultTheme.LIGHT   to ModeParams(lBase = 0.9481f, lSpread = 0.2482f, cFactor = 0.7528f),
  DefaultTheme.DARK    to ModeParams(lBase = 0.3124f, lSpread = -0.6795f, cFactor = 0.8827f),
  DefaultTheme.SIMPLEX to ModeParams(lBase = 0.3467f, lSpread = -1.3361f, cFactor = 1.1992f),
  DefaultTheme.BLACK   to ModeParams(lBase = 0.3249f, lSpread = -1.6120f, cFactor = 1.1654f),
)

private enum class ThemeElem(val params: ElemParams) {
  TINT(ElemParams(lOffset = 0.0007f, cFactor = 0.07096f)),
  SENT_MESSAGE(ElemParams(lOffset = 0.0040f, cFactor = 0.04810f)),
  SENT_QUOTE(ElemParams(lOffset = -0.0725f, cFactor = 0.07623f)),
  RECEIVED_MESSAGE(ElemParams(lOffset = 0.0584f, cFactor = 0.00691f)),
  RECEIVED_QUOTE(ElemParams(lOffset = 0.0094f, cFactor = 0.00969f)),
}

private const val BG_LIGHT_L = 0.9657f
private const val BG_LIGHT_C_BASE = 0.02721f

private fun gen(mode: DefaultTheme, elem: ThemeElem, p: PresetWallpaper): Color {
  val m = MODE_PARAMS[mode]!!
  val e = elem.params
  return oklch(m.lBase + m.lSpread * e.lOffset, m.cFactor * e.cFactor * p.cScale, p.hue)
}

private fun generateBackground(p: PresetWallpaper): Map<DefaultTheme, Color> =
  wallpaperBackgrounds(light = oklch(BG_LIGHT_L, BG_LIGHT_C_BASE * p.cScale, p.hue))

private fun generateTint(p: PresetWallpaper): Map<DefaultTheme, Color> =
  DefaultTheme.entries.associateWith { gen(it, ThemeElem.TINT, p) }

private fun generateColors(p: PresetWallpaper): Map<DefaultTheme, ResolvedColors> =
  DefaultTheme.entries.associateWith { mode ->
    ResolvedColors(
      sentMessage = gen(mode, ThemeElem.SENT_MESSAGE, p),
      sentQuote = gen(mode, ThemeElem.SENT_QUOTE, p),
      receivedMessage = gen(mode, ThemeElem.RECEIVED_MESSAGE, p),
      receivedQuote = gen(mode, ThemeElem.RECEIVED_QUOTE, p),
    )
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
