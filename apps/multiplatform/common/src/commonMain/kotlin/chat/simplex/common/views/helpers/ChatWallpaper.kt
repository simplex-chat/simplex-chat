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
      light = oklch(0.9838867f, 0.021334622f, 95.39152f),
      dark = oklch(0.1800f, 0.0250f, 77f),
    ),
    _tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.92822266f, 0.05571678f, 95.39152f),
      DefaultTheme.DARK to oklch(0.2940f, 0.0567f, 77f),
      DefaultTheme.SIMPLEX to oklch(0.3797781f, 0.06842897f, 88.88896f), // #ff51400f
      DefaultTheme.BLACK to oklch(0.36010742f, 0.07737845f, 64.32541f)
    ),
    _colors = mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.95458984f, 0.05769427f, 95.39152f),
        sentQuote = oklch(0.9350586f, 0.081934035f, 95.39152f),
        receivedMessage = oklch(0.9941406f, 0.009641647f, 95.39152f),
        receivedQuote = oklch(0.9741211f, 0.033454504f, 95.39152f),
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
        sentMessage = oklch(0.24145508f, 0.054447953f, 64.32541f),
        sentQuote = oklch(0.33200073f, 0.07415811f, 64.32541f),
        receivedMessage = oklch(0.12072754f, 0.0f, 64.32541f),
        receivedQuote = oklch(0.22133382f, 0.0f, 64.32541f),
      ),
    ),
  ),
  FLOWERS(MR.images.wallpaper_flowers, "flowers", 0.5f, 130f, 1.3553f,
    wallpaperBackgrounds(
      light = oklch(0.9838867f, 0.02136218f, 130.21953f),
      dark = oklch(0.1800f, 0.0250f, 130f),
    ),
    _tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.91506225f, 0.054591343f, 130.21953f),
      DefaultTheme.DARK to oklch(0.3130f, 0.0567f, 130f),
      DefaultTheme.SIMPLEX to oklch(0.4415422f, 0.1170956f, 133.8571f), // #ff36600f
      DefaultTheme.BLACK to oklch(0.36254883f, 0.10072228f, 132.04506f)
    ),
    _colors = mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9542969f, 0.067714185f, 130.21953f),
        sentQuote = oklch(0.9345703f, 0.09861551f, 130.21953f),
        receivedMessage = oklch(0.9951172f, 0.010222077f, 130.21953f),
        receivedQuote = oklch(0.97402346f, 0.036812846f, 130.21953f),
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
        sentMessage = oklch(0.24267578f, 0.06739509f, 132.04506f),
        sentQuote = oklch(0.3336792f, 0.091792114f, 132.04506f),
        receivedMessage = oklch(0.12133789f, 0.0f, 132.04506f),
        receivedQuote = oklch(0.2224528f, 0.0f, 132.04506f),
      ),
    ),
  ),
  HEARTS(MR.images.wallpaper_hearts, "hearts", 0.5f, 15f, 1.0504f,
    wallpaperBackgrounds(
      light = oklch(0.9780521f, 0.010869741f, 10.569774f),
      dark = oklch(0.1800f, 0.0250f, 5f),
    ),
    _tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.9399476f, 0.030744314f, 10.569774f),
      DefaultTheme.DARK to oklch(0.2940f, 0.0567f, 5f),
      DefaultTheme.SIMPLEX to oklch(0.2574974f, 0.07614605f, 24.19117f), // #ff411010
      DefaultTheme.BLACK to oklch(0.30926332f, 0.12386322f, 7.8465395f)
    ),
    _colors = mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.95283204f, 0.023875237f, 10.569774f),
        sentQuote = oklch(0.9364258f, 0.032649755f, 10.569774f),
        receivedMessage = oklch(0.99121094f, 0.004304409f, 10.569774f),
        receivedQuote = oklch(0.9665039f, 0.016754389f, 10.569774f),
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
        sentMessage = oklch(0.24780273f, 0.085917726f, 7.8465395f),
        sentQuote = oklch(0.34072876f, 0.117019944f, 7.8465395f),
        receivedMessage = oklch(0.12390137f, 0.0f, 7.8465395f),
        receivedQuote = oklch(0.22715251f, 0.0f, 7.8465395f),
      ),
    ),
  ),
  KIDS(MR.images.wallpaper_kids, "kids", 0.5f, 200f, 0.7723f,
    wallpaperBackgrounds(
      light = oklch(0.98472977f, 0.01831758f, 200.66322f),
      dark = oklch(0.18017578f, 0.025585549f, 199.76416f),
    ),
    _tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.9382386f, 0.035245255f, 200.66322f),
      DefaultTheme.DARK to oklch(0.2585798f, 0.043961763f, 199.76416f),
      DefaultTheme.SIMPLEX to oklch(0.3716418f, 0.05389406f, 217.7104f), // #ff184753
      DefaultTheme.BLACK to oklch(0.2880891f, 0.049433947f, 192.22177f)
    ),
    _colors = mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9599414f, 0.029920436f, 200.66322f),
        sentQuote = oklch(0.9388867f, 0.038093355f, 200.66322f),
        receivedMessage = oklch(0.9941406f, 0.007004261f, 200.66322f),
        receivedQuote = oklch(0.97333986f, 0.024719488f, 200.66322f),
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.3173828f, 0.05395502f, 199.76416f),
        sentQuote = oklch(0.37618583f, 0.062117502f, 199.76416f),
        receivedMessage = oklch(0.2585798f, 0.028428389f, 199.76416f),
        receivedQuote = oklch(0.29778183f, 0.032692645f, 199.76416f),
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.3662882f, 0.04909204f, 191.2229f), // #ff1a4745
        sentQuote = oklch(0.4817563f, 0.07299667f, 192.4874f), // #ff1d6b69
        receivedMessage = oklch(0.2675764f, 0.001466786f, 197.0692f), // #ff252626
        receivedQuote = oklch(0.3451987f, 0.004436687f, 174.2088f), // #ff373a39
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.24645996f, 0.042290688f, 192.22177f),
        sentQuote = oklch(0.33888245f, 0.057626568f, 192.22177f),
        receivedMessage = oklch(0.12322998f, 0.0f, 192.22177f),
        receivedQuote = oklch(0.22592163f, 0.0f, 192.22177f),
      ),
    ),
  ),
  SCHOOL(MR.images.wallpaper_school, "school", 0.5f, 239f, 0.7950f,
    wallpaperBackgrounds(
      light = oklch(0.9837532f, 0.010169387f, 225.85442f),
      dark = oklch(0.17883301f, 0.024886385f, 251.75946f),
    ),
    _tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.94544077f, 0.029549051f, 225.85442f),
      DefaultTheme.DARK to oklch(0.2515276f, 0.05627329f, 251.75946f),
      DefaultTheme.SIMPLEX to oklch(0.2929108f, 0.05102392f, 240.8139f), // #ff112f43
      DefaultTheme.BLACK to oklch(0.27668023f, 0.09167254f, 255.17474f)
    ),
    _colors = mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.95765626f, 0.026757836f, 225.85442f),
        sentQuote = oklch(0.93779296f, 0.038093355f, 225.85442f),
        receivedMessage = oklch(0.9941406f, 0.0036548972f, 225.85442f),
        receivedQuote = oklch(0.9731055f, 0.016901612f, 225.85442f),
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.31274414f, 0.06245613f, 251.75946f),
        sentQuote = oklch(0.37013462f, 0.07182455f, 251.75946f),
        receivedMessage = oklch(0.25535366f, 0.02765154f, 251.75946f),
        receivedQuote = oklch(0.29361397f, 0.031799268f, 251.75946f),
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.4187f, 0.1388f, 262.50f), // #204797
        sentQuote = oklch(0.4766f, 0.1487f, 262.58f), // #2c57af
        receivedMessage = oklch(0.2314f, 0.0685f, 268.59f), // #101a3d
        receivedQuote = oklch(0.3023f, 0.0900f, 267.90f), // #1b2a5b
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.24328613f, 0.08060831f, 255.17474f),
        sentQuote = oklch(0.33451843f, 0.10981243f, 255.17474f),
        receivedMessage = oklch(0.12164307f, 0.0f, 255.17474f),
        receivedQuote = oklch(0.22301228f, 0.0f, 255.17474f),
      ),
    ),
  ),
  TRAVEL(MR.images.wallpaper_travel, "travel", 0.5f, 315f, 1.2099f,
    wallpaperBackgrounds(
      light = oklch(0.9835678f, 0.01424849f, 325.37686f),
      dark = oklch(0.17980957f, 0.025278661f, 316.56534f),
    ),
    _tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.9464822f, 0.035007913f, 325.37686f),
      DefaultTheme.DARK to oklch(0.27441406f, 0.0701345f, 316.56534f),
      DefaultTheme.SIMPLEX to oklch(0.2948376f, 0.08277514f, 302.7197f), // #ff35204e
      DefaultTheme.BLACK to oklch(0.2714336f, 0.1344262f, 313.97916f)
    ),
    _colors = mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9571094f, 0.032885246f, 325.37686f),
        sentQuote = oklch(0.9408496f, 0.043789558f, 325.37686f),
        receivedMessage = oklch(0.9921875f, 0.0067495108f, 325.37686f),
        receivedQuote = oklch(0.9701172f, 0.024161799f, 325.37686f),
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.31201172f, 0.066930376f, 316.56534f),
        sentQuote = oklch(0.36866978f, 0.07696993f, 316.56534f),
        receivedMessage = oklch(0.25535366f, 0.028087402f, 316.56534f),
        receivedQuote = oklch(0.2931257f, 0.03230051f, 316.56534f),
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.3234681f, 0.09690244f, 299.9634f), // #ff3C255D
        sentQuote = oklch(0.4226042f, 0.1341495f, 307.8573f), // #ff623485
        receivedMessage = oklch(0.2812692f, 0.03669397f, 281.5485f), // #ff26273B
        receivedQuote = oklch(0.355058f, 0.03791292f, 286.3773f), // #ff3A394F
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.2446289f, 0.09382912f, 313.97916f),
        sentQuote = oklch(0.33636475f, 0.12779526f, 313.97916f),
        receivedMessage = oklch(0.12231445f, 0.0f, 313.97916f),
        receivedQuote = oklch(0.22424316f, 0.0f, 313.97916f),
      ),
    ),
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
// Each preset's per-mode bubble/tint colors are derived from the preset's hue
// and chroma scale, the mode's anchor (lBase) + range (lSpread) + chroma factor,
// and the element's lightness offset + chroma factor. Constants below were tuned
// empirically via the FormulaDevTools UI (see Appearance.kt) and locked in once
// every preset/mode combination satisfied the design contrast targets.
//
// Formula:
//   L = mode.lBase + mode.lSpread * elem.lOffset
//   C = mode.cFactor * elem.cFactor * theme.cScale
//   H = theme.hue

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

private fun colorForElement(mode: DefaultTheme, elem: ThemeElem, p: PresetWallpaper): Color {
  val m = MODE_PARAMS[mode]!!
  val e = elem.params
  return oklch(m.lBase + m.lSpread * e.lOffset, m.cFactor * e.cFactor * p.cScale, p.hue)
}

private fun generateBackground(p: PresetWallpaper): Map<DefaultTheme, Color> =
  wallpaperBackgrounds(light = oklch(BG_LIGHT_L, BG_LIGHT_C_BASE * p.cScale, p.hue))

private fun generateTint(p: PresetWallpaper): Map<DefaultTheme, Color> =
  DefaultTheme.entries.associateWith { colorForElement(it, ThemeElem.TINT, p) }

private fun generateColors(p: PresetWallpaper): Map<DefaultTheme, ResolvedColors> =
  DefaultTheme.entries.associateWith { mode ->
    ResolvedColors(
      sentMessage = colorForElement(mode, ThemeElem.SENT_MESSAGE, p),
      sentQuote = colorForElement(mode, ThemeElem.SENT_QUOTE, p),
      receivedMessage = colorForElement(mode, ThemeElem.RECEIVED_MESSAGE, p),
      receivedQuote = colorForElement(mode, ThemeElem.RECEIVED_QUOTE, p),
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

private fun drawToBitmap(image: ImageBitmap, imageScale: Float, tint: Color, size: Size, density: Float, layoutDirection: LayoutDirection, highQuality: Boolean = false): ImageBitmap {
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
    val tileW = (image.width * scale).roundToInt().coerceAtLeast(1)
    val tileH = (image.height * scale).roundToInt().coerceAtLeast(1)
    // Pre-scale: Compose Desktop ignores FilterQuality in drawImage, scale via platform API instead
    val tile = if (tileW == image.width && tileH == image.height) image else image.scale(tileW, tileH, highQuality)

    for (h in 0..(size.height / tileH).roundToInt()) {
      for (w in 0..(size.width / tileW).roundToInt()) {
        drawImage(
          tile,
          topLeft = Offset(w * tileW.toFloat(), h * tileH.toFloat()),
          colorFilter = ColorFilter.tint(tint, BlendMode.SrcIn),
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
  backgroundGraphicsLayer: GraphicsLayer? = null,
  highQuality: Boolean = true
): DrawResult {
  val desktopPatternScale = if (appPlatform.isDesktop) 0.55f else 1f
  val imageScale = if (imageType is WallpaperType.Preset) {
    (imageType.scale ?: 1f) * imageType.predefinedImageScale * desktopPatternScale
  } else if (imageType is WallpaperType.Image && imageType.scaleType == WallpaperScaleType.REPEAT) {
    imageType.scale ?: 1f
  } else {
    1f
  }
  val image = if (imageType is WallpaperType.Preset || (imageType is WallpaperType.Image && imageType.scaleType == WallpaperScaleType.REPEAT)) {
    drawToBitmap(image, imageScale, tint, size, density, layoutDirection, highQuality)
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
