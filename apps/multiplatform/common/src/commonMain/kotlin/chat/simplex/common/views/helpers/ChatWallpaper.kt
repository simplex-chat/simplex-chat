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
  CATS(MR.images.wallpaper_cats, "cats", 0.63f, 88.34f, 0.8172f,
    wallpaperBackgrounds(light = oklch(0.9714242f, 0.01596467f, 98.99223f)), // #ffF8F6EA
    _tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.897064f, 0.07281305f, 90.95935f), // #ffefdca6
      DefaultTheme.DARK to oklch(0.3603656f, 0.0643012f, 88.54155f), // #ff4b3b0e
      DefaultTheme.SIMPLEX to oklch(0.3797781f, 0.06842897f, 88.88896f), // #ff51400f
      DefaultTheme.BLACK to oklch(0.3603656f, 0.0643012f, 88.54155f) // #ff4b3b0e
    ),
    _colors = mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9854474f, 0.01790464f, 89.3544f), // #fffffaed
        sentQuote = oklch(0.9562038f, 0.0357691f, 89.44265f), // #fffaf0d6
        receivedMessage = oklch(0.9760699f, 0.004115805f, 91.44609f), // #ffF8F7F4
        receivedQuote = oklch(0.9465333f, 0.005762915f, 84.56661f), // #ffefede9
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.2827141f, 0.02844628f, 89.80136f), // #ff2f2919
        sentQuote = oklch(0.3550253f, 0.04770112f, 85.80835f), // #ff473a1d
        receivedMessage = oklch(0.2689313f, 0.003935312f, 84.58291f), // #ff272624
        receivedQuote = oklch(0.332832f, 0.005361989f, 91.54412f), // #ff373633
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.3402031f, 0.04537511f, 90.2498f), // #ff41371b
        sentQuote = oklch(0.4398707f, 0.0737883f, 85.23908f), // #ff654f1c
        receivedMessage = oklch(0.2689313f, 0.003935312f, 84.58291f), // #ff272624
        receivedQuote = oklch(0.332832f, 0.005361989f, 91.54412f), // #ff373633
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.3402031f, 0.04537511f, 90.2498f), // #ff41371b
        sentQuote = oklch(0.4398707f, 0.0737883f, 85.23908f), // #ff654f1c
        receivedMessage = oklch(0.2349937f, 0.005828091f, 91.60813f), // #ff1f1e1b
        receivedQuote = oklch(0.2971596f, 0.01092985f, 91.6846f), // #ff2f2d27
      ),
    )
  ),
  FLOWERS(MR.images.wallpaper_flowers, "flowers", 0.53f, 143.42f, 1.3553f,
    wallpaperBackgrounds(light = oklch(0.9718878f, 0.04671557f, 147.1246f)), // #ffE2FFE4
    _tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.8574244f, 0.1932141f, 133.0531f), // #ff9CEA59
      DefaultTheme.DARK to oklch(0.409874f, 0.1074549f, 133.4271f), // #ff31560D
      DefaultTheme.SIMPLEX to oklch(0.4415422f, 0.1170956f, 133.8571f), // #ff36600f
      DefaultTheme.BLACK to oklch(0.409874f, 0.1074549f, 133.4271f) // #ff31560D
    ),
    _colors = mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9827452f, 0.03710413f, 130.3627f), // #fff1ffe5
        sentQuote = oklch(0.9477894f, 0.07588911f, 131.1257f), // #ffdcf9c4
        receivedMessage = oklch(0.9744452f, 0.008958742f, 134.8726f), // #ffF4F8F2
        receivedQuote = oklch(0.9378814f, 0.008518542f, 145.5074f), // #ffe7ece7
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.2986395f, 0.05211595f, 153.5889f), // #ff163521
        sentQuote = oklch(0.3954021f, 0.08319059f, 152.8037f), // #ff1B5330
        receivedMessage = oklch(0.2626721f, 0.003936427f, 128.6285f), // #ff242523
        receivedQuote = oklch(0.3334174f, 0.007411477f, 128.7105f), // #ff353733
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.3611755f, 0.05678164f, 170.3752f), // #ff184739
        sentQuote = oklch(0.484029f, 0.09629127f, 159.5568f), // #ff1F6F4B
        receivedMessage = oklch(0.2626721f, 0.003936427f, 128.6285f), // #ff242523
        receivedQuote = oklch(0.3334174f, 0.007411477f, 128.7105f), // #ff353733
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.3611755f, 0.05678164f, 170.3752f), // #ff184739
        sentQuote = oklch(0.484029f, 0.09629127f, 159.5568f), // #ff1F6F4B
        receivedMessage = oklch(0.2342548f, 0.01039849f, 132.6996f), // #ff1c1f1a
        receivedQuote = oklch(0.2838948f, 0.01154375f, 128.9221f), // #ff282b25
      ),
    )
  ),
  HEARTS(MR.images.wallpaper_hearts, "hearts", 0.59f, 17.95f, 1.0504f,
    wallpaperBackgrounds(light = oklch(0.9565624f, 0.01848713f, 17.48077f)), // #ffFDECEC
    _tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.9304586f, 0.03207239f, 17.7425f), // #fffde0e0
      DefaultTheme.DARK to oklch(0.2458526f, 0.07098409f, 23.94782f), // #ff3C0F0F
      DefaultTheme.SIMPLEX to oklch(0.2574974f, 0.07614605f, 24.19117f), // #ff411010
      DefaultTheme.BLACK to oklch(0.2458526f, 0.07098409f, 23.94782f) // #ff3C0F0F
    ),
    _colors = mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9757184f, 0.01184164f, 17.35934f), // #fffff4f4
        sentQuote = oklch(0.9300344f, 0.0354728f, 17.80723f), // #ffffdfdf
        receivedMessage = oklch(0.9746758f, 0.002137086f, 17.19433f), // #fff8f6f6
        receivedQuote = oklch(0.9431687f, 0.004317648f, 17.23361f), // #ffefebeb
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.2353791f, 0.04398437f, 20.94719f), // #ff301515
        sentQuote = oklch(0.2920391f, 0.07914221f, 23.35544f), // #ff4C1818
        receivedMessage = oklch(0.2510736f, 0.004554155f, 17.46058f), // #ff242121
        receivedQuote = oklch(0.3352158f, 0.008515606f, 17.58481f), // #ff3b3535
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.2941874f, 0.07322977f, 4.102547f), // #ff491A28
        sentQuote = oklch(0.3831088f, 0.1201278f, 18.61089f), // #ff761F29
        receivedMessage = oklch(0.2510736f, 0.004554155f, 17.46058f), // #ff242121
        receivedQuote = oklch(0.3352158f, 0.008515606f, 17.58481f), // #ff3b3535
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.2941874f, 0.07322977f, 4.102547f), // #ff491A28
        sentQuote = oklch(0.3831088f, 0.1201278f, 18.61089f), // #ff761F29
        receivedMessage = oklch(0.2267386f, 0.00626924f, 17.6236f), // #ff1f1b1b
        receivedQuote = oklch(0.2776199f, 0.012034f, 17.89987f), // #ff2e2626
      ),
    )
  ),
  KIDS(MR.images.wallpaper_kids, "kids", 0.53f, 200.75f, 0.7723f,
    wallpaperBackgrounds(light = oklch(0.9693045f, 0.03516977f, 192.2433f)), // #ffdbfdfb
    _tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.9123625f, 0.06815507f, 211.1344f), // #ffadeffc
      DefaultTheme.DARK to oklch(0.3473769f, 0.04958945f, 218.0525f), // #ff16404B
      DefaultTheme.SIMPLEX to oklch(0.3716418f, 0.05389406f, 217.7104f), // #ff184753
      DefaultTheme.BLACK to oklch(0.3473769f, 0.04958945f, 218.0525f) // #ff16404B
    ),
    _colors = mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9827091f, 0.02093746f, 200.4479f), // #ffeafeff
        sentQuote = oklch(0.9392156f, 0.04239295f, 201.9221f), // #ffcbf4f7
        receivedMessage = oklch(0.9798523f, 0.007408877f, 197.0357f), // #fff3fafa
        receivedQuote = oklch(0.9438775f, 0.0117012f, 196.9581f), // #ffe4efef
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.2881511f, 0.03214503f, 192.2759f), // #ff16302F
        sentQuote = oklch(0.3764664f, 0.05129536f, 193.292f), // #ff1a4a49
        receivedMessage = oklch(0.2675764f, 0.001466786f, 197.0692f), // #ff252626
        receivedQuote = oklch(0.3451987f, 0.004436687f, 174.2088f), // #ff373a39
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.3662882f, 0.04909204f, 191.2229f), // #ff1a4745
        sentQuote = oklch(0.4817563f, 0.07299667f, 192.4874f), // #ff1d6b69
        receivedMessage = oklch(0.2675764f, 0.001466786f, 197.0692f), // #ff252626
        receivedQuote = oklch(0.3451987f, 0.004436687f, 174.2088f), // #ff373a39
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.3662882f, 0.04909204f, 191.2229f), // #ff1a4745
        sentQuote = oklch(0.4817563f, 0.07299667f, 192.4874f), // #ff1d6b69
        receivedMessage = oklch(0.2382215f, 0.001508911f, 197.0555f), // #ff1e1f1f
        receivedQuote = oklch(0.2833724f, 0.007955636f, 169.798f), // #ff262b29
      ),
    )
  ),
  SCHOOL(MR.images.wallpaper_school, "school", 0.53f, 243.37f, 0.7950f,
    wallpaperBackgrounds(light = oklch(0.9626785f, 0.02004578f, 238.6614f)), // #ffE7F5FF
    _tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.9252349f, 0.04096641f, 238.0518f), // #ffCEEBFF
      DefaultTheme.DARK to oklch(0.2700986f, 0.04630937f, 241.5568f), // #ff0F293B
      DefaultTheme.SIMPLEX to oklch(0.2929108f, 0.05102392f, 240.8139f), // #ff112f43
      DefaultTheme.BLACK to oklch(0.2700986f, 0.04630937f, 241.5568f) // #ff0F293B
    ),
    _colors = mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9756479f, 0.01416295f, 231.2013f), // #ffeef9ff
        sentQuote = oklch(0.9331527f, 0.03006113f, 232.4212f), // #ffD6EDFA
        receivedMessage = oklch(0.9697657f, 0.005748723f, 264.5325f), // #ffF3F5F9
        receivedQuote = oklch(0.9296755f, 0.00918803f, 258.3366f), // #ffe4e8ee
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.267226f, 0.03061943f, 237.8609f), // #ff172833
        sentQuote = oklch(0.3464064f, 0.04943852f, 232.4005f), // #ff1C3E4F
        receivedMessage = oklch(0.2764251f, 0.007910622f, 264.4375f), // #ff26282c
        receivedQuote = oklch(0.3548081f, 0.008034593f, 255.5451f), // #ff393c40
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.3481476f, 0.07023845f, 249.9259f), // #ff1A3C5D
        sentQuote = oklch(0.4520089f, 0.08394516f, 241.1934f), // #ff235b80
        receivedMessage = oklch(0.2764251f, 0.007910622f, 264.4375f), // #ff26282c
        receivedQuote = oklch(0.3548081f, 0.008034593f, 255.5451f), // #ff393c40
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.3481476f, 0.07023845f, 249.9259f), // #ff1A3C5D
        sentQuote = oklch(0.4520089f, 0.08394516f, 241.1934f), // #ff235b80
        receivedMessage = oklch(0.2356588f, 0.007789041f, 274.6063f), // #ff1d1e22
        receivedQuote = oklch(0.2886546f, 0.007823012f, 264.445f), // #ff292b2f
      ),
    )
  ),
  TRAVEL(MR.images.wallpaper_travel, "travel", 0.68f, 304.95f, 1.2099f,
    wallpaperBackgrounds(light = oklch(0.9626377f, 0.0253131f, 313.9639f)), // #fff9eeff
    _tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.9174161f, 0.05105522f, 309.6281f), // #ffeedbfe
      DefaultTheme.DARK to oklch(0.2817417f, 0.07665313f, 302.6645f), // #ff311E48
      DefaultTheme.SIMPLEX to oklch(0.2948376f, 0.08277514f, 302.7197f), // #ff35204e
      DefaultTheme.BLACK to oklch(0.2817417f, 0.07665313f, 302.6645f) // #ff311E48
    ),
    _colors = mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9803204f, 0.01342671f, 314.7601f), // #fffcf6ff
        sentQuote = oklch(0.9294779f, 0.04197705f, 313.6968f), // #fff2e0fc
        receivedMessage = oklch(0.9695303f, 0.004487354f, 314.8044f), // #ffF6F4F7
        receivedQuote = oklch(0.9385522f, 0.007899312f, 319.4466f), // #ffede9ee
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.2929984f, 0.04120036f, 312.1162f), // #ff33263B
        sentQuote = oklch(0.3876602f, 0.07087001f, 315.7654f), // #ff53385E
        receivedMessage = oklch(0.2678179f, 0.006190444f, 314.7144f), // #ff272528
        receivedQuote = oklch(0.3435397f, 0.01317027f, 310.9424f), // #ff3B373E
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.3234681f, 0.09690244f, 299.9634f), // #ff3C255D
        sentQuote = oklch(0.4226042f, 0.1341495f, 307.8573f), // #ff623485
        receivedMessage = oklch(0.2812692f, 0.03669397f, 281.5485f), // #ff26273B
        receivedQuote = oklch(0.355058f, 0.03791292f, 286.3773f), // #ff3A394F
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.3234681f, 0.09690244f, 299.9634f), // #ff3C255D
        sentQuote = oklch(0.4226042f, 0.1341495f, 307.8573f), // #ff623485
        receivedMessage = oklch(0.2454222f, 0.009540156f, 325.8636f), // #ff231f23
        receivedQuote = oklch(0.2874049f, 0.0149843f, 302.5009f), // #ff2c2931
      ),
    )
  );

  val background: Map<DefaultTheme, Color> get() = generateBackground(this) // legacy: _background
  val tint: Map<DefaultTheme, Color> get() = generateTint(this) // legacy: _tint
  val colors: Map<DefaultTheme, ResolvedColors> get() = generateColors(this) // legacy: _colors

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

fun wallpaperBackgrounds(light: Color): Map<DefaultTheme, Color> =
  mapOf(
    DefaultTheme.LIGHT to light,
    DefaultTheme.DARK to oklch(0.1822037f, 0f, 0f), // #ff121212
    DefaultTheme.SIMPLEX to oklch(0.2024453f, 0.03849037f, 273.4875f), // #ff111528
    DefaultTheme.BLACK to oklch(0.1285578f, 0f, 0f) // #ff070707
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
