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

// Per-wallpaper SIMPLEX gradient stops: chat background + the four bubble slots, plus the
// secondary/author text tints. Background stops are drawn by simplexGradient(); bubble stops
// and the semi-transparent text tints are consumed by SimplexBrushes.
data class SimplexStops(
  val bg: Array<Pair<Float, Color>>,
  val sent: Array<Pair<Float, Color>>,
  val sentQuote: Array<Pair<Float, Color>>,
  val received: Array<Pair<Float, Color>>,
  val receivedQuote: Array<Pair<Float, Color>>,
  val secondaryTint: Color,
  val authorTint: Color,
)

enum class PresetWallpaper(
  val res: ImageResource,
  val filename: String,
  val scale: Float,
  val background: Map<DefaultTheme, Color>,
  val tint: Map<DefaultTheme, Color>,
  val colors: Map<DefaultTheme, ThemeColors>,
  val simplexStops: SimplexStops? = null,
) {
  CATS(MR.images.wallpaper_cats, "cats", 0.63f,
    wallpaperBackgrounds(light = "#ffF8F6EA"),
    tint = mapOf(
      DefaultTheme.LIGHT to "#ffefdca6".colorFromReadableHex(),
      DefaultTheme.DARK to "#ff4b3b0e".colorFromReadableHex(),
      DefaultTheme.SIMPLEX to oklch(0.8356f, 0.1909f, 87.09f, 0.12f),
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
    ),
    simplexStops = SimplexStops(
      bg = arrayOf(
        0.10f to oklch(0.0f, 0.0f, 0.0f),
        0.55f to oklch(0.1650f, 0.0214f, 88f),
        0.85f to oklch(0.2850f, 0.0659f, 88f),
        1.00f to oklch(0.3400f, 0.0820f, 105f),
      ),
      sent = arrayOf(
        0.20f to oklch(0.3380f, 0.0780f, 88f),
        0.55f to oklch(0.3550f, 0.0795f, 88f),
        0.80f to oklch(0.4350f, 0.1005f, 88f),
        1.00f to oklch(0.4500f, 0.1085f, 105f),
      ),
      sentQuote = arrayOf(
        0.20f to oklch(0.3830f, 0.0884f, 88f),
        0.55f to oklch(0.3950f, 0.0885f, 88f),
        0.80f to oklch(0.4550f, 0.1051f, 88f),
        1.00f to oklch(0.4750f, 0.1145f, 105f),
      ),
      received = arrayOf(
        0.20f to oklch(0.2200f, 0.0259f, 88f),
        0.55f to oklch(0.2550f, 0.0301f, 88f),
        0.84f to oklch(0.3200f, 0.0641f, 88f),
        1.00f to oklch(0.4050f, 0.0976f, 105f),
      ),
      receivedQuote = arrayOf(
        0.20f to oklch(0.2730f, 0.0321f, 88f),
        0.55f to oklch(0.3000f, 0.0354f, 88f),
        0.84f to oklch(0.3700f, 0.0742f, 88f),
        1.00f to oklch(0.4350f, 0.1049f, 105f),
      ),
      secondaryTint = oklch(0.8874f, 0.1270f, 86.2f, 0.6f),
      authorTint = oklch(0.8518f, 0.0736f, 87.2f, 0.6f),
    )
  ),
  FLOWERS(MR.images.wallpaper_flowers, "flowers", 0.53f,
    wallpaperBackgrounds(light = "#ffE2FFE4"),
    tint = mapOf(
      DefaultTheme.LIGHT to "#ff9CEA59".colorFromReadableHex(),
      DefaultTheme.DARK to "#ff31560D".colorFromReadableHex(),
      DefaultTheme.SIMPLEX to oklch(0.8000f, 0.1500f, 140f, 0.12f),
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
    ),
    simplexStops = SimplexStops(
      bg = arrayOf(
        0.10f to oklch(0.0f, 0.0f, 0.0f),
        0.55f to oklch(0.1800f, 0.0330f, 130f),
        0.85f to oklch(0.3000f, 0.0926f, 130f),
        1.00f to oklch(0.4250f, 0.1080f, 113f),
      ),
      sent = arrayOf(
        0.20f to oklch(0.3450f, 0.1066f, 130f),
        0.55f to oklch(0.3700f, 0.1107f, 130f),
        0.80f to oklch(0.4820f, 0.1421f, 130f),
        1.00f to oklch(0.5030f, 0.1278f, 113f),
      ),
      sentQuote = arrayOf(
        0.20f to oklch(0.3930f, 0.1211f, 130f),
        0.55f to oklch(0.4100f, 0.1226f, 130f),
        0.80f to oklch(0.4940f, 0.1523f, 130f),
        1.00f to oklch(0.5220f, 0.1326f, 113f),
      ),
      received = arrayOf(
        0.20f to oklch(0.2210f, 0.0369f, 130f),
        0.55f to oklch(0.2700f, 0.0451f, 130f),
        0.84f to oklch(0.3610f, 0.1024f, 130f),
        1.00f to oklch(0.4800f, 0.1217f, 113f),
      ),
      receivedQuote = arrayOf(
        0.20f to oklch(0.2770f, 0.0461f, 130f),
        0.55f to oklch(0.3150f, 0.0526f, 130f),
        0.84f to oklch(0.4130f, 0.1171f, 130f),
        1.00f to oklch(0.5040f, 0.1217f, 113f),
      ),
      secondaryTint = oklch(0.8874f, 0.1270f, 130.0f, 0.6f),
      authorTint = oklch(0.8518f, 0.0736f, 130.0f, 0.6f),
    )
  ),
  HEARTS(MR.images.wallpaper_hearts, "hearts", 0.59f,
    wallpaperBackgrounds(light = "#ffFDECEC"),
    tint = mapOf(
      DefaultTheme.LIGHT to "#fffde0e0".colorFromReadableHex(),
      DefaultTheme.DARK to "#ff3c0f0f".colorFromReadableHex(),
      DefaultTheme.SIMPLEX to oklch(0.7500f, 0.1400f, 20f, 0.12f),
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
    ),
    simplexStops = SimplexStops(
      bg = arrayOf(
        0.10f to oklch(0.0f, 0.0f, 0.0f),
        0.55f to oklch(0.1900f, 0.0530f, 5f),
        0.85f to oklch(0.3100f, 0.1147f, 5f),
        1.00f to oklch(0.4350f, 0.1147f, 22f),
      ),
      sent = arrayOf(
        0.20f to oklch(0.3550f, 0.1236f, 5f),
        0.55f to oklch(0.3800f, 0.1442f, 5f),
        0.80f to oklch(0.4920f, 0.1442f, 5f),
        1.00f to oklch(0.5130f, 0.1545f, 22f),
      ),
      sentQuote = arrayOf(
        0.20f to oklch(0.4030f, 0.1339f, 5f),
        0.55f to oklch(0.4200f, 0.1545f, 5f),
        0.80f to oklch(0.5040f, 0.1545f, 5f),
        1.00f to oklch(0.5320f, 0.1545f, 22f),
      ),
      received = arrayOf(
        0.20f to oklch(0.2310f, 0.0586f, 5f),
        0.55f to oklch(0.2800f, 0.0710f, 5f),
        0.84f to oklch(0.3710f, 0.1234f, 5f),
        1.00f to oklch(0.4900f, 0.1234f, 22f),
      ),
      receivedQuote = arrayOf(
        0.20f to oklch(0.2860f, 0.0669f, 5f),
        0.55f to oklch(0.3250f, 0.0770f, 5f),
        0.84f to oklch(0.4230f, 0.1319f, 5f),
        1.00f to oklch(0.5140f, 0.1234f, 22f),
      ),
      secondaryTint = oklch(0.8874f, 0.1270f, 5.0f, 0.6f),
      authorTint = oklch(0.8518f, 0.0736f, 5.0f, 0.6f),
    )
  ),
  KIDS(MR.images.wallpaper_kids, "kids", 0.53f,
    wallpaperBackgrounds(light = "#ffdbfdfb"),
    tint = mapOf(
      DefaultTheme.LIGHT to "#ffadeffc".colorFromReadableHex(),
      DefaultTheme.DARK to "#ff16404B".colorFromReadableHex(),
      DefaultTheme.SIMPLEX to oklch(0.7700f, 0.1200f, 205f, 0.12f),
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
    ),
    simplexStops = SimplexStops(
      bg = arrayOf(
        0.10f to oklch(0.0f, 0.0f, 0.0f),
        0.55f to oklch(0.1900f, 0.0238f, 200f),
        0.85f to oklch(0.3100f, 0.0692f, 200f),
        1.00f to oklch(0.4350f, 0.1011f, 217f),
      ),
      sent = arrayOf(
        0.20f to oklch(0.3550f, 0.0794f, 200f),
        0.55f to oklch(0.3800f, 0.0823f, 200f),
        0.80f to oklch(0.4920f, 0.1099f, 200f),
        1.00f to oklch(0.5130f, 0.1193f, 217f),
      ),
      sentQuote = arrayOf(
        0.20f to oklch(0.4030f, 0.0899f, 200f),
        0.55f to oklch(0.4200f, 0.0909f, 200f),
        0.80f to oklch(0.5040f, 0.1126f, 200f),
        1.00f to oklch(0.5320f, 0.1237f, 217f),
      ),
      received = arrayOf(
        0.20f to oklch(0.2310f, 0.0263f, 200f),
        0.55f to oklch(0.2800f, 0.0319f, 200f),
        0.84f to oklch(0.3710f, 0.0719f, 200f),
        1.00f to oklch(0.4900f, 0.1139f, 217f),
      ),
      receivedQuote = arrayOf(
        0.20f to oklch(0.2860f, 0.0326f, 200f),
        0.55f to oklch(0.3250f, 0.0370f, 200f),
        0.84f to oklch(0.4230f, 0.0819f, 200f),
        1.00f to oklch(0.5140f, 0.1195f, 217f),
      ),
      secondaryTint = oklch(0.8874f, 0.1270f, 200.0f, 0.6f),
      authorTint = oklch(0.8518f, 0.0736f, 200.0f, 0.6f),
    )
  ),
  SCHOOL(MR.images.wallpaper_school, "school", 0.53f,
    wallpaperBackgrounds(light = "#ffE7F5FF"),
    tint = mapOf(
      DefaultTheme.LIGHT to "#ffCEEBFF".colorFromReadableHex(),
      DefaultTheme.DARK to "#ff0F293B".colorFromReadableHex(),
      DefaultTheme.SIMPLEX to oklch(0.7730f, 0.1419f, 239.87f, 0.12f),
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
    ),
    simplexStops = SimplexStops(
      bg = arrayOf(
        0.10f to oklch(0.0f, 0.0f, 0.0f),
        0.55f to oklch(0.2200f, 0.0717f, 271f),
        0.85f to oklch(0.3400f, 0.1218f, 271f),
        1.00f to oklch(0.5700f, 0.1218f, 254f),
      ),
      sent = arrayOf(
        0.20f to oklch(0.3750f, 0.1290f, 266f),
        0.55f to oklch(0.4100f, 0.1505f, 266f),
        0.80f to oklch(0.5700f, 0.1505f, 266f),
        1.00f to oklch(0.6000f, 0.1612f, 254f),
      ),
      sentQuote = arrayOf(
        0.20f to oklch(0.4250f, 0.1397f, 266f),
        0.55f to oklch(0.4500f, 0.1612f, 266f),
        0.80f to oklch(0.5700f, 0.1612f, 266f),
        1.00f to oklch(0.6100f, 0.1612f, 254f),
      ),
      received = arrayOf(
        0.20f to oklch(0.2400f, 0.0717f, 271f),
        0.55f to oklch(0.3100f, 0.0824f, 271f),
        0.84f to oklch(0.4400f, 0.1286f, 271f),
        1.00f to oklch(0.6100f, 0.1286f, 254f),
      ),
      receivedQuote = arrayOf(
        0.20f to oklch(0.3000f, 0.0771f, 271f),
        0.55f to oklch(0.3550f, 0.0875f, 271f),
        0.84f to oklch(0.4950f, 0.1346f, 271f),
        1.00f to oklch(0.6250f, 0.1286f, 254f),
      ),
      secondaryTint = oklch(0.8874f, 0.1270f, 271.0f, 0.6f),
      authorTint = oklch(0.8518f, 0.0736f, 271.0f, 0.6f),
    )
  ),
  TRAVEL(MR.images.wallpaper_travel, "travel", 0.68f,
    wallpaperBackgrounds(light = "#fff9eeff"),
    tint = mapOf(
      DefaultTheme.LIGHT to "#ffeedbfe".colorFromReadableHex(),
      DefaultTheme.DARK to "#ff311E48".colorFromReadableHex(),
      DefaultTheme.SIMPLEX to oklch(0.7800f, 0.1500f, 320f, 0.12f),
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
    ),
    simplexStops = SimplexStops(
      bg = arrayOf(
        0.10f to oklch(0.0f, 0.0f, 0.0f),
        0.55f to oklch(0.2000f, 0.0650f, 315f),
        0.85f to oklch(0.3200f, 0.1171f, 315f),
        1.00f to oklch(0.4800f, 0.1171f, 298f),
      ),
      sent = arrayOf(
        0.20f to oklch(0.3620f, 0.1254f, 310f),
        0.55f to oklch(0.3900f, 0.1463f, 310f),
        0.80f to oklch(0.5180f, 0.1463f, 310f),
        1.00f to oklch(0.5420f, 0.1567f, 298f),
      ),
      sentQuote = arrayOf(
        0.20f to oklch(0.4100f, 0.1358f, 310f),
        0.55f to oklch(0.4300f, 0.1567f, 310f),
        0.80f to oklch(0.5260f, 0.1567f, 310f),
        1.00f to oklch(0.5580f, 0.1567f, 298f),
      ),
      received = arrayOf(
        0.20f to oklch(0.2340f, 0.0650f, 315f),
        0.55f to oklch(0.2900f, 0.0754f, 315f),
        0.84f to oklch(0.3940f, 0.1251f, 315f),
        1.00f to oklch(0.5300f, 0.1251f, 298f),
      ),
      receivedQuote = arrayOf(
        0.20f to oklch(0.2910f, 0.0703f, 315f),
        0.55f to oklch(0.3350f, 0.0805f, 315f),
        0.84f to oklch(0.4470f, 0.1328f, 315f),
        1.00f to oklch(0.5510f, 0.1251f, 298f),
      ),
      secondaryTint = oklch(0.8874f, 0.1270f, 315.0f, 0.6f),
      authorTint = oklch(0.8518f, 0.0736f, 315.0f, 0.6f),
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

// SIMPLEX preset wallpapers paint a per-wallpaper gradient background (from SimplexStops.bg) instead of a
// flat colour, along the same 20° axis the chat bubbles/text sample. previewMode expands the axis so the
// small theme-preview swatch shows a representative slice rather than the full sweep.
private fun simplexGradient(filename: String, size: Size, previewMode: Boolean = false): Brush? {
  val stops = PresetWallpaper.from(filename)?.simplexStops?.bg ?: return null
  val theta = 20.0 * PI / 180.0
  val dx = sin(theta).toFloat()
  val dy = -cos(theta).toFloat()
  val cx = size.width / 2f
  val cy = size.height / 2f
  val maxP = abs(dx) * cx + abs(dy) * cy
  val bl = Offset(cx - dx * maxP, cy - dy * maxP)
  val tr = Offset(cx + dx * maxP, cy + dy * maxP)
  val span = tr - bl
  val axisStart = if (previewMode) bl - span else bl
  val axisEnd = if (previewMode) tr + span * 2f else tr
  return Brush.linearGradient(
    colorStops = stops,
    start = axisStart,
    end = axisEnd,
  )
}

fun CacheDrawScope.chatViewBackground(
  image: ImageBitmap,
  imageType: WallpaperType,
  background: Color,
  tint: Color,
  graphicsLayerSize: MutableState<IntSize>? = null,
  backgroundGraphicsLayer: GraphicsLayer? = null,
  theme: DefaultTheme = DefaultTheme.LIGHT,
  previewMode: Boolean = false,
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

  // SIMPLEX preset: paint the wallpaper gradient instead of the flat background; the pattern still draws on top.
  val simplexBrush: Brush? =
    if (theme == DefaultTheme.SIMPLEX && imageType is WallpaperType.Preset) simplexGradient(imageType.filename, size, previewMode)
    else null

  return onDrawBehind {
    copyBackgroundToAppBar(graphicsLayerSize, backgroundGraphicsLayer) {
      val quality = if (appPlatform.isAndroid) FilterQuality.High else FilterQuality.Low
      if (simplexBrush != null) {
        drawRect(brush = simplexBrush)
      } else {
        drawRect(background)
      }
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
