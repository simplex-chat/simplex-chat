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
  val colors: Map<DefaultTheme, ResolvedColors>,
) {
  CATS(MR.images.wallpaper_cats, "cats", 0.63f,
    mapOf(
      DefaultTheme.LIGHT to oklch(0.9714242f, 0.01596467f, 98.99223f),
      DefaultTheme.DARK to oklch(0.1822037f, 0f, 0f),
      DefaultTheme.SIMPLEX to oklch(0.2024453f, 0.03849037f, 273.4875f),
      DefaultTheme.BLACK to oklch(0.1285578f, 0f, 0f)
    ),
    tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.897064f, 0.07281305f, 90.95935f),
      DefaultTheme.DARK to oklch(0.3603656f, 0.0643012f, 88.54155f),
      DefaultTheme.SIMPLEX to oklch(0.3797781f, 0.06842897f, 88.88896f),
      DefaultTheme.BLACK to oklch(0.3603656f, 0.0643012f, 88.54155f)
    ),
    mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9854474f, 0.01790464f, 89.3544f),
        sentQuote = oklch(0.9562038f, 0.0357691f, 89.44265f),
        receivedMessage = oklch(0.9760699f, 0.004115805f, 91.44609f),
        receivedQuote = oklch(0.9465333f, 0.005762915f, 84.56661f),
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.2827141f, 0.02844628f, 89.80136f),
        sentQuote = oklch(0.3550253f, 0.04770112f, 85.80835f),
        receivedMessage = oklch(0.2689313f, 0.003935312f, 84.58291f),
        receivedQuote = oklch(0.332832f, 0.005361989f, 91.54412f),
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.3402031f, 0.04537511f, 90.2498f),
        sentQuote = oklch(0.4398707f, 0.0737883f, 85.23908f),
        receivedMessage = oklch(0.2689313f, 0.003935312f, 84.58291f),
        receivedQuote = oklch(0.332832f, 0.005361989f, 91.54412f),
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.3402031f, 0.04537511f, 90.2498f),
        sentQuote = oklch(0.4398707f, 0.0737883f, 85.23908f),
        receivedMessage = oklch(0.2349937f, 0.005828091f, 91.60813f),
        receivedQuote = oklch(0.2971596f, 0.01092985f, 91.6846f),
      ),
    )
  ),
  FLOWERS(MR.images.wallpaper_flowers, "flowers", 0.53f,
    mapOf(
      DefaultTheme.LIGHT to oklch(0.9718878f, 0.04671557f, 147.1246f),
      DefaultTheme.DARK to oklch(0.1822037f, 0f, 0f),
      DefaultTheme.SIMPLEX to oklch(0.2024453f, 0.03849037f, 273.4875f),
      DefaultTheme.BLACK to oklch(0.1285578f, 0f, 0f)
    ),
    tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.8574244f, 0.1932141f, 133.0531f),
      DefaultTheme.DARK to oklch(0.409874f, 0.1074549f, 133.4271f),
      DefaultTheme.SIMPLEX to oklch(0.4415422f, 0.1170956f, 133.8571f),
      DefaultTheme.BLACK to oklch(0.409874f, 0.1074549f, 133.4271f)
    ),
    mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9827452f, 0.03710413f, 130.3627f),
        sentQuote = oklch(0.9477894f, 0.07588911f, 131.1257f),
        receivedMessage = oklch(0.9744452f, 0.008958742f, 134.8726f),
        receivedQuote = oklch(0.9378814f, 0.008518542f, 145.5074f),
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.2986395f, 0.05211595f, 153.5889f),
        sentQuote = oklch(0.3954021f, 0.08319059f, 152.8037f),
        receivedMessage = oklch(0.2626721f, 0.003936427f, 128.6285f),
        receivedQuote = oklch(0.3334174f, 0.007411477f, 128.7105f),
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.3611755f, 0.05678164f, 170.3752f),
        sentQuote = oklch(0.484029f, 0.09629127f, 159.5568f),
        receivedMessage = oklch(0.2626721f, 0.003936427f, 128.6285f),
        receivedQuote = oklch(0.3334174f, 0.007411477f, 128.7105f),
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.3611755f, 0.05678164f, 170.3752f),
        sentQuote = oklch(0.484029f, 0.09629127f, 159.5568f),
        receivedMessage = oklch(0.2342548f, 0.01039849f, 132.6996f),
        receivedQuote = oklch(0.2838948f, 0.01154375f, 128.9221f),
      ),
    )
  ),
  HEARTS(MR.images.wallpaper_hearts, "hearts", 0.59f,
    mapOf(
      DefaultTheme.LIGHT to oklch(0.9565624f, 0.01848713f, 17.48077f),
      DefaultTheme.DARK to oklch(0.1822037f, 0f, 0f),
      DefaultTheme.SIMPLEX to oklch(0.2024453f, 0.03849037f, 273.4875f),
      DefaultTheme.BLACK to oklch(0.1285578f, 0f, 0f)
    ),
    tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.9304586f, 0.03207239f, 17.7425f),
      DefaultTheme.DARK to oklch(0.2458526f, 0.07098409f, 23.94782f),
      DefaultTheme.SIMPLEX to oklch(0.2574974f, 0.07614605f, 24.19117f),
      DefaultTheme.BLACK to oklch(0.2458526f, 0.07098409f, 23.94782f)
    ),
    mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9757184f, 0.01184164f, 17.35934f),
        sentQuote = oklch(0.9300344f, 0.0354728f, 17.80723f),
        receivedMessage = oklch(0.9746758f, 0.002137086f, 17.19433f),
        receivedQuote = oklch(0.9431687f, 0.004317648f, 17.23361f),
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.2353791f, 0.04398437f, 20.94719f),
        sentQuote = oklch(0.2920391f, 0.07914221f, 23.35544f),
        receivedMessage = oklch(0.2510736f, 0.004554155f, 17.46058f),
        receivedQuote = oklch(0.3352158f, 0.008515606f, 17.58481f),
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.2941874f, 0.07322977f, 4.102547f),
        sentQuote = oklch(0.3831088f, 0.1201278f, 18.61089f),
        receivedMessage = oklch(0.2510736f, 0.004554155f, 17.46058f),
        receivedQuote = oklch(0.3352158f, 0.008515606f, 17.58481f),
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.2941874f, 0.07322977f, 4.102547f),
        sentQuote = oklch(0.3831088f, 0.1201278f, 18.61089f),
        receivedMessage = oklch(0.2267386f, 0.00626924f, 17.6236f),
        receivedQuote = oklch(0.2776199f, 0.012034f, 17.89987f),
      ),
    )
  ),
  KIDS(MR.images.wallpaper_kids, "kids", 0.53f,
    mapOf(
      DefaultTheme.LIGHT to oklch(0.9693045f, 0.03516977f, 192.2433f),
      DefaultTheme.DARK to oklch(0.1822037f, 0f, 0f),
      DefaultTheme.SIMPLEX to oklch(0.2024453f, 0.03849037f, 273.4875f),
      DefaultTheme.BLACK to oklch(0.1285578f, 0f, 0f)
    ),
    tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.9123625f, 0.06815507f, 211.1344f),
      DefaultTheme.DARK to oklch(0.3473769f, 0.04958945f, 218.0525f),
      DefaultTheme.SIMPLEX to oklch(0.3716418f, 0.05389406f, 217.7104f),
      DefaultTheme.BLACK to oklch(0.3473769f, 0.04958945f, 218.0525f)
    ),
    mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9827091f, 0.02093746f, 200.4479f),
        sentQuote = oklch(0.9392156f, 0.04239295f, 201.9221f),
        receivedMessage = oklch(0.9798523f, 0.007408877f, 197.0357f),
        receivedQuote = oklch(0.9438775f, 0.0117012f, 196.9581f),
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.2881511f, 0.03214503f, 192.2759f),
        sentQuote = oklch(0.3764664f, 0.05129536f, 193.292f),
        receivedMessage = oklch(0.2675764f, 0.001466786f, 197.0692f),
        receivedQuote = oklch(0.3451987f, 0.004436687f, 174.2088f),
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.3662882f, 0.04909204f, 191.2229f),
        sentQuote = oklch(0.4817563f, 0.07299667f, 192.4874f),
        receivedMessage = oklch(0.2675764f, 0.001466786f, 197.0692f),
        receivedQuote = oklch(0.3451987f, 0.004436687f, 174.2088f),
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.3662882f, 0.04909204f, 191.2229f),
        sentQuote = oklch(0.4817563f, 0.07299667f, 192.4874f),
        receivedMessage = oklch(0.2382215f, 0.001508911f, 197.0555f),
        receivedQuote = oklch(0.2833724f, 0.007955636f, 169.798f),
      ),
    )
  ),
  SCHOOL(MR.images.wallpaper_school, "school", 0.53f,
    mapOf(
      DefaultTheme.LIGHT to oklch(0.9626785f, 0.02004578f, 238.6614f),
      DefaultTheme.DARK to oklch(0.1822037f, 0f, 0f),
      DefaultTheme.SIMPLEX to oklch(0.2024453f, 0.03849037f, 273.4875f),
      DefaultTheme.BLACK to oklch(0.1285578f, 0f, 0f)
    ),
    tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.9252349f, 0.04096641f, 238.0518f),
      DefaultTheme.DARK to oklch(0.2700986f, 0.04630937f, 241.5568f),
      DefaultTheme.SIMPLEX to oklch(0.2929108f, 0.05102392f, 240.8139f),
      DefaultTheme.BLACK to oklch(0.2700986f, 0.04630937f, 241.5568f)
    ),
    mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9756479f, 0.01416295f, 231.2013f),
        sentQuote = oklch(0.9331527f, 0.03006113f, 232.4212f),
        receivedMessage = oklch(0.9697657f, 0.005748723f, 264.5325f),
        receivedQuote = oklch(0.9296755f, 0.00918803f, 258.3366f),
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.267226f, 0.03061943f, 237.8609f),
        sentQuote = oklch(0.3464064f, 0.04943852f, 232.4005f),
        receivedMessage = oklch(0.2764251f, 0.007910622f, 264.4375f),
        receivedQuote = oklch(0.3548081f, 0.008034593f, 255.5451f),
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.3481476f, 0.07023845f, 249.9259f),
        sentQuote = oklch(0.4520089f, 0.08394516f, 241.1934f),
        receivedMessage = oklch(0.2764251f, 0.007910622f, 264.4375f),
        receivedQuote = oklch(0.3548081f, 0.008034593f, 255.5451f),
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.3481476f, 0.07023845f, 249.9259f),
        sentQuote = oklch(0.4520089f, 0.08394516f, 241.1934f),
        receivedMessage = oklch(0.2356588f, 0.007789041f, 274.6063f),
        receivedQuote = oklch(0.2886546f, 0.007823012f, 264.445f),
      ),
    )
  ),
  TRAVEL(MR.images.wallpaper_travel, "travel", 0.68f,
    mapOf(
      DefaultTheme.LIGHT to oklch(0.9626377f, 0.0253131f, 313.9639f),
      DefaultTheme.DARK to oklch(0.1822037f, 0f, 0f),
      DefaultTheme.SIMPLEX to oklch(0.2024453f, 0.03849037f, 273.4875f),
      DefaultTheme.BLACK to oklch(0.1285578f, 0f, 0f)
    ),
    tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.9174161f, 0.05105522f, 309.6281f),
      DefaultTheme.DARK to oklch(0.2817417f, 0.07665313f, 302.6645f),
      DefaultTheme.SIMPLEX to oklch(0.2948376f, 0.08277514f, 302.7197f),
      DefaultTheme.BLACK to oklch(0.2817417f, 0.07665313f, 302.6645f)
    ),
    mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9803204f, 0.01342671f, 314.7601f),
        sentQuote = oklch(0.9294779f, 0.04197705f, 313.6968f),
        receivedMessage = oklch(0.9695303f, 0.004487354f, 314.8044f),
        receivedQuote = oklch(0.9385522f, 0.007899312f, 319.4466f),
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.2929984f, 0.04120036f, 312.1162f),
        sentQuote = oklch(0.3876602f, 0.07087001f, 315.7654f),
        receivedMessage = oklch(0.2678179f, 0.006190444f, 314.7144f),
        receivedQuote = oklch(0.3435397f, 0.01317027f, 310.9424f),
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.3234681f, 0.09690244f, 299.9634f),
        sentQuote = oklch(0.4226042f, 0.1341495f, 307.8573f),
        receivedMessage = oklch(0.2812692f, 0.03669397f, 281.5485f),
        receivedQuote = oklch(0.355058f, 0.03791292f, 286.3773f),
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.3234681f, 0.09690244f, 299.9634f),
        sentQuote = oklch(0.4226042f, 0.1341495f, 307.8573f),
        receivedMessage = oklch(0.2454222f, 0.009540156f, 325.8636f),
        receivedQuote = oklch(0.2874049f, 0.0149843f, 302.5009f),
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
