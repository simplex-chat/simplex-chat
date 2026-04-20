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
      DefaultTheme.DARK to oklch(0.2024453f, 0.03849037f, 273.4875f),
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
      DefaultTheme.DARK to oklch(0.2024453f, 0.03849037f, 273.4875f),
      DefaultTheme.SIMPLEX to oklch(0.2024453f, 0.03849037f, 273.4875f),
      DefaultTheme.BLACK to oklch(0.1285578f, 0f, 0f)
    ),
    tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.8574244f, 0.1932141f, 133.0531f),
      DefaultTheme.DARK to oklch(0.3183068f, 0.07771506f, 136.6477f),
      DefaultTheme.SIMPLEX to oklch(0.3384358f, 0.08523031f, 136.3517f),
      DefaultTheme.BLACK to oklch(0.3183068f, 0.07771506f, 136.6477f)
    ),
    mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9795341f, 0.03522444f, 133.5366f),
        sentQuote = oklch(0.9336828f, 0.07752936f, 133.1505f),
        receivedMessage = oklch(0.9694833f, 0.007316795f, 132.4149f),
        receivedQuote = oklch(0.9357576f, 0.01475573f, 132.4773f),
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.2829779f, 0.05664382f, 137.7101f),
        sentQuote = oklch(0.3779453f, 0.09538202f, 137.6479f),
        receivedMessage = oklch(0.269555f, 0.006171017f, 134.9414f),
        receivedQuote = oklch(0.3480199f, 0.007943601f, 137.802f),
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.3464215f, 0.08494502f, 141.0868f),
        sentQuote = oklch(0.4739489f, 0.1235496f, 140.5453f),
        receivedMessage = oklch(0.269555f, 0.006171017f, 134.9414f),
        receivedQuote = oklch(0.3480199f, 0.007943601f, 137.802f),
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.3464215f, 0.08494502f, 141.0868f),
        sentQuote = oklch(0.4739489f, 0.1235496f, 140.5453f),
        receivedMessage = oklch(0.231738f, 0.006406116f, 134.9656f),
        receivedQuote = oklch(0.304409f, 0.009754443f, 132.6025f),
      ),
    )
  ),
  HEARTS(MR.images.wallpaper_hearts, "hearts", 0.59f,
    mapOf(
      DefaultTheme.LIGHT to oklch(0.9565624f, 0.01848713f, 17.48077f),
      DefaultTheme.DARK to oklch(0.2024453f, 0.03849037f, 273.4875f),
      DefaultTheme.SIMPLEX to oklch(0.2024453f, 0.03849037f, 273.4875f),
      DefaultTheme.BLACK to oklch(0.1285578f, 0f, 0f)
    ),
    tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.7986557f, 0.1086374f, 14.80189f),
      DefaultTheme.DARK to oklch(0.25332f, 0.06915632f, 12.58639f),
      DefaultTheme.SIMPLEX to oklch(0.267816f, 0.07407809f, 16.28852f),
      DefaultTheme.BLACK to oklch(0.25332f, 0.06915632f, 12.58639f)
    ),
    mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9543917f, 0.02302169f, 10.95497f),
        sentQuote = oklch(0.9097407f, 0.04732327f, 11.58726f),
        receivedMessage = oklch(0.9716819f, 0.002138744f, 17.19447f),
        receivedQuote = oklch(0.9446311f, 0.002154032f, 17.19577f),
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.2586266f, 0.03317007f, 16.50844f),
        sentQuote = oklch(0.336993f, 0.05366197f, 11.41689f),
        receivedMessage = oklch(0.2681172f, 0.004799012f, 355.0992f),
        receivedQuote = oklch(0.3440906f, 0.004499214f, 354.9628f),
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.3319502f, 0.07553982f, 5.77679f),
        sentQuote = oklch(0.3914789f, 0.1003936f, 7.041932f),
        receivedMessage = oklch(0.2681172f, 0.004799012f, 355.0992f),
        receivedQuote = oklch(0.3440906f, 0.004499214f, 354.9628f),
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.3319502f, 0.07553982f, 5.77679f),
        sentQuote = oklch(0.3914789f, 0.1003936f, 7.041932f),
        receivedMessage = oklch(0.2387909f, 0.004945965f, 355.1783f),
        receivedQuote = oklch(0.3006853f, 0.004658511f, 355.0313f),
      ),
    )
  ),
  KIDS(MR.images.wallpaper_kids, "kids", 0.53f,
    mapOf(
      DefaultTheme.LIGHT to oklch(0.9693045f, 0.03516977f, 192.2433f),
      DefaultTheme.DARK to oklch(0.2024453f, 0.03849037f, 273.4875f),
      DefaultTheme.SIMPLEX to oklch(0.2024453f, 0.03849037f, 273.4875f),
      DefaultTheme.BLACK to oklch(0.1285578f, 0f, 0f)
    ),
    tint = mapOf(
      DefaultTheme.LIGHT to oklch(0.8721581f, 0.1030755f, 189.3001f),
      DefaultTheme.DARK to oklch(0.3107921f, 0.04607738f, 186.1605f),
      DefaultTheme.SIMPLEX to oklch(0.3306122f, 0.05137846f, 185.1468f),
      DefaultTheme.BLACK to oklch(0.3107921f, 0.04607738f, 186.1605f)
    ),
    mapOf(
      DefaultTheme.LIGHT to ResolvedColors(
        sentMessage = oklch(0.9816451f, 0.02600448f, 193.8135f),
        sentQuote = oklch(0.9476761f, 0.05463824f, 193.449f),
        receivedMessage = oklch(0.9743321f, 0.006487189f, 185.2751f),
        receivedQuote = oklch(0.9380073f, 0.0109707f, 182.8632f),
      ),
      DefaultTheme.DARK to ResolvedColors(
        sentMessage = oklch(0.2964424f, 0.03780104f, 189.8327f),
        sentQuote = oklch(0.3897975f, 0.05223659f, 187.8233f),
        receivedMessage = oklch(0.2696843f, 0.004349819f, 196.8988f),
        receivedQuote = oklch(0.346296f, 0.008286395f, 184.8154f),
      ),
      DefaultTheme.SIMPLEX to ResolvedColors(
        sentMessage = oklch(0.3665172f, 0.05344185f, 180.3469f),
        sentQuote = oklch(0.486998f, 0.07291723f, 181.2082f),
        receivedMessage = oklch(0.2696843f, 0.004349819f, 196.8988f),
        receivedQuote = oklch(0.346296f, 0.008286395f, 184.8154f),
      ),
      DefaultTheme.BLACK to ResolvedColors(
        sentMessage = oklch(0.3665172f, 0.05344185f, 180.3469f),
        sentQuote = oklch(0.486998f, 0.07291723f, 181.2082f),
        receivedMessage = oklch(0.2318771f, 0.004503036f, 196.8468f),
        receivedQuote = oklch(0.2999201f, 0.007224831f, 182.5153f),
      ),
    )
  ),
  SCHOOL(MR.images.wallpaper_school, "school", 0.53f,
    mapOf(
      DefaultTheme.LIGHT to oklch(0.9626785f, 0.02004578f, 238.6614f),
      DefaultTheme.DARK to oklch(0.2024453f, 0.03849037f, 273.4875f),
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
      DefaultTheme.DARK to oklch(0.2024453f, 0.03849037f, 273.4875f),
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
