package chat.simplex.app

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.width
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.luminance
import androidx.compose.ui.layout.Layout
import androidx.compose.ui.unit.Constraints
import androidx.compose.ui.unit.DpSize
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.WindowState
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.simplexWindowState
import chat.simplex.common.views.chat.item.FramedItemView
import chat.simplex.common.views.chat.item.LocalTerminalLayout
import org.jetbrains.skia.Bitmap
import org.jetbrains.skia.Image
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import java.util.Base64
import javax.imageio.ImageIO
import kotlin.math.abs
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class FlatMediaTest {
  private val wide = png(40, 30)
  private val tall = png(30, 40)

  private class Media(val start: Int, val width: Int, val timeBeside: Boolean, val timeOver: Boolean)

  private fun png(width: Int, height: Int): String {
    val image = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    image.createGraphics().apply { color = java.awt.Color.RED; fillRect(0, 0, width, height); dispose() }
    val png = ByteArrayOutputStream().also { ImageIO.write(image, "png", it) }.toByteArray()
    return "data:image/png;base64," + Base64.getEncoder().encodeToString(png)
  }

  private fun renderItem(rowWidth: Int, content: MsgContent, flat: Boolean, item: @Composable (@Composable () -> Unit) -> Unit): Image {
    val ci = ChatItem.getSampleData(1, CIDirection.DirectRcv(), text = content.text).copy(content = CIContent.RcvMsgContent(content))
    val blurRadius = appPrefs.privacyMediaBlurRadius.get()
    return try {
      appPrefs.privacyMediaBlurRadius.set(0)
      simplexWindowState.windowState = WindowState(size = DpSize(rowWidth.dp, 1200.dp))
      renderScene(width = rowWidth, height = 1200) {
        CompositionLocalProvider(LocalTerminalLayout provides flat) {
          item {
            FramedItemView(
              ChatModel.ChatsContext(secondaryContextFilter = null), Chat.sampleData, ci, imageProvider = { error("not opened") },
              linkMode = SimplexLinkMode.DESCRIPTION, showViaProxy = false, showMenu = mutableStateOf(false), showTimestamp = true,
              receiveFile = {}, scrollToItemId = mutableStateOf(null)
            )
          }
        }
      }
    } finally {
      appPrefs.privacyMediaBlurRadius.set(blurRadius)
    }
  }

  private fun media(rowWidth: Int, content: MsgContent, flat: Boolean = true): Media {
    val rendered = renderItem(rowWidth, content, flat) { item -> Box(Modifier.width(rowWidth.dp).background(Color.White)) { item() } }
    val bitmap = Bitmap.makeFromImage(rendered)
    fun pixel(x: Int, y: Int) = Color(bitmap.getColor(x, y))
    fun red(x: Int, y: Int) = pixel(x, y).let { it.red > 0.8f && it.green < 0.2f && it.blue < 0.2f }
    val start = (0 until rowWidth).first { red(it, 20) }
    val end = (start until rowWidth).first { !red(it, 20) }
    val bottom = (20 until 1200).first { !red((start + end) / 2, it) }
    val timeBeside = (end + 2 until rowWidth).any { x -> (20 until bottom).any { y -> pixel(x, y).luminance() < 0.9f } }
    val timeOver = (start + 10 until end - 10).any { x -> (bottom - 30 until bottom - 8).any { y -> pixel(x, y).luminance() > 0.9f } }
    return Media(start, end - start, timeBeside, timeOver)
  }

  private fun maxIntrinsicHeight(content: MsgContent, flat: Boolean): Int {
    var height = -1
    renderItem(402, content, flat) { item ->
      Layout(item) { measurables, _ ->
        height = measurables[0].maxIntrinsicHeight(Constraints.Infinity)
        layout(0, 0) {}
      }
    }
    return height
  }

  private fun image(png: String) = MsgContent.MCImage("", png)

  private fun video(png: String) = MsgContent.MCVideo("", png, 10)

  private fun linkPreview(png: String) = MsgContent.MCLink("https://simplex.chat", LinkPreview("https://simplex.chat", "SimpleX Chat", "", png))

  private fun assertWidth(expected: Float, media: Media) {
    assertTrue(abs(media.width - expected) <= 1, "width ${media.width}, expected $expected")
    assertEquals(12, media.start)
  }

  @Test
  fun wideImageTakesRowLessPaddingsWithTimeOverItOnPhone() {
    val m = media(402, image(wide))
    assertWidth(378f, m)
    assertTrue(m.timeOver && !m.timeBeside)
  }

  @Test
  fun tallImageTakesThreeQuartersWithTimeBesideItOnPhone() {
    val m = media(402, image(tall))
    assertWidth(283.5f, m)
    assertTrue(m.timeBeside && !m.timeOver)
  }

  @Test
  fun imagesStopAt500WithTimeBesideThemInWideRows() {
    val wideImage = media(900, image(wide))
    assertWidth(500f, wideImage)
    assertTrue(wideImage.timeBeside && !wideImage.timeOver)
    val tallImage = media(900, image(tall))
    assertWidth(375f, tallImage)
    assertTrue(tallImage.timeBeside && !tallImage.timeOver)
  }

  @Test
  fun classicWideImageStaysAt500() {
    val m = media(900, image(wide), flat = false)
    assertTrue(abs(m.width - 500) <= 1, "width ${m.width}")
  }

  @Test
  fun tallLinkPreviewTakesThreeQuarters() {
    assertWidth(283.5f, media(402, linkPreview(tall)))
    assertWidth(375f, media(900, linkPreview(tall)))
  }

  @Test
  fun maxIntrinsicHeightAtUnboundedWidth() {
    for (flat in listOf(false, true)) {
      val layout = if (flat) "flat" else "classic"
      assertEquals(375, maxIntrinsicHeight(image(wide), flat), "$layout image")
      assertEquals(375, maxIntrinsicHeight(video(wide), flat), "$layout video")
      assertTrue(maxIntrinsicHeight(linkPreview(wide), flat) > 0, "$layout link preview")
    }
  }
}
