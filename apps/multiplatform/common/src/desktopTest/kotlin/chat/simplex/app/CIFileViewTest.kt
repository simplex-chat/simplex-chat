package chat.simplex.app

import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.unit.LayoutDirection
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.SimpleXTheme
import chat.simplex.common.views.chat.item.CIFileView
import kotlinx.datetime.Instant
import org.jetbrains.skia.EncodedImageFormat
import kotlin.test.Test
import kotlin.test.assertFalse

class CIFileViewTest {
  private val meta = CIMeta.getSample(1, Instant.fromEpochSeconds(0), "", CIStatus.RcvRead())

  private fun drawn(fileName: String, direction: LayoutDirection = LayoutDirection.Ltr): ByteArray =
    renderScene(direction) {
      SimpleXTheme {
        CIFileView(CIFile.getSample(fileName = fileName), meta, chatTTL = null, showViaProxy = false, showTimestamp = true, showMenu = mutableStateOf(false), receiveFile = {})
      }
    }.encodeToData(EncodedImageFormat.PNG)!!.bytes

  @Test
  fun testLongNameExtensionShown() {
    val name = "a".repeat(200)
    listOf("", ".", ". .").forEach { end ->
      assertFalse(drawn("$name.txt$end").contentEquals(drawn("$name.pdf$end")), end)
    }
  }

  @Test
  fun testExtensionShownInBothLayoutDirections() {
    listOf(LayoutDirection.Ltr, LayoutDirection.Rtl).forEach { direction ->
      listOf('a', Char(0x05E9), '1').forEach { c ->
        val name = c.toString().repeat(200)
        assertFalse(drawn("$name.12", direction).contentEquals(drawn("$name.34", direction)), "$direction U+%04X".format(c.code))
      }
    }
  }

  @Test
  fun testControlAndTextDirectionCharactersShown() {
    listOf('\n', Char(0x2028), Char(0x202E), Char(0x2066)).forEach { c ->
      assertFalse(drawn("a.txt$c").contentEquals(drawn("a.txt")), "U+%04X".format(c.code))
    }
  }
}
