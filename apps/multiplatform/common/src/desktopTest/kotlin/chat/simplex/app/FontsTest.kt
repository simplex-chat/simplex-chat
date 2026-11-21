package chat.simplex.app

import chat.simplex.common.platform.FONTCONFIG_FILE
import chat.simplex.common.platform.bitmapFontsRejectedConfig
import chat.simplex.common.platform.bitmapFontsRejectedFontconfig
import org.w3c.dom.Element
import org.xml.sax.InputSource
import java.io.File
import java.io.StringReader
import javax.xml.parsers.DocumentBuilderFactory
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull

class FontsTest {
  @Test
  fun userFontconfigFileIsKept() = withTempDir { dir ->
    val configDir = dir.resolve("simplex").toFile()
    assertNull(bitmapFontsRejectedFontconfig(mapOf(FONTCONFIG_FILE to "/home/user/fonts.conf"), configDir))
    assertFalse(configDir.exists(), "no config is written when the user sets $FONTCONFIG_FILE")
  }

  @Test
  fun configIsWrittenIntoTheMissingConfigDirectory() = withTempDir { dir ->
    val configDir = dir.resolve("simplex").toFile()
    val file = bitmapFontsRejectedFontconfig(emptyMap(), configDir)
    assertEquals(File(configDir, "fonts.conf"), file)
    assertEquals(bitmapFontsRejectedConfig, file?.readText())
  }

  @Test
  fun outdatedConfigIsReplaced() = withTempDir { dir ->
    val configDir = dir.toFile()
    File(configDir, "fonts.conf").writeText("<fontconfig/>")
    val file = bitmapFontsRejectedFontconfig(emptyMap(), configDir)
    assertEquals(bitmapFontsRejectedConfig, file?.readText())
  }

  @Test
  fun configIncludesTheSystemConfigAndRejectsBitmapFormats() {
    val builder = DocumentBuilderFactory.newInstance().newDocumentBuilder()
    builder.setEntityResolver { _, _ -> InputSource(StringReader("")) }
    val document = builder.parse(bitmapFontsRejectedConfig.byteInputStream())
    val includes = document.getElementsByTagName("include")
    assertEquals(1, includes.length)
    assertEquals("/etc/fonts/fonts.conf", includes.item(0).textContent)
    val formats = (document.getElementsByTagName("rejectfont").item(0) as Element).getElementsByTagName("string")
    assertEquals(listOf("PCF", "BDF"), (0 until formats.length).map { formats.item(it).textContent })
  }
}
