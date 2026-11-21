package chat.simplex.app

import org.jetbrains.skia.FontMgr
import org.jetbrains.skia.paragraph.FontCollection
import org.jetbrains.skia.paragraph.ParagraphBuilder
import org.jetbrains.skia.paragraph.ParagraphStyle
import org.jetbrains.skia.paragraph.TextStyle
import org.junit.Assume.assumeTrue
import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.TimeUnit
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

private const val LAYOUT_WAIT_SECONDS = 60L

private const val SNOWMAN_BDF = """STARTFONT 2.1
FONT -misc-snowman-medium-r-normal--8-80-75-75-c-80-iso10646-1
SIZE 8 75 75
FONTBOUNDINGBOX 8 8 0 0
STARTPROPERTIES 5
FAMILY_NAME "Snowman"
FONT_ASCENT 8
FONT_DESCENT 0
CHARSET_REGISTRY "ISO10646"
CHARSET_ENCODING "1"
ENDPROPERTIES
CHARS 1
STARTCHAR snowman
ENCODING 9731
SWIDTH 1000 0
DWIDTH 8 0
BBX 8 8 0 0
BITMAP
3C
42
3C
42
81
81
42
3C
ENDCHAR
ENDFONT
"""

class FallbackFontTest {
  @Test
  fun textIsLaidOutWhenTheOnlyFallbackFontIsBitmap() = withTempDir { dir ->
    assumeTrue("fontconfig selects fallback fonts only on Linux", System.getProperty("os.name") == "Linux")
    Files.createDirectory(dir.resolve("fonts"))
    Files.writeString(dir.resolve("fonts/snowman.bdf"), SNOWMAN_BDF)
    Files.writeString(dir.resolve("fonts.conf"), "<fontconfig><dir>$dir/fonts</dir><cachedir>$dir/cache</cachedir></fontconfig>")
    val fcCache = runCatching { startWithFonts(dir, "fc-cache").waitFor() }.getOrNull()
    assumeTrue("fc-cache indexes the font, as it does for installed fonts", fcCache == 0)
    val java = Path.of(System.getProperty("java.home"), "bin", "java").toString()
    val layout = startWithFonts(dir, java, "-cp", System.getProperty("java.class.path"), LayOutSnowman::class.java.name)
    val finished = layout.waitFor(LAYOUT_WAIT_SECONDS, TimeUnit.SECONDS)
    if (!finished) layout.destroyForcibly()
    assertTrue(finished, "layout must end within $LAYOUT_WAIT_SECONDS s")
    assertEquals(0, layout.exitValue(), Files.readString(dir.resolve("output.log")))
  }

  private fun startWithFonts(dir: Path, vararg command: String): Process =
    ProcessBuilder(*command)
      .directory(dir.toFile())
      .redirectErrorStream(true)
      .redirectOutput(dir.resolve("output.log").toFile())
      .apply { environment()["FONTCONFIG_FILE"] = dir.resolve("fonts.conf").toString() }
      .start()
}

object LayOutSnowman {
  @JvmStatic
  fun main(args: Array<String>) {
    val fonts = FontCollection().setDefaultFontManager(FontMgr.default)
    ParagraphBuilder(ParagraphStyle(), fonts).pushStyle(TextStyle()).addText("☃").build().layout(100f)
  }
}
