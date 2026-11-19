package chat.simplex.app

import chat.simplex.common.platform.fileChooserPatterns
import chat.simplex.common.platform.fileFromNativePath
import chat.simplex.common.platform.hasFileExtension
import chat.simplex.common.platform.imageFileExtensions
import chat.simplex.common.platform.videoFileExtensions
import org.junit.Assume.assumeTrue
import java.io.File
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class LinuxFilesTest {
  @Test
  fun extensionMatchIgnoresCase() {
    assertTrue(hasFileExtension("/home/user/Photo.JPG", imageFileExtensions))
    assertTrue(hasFileExtension("/home/user/clip.WebM", videoFileExtensions))
  }

  @Test
  fun extensionMatchRequiresTheDot() {
    assertFalse(hasFileExtension("/home/user/notapng", imageFileExtensions), "a name ending in the letters of an extension")
    assertFalse(hasFileExtension("/home/user/archive.zip", imageFileExtensions))
  }

  @Test
  fun chooserPatternsCoverBothCases() {
    assertEquals(listOf("*.png", "*.PNG", "*.jpeg", "*.JPEG"), fileChooserPatterns(listOf("png", "jpeg")).toList())
  }

  @Test
  fun nativePathKeepsNonAsciiNames() {
    assumeTrue("file names are not UTF-8 here", System.getProperty("sun.jnu.encoding") == Charsets.UTF_8.name())
    val name = "/home/user/фото 🎉.png"
    assertEquals(File(name), fileFromNativePath(name.toByteArray(Charsets.UTF_8)))
  }
}
