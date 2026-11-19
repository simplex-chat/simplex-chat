package chat.simplex.app

import chat.simplex.common.model.escapeNotificationMarkup
import chat.simplex.common.model.imageData
import chat.simplex.common.model.notificationActionKeys
import chat.simplex.common.platform.DesktopInstallation
import chat.simplex.common.platform.desktopEntryId
import java.awt.image.BufferedImage
import kotlin.test.Test
import kotlin.test.assertContentEquals
import kotlin.test.assertEquals
import kotlin.test.assertNull

class LinuxNotificationsTest {
  @Test
  fun markupCharactersAreShownAsText() {
    assertEquals("a &lt;b&gt;bold&lt;/b&gt; &amp; &lt;a href=\"x\"&gt;", escapeNotificationMarkup("a <b>bold</b> & <a href=\"x\">"))
  }

  @Test
  fun escapedAmpersandIsEscapedOnce() {
    assertEquals("&amp;lt;", escapeNotificationMarkup("&lt;"), "an entity in the message is text, not markup")
  }

  @Test
  fun actionKeysAreDistinctFromTheDefaultAction() {
    assertEquals(listOf("action-0", "action-1"), notificationActionKeys(2))
    assertEquals(listOf(), notificationActionKeys(0))
  }

  @Test
  fun imageDataIsRgbaInRowOrder() {
    val image = BufferedImage(2, 1, BufferedImage.TYPE_INT_ARGB)
    image.setRGB(0, 0, 0x80112233.toInt())
    image.setRGB(1, 0, 0xFF445566.toInt())
    val data = imageData(image)
    assertEquals(2, data.width)
    assertEquals(1, data.height)
    assertEquals(8, data.rowstride)
    assertEquals(4, data.channels)
    assertContentEquals(byteArrayOf(0x11, 0x22, 0x33, 0x80.toByte(), 0x44, 0x55, 0x66, 0xFF.toByte()), data.data)
  }

  @Test
  fun desktopEntryIdPerInstallation() {
    assertEquals("simplex-simplex", desktopEntryId(DesktopInstallation.LinuxPackage))
    assertEquals("chat.simplex.app", desktopEntryId(DesktopInstallation.AppImage("/home/user/SimpleX.AppImage")))
    assertEquals("chat.simplex.simplex", desktopEntryId(DesktopInstallation.Flatpak))
    assertNull(desktopEntryId(DesktopInstallation.Unpackaged))
    assertNull(desktopEntryId(DesktopInstallation.MacBundle))
  }
}
