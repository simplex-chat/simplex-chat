package chat.simplex.app

import chat.simplex.common.views.helpers.installLiteralFileNames
import java.io.File
import javax.swing.JFileChooser
import javax.swing.plaf.basic.BasicHTML
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse

class FileDialogLiteralNamesTest {
  @Test
  fun testHtmlNamesAreShownLiterally() {
    val fc = JFileChooser()
    installLiteralFileNames(fc)
    for (name in listOf("<html><img src=\"http:&#47;&#47;localhost&#47;x.png\" width=1 height=1>holiday.jpg", "<HTML>dir")) {
      val shown = fc.getName(File(name))
      assertFalse(BasicHTML.isHTMLString(shown))
      assertEquals(name, shown.trim())
    }
  }

  @Test
  fun testOtherNamesAreNotChanged() {
    val fc = JFileChooser()
    installLiteralFileNames(fc)
    for (name in listOf("holiday.jpg", "a<html>.txt", "<htm>.txt")) {
      assertEquals(name, fc.getName(File(name)))
    }
  }
}
