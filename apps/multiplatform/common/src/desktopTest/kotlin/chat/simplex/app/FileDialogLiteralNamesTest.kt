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
    for (name in listOf("<html><b>holiday</b>.jpg", "<HTML>dir")) {
      val shown = fc.getName(File(name))
      assertFalse(BasicHTML.isHTMLString(shown))
      assertEquals("\u00A0$name", shown)
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
