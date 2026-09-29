package chat.simplex.app

import chat.simplex.common.platform.appImageDesktopEntry
import chat.simplex.common.platform.desktopExecArgument
import chat.simplex.common.platform.windowsOpenCommand
import kotlin.test.Test
import kotlin.test.assertEquals

class AppLinkSchemeTest {
  @Test
  fun windowsCommandQuotesTheAppAndTheLink() {
    assertEquals(
      "\"C:\\Program Files\\SimpleX\\SimpleX.exe\" \"%1\"",
      windowsOpenCommand("C:\\Program Files\\SimpleX\\SimpleX.exe")
    )
  }

  @Test
  fun execArgumentOfAPlainPathIsOnlyQuoted() {
    assertEquals("\"/home/user/Apps/simplex.AppImage\"", desktopExecArgument("/home/user/Apps/simplex.AppImage"))
  }

  @Test
  fun execArgumentKeepsSpacesInsideTheQuotes() {
    assertEquals("\"/home/user/My Apps/simplex.AppImage\"", desktopExecArgument("/home/user/My Apps/simplex.AppImage"))
  }

  @Test
  fun execArgumentEscapesReservedCharacters() {
    // quoting escapes each of " ` $ with a backslash, and the string escape then doubles that backslash
    assertEquals("\"/a\\\\\"b\"", desktopExecArgument("/a\"b"), "double quote")
    assertEquals("\"/a\\\\`b\"", desktopExecArgument("/a`b"), "backtick")
    assertEquals("\"/a\\\\\$b\"", desktopExecArgument("/a\$b"), "dollar")
  }

  @Test
  fun execArgumentTurnsABackslashIntoFour() {
    assertEquals("\"/a\\\\\\\\b\"", desktopExecArgument("/a\\b"))
  }

  @Test
  fun execArgumentDoublesPercentSoItIsNotAFieldCode() {
    assertEquals("\"/a%%u\"", desktopExecArgument("/a%u"))
  }

  @Test
  fun appImageEntryHandlesTheSchemeAndStaysOutOfMenus() {
    assertEquals(
      """
      |[Desktop Entry]
      |Type=Application
      |Name=SimpleX Chat
      |NoDisplay=true
      |Exec="/home/user/My Apps/simplex.AppImage" %u
      |MimeType=x-scheme-handler/simplexchat;
      |""".trimMargin(),
      appImageDesktopEntry("/home/user/My Apps/simplex.AppImage")
    )
  }
}
