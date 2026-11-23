package chat.simplex.app

import chat.simplex.common.WindowPositionSize
import chat.simplex.common.getStoredWindowState
import chat.simplex.common.platform.appPreferences
import chat.simplex.common.storeWindowState
import chat.simplex.common.ui.theme.DEFAULT_WINDOW_WIDTH
import kotlin.test.AfterTest
import kotlin.test.BeforeTest
import kotlin.test.Test
import kotlin.test.assertEquals

class StoreWindowStateTest {
  private var storedBefore: String? = null

  @BeforeTest
  fun saveStoredState() {
    storedBefore = appPreferences.desktopWindowState.get()
  }

  @AfterTest
  fun restoreStoredState() {
    appPreferences.desktopWindowState.set(storedBefore)
  }

  @Test
  fun firstStartOpensTheDefaultSize() {
    appPreferences.desktopWindowState.set(null)
    assertEquals(WindowPositionSize(width = DEFAULT_WINDOW_WIDTH.value.toInt(), height = 768, x = 0, y = 0), getStoredWindowState())
  }

  @Test
  fun storedSizeIsReturnedUnchanged() {
    val sizes = listOf(
      WindowPositionSize(width = DEFAULT_WINDOW_WIDTH.value.toInt(), height = 768, x = 0, y = 0),
      WindowPositionSize(width = 1024, height = 700, x = 40, y = 25),
    )
    for (size in sizes) {
      storeWindowState(size)
      assertEquals(size, getStoredWindowState(), "stored ${appPreferences.desktopWindowState.get()}")
    }
  }

  @Test
  fun unreadableStoredStateOpensTheDefaultSize() {
    appPreferences.desktopWindowState.set("{\"width\":")
    assertEquals(WindowPositionSize(), getStoredWindowState())
  }
}
