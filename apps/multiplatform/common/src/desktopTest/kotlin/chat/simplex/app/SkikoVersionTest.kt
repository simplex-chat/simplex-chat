package chat.simplex.app

import kotlin.test.Test
import kotlin.test.assertEquals

class SkikoVersionTest {
  @Test
  fun textLayoutMatchesSkikoVersion() {
    assertEquals("0.9.24", org.jetbrains.skiko.Version.skiko)
  }
}
