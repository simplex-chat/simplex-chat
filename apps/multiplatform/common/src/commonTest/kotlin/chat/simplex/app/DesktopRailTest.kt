package chat.simplex.app

import chat.simplex.common.views.chatlist.firstGrapheme
import kotlin.test.Test
import kotlin.test.assertEquals

class DesktopRailTest {
  @Test
  fun firstGraphemeOfPlainName() {
    assertEquals("a", firstGrapheme("alice"))
  }

  @Test
  fun firstGraphemeKeepsSurrogatePairEmoji() {
    assertEquals("😀", firstGrapheme("😀 team"))
  }

  @Test
  fun firstGraphemeKeepsFlagEmoji() {
    assertEquals("🇺🇦", firstGrapheme("🇺🇦 news"))
  }

  @Test
  fun firstGraphemeTakesOnlyFirstOfTwoFlags() {
    assertEquals("🇺🇦", firstGrapheme("🇺🇦🇺🇸 news"))
  }

  @Test
  fun firstGraphemeKeepsCombiningMarks() {
    assertEquals("é", firstGrapheme("émilie"))
  }

  @Test
  fun firstGraphemeOfEmptyName() {
    assertEquals("", firstGrapheme(""))
  }
}
