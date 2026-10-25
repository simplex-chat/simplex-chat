package chat.simplex.app

import chat.simplex.common.model.ChatItemTTL
import chat.simplex.common.views.database.validKey
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class SettingsChatDataUnitTest {

  private val presets = listOf(ChatItemTTL.Day, ChatItemTTL.Week, ChatItemTTL.Month, ChatItemTTL.Year, ChatItemTTL.None)

  @Test
  fun presetTtlsAreOrderedWithNoneLongest() {
    assertTrue(ChatItemTTL.Day < ChatItemTTL.Week)
    assertTrue(ChatItemTTL.Week < ChatItemTTL.Month)
    assertTrue(ChatItemTTL.Month < ChatItemTTL.Year)
    assertTrue(ChatItemTTL.Year < ChatItemTTL.None)
    assertTrue(ChatItemTTL.None > ChatItemTTL.Day)
  }

  @Test
  fun customSecondsAreOrderedAmongPresets() {
    assertTrue(ChatItemTTL.Seconds(3600) < ChatItemTTL.Day)
    assertTrue(ChatItemTTL.Seconds(172800) > ChatItemTTL.Day)
    assertTrue(ChatItemTTL.Seconds(172800) < ChatItemTTL.Week)
    assertEquals(0, ChatItemTTL.Seconds(86400).compareTo(ChatItemTTL.Day))
  }

  @Test
  fun zeroSecondsComparesAsNone() {
    assertEquals(0, ChatItemTTL.None.compareTo(ChatItemTTL.None))
    assertEquals(0, ChatItemTTL.Seconds(0).compareTo(ChatItemTTL.None))
    assertTrue(ChatItemTTL.Seconds(0) > ChatItemTTL.Year)
  }

  @Test
  fun ttlsSortFromShortestToNever() {
    assertEquals(
      listOf(ChatItemTTL.Seconds(3600), ChatItemTTL.Day, ChatItemTTL.Week, ChatItemTTL.Month, ChatItemTTL.Year, ChatItemTTL.None),
      listOf(ChatItemTTL.None, ChatItemTTL.Year, ChatItemTTL.Seconds(3600), ChatItemTTL.Day, ChatItemTTL.Month, ChatItemTTL.Week).sorted()
    )
  }

  @Test
  fun ttlSeconds() {
    assertEquals(listOf(86400L, 604800L, 2592000L, 31536000L, 0L), presets.map { it.seconds })
    assertEquals(3600L, ChatItemTTL.Seconds(3600).seconds)
  }

  @Test
  fun ttlFromSeconds() {
    assertEquals(ChatItemTTL.None, ChatItemTTL.fromSeconds(0))
    assertEquals(ChatItemTTL.Day, ChatItemTTL.fromSeconds(86400))
    assertEquals(ChatItemTTL.Week, ChatItemTTL.fromSeconds(604800))
    assertEquals(ChatItemTTL.Month, ChatItemTTL.fromSeconds(2592000))
    assertEquals(ChatItemTTL.Year, ChatItemTTL.fromSeconds(31536000))
    assertEquals(ChatItemTTL.Seconds(3600), ChatItemTTL.fromSeconds(3600))
  }

  @Test
  fun presetTtlsRoundTripThroughSeconds() {
    presets.forEach {
      assertEquals(it, ChatItemTTL.fromSeconds(it.seconds))
    }
  }

  @Test
  fun onlyNoneNeverExpires() {
    assertEquals(listOf(false, false, false, false, true), presets.map { it.neverExpires })
  }

  @Test
  fun printableAsciiPassphrasesAreValid() {
    assertTrue(validKey(""))
    assertTrue(validKey("abc-123"))
    assertTrue(validKey("~!@#"))
    assertTrue(validKey((33..126).map { it.toChar() }.joinToString("")))
  }

  @Test
  fun passphrasesWithWhitespaceAreInvalid() {
    assertFalse(validKey("a b"))
    assertFalse(validKey("tab\t"))
    assertFalse(validKey("line\n"))
  }

  @Test
  fun passphrasesWithNonAsciiOrControlCharactersAreInvalid() {
    assertFalse(validKey("пароль"))
    assertFalse(validKey("emoji😀"))
    assertFalse(validKey("café"))
    assertFalse(validKey("del\u007f"))
    assertFalse(validKey("nul\u0000"))
  }
}
