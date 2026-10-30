package chat.simplex.app

import chat.simplex.common.model.CustomTimeUnit
import chat.simplex.common.model.shortTimeText
import chat.simplex.common.model.timeText
import java.util.Locale
import kotlin.test.AfterTest
import kotlin.test.BeforeTest
import kotlin.test.Test
import kotlin.test.assertEquals

class ComposeTimedTextUnitTest {
  private lateinit var savedLocale: Locale

  @BeforeTest
  fun useEnglish() {
    savedLocale = Locale.getDefault()
    Locale.setDefault(Locale.US)
  }

  @AfterTest
  fun restoreLocale() {
    Locale.setDefault(savedLocale)
  }

  @Test
  fun offWhenNoTimer() {
    assertEquals("off", shortTimeText(null))
    assertEquals("off", timeText(null))
  }

  @Test
  fun shortTextPicksLargestWholeUnit() {
    assertEquals("30s", shortTimeText(30))
    assertEquals("90s", shortTimeText(90))
    assertEquals("2m", shortTimeText(120))
    assertEquals("1h", shortTimeText(3600))
    assertEquals("36h", shortTimeText(129600))
    assertEquals("1d", shortTimeText(86400))
    assertEquals("3d", shortTimeText(259200))
    assertEquals("1w", shortTimeText(604800))
    assertEquals("1mth", shortTimeText(2592000))
    assertEquals("2mth", shortTimeText(5184000))
  }

  @Test
  fun longTextUsesSingularAndPlural() {
    assertEquals("30 sec", timeText(30))
    assertEquals("5 min", timeText(300))
    assertEquals("1 hour", timeText(3600))
    assertEquals("8 hours", timeText(28800))
    assertEquals("1 day", timeText(86400))
    assertEquals("2 days", timeText(172800))
    assertEquals("1 week", timeText(604800))
    assertEquals("2 weeks", timeText(1209600))
    assertEquals("1 month", timeText(2592000))
    assertEquals("3 months", timeText(7776000))
  }

  @Test
  fun zeroSeconds_shows0Months_currentBehaviour_item37() {
    assertEquals("0mth", shortTimeText(0))
    assertEquals("0 months", timeText(0))
  }

  @Test
  fun toTimeUnitSplitsIntoLargestDivisibleUnit() {
    assertEquals(CustomTimeUnit.Second to 45, CustomTimeUnit.toTimeUnit(45))
    assertEquals(CustomTimeUnit.Minute to 3, CustomTimeUnit.toTimeUnit(180))
    assertEquals(CustomTimeUnit.Hour to 25, CustomTimeUnit.toTimeUnit(90000))
    assertEquals(CustomTimeUnit.Week to 2, CustomTimeUnit.toTimeUnit(1209600))
    assertEquals(CustomTimeUnit.Day to 8, CustomTimeUnit.toTimeUnit(691200))
  }
}
