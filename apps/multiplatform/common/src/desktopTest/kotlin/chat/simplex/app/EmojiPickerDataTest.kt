package chat.simplex.app

import chat.simplex.common.views.chat.*
import kotlin.test.*

// use this command for testing:
// ./gradlew desktopTest
class EmojiPickerDataTest {
  private val skinToneModifiers = 0x1F3FB..0x1F3FF

  @Test
  fun testBundledDataHasAllCategoriesInOrder() {
    assertEquals(EmojiCategory.entries.toList(), emojiSections.map { it.category })
    emojiSections.forEach { assertTrue(it.emojis.isNotEmpty(), "empty category ${it.category}") }
    assertEquals(1870, emojiSections.sumOf { it.emojis.size })
  }

  @Test
  fun testBundledDataEntries() {
    val all = emojiSections.flatMap { it.emojis }
    assertEquals(all.size, all.map { it.emoji }.toSet().size, "duplicate emoji")
    all.forEach {
      assertTrue(it.emoji.isNotEmpty() && it.name.isNotBlank(), "bad entry $it")
      assertFalse(it.emoji.codePoints().anyMatch { cp -> cp in skinToneModifiers }, "skin tone variant ${it.name}")
    }
    assertEquals(EmojiData("😀", "grinning face"), emojiSections.first().emojis.first())
    assertEquals(EmojiData("🏴󠁧󠁢󠁷󠁬󠁳󠁿", "flag: Wales"), emojiSections.last().emojis.last())
  }

  @Test
  fun testParseEmojiData() {
    val sections = parseEmojiData(sequenceOf("@Smileys & Emotion", "😀\tgrinning face", "", "@Flags", "🏁\tchequered flag", "#️⃣\tkeycap: #"))
    assertEquals(
      listOf(
        EmojiSection(EmojiCategory.Smileys, listOf(EmojiData("😀", "grinning face"))),
        EmojiSection(EmojiCategory.Flags, listOf(EmojiData("🏁", "chequered flag"), EmojiData("#️⃣", "keycap: #"))),
      ),
      sections
    )
  }

  @Test
  fun testParseUnknownGroupFails() {
    assertFailsWith<IllegalArgumentException> { parseEmojiData(sequenceOf("@Component", "🏻\tlight skin tone")) }
  }

  @Test
  fun testSearchEmoji() {
    assertEquals(listOf("❤️"), searchEmoji(emojiSections, "red heart").map { it.emoji })
    assertEquals(searchEmoji(emojiSections, "red heart"), searchEmoji(emojiSections, "  RED Heart "))
    val flags = searchEmoji(emojiSections, "flag: ")
    assertTrue(flags.size > 250, "found ${flags.size} flags")
    assertTrue(searchEmoji(emojiSections, "no such emoji name").isEmpty())
  }
}
