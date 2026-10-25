package chat.simplex.app

import chat.simplex.common.views.chatlist.nameSearchCandidate
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

class ChatListNameSearchUnitTest {
  @Test
  fun dotlessWordOfFiveLettersIsCompletedWithDefaultTopLevelPart() {
    assertEquals("bobby.testing", nameSearchCandidate("bobby"))
  }

  @Test
  fun dotlessWordShorterThanFiveIsNotAName() {
    assertNull(nameSearchCandidate("bob"))
    assertNull(nameSearchCandidate("bobb"))
  }

  @Test
  fun nameWithTopLevelPartIsKeptAsTyped() {
    assertEquals("@bob.simplex", nameSearchCandidate("@bob.simplex"))
    assertEquals("a.b", nameSearchCandidate("a.b"))
  }

  @Test
  fun prefixIsKeptWhenCompleting() {
    assertEquals("#team1.testing", nameSearchCandidate("#team1"))
    assertEquals("@bobby.testing", nameSearchCandidate("@bobby"))
  }

  @Test
  fun prefixDoesNotCountTowardsMinimumLength() {
    assertNull(nameSearchCandidate("@bobb"))
  }

  @Test
  fun surroundingSpacesAreTrimmed() {
    assertEquals("bobby.testing", nameSearchCandidate("  bobby  "))
  }

  @Test
  fun caseIsPreserved() {
    assertEquals("Bobby.testing", nameSearchCandidate("Bobby"))
  }

  @Test
  fun innerHyphenIsAllowed() {
    assertEquals("bob-by.testing", nameSearchCandidate("bob-by"))
  }

  @Test
  fun leadingOrTrailingHyphenIsNotAName() {
    assertNull(nameSearchCandidate("-bobby"))
    assertNull(nameSearchCandidate("bobby-"))
  }

  @Test
  fun emptyLabelIsNotAName() {
    assertNull(nameSearchCandidate("bob..x"))
    assertNull(nameSearchCandidate("bob.simplex."))
    assertNull(nameSearchCandidate(".bobby"))
  }

  @Test
  fun nonAsciiLetterIsNotAName() {
    assertNull(nameSearchCandidate("bóbby"))
  }

  @Test
  fun innerSpaceIsNotAName() {
    assertNull(nameSearchCandidate("bob by"))
  }

  @Test
  fun bareOrDoublePrefixIsNotAName() {
    assertNull(nameSearchCandidate("@"))
    assertNull(nameSearchCandidate("#"))
    assertNull(nameSearchCandidate("@#bobby"))
  }

  @Test
  fun emptyOrBlankTextIsNotAName() {
    assertNull(nameSearchCandidate(""))
    assertNull(nameSearchCandidate("   "))
  }

  @Test
  fun labelOf63CharactersIsAcceptedAnd64IsNot() {
    assertEquals("${"a".repeat(63)}.testing", nameSearchCandidate("a".repeat(63)))
    assertNull(nameSearchCandidate("a".repeat(64)))
    assertNull(nameSearchCandidate("bob.${"a".repeat(64)}"))
  }
}
