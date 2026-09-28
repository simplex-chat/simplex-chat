package chat.simplex.app

import chat.simplex.common.MAX_APP_LINK_BYTES
import chat.simplex.common.appLinkFromArgs
import chat.simplex.common.isAcceptedAppLink
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertTrue

class AppLinksTest {
  @Test
  fun acceptsTheOnlyArgumentWhenItIsAnAppLink() {
    assertEquals(BADGE_LINK, appLinkFromArgs(arrayOf(BADGE_LINK)))
  }

  @Test
  fun acceptsTheSchemeInAnyCase() {
    val link = "SimplexChat:/badge/code/SB0000000000000000000"
    assertEquals(link, appLinkFromArgs(arrayOf(link)))
  }

  @Test
  fun ignoresNoArguments() {
    assertNull(appLinkFromArgs(arrayOf()))
  }

  @Test
  fun ignoresConnectionAndWebLinks() {
    assertNull(appLinkFromArgs(arrayOf("simplex:/contact#/?v=2-7&smp=smp%3A%2F%2Fexample")), "connection link")
    assertNull(appLinkFromArgs(arrayOf("https://simplex.chat/contact#/?v=2-7")), "web link")
  }

  @Test
  fun ignoresALinkThatIsNotTheOnlyArgument() {
    // a URL split by quotes in a handler command arrives as several arguments
    assertNull(appLinkFromArgs(arrayOf("--flag", BADGE_LINK)), "flag before link")
    assertNull(appLinkFromArgs(arrayOf(BADGE_LINK, "extra")), "argument after link")
  }

  @Test
  fun ignoresAnEmptyArgument() {
    assertNull(appLinkFromArgs(arrayOf("")))
  }

  @Test
  fun lengthBoundIsInclusive() {
    val atLimit = badgeLinkOfBytes(MAX_APP_LINK_BYTES)
    val overLimit = atLimit + "A"
    assertTrue(isAcceptedAppLink(atLimit), "link of exactly $MAX_APP_LINK_BYTES bytes")
    assertFalse(isAcceptedAppLink(overLimit), "link of ${overLimit.length} bytes")
    assertNull(appLinkFromArgs(arrayOf(overLimit)))
  }

  @Test
  fun lengthBoundCountsUtf8Bytes() {
    // The link has 613 characters but 1213 bytes, so a bound counted in characters would accept it.
    assertNull(appLinkFromArgs(arrayOf("simplexchat:/" + "é".repeat(600))))
  }
}
