package chat.simplex.app

import chat.simplex.common.MAX_APP_LINK_LENGTH
import chat.simplex.common.appLinkFromArgs
import chat.simplex.common.isAcceptedAppLink
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertTrue

class AppLinksTest {
  private val badgeLink = "simplexchat:/badge/code/SB0000000000000000000"

  @Test
  fun acceptsTheOnlyArgumentWhenItIsAnAppLink() {
    assertEquals(badgeLink, appLinkFromArgs(arrayOf(badgeLink)))
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
    assertNull(appLinkFromArgs(arrayOf("simplex:/contact#/?v=2-7&smp=smp%3A%2F%2Fexample")))
    assertNull(appLinkFromArgs(arrayOf("https://simplex.chat/contact#/?v=2-7")))
  }

  @Test
  fun ignoresALinkThatIsNotTheOnlyArgument() {
    // a URL split by quotes in a handler command arrives as several arguments
    assertNull(appLinkFromArgs(arrayOf("--flag", badgeLink)), "flag before link")
    assertNull(appLinkFromArgs(arrayOf(badgeLink, "extra")), "argument after link")
  }

  @Test
  fun ignoresAnEmptyArgument() {
    assertNull(appLinkFromArgs(arrayOf("")))
  }

  @Test
  fun lengthBoundIsInclusive() {
    val prefix = "simplexchat:/badge/code/"
    val atLimit = prefix + "A".repeat(MAX_APP_LINK_LENGTH - prefix.length)
    val overLimit = atLimit + "A"
    assertTrue(isAcceptedAppLink(atLimit), "link of exactly $MAX_APP_LINK_LENGTH chars")
    assertFalse(isAcceptedAppLink(overLimit), "link of ${overLimit.length} chars")
    assertNull(appLinkFromArgs(arrayOf(overLimit)))
  }
}
