package chat.simplex.app

import chat.simplex.common.MAX_LINK_BYTES
import chat.simplex.common.linkFromArgs
import chat.simplex.common.isAcceptedLink
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertTrue

class AppLinksTest {
  @Test
  fun acceptsTheOnlyArgumentWhenItIsAnAppLink() {
    assertEquals(BADGE_LINK, linkFromArgs(arrayOf(BADGE_LINK)))
  }

  @Test
  fun acceptsTheSchemeInAnyCase() {
    val link = "SimplexChat:/badge/code/SB0000000000000000000"
    assertEquals(link, linkFromArgs(arrayOf(link)))
  }

  @Test
  fun ignoresNoArguments() {
    assertNull(linkFromArgs(arrayOf()))
  }

  @Test
  fun acceptsAConnectionLink() {
    assertEquals(SHORT_CONNECTION_LINK, linkFromArgs(arrayOf(SHORT_CONNECTION_LINK)), "short link")
    val oneTimeLink = oneTimeLinkWithKemKey()
    assertEquals(oneTimeLink, linkFromArgs(arrayOf(oneTimeLink)), "one-time link of ${oneTimeLink.length} bytes with its post-quantum key")
    val upperCase = "SimpleX:/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw?h=smp6.simplex.im"
    assertEquals(upperCase, linkFromArgs(arrayOf(upperCase)), "scheme in another case")
  }

  @Test
  fun ignoresWebAndOtherLinks() {
    assertNull(linkFromArgs(arrayOf("https://simplex.chat/contact#/?v=2-7")), "web link")
    assertNull(linkFromArgs(arrayOf("simplexfile:/x")), "another scheme sharing the prefix")
    assertNull(linkFromArgs(arrayOf("/home/user/simplex:/a")), "path")
  }

  @Test
  fun ignoresALinkThatIsNotTheOnlyArgument() {
    // a URL split by quotes in a handler command arrives as several arguments
    assertNull(linkFromArgs(arrayOf("--flag", BADGE_LINK)), "flag before link")
    assertNull(linkFromArgs(arrayOf(BADGE_LINK, "extra")), "argument after link")
  }

  @Test
  fun ignoresAnEmptyArgument() {
    assertNull(linkFromArgs(arrayOf("")))
  }

  @Test
  fun lengthBoundIsInclusive() {
    val atLimit = badgeLinkOfBytes(MAX_LINK_BYTES)
    val overLimit = atLimit + "A"
    assertTrue(isAcceptedLink(atLimit), "link of exactly $MAX_LINK_BYTES bytes")
    assertFalse(isAcceptedLink(overLimit), "link of ${overLimit.length} bytes")
    assertNull(linkFromArgs(arrayOf(overLimit)))
  }

  @Test
  fun lengthBoundCountsUtf8Bytes() {
    // The link has 4213 characters but 8413 bytes, so a bound counted in characters would accept it.
    assertNull(linkFromArgs(arrayOf("simplexchat:/" + "é".repeat(4200))))
  }
}
