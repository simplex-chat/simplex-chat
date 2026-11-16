package chat.simplex.app

import chat.simplex.common.views.badges.badgePageUrl
import kotlin.test.Test
import kotlin.test.assertTrue

class BadgePageUrlTest {
  @Test
  fun pageEndsWithALinkBackWhenTheLinkReturnsToTheApp() {
    val url = badgePageUrl(linkReturns = true)
    assertTrue(url.endsWith("#/tier?app=true"), "url: $url")
  }

  @Test
  fun pageEndsWithACodeToPasteWhenNoLinkReturns() {
    val url = badgePageUrl(linkReturns = false)
    assertTrue(url.endsWith("#/tier?app=desktop"), "url: $url")
  }
}
