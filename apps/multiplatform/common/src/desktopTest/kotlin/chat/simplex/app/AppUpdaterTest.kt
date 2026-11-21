package chat.simplex.app

import chat.simplex.common.views.helpers.*
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

class AppUpdaterTest {
  private fun release(tag: String, preRelease: Boolean = false) =
    GitHubRelease(
      tagName = tag,
      htmlUrl = "https://github.com/simplex-chat/simplex-chat/releases/tag/$tag",
      name = tag,
      draft = false,
      preRelease = preRelease,
      body = "",
      publishedAt = "2026-11-17T00:00:00Z",
      assets = emptyList()
    )

  @Test
  fun testInternalRelease() {
    val newest = release("7.1.4-175005")
    val older = release("7.1.4-175004")
    assertEquals(newest, chooseInternalRelease(175, listOf(newest, older)))
    assertEquals(newest, chooseInternalRelease(175004, listOf(older, newest)))
    assertNull(chooseInternalRelease(175005, listOf(newest, older)))
    assertNull(chooseInternalRelease(175006, listOf(newest, older)))
    assertNull(chooseInternalRelease(175, listOf(release("v7.1.4"), release("7.1-beta.10"), release("7.1.4-internal.3"))))
    assertNull(chooseInternalRelease(175, emptyList()))
  }

  @Test
  fun testStableRelease() {
    val beta = release("v7.2.0-beta.1", preRelease = true)
    val stable = release("v7.1.4")
    val releases = listOf(beta, stable, release("v7.1.3"))
    assertEquals(stable, chooseStableRelease(SemVer(7, 1, 3), releases))
    assertNull(chooseStableRelease(SemVer(7, 1, 4), releases))
    assertNull(chooseStableRelease(SemVer(7, 2, 0, "beta", 1), releases))
  }

  @Test
  fun testBetaRelease() {
    val beta = release("v7.2.0-beta.1", preRelease = true)
    val releases = listOf(beta, release("v7.1.4"), release("v7.1.3"))
    assertEquals(beta, chooseBetaRelease(SemVer(7, 1, 3), releases))
    assertEquals(beta, chooseBetaRelease(SemVer(7, 1, 4), releases))
    assertNull(chooseBetaRelease(SemVer(7, 2, 0, "beta", 1), releases))
  }
}
