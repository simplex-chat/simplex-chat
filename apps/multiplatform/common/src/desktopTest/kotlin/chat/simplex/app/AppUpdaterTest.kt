package chat.simplex.app

import chat.simplex.common.views.helpers.*
import kotlinx.datetime.Instant
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

class AppUpdaterTest {
  private fun release(tag: String, publishedAt: String = "2026-11-01T00:00:00Z", preRelease: Boolean = false) =
    GitHubRelease(
      tagName = tag,
      htmlUrl = "https://github.com/simplex-chat/simplex-chat/releases/tag/$tag",
      name = tag,
      draft = false,
      preRelease = preRelease,
      body = "",
      publishedAt = Instant.parse(publishedAt),
      assets = emptyList()
    )

  @Test
  fun testInternalRelease() {
    val newest = release("7.1.4-internal.3", "2026-11-16T10:00:00Z")
    val older = release("7.1.4-internal.2", "2026-11-15T10:00:00Z")
    assertEquals(newest, chooseInternalRelease("7.1.4", listOf(newest, older)))
    assertEquals(newest, chooseInternalRelease("7.1.4", listOf(older, newest)))
    assertEquals(newest, chooseInternalRelease("7.1.4-internal.2", listOf(newest, older)))
    assertNull(chooseInternalRelease("7.1.4-internal.3", listOf(newest, older)))
    assertNull(chooseInternalRelease("7.1.4", emptyList()))
  }

  @Test
  fun testStableRelease() {
    val beta = release("v7.2.0-beta.1", preRelease = true)
    val stable = release("v7.1.4")
    val releases = listOf(beta, stable, release("v7.1.3"))
    assertEquals(stable, chooseStableRelease(SemVer(7, 1, 3), releases))
    assertEquals(stable, chooseStableRelease(SemVer(7, 1, 4, "internal", 3), releases))
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
