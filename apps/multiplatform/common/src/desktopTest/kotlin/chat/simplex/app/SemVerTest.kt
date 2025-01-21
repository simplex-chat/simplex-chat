package chat.simplex.app

import chat.simplex.common.views.helpers.SemVer
import kotlin.test.Test
import kotlin.test.assertEquals

// use this command for testing:
// ./gradlew desktopTest
class SemVerTest {
  @Test
  fun testValidSemVer() {
    assertEquals(SemVer.from("1.0.0"), SemVer(1, 0, 0))
    assertEquals(SemVer.from("1.0"), SemVer(1, 0, 0))
    assertEquals(SemVer.from("v1.0"), SemVer(1, 0, 0))
    assertEquals(SemVer.from("v1.0-beta.1"), SemVer(1, 0, 0, "beta", 1))
    val r = listOf<Pair<String, SemVer>>(
      "0.0.4" to SemVer(0, 0, 4),
      "1.2.3" to SemVer(1, 2, 3),
      "10.20.30" to SemVer(10, 20, 30),
      "1.0.0-alpha.1" to SemVer(1, 0, 0, "alpha", buildNumber = 1),
      "1.0.0" to SemVer(1, 0, 0),
      "2.0.0" to SemVer(2, 0, 0),
      "1.1.7" to SemVer(1, 1, 7),
      "2.0.1-alpha.1227" to SemVer(2, 0, 1, "alpha", 1227),
    )
    r.forEach { (value, correct) ->
      assertEquals(SemVer.from(value), correct)
    }
  }

  @Test
  fun testComparisonSemVer() {
    assert(SemVer(0, 1, 0) == SemVer.from("0.1.0"))
    assert(SemVer(1, 1, 0) == SemVer.from("v1.1.0"))
    assert(SemVer(0, 1, 0) > SemVer(0, 0, 1))
    assert(SemVer(1, 0, 0) > SemVer(0, 100, 100))
    assert(SemVer(0, 200, 0) > SemVer(0, 100, 100))
    assert(SemVer(0, 1, 0, "beta") > SemVer(0, 1, 0, "alpha"))
    assert(SemVer(0, 1, 0) > SemVer(0, 1, 0, "alpha"))
    assert(SemVer(0, 1, 0) > SemVer(0, 1, 0, "beta"))
    assert(SemVer(0, 1, 0) > SemVer(0, 1, 0, "beta.0"))
    assert(SemVer(0, 1, 0, "beta", 1) > SemVer(0, 1, 0, "beta", 0))
    assert(SemVer(0, 1, 0, "beta", 11) > SemVer(0, 1, 0, "beta", 10))
    assert(SemVer(0, 1, 0, "beta", 11) > SemVer(0, 1, 0, "beta", 9))
    assert(SemVer(0, 1, 0, "beta.1") > SemVer(0, 1, 0, "alpha.2"))
    assert(SemVer(1, 1, 0, "beta.1") > SemVer(0, 1, 0, "beta.1"))
    assert(SemVer(1, 0, 0) > SemVer(1, 0, 0, "beta.1"))
    assert(SemVer(1, 0, 0) > null)
    assert(SemVer.from("v6.0.0")!! > SemVer.from("v6.0.0-beta.3"))
    assert(SemVer.from("v6.0.0-beta.3")!! > SemVer.from("v6.0.0-beta.2"))
    assert(SemVer.from("0.1.0") == SemVer.from("0.1.0"))
    assert(SemVer.from("0.1.1")!! > SemVer.from("0.1.0"))
    assert(SemVer.from("0.2.1")!! > SemVer.from("0.1.1"))
    assert(SemVer.from("2.0.1")!! > SemVer.from("0.1.1"))
    assert(SemVer.from("0.1.1-beta.0")!! > SemVer.from("0.1.0-beta.0"))
    assert(SemVer.from("0.1.1-beta.0")!! == SemVer.from("0.1.1-beta.0"))
    assert(SemVer.from("0.1.1-beta.1")!! > SemVer.from("0.1.1-beta.0"))
    assert(SemVer.from("10.0.0-beta.12")!! > SemVer.from("1.1.1"))
    assert(SemVer.from("1.1.1-beta.120")!! > SemVer.from("1.1.1-alpha.9"))
    assert(SemVer.from("1.1.1-beta.120")!! > SemVer.from("1.1.1-alpha.120"))
    assert(SemVer.from("2.0.1")!! > SemVer.from("0.1.1"))
  }
}
