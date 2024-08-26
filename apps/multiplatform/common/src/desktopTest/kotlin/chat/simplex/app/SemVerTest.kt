package chat.simplex.app

import chat.simplex.common.views.helpers.SemVer
import kotlin.test.Test
import kotlin.test.assertEquals

// use this command for testing:
// ./gradlew desktopTest
class SemVerTest {
  @Test
  fun testInvalidSemVer() {
    assertEquals(SemVer.from("v-1.0.1"), null)
    assertEquals(SemVer.from("-1.0.1"), null)
    assertEquals(SemVer.from("v.0.1"), null)
    assertEquals(SemVer.from(".0.1"), null)
    assertEquals(SemVer.from("1.0."), null)

    val r = listOf(
      "1",
      //    "1.2",
      "1.2.3-0123",
      "1.2.3-0123.0123",
      "1.1.2+.123",
      "+invalid",
      "-invalid",
      "-invalid+invalid",
      "-invalid.01",
      "alpha",
      "alpha.beta",
      "alpha.beta.1",
      "alpha.1",
      "alpha+beta",
      "alpha_beta",
      "alpha.",
      "alpha..",
      "beta",
      "1.0.0-alpha_beta",
      "-alpha.",
      "1.0.0-alpha..",
      "1.0.0-alpha..1",
      "1.0.0-alpha...1",
      "1.0.0-alpha....1",
      "1.0.0-alpha.....1",
      "1.0.0-alpha......1",
      "1.0.0-alpha.......1",
      "01.1.1",
      "1.01.1",
      "1.1.01",
      "1.2.3.DEV",
//      "1.2-SNAPSHOT",
      "1.2.31.2.3----RC-SNAPSHOT.12.09.1--..12+788",
//      "1.2-RC-SNAPSHOT",
      "-1.0.3-gamma+b7718",
      "+justmeta",
      "9.8.7+meta+meta",
      "9.8.7-whatever+meta+meta",
      "99999999999999999999999.999999999999999999.99999999999999999----RC-SNAPSHOT.12.09.1--------------------------------..12",
    )
    r.forEach {
      assertEquals(SemVer.from(it), null)
    }
  }

  @Test
  fun testValidSemVer() {
    assertEquals(SemVer.from("1.0.0"), SemVer(1, 0, 0))
    assertEquals(SemVer.from("1.0"), SemVer(1, 0, 0))
    assertEquals(SemVer.from("v1.0"), SemVer(1, 0, 0))
    assertEquals(SemVer.from("v1.0-beta.1"), SemVer(1, 0, 0, "beta.1"))
    val r = listOf<Pair<String, SemVer>>(
      "0.0.4" to SemVer(0, 0, 4),
      "1.2.3" to SemVer(1, 2, 3),
      "10.20.30" to SemVer(10, 20, 30),
      "1.1.2-prerelease+meta" to SemVer(1, 1, 2, "prerelease", "meta"),
      "1.1.2+meta" to SemVer(1, 1, 2, null, "meta"),
      "1.1.2+meta-valid" to SemVer(1, 1, 2, null, "meta-valid"),
      "1.0.0-alpha" to SemVer(1, 0, 0, "alpha"),
      "1.0.0-beta" to SemVer(1, 0, 0, "beta"),
      "1.0.0-alpha.beta" to SemVer(1, 0, 0, "alpha.beta"),
      "1.0.0-alpha.beta.1" to SemVer(1, 0, 0, "alpha.beta.1"),
      "1.0.0-alpha.1" to SemVer(1, 0, 0, "alpha.1"),
      "1.0.0-alpha0.valid" to SemVer(1, 0, 0, "alpha0.valid"),
      "1.0.0-alpha.0valid" to SemVer(1, 0, 0, "alpha.0valid"),
          "1.0.0-alpha-a.b-c-somethinglong+build.1-aef.1-its-okay" to SemVer(1, 0, 0, "alpha-a.b-c-somethinglong", "build.1-aef.1-its-okay"),
      "1.0.0-rc.1+build.1" to SemVer(1, 0, 0, "rc.1", "build.1"),
      "2.0.0-rc.1+build.123" to SemVer(2, 0, 0, "rc.1", "build.123"),
      "1.2.3-beta" to SemVer(1, 2, 3, "beta"),
      "10.2.3-DEV-SNAPSHOT" to SemVer(10, 2, 3, "DEV-SNAPSHOT"),
      "1.2.3-SNAPSHOT-123" to SemVer(1, 2, 3, "SNAPSHOT-123"),
      "1.0.0" to SemVer(1, 0, 0),
      "2.0.0" to SemVer(2, 0, 0),
      "1.1.7" to SemVer(1, 1, 7),
      "2.0.0+build.1848" to SemVer(2, 0, 0, null, "build.1848"),
      "2.0.1-alpha.1227" to SemVer(2, 0, 1, "alpha.1227"),
      "1.0.0-alpha+beta" to SemVer(1, 0, 0, "alpha", "beta"),
      "1.2.3----RC-SNAPSHOT.12.9.1--.12+788" to SemVer(1, 2, 3, "---RC-SNAPSHOT.12.9.1--.12", "788"),
      "1.2.3----R-S.12.9.1--.12+meta" to SemVer(1, 2, 3, "---R-S.12.9.1--.12", "meta"),
      "1.2.3----RC-SNAPSHOT.12.9.1--.12" to SemVer(1, 2, 3, "---RC-SNAPSHOT.12.9.1--.12"),
      "1.0.0+0.build.1-rc.10000aaa-kk-0.1" to SemVer(1, 0, 0, null, "0.build.1-rc.10000aaa-kk-0.1"),
//      "99999999999999999999999.999999999999999999.99999999999999999",
      "1.0.0-0A.is.legal" to SemVer(1, 0, 0, "0A.is.legal"),
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
    assert(SemVer(0, 1, 0, "beta.1") > SemVer(0, 1, 0, "beta.0"))
    assert(SemVer(0, 1, 0, "beta.11") > SemVer(0, 1, 0, "beta.10"))
    assert(SemVer(0, 1, 0, "beta.1") > SemVer(0, 1, 0, "alpha.2"))
    assert(SemVer(1, 1, 0, "beta.1") > SemVer(0, 1, 0, "beta.1"))
    assert(SemVer(1, 0, 0) > SemVer(1, 0, 0, "beta.1"))
    assert(SemVer(1, 0, 0) > null)
    assert(SemVer.from("v6.0.0")!! > SemVer.from("v6.0.0-beta.3"))
    assert(SemVer.from("v6.0.0-beta.3")!! > SemVer.from("v6.0.0-beta.2"))
  }
}
