package chat.simplex.app

import chat.simplex.common.views.call.LZString
import kotlinx.serialization.Serializable
import kotlinx.serialization.builtins.ListSerializer
import kotlinx.serialization.json.Json
import kotlin.random.Random
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull

// Fixtures in resources/call/lzstring.json were produced by the lz-string.min.js shipped with call.js
class LZStringTest {
  @Serializable
  private data class Vector(val input: String, val compressed: String)

  private val vectors = Json.decodeFromString(
    ListSerializer(Vector.serializer()),
    javaClass.getResource("/call/lzstring.json")!!.readText()
  )

  @Test
  fun testCompressMatchesJsLibrary() {
    vectors.forEach { v ->
      assertEquals(v.compressed, LZString.compressToBase64(v.input), "compressing ${v.input.take(40)}")
    }
  }

  @Test
  fun testDecompressJsOutput() {
    vectors.filter { it.input.isNotEmpty() }.forEach { v ->
      assertEquals(v.input, LZString.decompressFromBase64(v.compressed), "decompressing ${v.compressed.take(40)}")
    }
  }

  @Test
  fun testEmptyInputDecompressesToNull() {
    assertNull(LZString.decompressFromBase64(""))
  }

  @Test
  fun testRoundTripRandomUtf16() {
    val random = Random(42)
    repeat(200) { i ->
      val alphabet = if (i % 2 == 0) 128 else 0x10000
      val input = String(CharArray(random.nextInt(0, 3000)) { random.nextInt(alphabet).toChar() })
      assertEquals(input, LZString.decompressFromBase64(LZString.compressToBase64(input)), "round trip $i, length ${input.length}")
    }
  }
}
