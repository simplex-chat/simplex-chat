package chat.simplex.app

import chat.simplex.common.views.call.CallFrameCipher
import chat.simplex.common.views.call.CallFrameCipher.Companion.AUDIO_CLEAR_BYTES
import chat.simplex.common.views.call.CallFrameCipher.Companion.IV_LENGTH
import chat.simplex.common.views.call.CallFrameCipher.Companion.VIDEO_DELTA_FRAME_CLEAR_BYTES
import chat.simplex.common.views.call.CallFrameCipher.Companion.VIDEO_KEY_FRAME_CLEAR_BYTES
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import kotlin.test.*

// Fixtures in resources/call/frames.json were encrypted by callCryptoFunction from the shipped call.js
class CallFrameCipherTest {
  @Serializable
  private data class Frame(val type: String, val plaintext: String, val encrypted: String)

  @Serializable
  private data class Fixture(val aesKey: String, val frames: List<Frame>)

  private val fixture = Json.decodeFromString(Fixture.serializer(), javaClass.getResource("/call/frames.json")!!.readText())
  private val otherKey = "a6qN3UtBYgo8Hvn2kSy4uHvJbcHXRly4o64aBW5BlpI="
  private val tagBytes = 16

  private fun clearBytes(type: String, size: Int) = minOf(
    when (type) {
      "key" -> VIDEO_KEY_FRAME_CLEAR_BYTES
      "delta" -> VIDEO_DELTA_FRAME_CLEAR_BYTES
      else -> AUDIO_CLEAR_BYTES
    },
    size
  )

  private fun String.hex(): ByteArray = chunked(2).map { it.toInt(16).toByte() }.toByteArray()

  @Test
  fun testDecryptsFramesEncryptedByCallJs() {
    val cipher = CallFrameCipher(fixture.aesKey)
    fixture.frames.forEach { f ->
      val encrypted = f.encrypted.hex()
      val plaintext = f.plaintext.hex()
      val decrypted = cipher.decrypt(encrypted, clearBytes(f.type, plaintext.size))
      assertNotNull(decrypted, "${f.type} frame of ${plaintext.size} bytes")
      assertContentEquals(plaintext, decrypted, "${f.type} frame of ${plaintext.size} bytes")
    }
  }

  @Test
  fun testEncryptedLayoutMatchesCallJs() {
    val cipher = CallFrameCipher(fixture.aesKey)
    fixture.frames.forEach { f ->
      val plaintext = f.plaintext.hex()
      val clear = clearBytes(f.type, plaintext.size)
      val encrypted = cipher.encrypt(plaintext, clear)
      assertEquals(f.encrypted.length / 2, encrypted.size, "${f.type} frame of ${plaintext.size} bytes has the call.js size")
      assertContentEquals(plaintext.copyOfRange(0, clear), encrypted.copyOfRange(0, clear), "clear header of ${f.type} frame")
      val expectedSize = if (plaintext.size > clear) plaintext.size + tagBytes + IV_LENGTH else plaintext.size + IV_LENGTH
      assertEquals(expectedSize, encrypted.size)
      assertContentEquals(plaintext, cipher.decrypt(encrypted, clear))
    }
  }

  @Test
  fun testEncryptionUsesFreshIv() {
    val cipher = CallFrameCipher(fixture.aesKey)
    val plaintext = ByteArray(100) { it.toByte() }
    val first = cipher.encrypt(plaintext, VIDEO_DELTA_FRAME_CLEAR_BYTES)
    val second = cipher.encrypt(plaintext, VIDEO_DELTA_FRAME_CLEAR_BYTES)
    assertFalse(first.contentEquals(second))
  }

  @Test
  fun testRejectsFramesWithWrongKey() {
    val cipher = CallFrameCipher(otherKey)
    fixture.frames.filter { it.plaintext.length / 2 > clearBytes(it.type, Int.MAX_VALUE) }.forEach { f ->
      assertNull(cipher.decrypt(f.encrypted.hex(), clearBytes(f.type, f.plaintext.length / 2)), "${f.type} frame decrypted with a wrong key")
    }
  }

  @Test
  fun testRejectsTamperedFrames() {
    val cipher = CallFrameCipher(fixture.aesKey)
    val f = fixture.frames.first { it.type == "key" }
    val clear = clearBytes(f.type, f.plaintext.length / 2)
    val encrypted = f.encrypted.hex()
    for (i in listOf(clear, encrypted.size - IV_LENGTH - 1, encrypted.size - 1)) {
      val tampered = encrypted.copyOf().also { it[i] = (it[i].toInt() xor 1).toByte() }
      assertNull(cipher.decrypt(tampered, clear), "byte $i flipped")
    }
  }

  @Test
  fun testRejectsTruncatedFrames() {
    val cipher = CallFrameCipher(fixture.aesKey)
    assertNull(cipher.decrypt(ByteArray(VIDEO_KEY_FRAME_CLEAR_BYTES + IV_LENGTH - 1), VIDEO_KEY_FRAME_CLEAR_BYTES))
  }
}
