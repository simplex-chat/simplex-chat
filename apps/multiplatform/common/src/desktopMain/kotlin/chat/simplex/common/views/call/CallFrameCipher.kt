package chat.simplex.common.views.call

import dev.onvoid.webrtc.RTCEncodedFrame
import dev.onvoid.webrtc.RTCEncodedVideoFrame
import java.security.SecureRandom
import java.util.Base64
import javax.crypto.AEADBadTagException
import javax.crypto.Cipher
import javax.crypto.spec.GCMParameterSpec
import javax.crypto.spec.SecretKeySpec

// Frame layout of call.ts encryptFrame/decryptFrame: clear codec header, AES-256-GCM ciphertext with tag, 12-byte IV.
// The header stays in the clear because packetizers read it: VP8 key frames 10 bytes, VP8 delta frames 3, audio (Opus TOC) 1.
internal class CallFrameCipher(aesKey: String, private val random: SecureRandom = SecureRandom()) {
  private val key = SecretKeySpec(Base64.getUrlDecoder().decode(aesKey), "AES")

  fun encrypt(frame: RTCEncodedFrame) {
    val data = frameData(frame)
    frame.setData(encrypt(data, clearBytes(frame, data.size)))
  }

  // A frame that fails authentication is dropped, never passed to the decoder.
  fun decrypt(frame: RTCEncodedFrame) {
    val data = frameData(frame)
    val plaintext = decrypt(data, clearBytes(frame, data.size))
    if (plaintext == null) frame.drop() else frame.setData(plaintext)
  }

  fun encrypt(data: ByteArray, clear: Int): ByteArray {
    val iv = ByteArray(IV_LENGTH).also(random::nextBytes)
    val ciphertext = if (data.size > clear) gcm(Cipher.ENCRYPT_MODE, iv).doFinal(data, clear, data.size - clear) else ByteArray(0)
    return data.copyOfRange(0, clear) + ciphertext + iv
  }

  fun decrypt(data: ByteArray, clear: Int): ByteArray? {
    if (data.size < clear + IV_LENGTH) return null
    val ciphertextLength = data.size - clear - IV_LENGTH
    if (ciphertextLength == 0) return data.copyOfRange(0, clear)
    val iv = data.copyOfRange(data.size - IV_LENGTH, data.size)
    return try {
      data.copyOfRange(0, clear) + gcm(Cipher.DECRYPT_MODE, iv).doFinal(data, clear, ciphertextLength)
    } catch (e: AEADBadTagException) {
      null
    }
  }

  private fun gcm(mode: Int, iv: ByteArray): Cipher =
    Cipher.getInstance("AES/GCM/NoPadding").apply { init(mode, key, GCMParameterSpec(TAG_BITS, iv)) }

  private fun frameData(frame: RTCEncodedFrame): ByteArray = ByteArray(frame.size).also { frame.data.get(it) }

  private fun clearBytes(frame: RTCEncodedFrame, size: Int): Int {
    val clear = when {
      frame !is RTCEncodedVideoFrame -> AUDIO_CLEAR_BYTES
      frame.isKeyFrame -> VIDEO_KEY_FRAME_CLEAR_BYTES
      else -> VIDEO_DELTA_FRAME_CLEAR_BYTES
    }
    return minOf(clear, size)
  }

  companion object {
    const val IV_LENGTH = 12
    private const val TAG_BITS = 128
    const val AUDIO_CLEAR_BYTES = 1
    const val VIDEO_KEY_FRAME_CLEAR_BYTES = 10
    const val VIDEO_DELTA_FRAME_CLEAR_BYTES = 3
  }
}
