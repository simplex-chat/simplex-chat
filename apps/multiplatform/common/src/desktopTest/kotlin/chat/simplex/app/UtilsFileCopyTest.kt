package chat.simplex.app

import chat.simplex.common.views.helpers.readInputStreamBounded
import chat.simplex.common.views.helpers.FileTooLargeException
import chat.simplex.common.views.helpers.copyInputStreamToFile
import java.io.ByteArrayInputStream
import java.io.InputStream
import java.io.IOException
import kotlin.io.path.createTempFile
import kotlin.test.Test
import kotlin.test.assertContentEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class UtilsFileCopyTest {
  @Test
  fun testCopyInputStreamAllowsLimitBoundary() {
    val dest = createTempFile().toFile()
    val data = ByteArray(4) { it.toByte() }

    copyInputStreamToFile(ByteArrayInputStream(data), dest, maxBytes = 4)

    assertTrue(dest.exists())
    assertContentEquals(data, dest.readBytes())
    dest.delete()
  }

  @Test
  fun testCopyInputStreamRejectsAndDeletesOversizedOutput() {
    val dest = createTempFile().toFile()
    val data = ByteArray(5) { it.toByte() }

    assertFailsWith<FileTooLargeException> {
      copyInputStreamToFile(ByteArrayInputStream(data), dest, maxBytes = 4)
    }

    assertFalse(dest.exists())
  }

  @Test
  fun testCopyInputStreamCopiesAcrossMultipleReads() {
    val dest = createTempFile().toFile()
    val data = ByteArray(20) { it.toByte() }

    // Delivered 3 bytes per read, so the copy loop runs many iterations and accumulates copied > 0
    copyInputStreamToFile(chunkedStream(data, 3), dest, maxBytes = 20)

    assertTrue(dest.exists())
    assertContentEquals(data, dest.readBytes())
    dest.delete()
  }

  @Test
  fun testCopyInputStreamRejectsAndDeletesWhenLimitCrossedMidStream() {
    val dest = createTempFile().toFile()
    val data = ByteArray(10) { it.toByte() }

    // With 3-byte reads and a 7-byte limit, 6 bytes are written before the next read would exceed it
    assertFailsWith<FileTooLargeException> {
      copyInputStreamToFile(chunkedStream(data, 3), dest, maxBytes = 7)
    }

    assertFalse(dest.exists())
  }

  @Test
  fun boundedMemoryAndDiskAllowZeroAndRejectOneExtraByte() {
    assertContentEquals(byteArrayOf(), readInputStreamBounded(ByteArrayInputStream(byteArrayOf()), 0))
    for (limit in listOf(0L, 7L)) {
      val bytes = ByteArray((limit + 1).toInt())
      assertFailsWith<FileTooLargeException> { readInputStreamBounded(chunkedStream(bytes, 1), limit) }
      val dest = createTempFile().toFile()
      assertFailsWith<FileTooLargeException> { copyInputStreamToFile(chunkedStream(bytes, 1), dest, limit) }
      assertFalse(dest.exists())
    }
    val bytes = ByteArray(17) { it.toByte() }
    assertContentEquals(bytes, readInputStreamBounded(chunkedStream(bytes, 3), 17))
    assertContentEquals(bytes, readInputStreamBounded(chunkedStream(bytes, 3), Long.MAX_VALUE))
  }

  @Test
  fun readFailureDeletesIncompleteOutputAndCannotReturnMemoryPrefix() {
    fun failing() = object : InputStream() {
      var reads = 0
      override fun read(): Int = error("bulk reads only")
      override fun read(b: ByteArray, off: Int, len: Int): Int {
        if (reads++ > 0) throw IOException("ordinary read failure")
        b[off] = 1
        return 1
      }
    }
    val dest = createTempFile().toFile()
    assertFailsWith<IOException> { copyInputStreamToFile(failing(), dest, 10) }
    assertFalse(dest.exists())
    assertFailsWith<IOException> { readInputStreamBounded(failing(), 10) }
  }

  // Returns at most chunkSize bytes per read to force the copy loop to iterate, regardless of buffer size
  private fun chunkedStream(data: ByteArray, chunkSize: Int): InputStream =
    object : ByteArrayInputStream(data) {
      override fun read(b: ByteArray, off: Int, len: Int): Int = super.read(b, off, minOf(len, chunkSize))
    }
}
