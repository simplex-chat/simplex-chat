package chat.simplex.app

import chat.simplex.common.platform.*
import java.io.File
import kotlin.test.*

// --- Shared test helpers ---

/** Load a binary test fixture from `metadata-test-files/`. */
private fun loadResource(name: String): ByteArray =
  object {}.javaClass.classLoader!!.getResourceAsStream("metadata-test-files/$name")!!.readBytes()

/** Load a binary test fixture into a fresh temp file (deleted on JVM exit). */
private fun loadResourceToTempFile(name: String): File {
  val tmp = File.createTempFile("test_", "_$name")
  tmp.deleteOnExit()
  tmp.writeBytes(loadResource(name))
  return tmp
}

/** Build an MP4 box: 4-byte BE size + 4-char ASCII type + payload. */
private fun box(type: String, payload: ByteArray): ByteArray {
  val size = 8 + payload.size
  return byteArrayOf(
    ((size shr 24) and 0xFF).toByte(),
    ((size shr 16) and 0xFF).toByte(),
    ((size shr 8) and 0xFF).toByte(),
    (size and 0xFF).toByte()
  ) + type.toByteArray(Charsets.US_ASCII) + payload
}

class GifMetadataStripTest {

  private fun buildTestGif(
    includeComment: Boolean = false,
    includeXmpApp: Boolean = false,
    includeNetscape: Boolean = true
  ): ByteArray {
    val out = java.io.ByteArrayOutputStream()
    out.write("GIF89a".toByteArray())
    out.write(byteArrayOf(1, 0, 1, 0))
    out.write(0x80)
    out.write(0)
    out.write(0)
    out.write(byteArrayOf(0, 0, 0, -1, -1, -1))
    if (includeNetscape) {
      out.write(byteArrayOf(0x21, -1))
      out.write(11)
      out.write("NETSCAPE2.0".toByteArray())
      out.write(3); out.write(1); out.write(byteArrayOf(0, 0)); out.write(0)
    }
    if (includeComment) {
      out.write(byteArrayOf(0x21, -2))
      val comment = "GPS: 37.7749 -122.4194"
      out.write(comment.length)
      out.write(comment.toByteArray())
      out.write(0)
    }
    if (includeXmpApp) {
      out.write(byteArrayOf(0x21, -1))
      out.write(11)
      out.write("XMP DataXMP".toByteArray())
      out.write(4)
      out.write("test".toByteArray())
      out.write(0)
    }
    // Frame 1
    out.write(byteArrayOf(0x21, -7))
    out.write(4); out.write(byteArrayOf(0, 10, 0, 0)); out.write(0)
    out.write(0x2C)
    out.write(byteArrayOf(0, 0, 0, 0, 1, 0, 1, 0, 0))
    out.write(2); out.write(2); out.write(byteArrayOf(0x4C, 0x01)); out.write(0)
    // Frame 2
    out.write(byteArrayOf(0x21, -7))
    out.write(4); out.write(byteArrayOf(0, 10, 0, 0)); out.write(0)
    out.write(0x2C)
    out.write(byteArrayOf(0, 0, 0, 0, 1, 0, 1, 0, 0))
    out.write(2); out.write(2); out.write(byteArrayOf(0x4C, 0x01)); out.write(0)
    out.write(0x3B)
    return out.toByteArray()
  }

  @Test fun testStripCommentExtension() {
    val input = buildTestGif(includeComment = true)
    val output = stripGifMetadata(input)
    assertFalse(containsCommentBlock(output), "Output should not contain Comment Extension")
    assertEquals("GIF89a", String(output, 0, 6))
    assertEquals(0x3B.toByte(), output.last())
  }

  @Test fun testStripXmpAppExtension() {
    val input = buildTestGif(includeXmpApp = true)
    val output = stripGifMetadata(input)
    assertFalse(String(output, Charsets.US_ASCII).contains("XMP DataXMP"))
  }

  @Test fun testPreserveNetscapeExtension() {
    val input = buildTestGif(includeNetscape = true, includeComment = true)
    val output = stripGifMetadata(input)
    assertTrue(String(output, Charsets.US_ASCII).contains("NETSCAPE2.0"))
    assertFalse(containsCommentBlock(output))
  }

  @Test fun testPreserveFrames() {
    val input = buildTestGif(includeComment = true, includeXmpApp = true)
    val output = stripGifMetadata(input)
    // Count image descriptors (0x2C) — works for controlled 1x1 test fixtures
    val inputFrames = input.count { it == 0x2C.toByte() }
    val outputFrames = output.count { it == 0x2C.toByte() }
    assertEquals(inputFrames, outputFrames, "Frame count should be preserved")
  }

  @Test fun testNoMetadataPassthrough() {
    val input = buildTestGif(includeComment = false, includeXmpApp = false)
    val output = stripGifMetadata(input)
    assertContentEquals(input, output, "Clean GIF should pass through unchanged")
  }

  @Test fun testPreserveAnimextsExtension() {
    val out = java.io.ByteArrayOutputStream()
    out.write("GIF89a".toByteArray())
    out.write(byteArrayOf(1, 0, 1, 0)); out.write(0x80); out.write(0); out.write(0)
    out.write(byteArrayOf(0, 0, 0, -1, -1, -1))
    out.write(byteArrayOf(0x21, -1)); out.write(11)
    out.write("ANIMEXTS1.0".toByteArray())
    out.write(3); out.write(1); out.write(byteArrayOf(0, 0)); out.write(0)
    out.write(byteArrayOf(0x21, -2)); val c = "metadata"; out.write(c.length); out.write(c.toByteArray()); out.write(0)
    out.write(0x2C); out.write(byteArrayOf(0, 0, 0, 0, 1, 0, 1, 0, 0))
    out.write(2); out.write(2); out.write(byteArrayOf(0x4C, 0x01)); out.write(0)
    out.write(0x3B)
    val input = out.toByteArray()
    val output = stripGifMetadata(input)
    assertTrue(String(output, Charsets.US_ASCII).contains("ANIMEXTS1.0"))
    assertFalse(containsCommentBlock(output))
  }

  @Test fun testInvalidHeaderThrows() {
    assertFailsWith<IllegalArgumentException> { stripGifMetadata("NOT_GIF".toByteArray()) }
  }

  @Test fun testTooShortThrows() {
    assertFailsWith<IllegalArgumentException> { stripGifMetadata(ByteArray(5)) }
  }

  // Naive scan — works for controlled 1x1 pixel test fixtures where 0x21 0xFE won't appear in LZW data
  private fun containsCommentBlock(data: ByteArray): Boolean {
    for (i in 0 until data.size - 1) {
      if (data[i] == 0x21.toByte() && data[i + 1] == 0xFE.toByte()) return true
    }
    return false
  }
}

class WebPMetadataStripTest {

  private fun buildTestWebP(
    includeExif: Boolean = false,
    includeXmp: Boolean = false,
    includeIccp: Boolean = false,
    animated: Boolean = false
  ): ByteArray {
    val chunks = java.io.ByteArrayOutputStream()
    val flags = (if (animated) 0x02 else 0) or
                (if (includeIccp) 0x20 else 0) or
                (if (includeExif) 0x08 else 0) or
                (if (includeXmp) 0x04 else 0)
    writeChunk(chunks, "VP8X", byteArrayOf(flags.toByte(), 0, 0, 0, 0, 0, 0, 0, 0, 0))
    if (includeIccp) { writeChunk(chunks, "ICCP", "fake-icc-profile".toByteArray()) }
    if (animated) {
      writeChunk(chunks, "ANIM", byteArrayOf(0, 0, 0, 0, 0, 0))
      writeChunk(chunks, "ANMF", ByteArray(16))
    } else {
      writeChunk(chunks, "VP8 ", ByteArray(10))
    }
    if (includeExif) { writeChunk(chunks, "EXIF", "Exif\u0000\u0000fake-exif-data".toByteArray()) }
    if (includeXmp) { writeChunk(chunks, "XMP ", "<x:xmpmeta>fake</x:xmpmeta>".toByteArray()) }
    val out = java.io.ByteArrayOutputStream()
    val chunkData = chunks.toByteArray()
    out.write("RIFF".toByteArray())
    writeLE32Bytes(out, chunkData.size + 4)
    out.write("WEBP".toByteArray())
    out.write(chunkData)
    return out.toByteArray()
  }

  private fun writeChunk(out: java.io.ByteArrayOutputStream, fourCC: String, payload: ByteArray) {
    out.write(fourCC.toByteArray())
    writeLE32Bytes(out, payload.size)
    out.write(payload)
    if (payload.size % 2 != 0) out.write(0)
  }

  private fun writeLE32Bytes(out: java.io.ByteArrayOutputStream, value: Int) {
    out.write(value and 0xFF)
    out.write((value shr 8) and 0xFF)
    out.write((value shr 16) and 0xFF)
    out.write((value shr 24) and 0xFF)
  }

  @Test fun testStripExifChunk() {
    val input = buildTestWebP(includeExif = true)
    val output = stripWebPMetadata(input)
    assertFalse(containsChunk(output, "EXIF"))
    assertTrue(containsChunk(output, "VP8X"))
  }

  @Test fun testStripXmpChunk() {
    val input = buildTestWebP(includeXmp = true)
    val output = stripWebPMetadata(input)
    assertFalse(containsChunk(output, "XMP "))
  }

  @Test fun testStripIccpChunk() {
    val input = buildTestWebP(includeIccp = true)
    val output = stripWebPMetadata(input)
    assertFalse(containsChunk(output, "ICCP"))
  }

  @Test fun testStripAllMetadata() {
    val input = buildTestWebP(includeExif = true, includeXmp = true, includeIccp = true)
    val output = stripWebPMetadata(input)
    assertFalse(containsChunk(output, "EXIF"))
    assertFalse(containsChunk(output, "XMP "))
    assertFalse(containsChunk(output, "ICCP"))
    assertTrue(containsChunk(output, "VP8X"))
    assertTrue(containsChunk(output, "VP8 "))
  }

  @Test fun testVp8xFlagsCleared() {
    val input = buildTestWebP(includeExif = true, includeXmp = true, includeIccp = true)
    val output = stripWebPMetadata(input)
    val vp8xPos = findChunkOffset(output, "VP8X")
    assertNotEquals(-1, vp8xPos)
    val flags = output[vp8xPos + 8].toInt() and 0xFF
    assertEquals(0, flags and 0x20, "ICC flag should be cleared")
    assertEquals(0, flags and 0x08, "EXIF flag should be cleared")
    assertEquals(0, flags and 0x04, "XMP flag should be cleared")
  }

  @Test fun testPreserveAnimationChunks() {
    val input = buildTestWebP(includeExif = true, animated = true)
    val output = stripWebPMetadata(input)
    assertTrue(containsChunk(output, "ANIM"))
    assertTrue(containsChunk(output, "ANMF"))
    assertFalse(containsChunk(output, "EXIF"))
  }

  @Test fun testNoMetadataPassthrough() {
    val input = buildTestWebP()
    val output = stripWebPMetadata(input)
    assertContentEquals(input, output)
  }

  @Test fun testRiffSizeUpdated() {
    val input = buildTestWebP(includeExif = true)
    val output = stripWebPMetadata(input)
    val riffSize = (output[4].toInt() and 0xFF) or
                   ((output[5].toInt() and 0xFF) shl 8) or
                   ((output[6].toInt() and 0xFF) shl 16) or
                   ((output[7].toInt() and 0xFF) shl 24)
    assertEquals(output.size - 8, riffSize)
  }

  @Test fun testOddSizedChunkPadding() {
    val chunks = java.io.ByteArrayOutputStream()
    writeChunk(chunks, "VP8X", byteArrayOf(0x08, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    writeChunk(chunks, "VP8 ", ByteArray(10))
    writeChunk(chunks, "EXIF", "odd".toByteArray()) // 3 bytes = odd, needs padding
    val out = java.io.ByteArrayOutputStream()
    val chunkData = chunks.toByteArray()
    out.write("RIFF".toByteArray())
    writeLE32Bytes(out, chunkData.size + 4)
    out.write("WEBP".toByteArray())
    out.write(chunkData)
    val input = out.toByteArray()
    val output = stripWebPMetadata(input)
    assertFalse(containsChunk(output, "EXIF"))
    val riffSize = (output[4].toInt() and 0xFF) or
                   ((output[5].toInt() and 0xFF) shl 8) or
                   ((output[6].toInt() and 0xFF) shl 16) or
                   ((output[7].toInt() and 0xFF) shl 24)
    assertEquals(output.size - 8, riffSize)
  }

  @Test fun testInvalidHeaderThrows() {
    assertFailsWith<IllegalArgumentException> { stripWebPMetadata("NOT_WEBP_AT_ALL".toByteArray()) }
  }

  private fun containsChunk(data: ByteArray, fourCC: String): Boolean = findChunkOffset(data, fourCC) != -1

  private fun findChunkOffset(data: ByteArray, fourCC: String): Int {
    val target = fourCC.toByteArray()
    var pos = 12
    while (pos + 8 <= data.size) {
      if (data[pos] == target[0] && data[pos+1] == target[1] && data[pos+2] == target[2] && data[pos+3] == target[3]) return pos
      val size = (data[pos+4].toInt() and 0xFF) or ((data[pos+5].toInt() and 0xFF) shl 8) or
                 ((data[pos+6].toInt() and 0xFF) shl 16) or ((data[pos+7].toInt() and 0xFF) shl 24)
      val padded = if (size % 2 == 0) size else size + 1
      pos += 8 + padded
    }
    return -1
  }
}

class VideoFileDetectionTest {

  @Test fun testMp4Detected() {
    assertTrue(isVideoFile("video.mp4"))
    assertTrue(isVideoFile("VIDEO.MP4"))
    assertTrue(isVideoFile("my.video.mp4"))
  }

  @Test fun testMovDetected() {
    assertTrue(isVideoFile("clip.mov"))
  }

  @Test fun test3gpDetected() {
    assertTrue(isVideoFile("phone.3gp"))
  }

  @Test fun testM4vDetected() {
    assertTrue(isVideoFile("video.m4v"))
  }

  @Test fun testMpgNotDetected() {
    // MPEG-PS can't be remuxed into MP4 container
    assertFalse(isVideoFile("clip.mpg"))
    assertFalse(isVideoFile("clip.mpeg"))
  }

  @Test fun testMkvDetected() {
    assertTrue(isVideoFile("movie.mkv"))
  }

  @Test fun testWebmDetected() {
    assertTrue(isVideoFile("clip.webm"))
  }

  @Test fun testAviDetected() {
    assertTrue(isVideoFile("old.avi"))
  }

  @Test fun testM4aDetected() {
    // .m4a is an MPEG-4 audio-only container (same atom structure as MP4). Voice messages
    // recorded via MediaRecorder.OutputFormat.MPEG_4 use this extension and MUST be stripped
    // — mvhd/tkhd/mdhd creation_time atoms otherwise leak the UTC second of recording.
    assertTrue(isVideoFile("voice.m4a"))
    assertTrue(isVideoFile("VOICE.M4A"))
  }

  @Test fun testNonVideoNotDetected() {
    assertFalse(isVideoFile("photo.jpg"))
    assertFalse(isVideoFile("document.pdf"))
    assertFalse(isVideoFile("archive.zip"))
    assertFalse(isVideoFile("image.gif"))
  }

  @Test fun testEdgeCases() {
    assertFalse(isVideoFile("noextension"))
    assertFalse(isVideoFile(""))
    assertTrue(isVideoFile(".mp4")) // .mp4 is a valid video extension even as hidden file
    assertTrue(isVideoFile("file.tar.mp4"))
  }
}

/** Tests against files with actual metadata generated by ImageMagick/FFmpeg/ExifTool. */
class RealFileMetadataStripTest {

  // --- GIF with comment block + XMP app extensions ---

  @Test fun testRealGifStripComment() {
    val input = loadResource("gif_with_comment.gif")
    val output = stripGifMetadata(input)
    for (i in 0 until output.size - 1) {
      if (output[i] == 0x21.toByte() && output[i + 1] == 0xFE.toByte()) {
        fail("Comment extension found at offset $i in stripped output")
      }
    }
  }

  @Test fun testRealGifStripXmp() {
    val input = loadResource("gif_with_comment.gif")
    val output = stripGifMetadata(input)
    assertFalse(
      String(output, Charsets.US_ASCII).contains("XMP DataXMP"),
      "XMP app extension should be stripped"
    )
  }

  @Test fun testRealGifPreservesImageData() {
    val input = loadResource("gif_with_comment.gif")
    val output = stripGifMetadata(input)
    assertEquals("GIF89a", String(output, 0, 6))
    assertEquals(0x3B.toByte(), output.last())
    // This file has no NETSCAPE2.0 — it's not a looping animation
    assertTrue(output.any { it == 0x2C.toByte() }, "Should have at least one frame")
  }

  @Test fun testRealGifOutputSmaller() {
    val input = loadResource("gif_with_comment.gif")
    val output = stripGifMetadata(input)
    assertTrue(output.size < input.size, "Stripped file should be smaller (metadata removed)")
  }

  // --- GIF animated with NETSCAPE2.0 + XMP extensions ---

  @Test fun testRealAnimatedGifStripXmp() {
    val input = loadResource("gif_animated_with_xmp.gif")
    val output = stripGifMetadata(input)
    assertFalse(
      String(output, Charsets.US_ASCII).contains("XMP DataXMP"),
      "XMP app extensions should be stripped"
    )
  }

  @Test fun testRealAnimatedGifPreservesNetscape() {
    val input = loadResource("gif_animated_with_xmp.gif")
    val output = stripGifMetadata(input)
    assertTrue(String(output, Charsets.US_ASCII).contains("NETSCAPE2.0"), "NETSCAPE2.0 should be preserved")
  }

  @Test fun testRealAnimatedGifPreservesFrames() {
    val input = loadResource("gif_animated_with_xmp.gif")
    val output = stripGifMetadata(input)
    assertEquals("GIF89a", String(output, 0, 6))
    assertEquals(0x3B.toByte(), output.last())
    // Count Graphic Control Extensions (0x21 0xF9) as frame count — all should survive
    var inputGCE = 0
    var outputGCE = 0
    for (i in 0 until input.size - 1) {
      if (input[i] == 0x21.toByte() && input[i+1] == 0xF9.toByte()) inputGCE++
    }
    for (i in 0 until output.size - 1) {
      if (output[i] == 0x21.toByte() && output[i+1] == 0xF9.toByte()) outputGCE++
    }
    assertEquals(inputGCE, outputGCE, "All frames should be preserved")
    assertTrue(inputGCE > 1, "Test file should be animated")
  }

  @Test fun testRealAnimatedGifOutputSmaller() {
    val input = loadResource("gif_animated_with_xmp.gif")
    val output = stripGifMetadata(input)
    assertTrue(output.size < input.size, "Stripped file should be smaller (XMP blocks removed)")
  }

  // --- WebP with EXIF + XMP chunks (GPS coordinates) ---

  @Test fun testRealWebPStripExif() {
    val input = loadResource("webp_with_exif.webp")
    val output = stripWebPMetadata(input)
    assertFalse(containsRiffChunk(output, "EXIF"), "EXIF chunk should be stripped")
  }

  @Test fun testRealWebPStripXmp() {
    val input = loadResource("webp_with_exif.webp")
    val output = stripWebPMetadata(input)
    assertFalse(containsRiffChunk(output, "XMP"), "XMP chunk should be stripped")
  }

  @Test fun testRealWebPPreservesImageData() {
    val input = loadResource("webp_with_exif.webp")
    val output = stripWebPMetadata(input)
    assertEquals("RIFF", String(output, 0, 4))
    assertEquals("WEBP", String(output, 8, 4))
    assertTrue(containsRiffChunk(output, "VP8 "), "VP8 chunk should be preserved")
    assertTrue(containsRiffChunk(output, "VP8X"), "VP8X chunk should be preserved")
  }

  @Test fun testRealWebPRiffSizeCorrect() {
    val input = loadResource("webp_with_exif.webp")
    val output = stripWebPMetadata(input)
    val riffSize = (output[4].toInt() and 0xFF) or
                   ((output[5].toInt() and 0xFF) shl 8) or
                   ((output[6].toInt() and 0xFF) shl 16) or
                   ((output[7].toInt() and 0xFF) shl 24)
    assertEquals(output.size - 8, riffSize, "RIFF size should match actual file size")
  }

  @Test fun testRealWebPOutputSmaller() {
    val input = loadResource("webp_with_exif.webp")
    val output = stripWebPMetadata(input)
    assertTrue(output.size < input.size, "Stripped file should be smaller (metadata removed)")
  }

  @Test fun testRealWebPVp8xFlagsCleared() {
    val input = loadResource("webp_with_exif.webp")
    val output = stripWebPMetadata(input)
    val vp8xPos = findRiffChunkOffset(output, "VP8X")
    assertNotEquals(-1, vp8xPos, "VP8X should exist")
    val flags = output[vp8xPos + 8].toInt() and 0xFF
    assertEquals(0, flags and 0x08, "EXIF flag should be cleared")
    assertEquals(0, flags and 0x04, "XMP flag should be cleared")
  }

  // --- WebP with ICC profile ---

  @Test fun testRealWebPWithIccpInputHasProfile() {
    val input = loadResource("webp_with_iccp.webp")
    assertTrue(containsRiffChunk(input, "ICCP"), "Test fixture should have ICCP chunk")
    assertTrue(containsRiffChunk(input, "EXIF"), "Test fixture should have EXIF chunk")
  }

  @Test fun testRealWebPWithIccpStripAll() {
    val input = loadResource("webp_with_iccp.webp")
    val output = stripWebPMetadata(input)
    assertFalse(containsRiffChunk(output, "ICCP"), "ICCP should be stripped")
    assertFalse(containsRiffChunk(output, "EXIF"), "EXIF should be stripped")
    assertTrue(containsRiffChunk(output, "VP8X"), "VP8X should be preserved")
    assertTrue(containsRiffChunk(output, "VP8 "), "VP8 should be preserved")
    // VP8X ICC flag should be cleared
    val vp8xPos = findRiffChunkOffset(output, "VP8X")
    val flags = output[vp8xPos + 8].toInt() and 0xFF
    assertEquals(0, flags and 0x20, "ICC flag should be cleared")
    assertEquals(0, flags and 0x08, "EXIF flag should be cleared")
    // Structure valid
    val riffSize = (output[4].toInt() and 0xFF) or ((output[5].toInt() and 0xFF) shl 8) or
                   ((output[6].toInt() and 0xFF) shl 16) or ((output[7].toInt() and 0xFF) shl 24)
    assertEquals(output.size - 8, riffSize, "RIFF size should be correct")
    assertTrue(output.size < input.size, "Should be smaller after stripping")
  }

  private fun containsRiffChunk(data: ByteArray, prefix: String): Boolean =
    findRiffChunkOffset(data, prefix) != -1

  private fun findRiffChunkOffset(data: ByteArray, prefix: String): Int {
    val target = prefix.toByteArray()
    var pos = 12
    while (pos + 8 <= data.size) {
      var match = true
      for (j in target.indices) {
        if (data[pos + j] != target[j]) { match = false; break }
      }
      if (match) return pos
      val size = (data[pos+4].toInt() and 0xFF) or ((data[pos+5].toInt() and 0xFF) shl 8) or
                 ((data[pos+6].toInt() and 0xFF) shl 16) or ((data[pos+7].toInt() and 0xFF) shl 24)
      val padded = if (size % 2 == 0) size else size + 1
      pos += 8 + padded
    }
    return -1
  }
}

/** Test video metadata stripping with MP4 files. */
class RealVideoMetadataStripTest {

  /** Scan MP4 box tree for a box type. Recurses into container boxes. */
  private fun mp4HasBox(data: ByteArray, target: ByteArray): Boolean {
    val containers = listOf("moov", "trak", "mdia", "minf", "stbl", "dinf", "edts", "udta").map { it.toByteArray() }
    fun matchFourCC(offset: Int, fourCC: ByteArray): Boolean {
      for (j in fourCC.indices) { if (data[offset + j] != fourCC[j]) return false }
      return true
    }
    fun isContainer(offset: Int): Boolean = containers.any { matchFourCC(offset, it) }
    fun scan(offset: Int, end: Int): Boolean {
      var pos = offset
      while (pos + 8 <= end) {
        val size = ((data[pos].toInt() and 0xFF) shl 24) or ((data[pos+1].toInt() and 0xFF) shl 16) or
                   ((data[pos+2].toInt() and 0xFF) shl 8) or (data[pos+3].toInt() and 0xFF)
        if (size < 8) break
        if (matchFourCC(pos + 4, target)) return true
        if (isContainer(pos + 4) && scan(pos + 8, pos + size)) return true
        pos += size
      }
      return false
    }
    return scan(0, data.size)
  }

  @Test fun testRealMp4InputHasMetadata() {
    val input = this::class.java.classLoader!!.getResourceAsStream("metadata-test-files/mp4_with_gps.mp4")!!.readBytes()
    assertTrue(mp4HasBox(input, "udta".toByteArray()), "Test fixture should have udta box")
    assertTrue(mp4HasBox(input, "meta".toByteArray()), "Test fixture should have meta box")
    assertTrue(mp4HasBox(input, "uuid".toByteArray()), "Test fixture should have uuid box (GPS/XMP)")
  }

  @Test fun testRealMp4StripMetadata() {
    val inputFile = loadResourceToTempFile("mp4_with_gps.mp4")
    val outputFile = File.createTempFile("test_stripped_", ".mp4")
    outputFile.deleteOnExit()
    try {
      stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      assertTrue(outputFile.exists(), "Output file should exist")
      assertTrue(outputFile.length() > 0, "Output file should not be empty")

      val outputData = outputFile.readBytes()
      // Verify metadata boxes replaced with free (content zeroed, byte offsets preserved)
      assertFalse(mp4HasBox(outputData, "udta".toByteArray()), "udta should be replaced with free")
      assertFalse(mp4HasBox(outputData, "meta".toByteArray()), "meta should be replaced with free")
      assertFalse(mp4HasBox(outputData, "uuid".toByteArray()), "uuid should be replaced with free")

      // Verify it's still a valid MP4 (has ftyp and moov with trak)
      assertTrue(mp4HasBox(outputData, "ftyp".toByteArray()), "Output should have ftyp box")
      assertTrue(mp4HasBox(outputData, "moov".toByteArray()), "Output should have moov box")
      assertTrue(mp4HasBox(outputData, "trak".toByteArray()), "Output should have trak box")

      // Output should be same size (metadata replaced with free, not removed, to preserve stco/co64 offsets)
      assertEquals(inputFile.length(), outputFile.length(), "File size should be preserved")
    } finally {
      inputFile.delete()
      outputFile.delete()
    }
  }

  @Test fun testRealMp4TimestampsZeroed() {
    val inputFile = loadResourceToTempFile("mp4_with_gps.mp4")
    val outputFile = File.createTempFile("test_stripped_", ".mp4")
    outputFile.deleteOnExit()
    try {
      // Verify input has non-zero timestamps
      val inputData = inputFile.readBytes()
      val inputTimestamps = extractTimestamps(inputData)
      assertTrue(inputTimestamps.any { it != 0L }, "Input should have non-zero timestamps")

      stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      val outputData = outputFile.readBytes()

      // All creation_time and modification_time should be zeroed
      val outputTimestamps = extractTimestamps(outputData)
      assertTrue(outputTimestamps.all { it == 0L }, "All timestamps should be zeroed, got: $outputTimestamps")
    } finally {
      inputFile.delete()
      outputFile.delete()
    }
  }

  @Test fun testRealMp4MdhdLanguageZeroed() {
    val inputFile = loadResourceToTempFile("mp4_with_gps.mp4")
    val outputFile = File.createTempFile("test_stripped_", ".mp4")
    outputFile.deleteOnExit()
    try {
      stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      val outputData = outputFile.readBytes()
      // Find all mdhd boxes and check language is "und" (0x55C4)
      val languages = extractMdhdLanguages(outputData)
      assertTrue(languages.isNotEmpty(), "Should have mdhd boxes")
      assertTrue(languages.all { it == 0x55C4 }, "All mdhd languages should be 'und' (0x55C4), got: ${languages.map { "0x${it.toString(16)}" }}")
    } finally {
      inputFile.delete()
      outputFile.delete()
    }
  }

  @Test fun testRealMp4BoxStructureIntegrity() {
    val inputFile = loadResourceToTempFile("mp4_with_gps.mp4")
    val outputFile = File.createTempFile("test_stripped_", ".mp4")
    outputFile.deleteOnExit()
    try {
      stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      val data = outputFile.readBytes()

      // Walk all boxes and verify sizes add up (no overlaps, no gaps in top-level)
      var pos = 0
      var boxCount = 0
      while (pos + 8 <= data.size) {
        val size = ((data[pos].toInt() and 0xFF) shl 24) or ((data[pos+1].toInt() and 0xFF) shl 16) or
                   ((data[pos+2].toInt() and 0xFF) shl 8) or (data[pos+3].toInt() and 0xFF)
        assertTrue(size >= 8, "Box at offset $pos has invalid size: $size")
        assertTrue(pos + size <= data.size, "Box at offset $pos overflows file (size=$size, fileSize=${data.size})")
        pos += size
        boxCount++
      }
      assertEquals(data.size, pos, "Top-level boxes should cover entire file")
      assertTrue(boxCount >= 2, "Should have at least ftyp + moov")
    } finally {
      inputFile.delete()
      outputFile.delete()
    }
  }

  /** Extract mdhd language fields (2-byte packed ISO 639-2). */
  private fun extractMdhdLanguages(data: ByteArray): List<Int> {
    val languages = mutableListOf<Int>()
    val containers = listOf("moov", "trak", "mdia", "minf", "stbl", "dinf", "edts").map { it.toByteArray() }
    fun matchFourCC(offset: Int, fourCC: ByteArray): Boolean {
      for (j in fourCC.indices) { if (data[offset + j] != fourCC[j]) return false }
      return true
    }
    fun isContainer(offset: Int): Boolean = containers.any { matchFourCC(offset, it) }
    fun scan(offset: Int, end: Int) {
      var pos = offset
      while (pos + 8 <= end) {
        val size = ((data[pos].toInt() and 0xFF) shl 24) or ((data[pos+1].toInt() and 0xFF) shl 16) or
                   ((data[pos+2].toInt() and 0xFF) shl 8) or (data[pos+3].toInt() and 0xFF)
        if (size < 8 || pos + size > end) break
        val btype = String(data, pos + 4, 4, Charsets.US_ASCII)
        if (btype == "mdhd") {
          val version = data[pos + 8].toInt() and 0xFF
          val langOffset = if (version == 0) pos + 28 else pos + 40
          if (langOffset + 2 <= pos + size) {
            val lang = ((data[langOffset].toInt() and 0xFF) shl 8) or (data[langOffset + 1].toInt() and 0xFF)
            languages.add(lang)
          }
        }
        if (isContainer(pos + 4)) scan(pos + 8, pos + size)
        pos += size
      }
    }
    scan(0, data.size)
    return languages
  }

  /** Extract creation_time and modification_time from mvhd/tkhd/mdhd boxes. */
  private fun extractTimestamps(data: ByteArray): List<Long> {
    val timestamps = mutableListOf<Long>()
    val targetBoxes = setOf("mvhd", "tkhd", "mdhd")
    val containers = listOf("moov", "trak", "mdia", "minf", "stbl", "dinf", "edts").map { it.toByteArray() }

    fun matchFourCC(offset: Int, fourCC: ByteArray): Boolean {
      for (j in fourCC.indices) { if (data[offset + j] != fourCC[j]) return false }
      return true
    }
    fun isContainer(offset: Int): Boolean = containers.any { matchFourCC(offset, it) }

    fun scan(offset: Int, end: Int) {
      var pos = offset
      while (pos + 8 <= end) {
        val size = ((data[pos].toInt() and 0xFF) shl 24) or ((data[pos+1].toInt() and 0xFF) shl 16) or
                   ((data[pos+2].toInt() and 0xFF) shl 8) or (data[pos+3].toInt() and 0xFF)
        if (size < 8 || pos + size > end) break
        val btype = String(data, pos + 4, 4, Charsets.US_ASCII)
        if (btype in targetBoxes && pos + 20 <= end) {
          val version = data[pos + 8].toInt() and 0xFF
          if (version == 0 && pos + 20 <= end) {
            val ctime = ((data[pos+12].toLong() and 0xFF) shl 24) or ((data[pos+13].toLong() and 0xFF) shl 16) or
                        ((data[pos+14].toLong() and 0xFF) shl 8) or (data[pos+15].toLong() and 0xFF)
            val mtime = ((data[pos+16].toLong() and 0xFF) shl 24) or ((data[pos+17].toLong() and 0xFF) shl 16) or
                        ((data[pos+18].toLong() and 0xFF) shl 8) or (data[pos+19].toLong() and 0xFF)
            timestamps.add(ctime)
            timestamps.add(mtime)
          }
        }
        if (isContainer(pos + 4)) scan(pos + 8, pos + size)
        pos += size
      }
    }
    scan(0, data.size)
    return timestamps
  }
}

/** Test MP4 with free boxes — verify they get stripped. */
class RealMp4FreeBoxStripTest {

  private fun mp4HasBox(data: ByteArray, target: String): Boolean {
    var pos = 0
    while (pos + 8 <= data.size) {
      val size = ((data[pos].toInt() and 0xFF) shl 24) or ((data[pos+1].toInt() and 0xFF) shl 16) or
                 ((data[pos+2].toInt() and 0xFF) shl 8) or (data[pos+3].toInt() and 0xFF)
      if (size < 8) break
      val btype = String(data, pos + 4, 4, Charsets.US_ASCII)
      if (btype == target) return true
      pos += size
    }
    return false
  }

  @Test fun testRealMp4InputHasFreeBoxes() {
    val data = this::class.java.classLoader!!.getResourceAsStream("metadata-test-files/mp4_with_free.mp4")!!.readBytes()
    assertTrue(mp4HasBox(data, "free"), "Test fixture should have free boxes")
  }

  @Test fun testRealMp4PreservesByteOffsets() {
    // Top-level free boxes are preserved (not removed) to maintain stco/co64 offsets
    val inputFile = loadResourceToTempFile("mp4_with_free.mp4")
    val outputFile = File.createTempFile("test_stripped_", ".mp4")
    outputFile.deleteOnExit()
    try {
      stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      val outputData = outputFile.readBytes()
      assertTrue(mp4HasBox(outputData, "ftyp"), "ftyp should be preserved")
      assertTrue(mp4HasBox(outputData, "mdat"), "mdat should be preserved")
      // File size preserved — metadata replaced with free, not removed
      assertEquals(inputFile.length(), outputFile.length(), "File size should be preserved")
      // Structural integrity
      var pos = 0; var count = 0
      while (pos + 8 <= outputData.size) {
        val size = ((outputData[pos].toInt() and 0xFF) shl 24) or ((outputData[pos+1].toInt() and 0xFF) shl 16) or
                   ((outputData[pos+2].toInt() and 0xFF) shl 8) or (outputData[pos+3].toInt() and 0xFF)
        assertTrue(size >= 8); pos += size; count++
      }
      assertEquals(outputData.size, pos, "Boxes should cover entire file")
    } finally { inputFile.delete(); outputFile.delete() }
  }
}

/** Test AVI metadata stripping with a real AVI file. */
class RealAviMetadataStripTest {

  /** Check if a RIFF LIST/INFO chunk exists in AVI data. */
  private fun aviHasInfoList(data: ByteArray): Boolean {
    var pos = 12 // skip RIFF header
    while (pos + 8 <= data.size) {
      val fourCC = String(data, pos, 4, Charsets.US_ASCII)
      val size = (data[pos+4].toInt() and 0xFF) or ((data[pos+5].toInt() and 0xFF) shl 8) or
                 ((data[pos+6].toInt() and 0xFF) shl 16) or ((data[pos+7].toInt() and 0xFF) shl 24)
      if (size < 0) break
      val padded = if (size % 2 == 0) size else size + 1
      if (fourCC == "LIST" && pos + 12 <= data.size) {
        val listType = String(data, pos + 8, 4, Charsets.US_ASCII)
        if (listType == "INFO") return true
      }
      pos += 8 + padded
    }
    return false
  }

  @Test fun testRealAviInputHasMetadata() {
    val data = this::class.java.classLoader!!.getResourceAsStream("metadata-test-files/avi_with_metadata.avi")!!.readBytes()
    assertTrue(aviHasInfoList(data), "Test fixture should have INFO list")
  }

  @Test fun testRealAviStripMetadata() {
    val inputFile = loadResourceToTempFile("avi_with_metadata.avi")
    val outputFile = File.createTempFile("test_stripped_", ".avi")
    outputFile.deleteOnExit()
    try {
      stripAviMetadata(inputFile.absolutePath, outputFile.absolutePath)
      assertTrue(outputFile.exists())
      assertTrue(outputFile.length() > 0)

      val outputData = outputFile.readBytes()
      // INFO list replaced with JUNK of same size (not removed, to preserve idx1 offsets)
      assertFalse(aviHasInfoList(outputData), "INFO list should be replaced with JUNK")

      // Valid AVI header
      assertEquals("RIFF", String(outputData, 0, 4))
      assertEquals("AVI ", String(outputData, 8, 4))

      // RIFF size correct
      val riffSize = (outputData[4].toInt() and 0xFF) or ((outputData[5].toInt() and 0xFF) shl 8) or
                     ((outputData[6].toInt() and 0xFF) shl 16) or ((outputData[7].toInt() and 0xFF) shl 24)
      assertEquals(outputData.size - 8, riffSize, "RIFF size should match file")

      // File size preserved (metadata replaced with JUNK, not removed, to preserve idx1 offsets)
      assertEquals(inputFile.length(), outputFile.length(), "File size should be preserved")
    } finally {
      inputFile.delete()
      outputFile.delete()
    }
  }
}

/** Test MKV/WebM metadata stripping. */
class RealMkvMetadataStripTest {

  @Test fun testRealMkvInputHasMetadata() {
    val data = this::class.java.classLoader!!.getResourceAsStream("metadata-test-files/mkv_with_tags.mkv")!!.readBytes()
    val text = String(data, Charsets.ISO_8859_1)
    assertTrue(text.contains("Lavf"), "Should contain MuxingApp string")
    assertTrue(text.contains("ENCODER"), "Should contain tag names")
  }

  @Test fun testRealMkvStripMetadata() {
    val inputFile = loadResourceToTempFile("mkv_with_tags.mkv")
    val outputFile = File.createTempFile("test_stripped_", ".mkv")
    outputFile.deleteOnExit()
    try {
      stripMkvMetadata(inputFile.absolutePath, outputFile.absolutePath)
      assertTrue(outputFile.exists())
      assertTrue(outputFile.length() > 0)

      val outputData = outputFile.readBytes()
      val outputText = String(outputData, Charsets.ISO_8859_1)

      // Tag content strings should be gone (Tags element removed)
      assertFalse(outputText.contains("ENCODER"), "Tag names should be stripped")
      assertFalse(outputText.contains("DURATION"), "Tag names should be stripped")

      // MuxingApp/WritingApp should be zeroed
      assertFalse(outputText.contains("Lavf"), "MuxingApp should be zeroed")

      assertTrue(outputFile.length() < inputFile.length(), "Stripped file should be smaller")

      // EBML header should still be valid
      assertEquals(0x1A.toByte(), outputData[0])
      assertEquals(0x45.toByte(), outputData[1])
      assertEquals(0xDF.toByte(), outputData[2])
      assertEquals(0xA3.toByte(), outputData[3])
    } finally {
      inputFile.delete()
      outputFile.delete()
    }
  }

  // --- WebM tests ---

  @Test fun testRealWebMInputHasMetadata() {
    val data = this::class.java.classLoader!!.getResourceAsStream("metadata-test-files/webm_with_tags.webm")!!.readBytes()
    val text = String(data, Charsets.ISO_8859_1)
    assertTrue(text.contains("Lavf"), "Should contain WritingApp string")
    assertTrue(text.contains("ARTIST"), "Should contain tag names")
  }

  @Test fun testRealWebMStripMetadata() {
    val inputFile = loadResourceToTempFile("webm_with_tags.webm")
    val outputFile = File.createTempFile("test_stripped_", ".webm")
    outputFile.deleteOnExit()
    try {
      stripMkvMetadata(inputFile.absolutePath, outputFile.absolutePath)
      assertTrue(outputFile.exists())
      assertTrue(outputFile.length() > 0)

      val outputData = outputFile.readBytes()
      val outputText = String(outputData, Charsets.ISO_8859_1)

      // Tag content strings should be gone
      assertFalse(outputText.contains("ARTIST"), "Tag names should be stripped")
      assertFalse(outputText.contains("ENCODER"), "Tag names should be stripped")

      // WritingApp/MuxingApp should be zeroed
      assertFalse(outputText.contains("Lavf"), "WritingApp should be zeroed")

      assertTrue(outputFile.length() < inputFile.length(), "Stripped file should be smaller")
    } finally {
      inputFile.delete()
      outputFile.delete()
    }
  }
}

/** Verify stripped GIF/WebP files are not corrupt by decoding them. */
class CorruptionCheckTest {

  @Test fun testStrippedGifDecodable() {
    val input = loadResource("gif_with_comment.gif")
    val output = stripGifMetadata(input)
    val image = javax.imageio.ImageIO.read(java.io.ByteArrayInputStream(output))
    assertNotNull(image, "Stripped GIF should be decodable")
    assertTrue(image.width > 0 && image.height > 0, "Decoded image should have valid dimensions")
  }

  @Test fun testStrippedAnimatedGifDecodable() {
    val input = loadResource("gif_animated_with_xmp.gif")
    val output = stripGifMetadata(input)
    val image = javax.imageio.ImageIO.read(java.io.ByteArrayInputStream(output))
    assertNotNull(image, "Stripped animated GIF should be decodable")
    assertTrue(image.width > 0 && image.height > 0)
  }

  @Test fun testStrippedWebpDecodable() {
    // Verifies that (a) the WebP stripper produces a valid file and (b) the TwelveMonkeys
    // imageio-webp plugin is registered on desktop so `ImageIO.read` doesn't return null —
    // without the plugin the desktop image picker rejects every WebP with "cannot be decoded".
    val input = loadResource("webp_with_exif.webp")
    val output = stripWebPMetadata(input)
    val image = javax.imageio.ImageIO.read(java.io.ByteArrayInputStream(output))
    assertNotNull(image, "Stripped WebP should be decodable via ImageIO")
    assertTrue(image.width > 0 && image.height > 0)
  }

  @Test fun testStrippedMp4HasValidStructure() {
    val inputFile = File.createTempFile("test_", ".mp4")
    inputFile.writeBytes(loadResource("mp4_with_gps.mp4"))
    val outputFile = File.createTempFile("test_stripped_", ".mp4")
    try {
      stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      val data = outputFile.readBytes()
      var pos = 0
      var boxCount = 0
      while (pos + 8 <= data.size) {
        val size = ((data[pos].toInt() and 0xFF) shl 24) or ((data[pos+1].toInt() and 0xFF) shl 16) or
                   ((data[pos+2].toInt() and 0xFF) shl 8) or (data[pos+3].toInt() and 0xFF)
        assertTrue(size >= 8, "Box at $pos has invalid size $size")
        assertTrue(pos + size <= data.size, "Box at $pos overflows")
        pos += size
        boxCount++
      }
      assertEquals(data.size, pos, "Boxes should cover entire file")
      assertTrue(boxCount >= 2, "Should have at least ftyp + moov")
    } finally { inputFile.delete(); outputFile.delete() }
  }

  @Test fun testStrippedAviHasValidStructure() {
    val inputFile = File.createTempFile("test_", ".avi")
    inputFile.writeBytes(loadResource("avi_with_metadata.avi"))
    val outputFile = File.createTempFile("test_stripped_", ".avi")
    try {
      stripAviMetadata(inputFile.absolutePath, outputFile.absolutePath)
      val data = outputFile.readBytes()
      assertEquals("RIFF", String(data, 0, 4))
      assertEquals("AVI ", String(data, 8, 4))
      val riffSize = (data[4].toInt() and 0xFF) or ((data[5].toInt() and 0xFF) shl 8) or
                     ((data[6].toInt() and 0xFF) shl 16) or ((data[7].toInt() and 0xFF) shl 24)
      assertEquals(data.size - 8, riffSize, "RIFF size should match file")
      // Walk top-level chunks
      var pos = 12
      while (pos + 8 <= data.size) {
        val chunkSize = (data[pos+4].toInt() and 0xFF) or ((data[pos+5].toInt() and 0xFF) shl 8) or
                        ((data[pos+6].toInt() and 0xFF) shl 16) or ((data[pos+7].toInt() and 0xFF) shl 24)
        assertTrue(chunkSize >= 0, "Chunk at $pos has invalid size")
        val padded = if (chunkSize % 2 == 0) chunkSize else chunkSize + 1
        pos += 8 + padded
      }
      assertEquals(data.size, pos, "All chunks should cover the entire file")
    } finally { inputFile.delete(); outputFile.delete() }
  }

  @Test fun testStrippedMkvHasValidEbmlStructure() {
    val inputFile = File.createTempFile("test_", ".mkv")
    inputFile.writeBytes(loadResource("mkv_with_tags.mkv"))
    val outputFile = File.createTempFile("test_stripped_", ".mkv")
    try {
      stripMkvMetadata(inputFile.absolutePath, outputFile.absolutePath)
      val data = outputFile.readBytes()
      // EBML header magic
      assertEquals(0x1A.toByte(), data[0])
      assertEquals(0x45.toByte(), data[1])
      assertEquals(0xDF.toByte(), data[2])
      assertEquals(0xA3.toByte(), data[3])
      // File should be non-trivially sized (more than just headers)
      assertTrue(data.size > 100, "Stripped MKV should still have content")
      // Segment element should follow EBML header
      // EBML header size at offset 4 (VINT)
      val firstByte = data[4].toInt() and 0xFF
      val ebmlSizeLen = if (firstByte and 0x80 != 0) 1 else if (firstByte and 0x40 != 0) 2 else 3
      val mask = 0xFF shr ebmlSizeLen
      var ebmlSize = firstByte and mask
      for (i in 1 until ebmlSizeLen) ebmlSize = (ebmlSize shl 8) or (data[4 + i].toInt() and 0xFF)
      val segmentPos = 4 + ebmlSizeLen + ebmlSize
      // Segment ID: 0x18538067
      assertEquals(0x18.toByte(), data[segmentPos])
      assertEquals(0x53.toByte(), data[segmentPos + 1])
      assertEquals(0x80.toByte(), data[segmentPos + 2])
      assertEquals(0x67.toByte(), data[segmentPos + 3])
    } finally { inputFile.delete(); outputFile.delete() }
  }

  @Test fun testStrippedWebPHasValidStructure() {
    val input = loadResource("webp_with_exif.webp")
    val output = stripWebPMetadata(input)
    // Verify RIFF structure: header + all chunks account for full file
    assertEquals("RIFF", String(output, 0, 4))
    assertEquals("WEBP", String(output, 8, 4))
    val riffSize = (output[4].toInt() and 0xFF) or ((output[5].toInt() and 0xFF) shl 8) or
                   ((output[6].toInt() and 0xFF) shl 16) or ((output[7].toInt() and 0xFF) shl 24)
    assertEquals(output.size - 8, riffSize, "RIFF size should match file")
    // Walk chunks and verify they cover the file
    var pos = 12
    while (pos + 8 <= output.size) {
      val chunkSize = (output[pos+4].toInt() and 0xFF) or ((output[pos+5].toInt() and 0xFF) shl 8) or
                      ((output[pos+6].toInt() and 0xFF) shl 16) or ((output[pos+7].toInt() and 0xFF) shl 24)
      assertTrue(chunkSize >= 0, "Chunk at $pos has invalid size")
      val padded = if (chunkSize % 2 == 0) chunkSize else chunkSize + 1
      pos += 8 + padded
    }
    assertEquals(output.size, pos, "All chunks should cover the entire file")
  }
}

/** Test edge cases: 64-bit extended size moov, AVI output truncation. */
class EdgeCaseStripTest {

  /**
   * Convert an MP4's 32-bit moov header into 64-bit extended size form (size=1 + 8-byte ext_size).
   * The moov payload is unchanged; only the header grows from 8 to 16 bytes, shifting everything after.
   */
  private fun convertMoovTo64BitExtended(input: ByteArray): ByteArray {
    var pos = 0
    while (pos + 8 <= input.size) {
      val size = ((input[pos].toInt() and 0xFF) shl 24) or ((input[pos+1].toInt() and 0xFF) shl 16) or
                 ((input[pos+2].toInt() and 0xFF) shl 8) or (input[pos+3].toInt() and 0xFF)
      val type = String(input, pos + 4, 4, Charsets.US_ASCII)
      if (type == "moov") {
        // Original moov: [4 bytes size=N][4 bytes "moov"][N-8 bytes payload]
        // New moov: [4 bytes size=1][4 bytes "moov"][8 bytes ext_size=N+8][N-8 bytes payload]
        val out = java.io.ByteArrayOutputStream(input.size + 8)
        out.write(input, 0, pos) // pre-moov bytes
        // size=1 marker
        out.write(byteArrayOf(0, 0, 0, 1))
        // "moov"
        out.write(byteArrayOf('m'.code.toByte(), 'o'.code.toByte(), 'o'.code.toByte(), 'v'.code.toByte()))
        // 8-byte ext_size = original size + 8
        val newSize = (size + 8).toLong()
        for (i in 7 downTo 0) {
          out.write(((newSize shr (i * 8)) and 0xFF).toInt())
        }
        // moov payload
        out.write(input, pos + 8, size - 8)
        // remaining boxes
        if (pos + size < input.size) {
          out.write(input, pos + size, input.size - pos - size)
        }
        return out.toByteArray()
      }
      pos += size
    }
    error("No moov box found")
  }

  private fun mp4HasBox(data: ByteArray, target: String): Boolean {
    var pos = 0
    while (pos + 8 <= data.size) {
      val size = ((data[pos].toInt() and 0xFF) shl 24) or ((data[pos+1].toInt() and 0xFF) shl 16) or
                 ((data[pos+2].toInt() and 0xFF) shl 8) or (data[pos+3].toInt() and 0xFF)
      if (size < 8) break
      val type = String(data, pos + 4, 4, Charsets.US_ASCII)
      if (type == target) return true
      pos += size
    }
    return false
  }

  @Test fun testMp4WithExtendedSizeMoovPreservesByteOffsets() {
    // Construct an MP4 with 64-bit extended size moov header
    val original = loadResource("mp4_with_gps.mp4")
    val extendedInput = convertMoovTo64BitExtended(original)

    // Verify the conversion: moov is now 8 bytes larger (rawSize=1 + ext_size)
    assertEquals(original.size + 8, extendedInput.size, "Extended-size input should be 8 bytes larger")
    // moov box should have size field = 1 (rawSize marker)
    val moovOffset = run {
      var p = 0
      while (p + 8 <= extendedInput.size) {
        val s = ((extendedInput[p].toInt() and 0xFF) shl 24) or ((extendedInput[p+1].toInt() and 0xFF) shl 16) or
                ((extendedInput[p+2].toInt() and 0xFF) shl 8) or (extendedInput[p+3].toInt() and 0xFF)
        val t = String(extendedInput, p + 4, 4, Charsets.US_ASCII)
        if (t == "moov") return@run p
        // For non-moov boxes, advance normally
        p += s
      }
      fail("No moov found")
    }
    assertEquals(1, ((extendedInput[moovOffset].toInt() and 0xFF) shl 24) or
                    ((extendedInput[moovOffset+1].toInt() and 0xFF) shl 16) or
                    ((extendedInput[moovOffset+2].toInt() and 0xFF) shl 8) or
                    (extendedInput[moovOffset+3].toInt() and 0xFF),
      "moov size field should be 1 (ext-size marker)")

    val inputFile = File.createTempFile("test_ext_", ".mp4")
    inputFile.writeBytes(extendedInput)
    val outputFile = File.createTempFile("test_ext_stripped_", ".mp4")
    try {
      stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      val output = outputFile.readBytes()

      // Byte offsets must be preserved (stco/co64 inside moov reference mdat)
      assertEquals(extendedInput.size, output.size, "Output must preserve byte count for stco/co64 integrity")

      // Output moov must still use 64-bit extended size format
      var pos = 0
      var foundMoov = false
      while (pos + 8 <= output.size) {
        val size = ((output[pos].toInt() and 0xFF) shl 24) or ((output[pos+1].toInt() and 0xFF) shl 16) or
                   ((output[pos+2].toInt() and 0xFF) shl 8) or (output[pos+3].toInt() and 0xFF)
        val type = String(output, pos + 4, 4, Charsets.US_ASCII)
        if (type == "moov") {
          foundMoov = true
          assertEquals(1, size, "Output moov should preserve 64-bit extended size format (size=1 marker)")
          // Parse 64-bit ext_size at [pos+8..pos+15]
          var extSize = 0L
          for (i in 0 until 8) {
            extSize = (extSize shl 8) or (output[pos + 8 + i].toLong() and 0xFF)
          }
          assertTrue(extSize >= 16, "Extended size must be at least 16 (header itself)")
          assertEquals(extSize, ((moovEndOffset(extendedInput, moovOffset)) - moovOffset).toLong(),
            "Extended size should match original moov size")
          pos += extSize.toInt()
        } else {
          pos += size
        }
      }
      assertTrue(foundMoov, "Output should contain moov box")
      assertEquals(output.size, pos, "Boxes should cover entire output file")

      // Metadata still stripped (udta replaced with free)
      assertFalse(mp4HasBox(output, "udta"), "udta should be stripped from extended-size moov")
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }

  /** Find the byte offset of the first box AFTER moov in [data], used to compute moov's total size. */
  private fun moovEndOffset(data: ByteArray, moovStart: Int): Int {
    // moov uses extended size: size=1 marker + ext_size at offset moovStart+8
    var extSize = 0L
    for (i in 0 until 8) {
      extSize = (extSize shl 8) or (data[moovStart + 8 + i].toLong() and 0xFF)
    }
    return moovStart + extSize.toInt()
  }

  @Test fun testAviStripTruncatesPreExistingLongerOutput() {
    val inputFile = File.createTempFile("test_avi_", ".avi")
    inputFile.writeBytes(loadResource("avi_with_metadata.avi"))
    val outputFile = File.createTempFile("test_avi_truncate_", ".avi")
    try {
      // Pre-populate output with content longer than what the stripper will produce
      val garbageSize = inputFile.length() + 10_000
      outputFile.writeBytes(ByteArray(garbageSize.toInt()) { 0xAB.toByte() })
      assertEquals(garbageSize, outputFile.length(), "Pre-existing output should be longer than input")

      stripAviMetadata(inputFile.absolutePath, outputFile.absolutePath)

      // Output should be truncated to the actual stripped size, not retain garbage tail
      assertEquals(inputFile.length(), outputFile.length(), "Output should be truncated to written length")

      // No leftover 0xAB bytes
      val output = outputFile.readBytes()
      val lastByte = output[output.size - 1].toInt() and 0xFF
      assertNotEquals(0xAB, lastByte, "Trailing garbage byte must be truncated")

      // RIFF header still valid
      assertEquals("RIFF", String(output, 0, 4))
      assertEquals("AVI ", String(output, 8, 4))
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }
}

/** Fail-closed tests for malformed input and additional privacy guarantees. */
class FailClosedAndScrubTest {

  // --- WebP malformed chunk: must throw (not silently bypass stripping) ---

  @Test fun testWebPMalformedChunkSizeThrows() {
    // Build a WebP: RIFF header + VP8X + VP8 + EXIF with INFLATED size field that exceeds file
    val out = java.io.ByteArrayOutputStream()
    out.write("RIFF".toByteArray())
    // Placeholder RIFF size — fix later
    out.write(ByteArray(4))
    out.write("WEBP".toByteArray())
    // VP8X chunk
    out.write("VP8X".toByteArray())
    out.write(byteArrayOf(10, 0, 0, 0)) // size=10
    out.write(byteArrayOf(0x08.toByte(), 0, 0, 0, 0, 0, 0, 0, 0, 0)) // EXIF flag set
    // VP8 chunk
    out.write("VP8 ".toByteArray())
    out.write(byteArrayOf(10, 0, 0, 0)) // size=10
    out.write(ByteArray(10))
    // EXIF chunk with inflated size — claims much larger than actual bytes
    out.write("EXIF".toByteArray())
    out.write(byteArrayOf(0xFF.toByte(), 0xFF.toByte(), 0xFF.toByte(), 0x7F.toByte())) // size near Int.MAX_VALUE
    out.write("real-exif-bytes".toByteArray())

    val bytes = out.toByteArray()
    // Fix RIFF size to reflect total chunks (won't actually be honored — the EXIF size lies)
    val riffSize = bytes.size - 8
    bytes[4] = (riffSize and 0xFF).toByte()
    bytes[5] = ((riffSize shr 8) and 0xFF).toByte()
    bytes[6] = ((riffSize shr 16) and 0xFF).toByte()
    bytes[7] = ((riffSize shr 24) and 0xFF).toByte()

    // Must throw — silently returning original would leak the "real-exif-bytes"
    assertFailsWith<IllegalArgumentException> { stripWebPMetadata(bytes) }
  }

  // --- MP4 malformed nested box: must throw (not silently corrupt moov) ---

  @Test fun testMp4MalformedNestedBoxThrows() {
    // Take the real MP4 with metadata, corrupt the first inner box's size to be invalid
    val good = loadResource("mp4_with_gps.mp4")
    // Find moov, then corrupt its first child's size to 0 (invalid: size < 8)
    var pos = 0
    var moovStart = -1
    while (pos + 8 <= good.size) {
      val size = ((good[pos].toInt() and 0xFF) shl 24) or ((good[pos+1].toInt() and 0xFF) shl 16) or
                 ((good[pos+2].toInt() and 0xFF) shl 8) or (good[pos+3].toInt() and 0xFF)
      val type = String(good, pos + 4, 4, Charsets.US_ASCII)
      if (type == "moov") { moovStart = pos; break }
      pos += size
    }
    assertTrue(moovStart >= 0, "Test fixture should have moov box")

    val corrupted = good.copyOf()
    // First child of moov starts at moovStart + 8. Set its size to 0 (malformed).
    val firstChildSizeOffset = moovStart + 8
    corrupted[firstChildSizeOffset] = 0
    corrupted[firstChildSizeOffset + 1] = 0
    corrupted[firstChildSizeOffset + 2] = 0
    corrupted[firstChildSizeOffset + 3] = 0

    val inputFile = File.createTempFile("test_malformed_", ".mp4")
    inputFile.writeBytes(corrupted)
    val outputFile = File.createTempFile("test_malformed_out_", ".mp4")
    try {
      assertFailsWith<IllegalArgumentException> {
        stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      }
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }

  // --- MKV unknown-size Cluster: must fail closed (boundary scan is vulnerable to embedded IDs) ---

  @Test fun testMkvUnknownSizeClusterRejected() {
    // Build a minimal WebM with an unknown-size Cluster. Boundary scanning for the next Level-1 ID
    // is fundamentally vulnerable to attacker-embedded Cluster/Tags IDs inside SimpleBlock data —
    // we now reject unknown-size elements outright. Users with streaming WebM must re-encode.
    val ebml = byteArrayOf(
      // EBML header: ID=0x1A45DFA3, size=0x80|3 (1-byte size=3), payload=3 bytes (EBMLVersion=1)
      0x1A.toByte(), 0x45.toByte(), 0xDF.toByte(), 0xA3.toByte(),
      0x83.toByte(),  // size VINT = 3
      0x42.toByte(), 0x86.toByte(), 0x81.toByte()  // EBMLVersion = 1
    )
    val segmentInfo = byteArrayOf(
      0x15.toByte(), 0x49.toByte(), 0xA9.toByte(), 0x66.toByte(),  // ID
      0x85.toByte(),  // size = 5
      0x73.toByte(), 0xA4.toByte(),  // SegmentUUID ID
      0x82.toByte(),  // size = 2
      0xDE.toByte(), 0xAD.toByte()  // UUID payload (should be zeroed)
    )
    val tracks = byteArrayOf(
      0x16.toByte(), 0x54.toByte(), 0xAE.toByte(), 0x6B.toByte(),  // Tracks ID
      0x81.toByte(), 0x00.toByte()  // size = 1, dummy content
    )
    // Cluster with UNKNOWN size (all-ones 1-byte VINT = 0xFF)
    val clusterContents = byteArrayOf(0xA3.toByte(), 0x81.toByte(), 0x42.toByte()) // dummy SimpleBlock
    val cluster = byteArrayOf(
      0x1F.toByte(), 0x43.toByte(), 0xB6.toByte(), 0x75.toByte(),  // Cluster ID
      0xFF.toByte()  // size VINT = unknown
    ) + clusterContents
    // Tags element AFTER the cluster's contents — should be picked up as Level-1 boundary
    val tags = byteArrayOf(
      0x12.toByte(), 0x54.toByte(), 0xC3.toByte(), 0x67.toByte(),  // Tags ID
      0x90.toByte(),  // size = 16
    ) + "SECRET-TAG-DATA!".toByteArray()

    val segmentPayload = segmentInfo + tracks + cluster + tags
    // Segment with definite size
    val segmentHeader = byteArrayOf(
      0x18.toByte(), 0x53.toByte(), 0x80.toByte(), 0x67.toByte(),  // Segment ID
      // 4-byte size VINT: 0x10 marker + 3 bytes value
      (0x10 or ((segmentPayload.size shr 24) and 0xFF)).toByte(),
      ((segmentPayload.size shr 16) and 0xFF).toByte(),
      ((segmentPayload.size shr 8) and 0xFF).toByte(),
      (segmentPayload.size and 0xFF).toByte()
    )

    val inputBytes = ebml + segmentHeader + segmentPayload
    val inputFile = File.createTempFile("test_unknown_cluster_", ".mkv")
    inputFile.writeBytes(inputBytes)
    val outputFile = File.createTempFile("test_unknown_cluster_out_", ".mkv")
    try {
      assertFailsWith<IllegalArgumentException> {
        stripMkvMetadata(inputFile.absolutePath, outputFile.absolutePath)
      }
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }

  // --- MP4 timed metadata: zero raw GPS sample bytes in mdat ---

  /**
   * Build a minimal MP4 with a timed-metadata trak that points to attacker-readable bytes in mdat.
   * After stripping, the trak should be removed AND the referenced mdat bytes must be zeroed.
   */
  @Test fun testMp4TimedMetadataMdatSamplesZeroed() {
    // We build a synthetic MP4: ftyp + moov (with mvhd, two traks: one fake video, one timed-meta) + mdat.
    // The timed-meta trak has stbl referencing a single chunk in mdat containing "SECRET-GPS-DATA!"
    // After stripping, those bytes in the output file must be zeroed.

    val secretBytes = "SECRET-GPS-DATA!".toByteArray() // 16 bytes
    // We need to know mdat offset to set stco. Simplest: build everything else first, measure, then build mdat.
    // Use placeholder for stco offset, will patch later.

    // mvhd v0: version(1) + flags(3) + ctime(4) + mtime(4) + timescale(4) + duration(4)
    //   + rate(4) + volume(2) + reserved(10) + matrix(36) + pre_defined(24) + next_track_id(4) = 100 bytes payload
    val mvhd = box("mvhd", ByteArray(100).also {
      // timescale at offset 12 must be non-zero
      it[12] = 0; it[13] = 0; it[14] = 0x03; it[15] = 0xE8.toByte() // 1000
    })

    // Build a timed-metadata trak with handler_type = "meta"
    fun makeMetaTrak(stcoOffset: Int): ByteArray {
      // tkhd v0: 92 bytes total = 8 header + 84 payload
      val tkhd = box("tkhd", ByteArray(84))
      // hdlr: 8 + 24 + name(0) = 32 bytes — payload = version/flags(4) + pre_def(4) + handler_type(4) + reserved(12)
      val hdlrPayload = ByteArray(24)
      hdlrPayload[8] = 'm'.code.toByte(); hdlrPayload[9] = 'e'.code.toByte()
      hdlrPayload[10] = 't'.code.toByte(); hdlrPayload[11] = 'a'.code.toByte()
      val hdlr = box("hdlr", hdlrPayload)
      // mdhd v0: payload = version/flags(4) + ctime(4) + mtime(4) + timescale(4) + duration(4) + language(2) + pre_def(2) = 24
      val mdhd = box("mdhd", ByteArray(24).also {
        it[12] = 0; it[13] = 0; it[14] = 0x03; it[15] = 0xE8.toByte()  // timescale=1000
      })
      // stbl children: stsd(minimal) + stsc + stsz + stco
      // stsd: version/flags(4) + entry_count(4) = 8 bytes (no entries — minimal)
      val stsd = box("stsd", ByteArray(8))
      // stsc: version/flags(4) + entry_count(4) + 1 entry(12) = 20 bytes payload
      val stscPayload = ByteArray(20)
      stscPayload[7] = 1  // entry_count = 1
      stscPayload[11] = 1 // first_chunk = 1
      stscPayload[15] = 1 // samples_per_chunk = 1
      stscPayload[19] = 1 // sample_description_index = 1
      val stsc = box("stsc", stscPayload)
      // stsz: version/flags(4) + sample_size(4) + sample_count(4) = 12 bytes payload with fixed size
      val stszPayload = ByteArray(12)
      stszPayload[7] = 0 // sample_size = 0 means variable, use stsz entries
      // Actually use fixed sample_size=16 (length of secretBytes)
      stszPayload[4] = 0; stszPayload[5] = 0; stszPayload[6] = 0; stszPayload[7] = 16
      stszPayload[11] = 1 // sample_count = 1
      val stsz = box("stsz", stszPayload)
      // stco: version/flags(4) + entry_count(4) + 1 offset(4) = 12 bytes payload
      val stcoPayload = ByteArray(12)
      stcoPayload[7] = 1  // entry_count = 1
      stcoPayload[8] = ((stcoOffset shr 24) and 0xFF).toByte()
      stcoPayload[9] = ((stcoOffset shr 16) and 0xFF).toByte()
      stcoPayload[10] = ((stcoOffset shr 8) and 0xFF).toByte()
      stcoPayload[11] = (stcoOffset and 0xFF).toByte()
      val stco = box("stco", stcoPayload)
      val stbl = box("stbl", stsd + stsc + stsz + stco)
      // dinf needed by spec but content irrelevant — make minimal
      val dref = box("dref", ByteArray(8))
      val dinf = box("dinf", dref)
      val minf = box("minf", dinf + stbl)
      val mdia = box("mdia", mdhd + hdlr + minf)
      return box("trak", tkhd + mdia)
    }

    // First pass: build with placeholder stcoOffset=0 to measure header sizes
    fun buildWith(stcoOffset: Int): Pair<ByteArray, Int> {
      val ftyp = box("ftyp", "isom".toByteArray() + ByteArray(8))
      val metaTrak = makeMetaTrak(stcoOffset)
      val moov = box("moov", mvhd + metaTrak)
      val headerLen = ftyp.size + moov.size
      val mdatBox = box("mdat", secretBytes)
      // mdat payload starts at headerLen + 8 (mdat box header)
      val mdatPayloadOffset = headerLen + 8
      return (ftyp + moov + mdatBox) to mdatPayloadOffset
    }
    // Get the right mdat offset by building twice (sizes are stable since stcoOffset doesn't change size)
    val (_, mdatOffset) = buildWith(0)
    val (fileBytes, _) = buildWith(mdatOffset)

    // Sanity: input contains the secret bytes at mdatOffset
    val inputSecret = String(fileBytes, mdatOffset, secretBytes.size, Charsets.US_ASCII)
    assertEquals("SECRET-GPS-DATA!", inputSecret, "Test fixture should have secret bytes in mdat")

    val inputFile = File.createTempFile("test_timedmeta_", ".mp4")
    inputFile.writeBytes(fileBytes)
    val outputFile = File.createTempFile("test_timedmeta_out_", ".mp4")
    try {
      stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      val output = outputFile.readBytes()
      // File size preserved
      assertEquals(fileBytes.size, output.size, "Stripping must preserve byte layout (stco/co64)")
      // The 16 bytes at mdatOffset must be zeroed
      for (i in 0 until secretBytes.size) {
        assertEquals(0.toByte(), output[mdatOffset + i],
          "Byte at mdat+$i must be zeroed (raw timed-metadata sample), got 0x${output[mdatOffset + i].toInt().and(0xFF).toString(16)}")
      }
      // The secret string must NOT appear anywhere in the output
      val text = String(output, Charsets.US_ASCII)
      assertFalse(text.contains("SECRET-GPS-DATA"), "Timed metadata sample bytes must be fully scrubbed from mdat")
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }
}

/** Round-4 hardening tests: malformed extended-size, DoS, and external-box scrubbing. */
class Round4HardeningTest {

  /** MP4 with a top-level size=1 box whose extended size is < 16 — must not hang. */
  @Test fun testMp4ExtendedSizeTooSmallDoesNotHang() {
    // Box: [00 00 00 01]['f' 'r' 'e' 'e'][00 00 00 00 00 00 00 00]  (extSize = 0)
    val malformed = byteArrayOf(
      0, 0, 0, 1,  // rawSize = 1 (extended-size marker)
      'f'.code.toByte(), 'r'.code.toByte(), 'e'.code.toByte(), 'e'.code.toByte(),
      0, 0, 0, 0, 0, 0, 0, 0  // extSize = 0 (would yield dataSize = -16)
    )
    val inputFile = File.createTempFile("test_extsize_", ".mp4")
    inputFile.writeBytes(malformed)
    val outputFile = File.createTempFile("test_extsize_out_", ".mp4")
    try {
      // Must complete (either silently or by throwing) — must NOT hang the process.
      // Prescan returns early via `return@use` when extSize < 16, then main pass throws.
      assertFailsWith<IllegalArgumentException> {
        stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      }
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }

  /** MKV with a top-level Segment unknown-size element inside SegmentInfo — must throw, not leak. */
  @Test fun testMkvSegmentInfoUnknownSizeChildThrows() {
    // Build minimal EBML + Segment(SegmentInfo with unknown-size first child + DateUTC + SegmentUUID)
    val ebml = byteArrayOf(
      0x1A.toByte(), 0x45.toByte(), 0xDF.toByte(), 0xA3.toByte(),
      0x83.toByte(), 0x42.toByte(), 0x86.toByte(), 0x81.toByte()
    )
    // SegmentInfo child with unknown size (size VINT = 0xFF = all-ones 1-byte VINT)
    // ID = MuxingApp (0x4D80), size = unknown
    val infoChildren = byteArrayOf(
      0x4D.toByte(), 0x80.toByte(),  // MuxingApp ID
      0xFF.toByte()                   // size = unknown (all-ones)
    ) + // followed by DateUTC that should be unreachable for stripping
    byteArrayOf(
      0x44.toByte(), 0x61.toByte(),  // DateUTC ID
      0x88.toByte(),  // size = 8
      0, 0, 0, 0, 0, 0, 0, 0  // payload (would be the date)
    )
    val segmentInfo = byteArrayOf(
      0x15.toByte(), 0x49.toByte(), 0xA9.toByte(), 0x66.toByte(),  // SegmentInfo ID
      (0x80 or infoChildren.size).toByte()  // 1-byte size VINT
    ) + infoChildren
    val tracks = byteArrayOf(
      0x16.toByte(), 0x54.toByte(), 0xAE.toByte(), 0x6B.toByte(),
      0x81.toByte(), 0x00.toByte()
    )
    val segmentPayload = segmentInfo + tracks
    val segmentHeader = byteArrayOf(
      0x18.toByte(), 0x53.toByte(), 0x80.toByte(), 0x67.toByte(),
      (0x10 or ((segmentPayload.size shr 24) and 0xFF)).toByte(),
      ((segmentPayload.size shr 16) and 0xFF).toByte(),
      ((segmentPayload.size shr 8) and 0xFF).toByte(),
      (segmentPayload.size and 0xFF).toByte()
    )
    val inputBytes = ebml + segmentHeader + segmentPayload

    val inputFile = File.createTempFile("test_seginfo_unknown_", ".mkv")
    inputFile.writeBytes(inputBytes)
    val outputFile = File.createTempFile("test_seginfo_unknown_out_", ".mkv")
    try {
      // Must fail closed instead of writing remainder verbatim (which would leak DateUTC).
      assertFailsWith<IllegalArgumentException> {
        stripMkvMetadata(inputFile.absolutePath, outputFile.absolutePath)
      }
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }

  /** MP4 where timed-metadata stco points OUTSIDE mdat — those bytes must still be zeroed. */
  @Test fun testMp4TimedMetadataExternalBoxBytesZeroed() {
    // Synthesize: ftyp + free(contains SECRET) + moov(timed-meta trak with stco→free's payload) + mdat
    val secret = "EXTERNAL-GPS-XX!".toByteArray() // 16 bytes
    // mvhd v0: 100 bytes payload with valid timescale
    val mvhd = box("mvhd", ByteArray(100).also {
      it[12] = 0; it[13] = 0; it[14] = 0x03; it[15] = 0xE8.toByte()
    })

    fun makeMetaTrak(stcoOffset: Int): ByteArray {
      val tkhd = box("tkhd", ByteArray(84))
      val hdlrPayload = ByteArray(24).also {
        it[8] = 'm'.code.toByte(); it[9] = 'e'.code.toByte()
        it[10] = 't'.code.toByte(); it[11] = 'a'.code.toByte()
      }
      val hdlr = box("hdlr", hdlrPayload)
      val mdhd = box("mdhd", ByteArray(24).also {
        it[12] = 0; it[13] = 0; it[14] = 0x03; it[15] = 0xE8.toByte()
      })
      val stsd = box("stsd", ByteArray(8))
      val stscPayload = ByteArray(20).also {
        it[7] = 1; it[11] = 1; it[15] = 1; it[19] = 1
      }
      val stsc = box("stsc", stscPayload)
      val stszPayload = ByteArray(12).also {
        it[7] = 16  // fixed sample_size = 16
        it[11] = 1  // sample_count = 1
      }
      val stsz = box("stsz", stszPayload)
      val stcoPayload = ByteArray(12).also {
        it[7] = 1
        it[8] = ((stcoOffset shr 24) and 0xFF).toByte()
        it[9] = ((stcoOffset shr 16) and 0xFF).toByte()
        it[10] = ((stcoOffset shr 8) and 0xFF).toByte()
        it[11] = (stcoOffset and 0xFF).toByte()
      }
      val stco = box("stco", stcoPayload)
      val stbl = box("stbl", stsd + stsc + stsz + stco)
      val dref = box("dref", ByteArray(8))
      val dinf = box("dinf", dref)
      val minf = box("minf", dinf + stbl)
      val mdia = box("mdia", mdhd + hdlr + minf)
      return box("trak", tkhd + mdia)
    }

    // Build the file with the secret in a top-level "skip" box
    fun buildWith(stcoOffset: Int): Pair<ByteArray, Int> {
      val ftyp = box("ftyp", "isom".toByteArray() + ByteArray(8))
      val skipWithSecret = box("skip", secret) // attacker hides bytes here
      val metaTrak = makeMetaTrak(stcoOffset)
      val moov = box("moov", mvhd + metaTrak)
      val mdat = box("mdat", ByteArray(16)) // dummy mdat
      // secret is at offset ftyp.size + 8 (skip box header)
      val secretOffset = ftyp.size + 8
      return (ftyp + skipWithSecret + moov + mdat) to secretOffset
    }
    val (_, secretOffset) = buildWith(0)
    val (fileBytes, _) = buildWith(secretOffset)

    // Sanity: secret is at expected location
    assertEquals("EXTERNAL-GPS-XX!", String(fileBytes, secretOffset, secret.size, Charsets.US_ASCII))

    val inputFile = File.createTempFile("test_ext_box_", ".mp4")
    inputFile.writeBytes(fileBytes)
    val outputFile = File.createTempFile("test_ext_box_out_", ".mp4")
    try {
      stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      val output = outputFile.readBytes()
      // Bytes at secretOffset must be zeroed because stco pointed there
      for (i in 0 until secret.size) {
        assertEquals(0.toByte(), output[secretOffset + i],
          "Byte $i in external 'skip' box must be zeroed via stco range")
      }
      val text = String(output, Charsets.US_ASCII)
      assertFalse(text.contains("EXTERNAL-GPS-XX"), "External timed-metadata bytes must not survive stripping")
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }

  /** Pathological MP4 with millions of stsc entries — must complete in reasonable time (forward cursor). */
  @Test fun testMp4LargeStscDoesNotHang() {
    // We can't easily craft a 50MB file in tests. Instead verify the algorithm's correctness
    // by constructing a stbl with many stsc entries and confirming extraction is fast.
    // This is more of a smoke test — the real DoS protection (forward cursor) is verified by
    // the test completing quickly (< 1 second) rather than minutes.

    val mvhd = box("mvhd", ByteArray(100).also {
      it[12] = 0; it[13] = 0; it[14] = 0x03; it[15] = 0xE8.toByte()
    })

    // 1000 stsc entries (small enough to fit in test, large enough to detect O(N²) regression)
    val stscEntryCount = 1000
    val stscPayload = ByteArray(8 + stscEntryCount * 12).also {
      it[7] = (stscEntryCount and 0xFF).toByte()
      it[6] = ((stscEntryCount shr 8) and 0xFF).toByte()
      // Each entry: first_chunk = i+1, samples_per_chunk = 1
      for (i in 0 until stscEntryCount) {
        val off = 8 + i * 12
        val firstChunk = i + 1
        it[off + 3] = (firstChunk and 0xFF).toByte()
        it[off + 2] = ((firstChunk shr 8) and 0xFF).toByte()
        it[off + 7] = 1 // samples_per_chunk
        it[off + 11] = 1 // sample_description_index
      }
    }
    val stsc = box("stsc", stscPayload)

    // 1000 chunks via stco — would be O(N²) = 1M iterations under old code, O(N) = 1000 with forward cursor
    val stcoEntryCount = 1000
    val stcoPayload = ByteArray(8 + stcoEntryCount * 4).also {
      it[7] = (stcoEntryCount and 0xFF).toByte()
      it[6] = ((stcoEntryCount shr 8) and 0xFF).toByte()
      // Chunk offsets: 1000, 2000, ... (sparse so ranges are distinct)
      for (i in 0 until stcoEntryCount) {
        val off = 8 + i * 4
        val chunkOff = (i + 1) * 1000
        it[off + 3] = (chunkOff and 0xFF).toByte()
        it[off + 2] = ((chunkOff shr 8) and 0xFF).toByte()
        it[off + 1] = ((chunkOff shr 16) and 0xFF).toByte()
      }
    }
    val stco = box("stco", stcoPayload)

    val stszPayload = ByteArray(12).also {
      it[7] = 16 // fixed sample_size
      it[11] = (stcoEntryCount and 0xFF).toByte()  // sample_count = 1000
      it[10] = ((stcoEntryCount shr 8) and 0xFF).toByte()
    }
    val stsz = box("stsz", stszPayload)
    val stsd = box("stsd", ByteArray(8))

    val stbl = box("stbl", stsd + stsc + stsz + stco)
    val dinf = box("dinf", box("dref", ByteArray(8)))
    val mdhd = box("mdhd", ByteArray(24).also {
      it[12] = 0; it[13] = 0; it[14] = 0x03; it[15] = 0xE8.toByte()
    })
    val hdlrPayload = ByteArray(24).also {
      it[8] = 'm'.code.toByte(); it[9] = 'e'.code.toByte()
      it[10] = 't'.code.toByte(); it[11] = 'a'.code.toByte()
    }
    val hdlr = box("hdlr", hdlrPayload)
    val minf = box("minf", dinf + stbl)
    val mdia = box("mdia", mdhd + hdlr + minf)
    val tkhd = box("tkhd", ByteArray(84))
    val trak = box("trak", tkhd + mdia)
    val moov = box("moov", mvhd + trak)
    val ftyp = box("ftyp", "isom".toByteArray() + ByteArray(8))
    val mdat = box("mdat", ByteArray(16))
    val fileBytes = ftyp + moov + mdat

    val inputFile = File.createTempFile("test_large_stsc_", ".mp4")
    inputFile.writeBytes(fileBytes)
    val outputFile = File.createTempFile("test_large_stsc_out_", ".mp4")
    try {
      val start = System.currentTimeMillis()
      stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      val elapsed = System.currentTimeMillis() - start
      // With forward cursor: ~10ms. Without (O(N²)): ~100-1000ms for 1000×1000. Allow generous margin.
      assertTrue(elapsed < 5000, "Stripping with $stscEntryCount stsc × $stcoEntryCount chunks took $elapsed ms — possible O(N²) regression")
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }
}

/** Round-5 hardening tests: privacy/DoS bypasses via crafted malformed input. */
class Round5HardeningTest {

  /** MP4 top-level metadata box (uuid) with huge extended size must throw, not fill disk. */
  @Test fun testMp4TopLevelMetadataBoxHugeExtSizeThrows() {
    // Build: ftyp + uuid with size=1 + extSize = huge (would be a 2^40 byte writeZeros)
    val ftyp = byteArrayOf(
      0, 0, 0, 16, 'f'.code.toByte(), 't'.code.toByte(), 'y'.code.toByte(), 'p'.code.toByte(),
      'i'.code.toByte(), 's'.code.toByte(), 'o'.code.toByte(), 'm'.code.toByte(), 0, 0, 0, 0
    )
    // uuid header: size=1 (marker) + "uuid" + 8-byte extSize = ~1TB
    val uuid = byteArrayOf(
      0, 0, 0, 1,  // size=1 (extended marker)
      'u'.code.toByte(), 'u'.code.toByte(), 'i'.code.toByte(), 'd'.code.toByte(),
      0, 0, 1, 0, 0, 0, 0, 0  // extSize = 2^40 (~1 TB)
    )
    val inputFile = File.createTempFile("test_huge_meta_", ".mp4")
    inputFile.writeBytes(ftyp + uuid)
    val outputFile = File.createTempFile("test_huge_meta_out_", ".mp4")
    try {
      assertFailsWith<IllegalArgumentException> {
        stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      }
      // Output must not have grown to absurd size
      assertTrue(outputFile.length() < 1_000_000, "Output should not have ballooned")
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }

  /** Fragmented MP4 (moof) must throw — currently not supported. */
  @Test fun testMp4FragmentedMoofThrows() {
    // Build: ftyp + minimal moov + moof
    val ftyp = byteArrayOf(
      0, 0, 0, 16, 'f'.code.toByte(), 't'.code.toByte(), 'y'.code.toByte(), 'p'.code.toByte(),
      'i'.code.toByte(), 's'.code.toByte(), 'o'.code.toByte(), 'm'.code.toByte(), 0, 0, 0, 0
    )
    // Minimal moov containing just mvhd (so it parses without error)
    val mvhd = ByteArray(108).also {
      it[3] = 108
      it[4] = 'm'.code.toByte(); it[5] = 'v'.code.toByte(); it[6] = 'h'.code.toByte(); it[7] = 'd'.code.toByte()
      it[8 + 8 + 4] = 0; it[8 + 8 + 5] = 0; it[8 + 8 + 6] = 0x03; it[8 + 8 + 7] = 0xE8.toByte() // timescale
    }
    val moov = ByteArray(8 + mvhd.size).also {
      val sz = 8 + mvhd.size
      it[0] = ((sz shr 24) and 0xFF).toByte()
      it[1] = ((sz shr 16) and 0xFF).toByte()
      it[2] = ((sz shr 8) and 0xFF).toByte()
      it[3] = (sz and 0xFF).toByte()
      it[4] = 'm'.code.toByte(); it[5] = 'o'.code.toByte(); it[6] = 'o'.code.toByte(); it[7] = 'v'.code.toByte()
      mvhd.copyInto(it, 8)
    }
    val moof = byteArrayOf(
      0, 0, 0, 16,
      'm'.code.toByte(), 'o'.code.toByte(), 'o'.code.toByte(), 'f'.code.toByte(),
      0, 0, 0, 0, 0, 0, 0, 0
    )
    val inputFile = File.createTempFile("test_moof_", ".mp4")
    inputFile.writeBytes(ftyp + moov + moof)
    val outputFile = File.createTempFile("test_moof_out_", ".mp4")
    try {
      assertFailsWith<IllegalArgumentException> {
        stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      }
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }

  /** MKV with unknown-size Tags (not Cluster) must throw — boundary scan would otherwise leak. */
  @Test fun testMkvUnknownSizeNonClusterThrows() {
    // EBML header
    val ebml = byteArrayOf(
      0x1A.toByte(), 0x45.toByte(), 0xDF.toByte(), 0xA3.toByte(),
      0x83.toByte(), 0x42.toByte(), 0x86.toByte(), 0x81.toByte()
    )
    // Tags with UNKNOWN size, content has Cluster ID embedded — attacker payload follows
    val tagsAndPayload = byteArrayOf(
      0x12.toByte(), 0x54.toByte(), 0xC3.toByte(), 0x67.toByte(),  // Tags ID
      0xFF.toByte(),  // size = unknown
      // Tags content: 10 bytes, then embedded Cluster ID + attacker bytes
      0xAA.toByte(), 0xBB.toByte(), 0xCC.toByte(), 0xDD.toByte(), 0xEE.toByte(),
      0x1F.toByte(), 0x43.toByte(), 0xB6.toByte(), 0x75.toByte(),  // embedded Cluster ID
      0x85.toByte(),  // size = 5
      'P'.code.toByte(), 'W'.code.toByte(), 'N'.code.toByte(), 'E'.code.toByte(), 'D'.code.toByte()
    )
    val segmentPayload = tagsAndPayload
    val segmentHeader = byteArrayOf(
      0x18.toByte(), 0x53.toByte(), 0x80.toByte(), 0x67.toByte(),
      (0x10 or ((segmentPayload.size shr 24) and 0xFF)).toByte(),
      ((segmentPayload.size shr 16) and 0xFF).toByte(),
      ((segmentPayload.size shr 8) and 0xFF).toByte(),
      (segmentPayload.size and 0xFF).toByte()
    )
    val inputBytes = ebml + segmentHeader + segmentPayload
    val inputFile = File.createTempFile("test_mkv_uksz_", ".mkv")
    inputFile.writeBytes(inputBytes)
    val outputFile = File.createTempFile("test_mkv_uksz_out_", ".mkv")
    try {
      assertFailsWith<IllegalArgumentException> {
        stripMkvMetadata(inputFile.absolutePath, outputFile.absolutePath)
      }
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }

  /** AVI with LIST chunkSize < 4 must throw — otherwise alignment is corrupted. */
  @Test fun testAviListChunkSizeTooSmallThrows() {
    // RIFF/AVI header + LIST chunk with chunkSize=0 (no listType bytes allocated)
    val riffSize = 4 + 8 + 8  // "AVI " + LIST hdr + JUNK pad
    val data = byteArrayOf(
      'R'.code.toByte(), 'I'.code.toByte(), 'F'.code.toByte(), 'F'.code.toByte(),
      (riffSize and 0xFF).toByte(), ((riffSize shr 8) and 0xFF).toByte(), 0, 0,
      'A'.code.toByte(), 'V'.code.toByte(), 'I'.code.toByte(), ' '.code.toByte(),
      // Malformed LIST with chunkSize=0
      'L'.code.toByte(), 'I'.code.toByte(), 'S'.code.toByte(), 'T'.code.toByte(),
      0, 0, 0, 0,
      // Dummy follow-up (would be misparsed)
      'J'.code.toByte(), 'U'.code.toByte(), 'N'.code.toByte(), 'K'.code.toByte(),
      0, 0, 0, 0
    )
    val inputFile = File.createTempFile("test_avi_small_list_", ".avi")
    inputFile.writeBytes(data)
    val outputFile = File.createTempFile("test_avi_small_list_out_", ".avi")
    try {
      assertFailsWith<IllegalArgumentException> {
        stripAviMetadata(inputFile.absolutePath, outputFile.absolutePath)
      }
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }

  /** MP4 with timed-metadata trak whose co64 has top-bit-set (negative Long) offset must throw. */
  @Test fun testMp4Co64NegativeOffsetThrows() {
    val mvhd = box("mvhd", ByteArray(100).also {
      it[12] = 0; it[13] = 0; it[14] = 0x03; it[15] = 0xE8.toByte()
    })

    // Build a timed-metadata trak with co64 entry having top-bit set
    val tkhd = box("tkhd", ByteArray(84))
    val hdlrPayload = ByteArray(24).also {
      it[8] = 'm'.code.toByte(); it[9] = 'e'.code.toByte(); it[10] = 't'.code.toByte(); it[11] = 'a'.code.toByte()
    }
    val hdlr = box("hdlr", hdlrPayload)
    val mdhd = box("mdhd", ByteArray(24).also {
      it[12] = 0; it[13] = 0; it[14] = 0x03; it[15] = 0xE8.toByte()
    })
    val stsd = box("stsd", ByteArray(8))
    val stscPayload = ByteArray(20).also {
      it[7] = 1; it[11] = 1; it[15] = 1; it[19] = 1
    }
    val stsc = box("stsc", stscPayload)
    val stszPayload = ByteArray(12).also { it[7] = 16; it[11] = 1 }
    val stsz = box("stsz", stszPayload)
    // co64 with 1 entry, offset = 0x8000000000000000 (negative Long)
    val co64Payload = ByteArray(20).also {
      it[7] = 1  // entry_count = 1
      it[8] = 0x80.toByte()  // top bit of 8-byte offset set
    }
    val co64 = box("co64", co64Payload)
    val stbl = box("stbl", stsd + stsc + stsz + co64)
    val dinf = box("dinf", box("dref", ByteArray(8)))
    val minf = box("minf", dinf + stbl)
    val mdia = box("mdia", mdhd + hdlr + minf)
    val trak = box("trak", tkhd + mdia)
    val moov = box("moov", mvhd + trak)
    val ftyp = box("ftyp", "isom".toByteArray() + ByteArray(8))
    val mdat = box("mdat", ByteArray(16))

    val inputFile = File.createTempFile("test_co64_neg_", ".mp4")
    inputFile.writeBytes(ftyp + moov + mdat)
    val outputFile = File.createTempFile("test_co64_neg_out_", ".mp4")
    try {
      assertFailsWith<IllegalArgumentException> {
        stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      }
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }

  /** MP4 with stsc samples_per_chunk > remaining samples must throw (otherwise skips chunks 1..N). */
  @Test fun testMp4StscInconsistencyThrows() {
    val mvhd = box("mvhd", ByteArray(100).also {
      it[12] = 0; it[13] = 0; it[14] = 0x03; it[15] = 0xE8.toByte()
    })

    val tkhd = box("tkhd", ByteArray(84))
    val hdlrPayload = ByteArray(24).also {
      it[8] = 'g'.code.toByte(); it[9] = 'p'.code.toByte(); it[10] = 'm'.code.toByte(); it[11] = 'd'.code.toByte()
    }
    val hdlr = box("hdlr", hdlrPayload)
    val mdhd = box("mdhd", ByteArray(24).also {
      it[12] = 0; it[13] = 0; it[14] = 0x03; it[15] = 0xE8.toByte()
    })
    val stsd = box("stsd", ByteArray(8))
    // stsc: 1 entry — first_chunk=1, samples_per_chunk = 1_000_000 (way > sampleCount=2)
    val stscPayload = ByteArray(20).also {
      it[7] = 1  // entry_count = 1
      it[11] = 1  // first_chunk = 1
      it[15] = 0x40; it[14] = 0x42; it[13] = 0x0F  // samples_per_chunk = 1000000
      it[19] = 1
    }
    val stsc = box("stsc", stscPayload)
    val stszPayload = ByteArray(12).also {
      it[7] = 16  // fixed sample_size
      it[11] = 2  // sample_count = 2 (much less than 1M)
    }
    val stsz = box("stsz", stszPayload)
    val stcoPayload = ByteArray(16).also {
      it[7] = 2  // 2 chunks
      it[11] = 100.toByte(); it[15] = 200.toByte()  // dummy offsets
    }
    val stco = box("stco", stcoPayload)
    val stbl = box("stbl", stsd + stsc + stsz + stco)
    val dinf = box("dinf", box("dref", ByteArray(8)))
    val minf = box("minf", dinf + stbl)
    val mdia = box("mdia", mdhd + hdlr + minf)
    val trak = box("trak", tkhd + mdia)
    val moov = box("moov", mvhd + trak)
    val ftyp = box("ftyp", "isom".toByteArray() + ByteArray(8))
    val mdat = box("mdat", ByteArray(256))

    val inputFile = File.createTempFile("test_stsc_bad_", ".mp4")
    inputFile.writeBytes(ftyp + moov + mdat)
    val outputFile = File.createTempFile("test_stsc_bad_out_", ".mp4")
    try {
      assertFailsWith<IllegalArgumentException> {
        stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      }
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }
}

/** Round-6 hardening tests: subtle privacy leak vectors caught by multi-round audit. */
class Round6HardeningTest {


  /** stsc samples_per_chunk with high-bit set (negative Int) must throw, not skip via `<= 0`. */
  @Test fun testMp4StscSpcHighBitSetThrows() {
    val mvhd = box("mvhd", ByteArray(100).also {
      it[12] = 0; it[13] = 0; it[14] = 0x03; it[15] = 0xE8.toByte()
    })
    val tkhd = box("tkhd", ByteArray(84))
    val hdlrPayload = ByteArray(24).also {
      it[8] = 'g'.code.toByte(); it[9] = 'p'.code.toByte()
      it[10] = 'm'.code.toByte(); it[11] = 'd'.code.toByte()
    }
    val hdlr = box("hdlr", hdlrPayload)
    val mdhd = box("mdhd", ByteArray(24).also {
      it[12] = 0; it[13] = 0; it[14] = 0x03; it[15] = 0xE8.toByte()
    })
    val stsd = box("stsd", ByteArray(8))
    val stscPayload = ByteArray(20).also {
      it[7] = 1
      it[11] = 1  // first_chunk = 1
      it[12] = 0x80.toByte(); it[13] = 0; it[14] = 0; it[15] = 0  // samples_per_chunk = 0x80000000
      it[19] = 1
    }
    val stsc = box("stsc", stscPayload)
    val stszPayload = ByteArray(12).also { it[7] = 16; it[11] = 1 }
    val stsz = box("stsz", stszPayload)
    val stcoPayload = ByteArray(12).also { it[7] = 1; it[11] = 100.toByte() }
    val stco = box("stco", stcoPayload)
    val stbl = box("stbl", stsd + stsc + stsz + stco)
    val dinf = box("dinf", box("dref", ByteArray(8)))
    val minf = box("minf", dinf + stbl)
    val mdia = box("mdia", mdhd + hdlr + minf)
    val trak = box("trak", tkhd + mdia)
    val moov = box("moov", mvhd + trak)
    val ftyp = box("ftyp", "isom".toByteArray() + ByteArray(8))
    val mdat = box("mdat", ByteArray(256))

    val inputFile = File.createTempFile("test_stsc_neg_", ".mp4")
    inputFile.writeBytes(ftyp + moov + mdat)
    val outputFile = File.createTempFile("test_stsc_neg_out_", ".mp4")
    try {
      assertFailsWith<IllegalArgumentException> {
        stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      }
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }

  /** MKV SegmentInfo child with elementSize > Int.MAX_VALUE must throw, not truncate via toInt(). */
  @Test fun testMkvSegmentInfoHugeChildSizeThrows() {
    val ebml = byteArrayOf(
      0x1A.toByte(), 0x45.toByte(), 0xDF.toByte(), 0xA3.toByte(),
      0x83.toByte(), 0x42.toByte(), 0x86.toByte(), 0x81.toByte()
    )
    // SegmentInfo with a child whose VINT size = 0x100000001 (4 GB + 1)
    // 8-byte VINT marker 0x01, value bytes follow
    val childWithHugeSize = byteArrayOf(
      0x44.toByte(), 0x61.toByte(),  // DateUTC ID
      0x01.toByte(), 0x00.toByte(), 0x00.toByte(), 0x01.toByte(),
      0x00.toByte(), 0x00.toByte(), 0x00.toByte(), 0x01.toByte()
    )
    val infoPayload = childWithHugeSize
    val infoBox = byteArrayOf(
      0x15.toByte(), 0x49.toByte(), 0xA9.toByte(), 0x66.toByte(),
      (0x80 or infoPayload.size).toByte()
    ) + infoPayload
    val tracks = byteArrayOf(
      0x16.toByte(), 0x54.toByte(), 0xAE.toByte(), 0x6B.toByte(),
      0x81.toByte(), 0x00.toByte()
    )
    val segmentPayload = infoBox + tracks
    val segmentHeader = byteArrayOf(
      0x18.toByte(), 0x53.toByte(), 0x80.toByte(), 0x67.toByte(),
      (0x10 or ((segmentPayload.size shr 24) and 0xFF)).toByte(),
      ((segmentPayload.size shr 16) and 0xFF).toByte(),
      ((segmentPayload.size shr 8) and 0xFF).toByte(),
      (segmentPayload.size and 0xFF).toByte()
    )
    val inputBytes = ebml + segmentHeader + segmentPayload

    val inputFile = File.createTempFile("test_seginfo_huge_", ".mkv")
    inputFile.writeBytes(inputBytes)
    val outputFile = File.createTempFile("test_seginfo_huge_out_", ".mkv")
    try {
      assertFailsWith<IllegalArgumentException> {
        stripMkvMetadata(inputFile.absolutePath, outputFile.absolutePath)
      }
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }

  /** MP4 main pass must throw (not silently break) on malformed top-level box. */
  @Test fun testMp4MainPassMalformedBoxThrows() {
    val ftyp = box("ftyp", "isom".toByteArray() + ByteArray(8))
    val mdat = box("mdat", "SECRET-LEAKED-MDAT-CONTENT!".toByteArray())
    val malformedBox = byteArrayOf(0, 0, 0, 2, 'b'.code.toByte(), 'a'.code.toByte(), 'd'.code.toByte(), '!'.code.toByte())
    val mvhd = box("mvhd", ByteArray(100).also {
      it[12] = 0; it[13] = 0; it[14] = 0x03; it[15] = 0xE8.toByte()
    })
    val moov = box("moov", mvhd)

    val inputFile = File.createTempFile("test_main_malformed_", ".mp4")
    inputFile.writeBytes(ftyp + mdat + malformedBox + moov)
    val outputFile = File.createTempFile("test_main_malformed_out_", ".mp4")
    try {
      assertFailsWith<IllegalArgumentException> {
        stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      }
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }

  /** MP4 without ftyp first must throw (engulfment defense). */
  @Test fun testMp4MissingFtypFirstThrows() {
    // File starts with a different box type (mdat) — would normally be an engulfing wrapper
    val mdat = box("mdat", "fake-mdat-data".toByteArray())
    val moov = box("moov", ByteArray(20))
    val inputFile = File.createTempFile("test_no_ftyp_", ".mp4")
    inputFile.writeBytes(mdat + moov)
    val outputFile = File.createTempFile("test_no_ftyp_out_", ".mp4")
    try {
      assertFailsWith<IllegalArgumentException> {
        stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      }
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }

  /** MP4 engulfment attack: lying mdat size that swallows moov must be rejected. */
  @Test fun testMp4EngulfmentAttackRejected() {
    val ftyp = box("ftyp", "isom".toByteArray() + ByteArray(8))
    // The "real" moov (would normally be at top-level, but it's hidden inside engulfing mdat)
    val mvhd = box("mvhd", ByteArray(100).also {
      it[12] = 0; it[13] = 0; it[14] = 0x03; it[15] = 0xE8.toByte()
    })
    val moov = box("moov", mvhd)
    val realMdatContent = "actual-media-data".toByteArray()
    // mdat header declares a size covering both itself AND the engulfed moov
    val engulfingSize = 8 + realMdatContent.size + moov.size
    val mdatHeader = byteArrayOf(
      ((engulfingSize shr 24) and 0xFF).toByte(),
      ((engulfingSize shr 16) and 0xFF).toByte(),
      ((engulfingSize shr 8) and 0xFF).toByte(),
      (engulfingSize and 0xFF).toByte(),
      'm'.code.toByte(), 'd'.code.toByte(), 'a'.code.toByte(), 't'.code.toByte()
    )
    // File: ftyp + lying mdat header + actual mdat data + engulfed moov (no top-level moov)
    val inputBytes = ftyp + mdatHeader + realMdatContent + moov
    val inputFile = File.createTempFile("test_engulf_", ".mp4")
    inputFile.writeBytes(inputBytes)
    val outputFile = File.createTempFile("test_engulf_out_", ".mp4")
    try {
      // Must throw — engulfment hides moov from our top-level scan, so no moov is "seen".
      // Previously: mdat would be stream-copied verbatim, leaking moov bytes (including udta/uuid).
      assertFailsWith<IllegalArgumentException> {
        stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      }
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }

  /** MKV engulfment attack: lying Cluster size that swallows Tracks must be rejected. */
  @Test fun testMkvEngulfmentAttackRejected() {
    val ebml = byteArrayOf(
      0x1A.toByte(), 0x45.toByte(), 0xDF.toByte(), 0xA3.toByte(),
      0x83.toByte(), 0x42.toByte(), 0x86.toByte(), 0x81.toByte()
    )
    val segmentInfo = byteArrayOf(
      0x15.toByte(), 0x49.toByte(), 0xA9.toByte(), 0x66.toByte(),
      0x82.toByte(), 0x44.toByte(), 0x61.toByte()  // minimal SegmentInfo with DateUTC partial
    )
    // Tracks element placed AFTER Cluster header, so engulfing Cluster claims to cover it
    val realTracks = byteArrayOf(
      0x16.toByte(), 0x54.toByte(), 0xAE.toByte(), 0x6B.toByte(),
      0x81.toByte(), 0x00.toByte()
    )
    val clusterData = byteArrayOf(0xA3.toByte(), 0x81.toByte(), 0x42.toByte())  // fake SimpleBlock
    // Cluster header with a definite size that engulfs realTracks
    val engulfedSize = clusterData.size + realTracks.size
    val cluster = byteArrayOf(
      0x1F.toByte(), 0x43.toByte(), 0xB6.toByte(), 0x75.toByte(),
      (0x80 or engulfedSize).toByte()
    ) + clusterData + realTracks
    // Segment WITHOUT real top-level Tracks — Tracks only exists inside engulfed Cluster
    val segmentPayload = segmentInfo + cluster
    val segmentHeader = byteArrayOf(
      0x18.toByte(), 0x53.toByte(), 0x80.toByte(), 0x67.toByte(),
      (0x10 or ((segmentPayload.size shr 24) and 0xFF)).toByte(),
      ((segmentPayload.size shr 16) and 0xFF).toByte(),
      ((segmentPayload.size shr 8) and 0xFF).toByte(),
      (segmentPayload.size and 0xFF).toByte()
    )
    val inputFile = File.createTempFile("test_mkv_engulf_", ".mkv")
    inputFile.writeBytes(ebml + segmentHeader + segmentPayload)
    val outputFile = File.createTempFile("test_mkv_engulf_out_", ".mkv")
    try {
      // Must throw — Tracks is not visible at Segment level (engulfed inside Cluster)
      assertFailsWith<IllegalArgumentException> {
        stripMkvMetadata(inputFile.absolutePath, outputFile.absolutePath)
      }
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }

  /** Malformed stbl (missing stsc) on a timed-metadata trak must throw — not silently leave samples. */
  @Test fun testMp4MalformedStblOnTimedMetadataThrows() {
    val mvhd = box("mvhd", ByteArray(100).also {
      it[12] = 0; it[13] = 0; it[14] = 0x03; it[15] = 0xE8.toByte()
    })
    val tkhd = box("tkhd", ByteArray(84))
    val hdlrPayload = ByteArray(24).also {
      it[8] = 'c'.code.toByte(); it[9] = 'a'.code.toByte()
      it[10] = 'm'.code.toByte(); it[11] = 'm'.code.toByte()
    }
    val hdlr = box("hdlr", hdlrPayload)
    val mdhd = box("mdhd", ByteArray(24).also {
      it[12] = 0; it[13] = 0; it[14] = 0x03; it[15] = 0xE8.toByte()
    })
    val stsd = box("stsd", ByteArray(8))
    val stcoPayload = ByteArray(12).also { it[7] = 1; it[11] = 100.toByte() }
    val stco = box("stco", stcoPayload)
    val stbl = box("stbl", stsd + stco)  // no stsc, no stsz — malformed
    val dinf = box("dinf", box("dref", ByteArray(8)))
    val minf = box("minf", dinf + stbl)
    val mdia = box("mdia", mdhd + hdlr + minf)
    val trak = box("trak", tkhd + mdia)
    val moov = box("moov", mvhd + trak)
    val ftyp = box("ftyp", "isom".toByteArray() + ByteArray(8))
    val mdat = box("mdat", "GPS-COORDS-HERE!".toByteArray())

    val inputFile = File.createTempFile("test_mal_stbl_", ".mp4")
    inputFile.writeBytes(ftyp + moov + mdat)
    val outputFile = File.createTempFile("test_mal_stbl_out_", ".mp4")
    try {
      assertFailsWith<IllegalArgumentException> {
        stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      }
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }
}

/**
 * Verifies the public API used by the file-attach path and remote-host strip-to-tmp helpers:
 * isAnimImageFile() routing and stripAnimImageMetadataByExtension() dispatch / fail-closed.
 */
class AnimImageFileDispatchTest {

  @Test fun testGifDetected() {
    assertTrue(isAnimImageFile("photo.gif"))
    assertTrue(isAnimImageFile("PHOTO.GIF"))
    assertTrue(isAnimImageFile("a.b.gif"))
  }

  @Test fun testWebpDetected() {
    assertTrue(isAnimImageFile("sticker.webp"))
    assertTrue(isAnimImageFile("STICKER.WEBP"))
  }

  @Test fun testNonAnimImageNotDetected() {
    assertFalse(isAnimImageFile("photo.jpg"))
    assertFalse(isAnimImageFile("photo.png"))
    assertFalse(isAnimImageFile("video.mp4"))
    assertFalse(isAnimImageFile("doc.pdf"))
    assertFalse(isAnimImageFile("noext"))
    assertFalse(isAnimImageFile(""))
    // HEIC/AVIF intentionally NOT included — they bypass file-attach stripping today; the
    // animated-image media-attach path rejects them via stripAnimImageMetadataByExtension below.
    assertFalse(isAnimImageFile("photo.heic"))
    assertFalse(isAnimImageFile("photo.avif"))
  }

  // Minimum GIF89a with no comment / app extension blocks (still a valid stripper input).
  private val minimalGif: ByteArray = byteArrayOf(
    0x47, 0x49, 0x46, 0x38, 0x39, 0x61,                  // "GIF89a"
    0x01, 0x00, 0x01, 0x00,                              // 1x1
    0x00, 0x00, 0x00,                                    // no GCT, bg, aspect
    0x2C, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x01, 0x00, 0x00, // image descriptor
    0x02, 0x02, 0x44, 0x01, 0x00,                        // image data
    0x3B                                                  // trailer
  )

  // Minimum WebP VP8L with no metadata chunks.
  private val minimalWebP: ByteArray = byteArrayOf(
    0x52, 0x49, 0x46, 0x46,           // "RIFF"
    0x1A, 0x00, 0x00, 0x00,           // file size - 8 = 26
    0x57, 0x45, 0x42, 0x50,           // "WEBP"
    0x56, 0x50, 0x38, 0x4C,           // "VP8L"
    0x0E, 0x00, 0x00, 0x00,           // VP8L chunk size = 14
    0x2F,                              // signature
    0x00, 0x00, 0x00, 0x00,           // width-1, height-1, alpha+version
    0x10, 0x07, 0x10, 0x11, 0x11, 0x88.toByte(), 0x88.toByte(), 0x08, 0x00 // 9 bytes payload (random valid-ish LZ77 prefix)
  )

  @Test fun testStripDispatchesGif() {
    // Case-insensitive: dispatcher lowercases the ext.
    val out = stripAnimImageMetadataByExtension("GIF", minimalGif)
    // First 6 bytes must remain a valid GIF89a header.
    assertEquals("GIF89a", String(out, 0, 6, Charsets.US_ASCII))
  }

  @Test fun testStripDispatchesWebp() {
    val out = stripAnimImageMetadataByExtension("webp", minimalWebP)
    assertEquals("RIFF", String(out, 0, 4, Charsets.US_ASCII))
    assertEquals("WEBP", String(out, 8, 4, Charsets.US_ASCII))
  }

  @Test fun testStripFailsClosedOnUnsupported() {
    // HEIC/AVIF/PNG/JPG/etc must throw rather than silently pass through — keeps the
    // remote-host strip-to-tmp and file-attach paths fail-closed.
    assertFailsWith<IllegalArgumentException> {
      stripAnimImageMetadataByExtension("heic", byteArrayOf(0, 0, 0, 0))
    }
    assertFailsWith<IllegalArgumentException> {
      stripAnimImageMetadataByExtension("png", byteArrayOf(0, 0, 0, 0))
    }
    assertFailsWith<IllegalArgumentException> {
      stripAnimImageMetadataByExtension("", byteArrayOf(0, 0, 0, 0))
    }
  }
}

/** Regression tests for fail-closed gaps flagged in PR #6999 review. */
class ReviewFailClosedTest {

  // --- GIF bounds checks (review #4, #5) ---

  @Test fun testGifTruncatedGctThrowsIllegalArgument() {
    // GIF89a header + LSD where the packed byte advertises a GCT (0x80) of 3*2^(0+1) = 6 bytes,
    // but the file ends right after the LSD. Without the bounds check this throws IOB from
    // ByteArrayOutputStream.write; with it the stripper throws IllegalArgumentException.
    val truncated = byteArrayOf(
      0x47, 0x49, 0x46, 0x38, 0x39, 0x61,           // "GIF89a"
      0x01, 0x00, 0x01, 0x00,                       // 1x1
      0x80.toByte(),                                // packed: GCT bit set, size code 0 → 6 bytes
      0x00, 0x00                                    // bg, aspect
    )
    assertFailsWith<IllegalArgumentException> { stripGifMetadata(truncated) }
  }

  @Test fun testGifTruncatedImageDescriptorThrowsIllegalArgument() {
    // Header + LSD (no GCT) + image-descriptor sentinel only — descriptor needs 10 bytes total.
    val truncated = byteArrayOf(
      0x47, 0x49, 0x46, 0x38, 0x39, 0x61,           // "GIF89a"
      0x01, 0x00, 0x01, 0x00,                       // 1x1
      0x00, 0x00, 0x00,                             // no GCT, bg, aspect
      0x2C, 0x00                                    // image-descriptor sentinel + 1 stray byte
    )
    assertFailsWith<IllegalArgumentException> { stripGifMetadata(truncated) }
  }

  // --- WebP VP8X size check (review #6) ---

  @Test fun testWebpVp8xTooSmallThrowsIllegalArgument() {
    // VP8X with chunkSize = 0 (spec mandates 10). Without the check, vp8xOffset would land on
    // the next chunk's FourCC and the post-loop flag-clear would corrupt that byte.
    val malformed = byteArrayOf(
      0x52, 0x49, 0x46, 0x46,                       // "RIFF"
      0x14, 0x00, 0x00, 0x00,                       // file size - 8 = 20
      0x57, 0x45, 0x42, 0x50,                       // "WEBP"
      0x56, 0x50, 0x38, 0x58,                       // "VP8X"
      0x00, 0x00, 0x00, 0x00,                       // chunkSize = 0 (truncated, malformed)
      0x45, 0x58, 0x49, 0x46,                       // "EXIF" — would be flag-corrupted by bug
      0x00, 0x00, 0x00, 0x00                        // EXIF chunkSize = 0
    )
    assertFailsWith<IllegalArgumentException> { stripWebPMetadata(malformed) }
  }

  // --- MP4 processContainer trailing bytes (review #1) ---

  @Test fun testMp4ContainerWithTrailingBytesThrowsIllegalArgument() {
    // moov containing a single trak that DOES NOT TILE — i.e., trak's declared size leaves
    // a few bytes inside moov unaccounted for. Without the trailing-byte throw, processContainer
    // silently drops those bytes, shrinking moov and shifting mdat → broken stco/co64.
    val mvhd = box("mvhd", ByteArray(100))
    val trak = box("trak", ByteArray(100))
    val moov = box("moov", mvhd + trak + ByteArray(3))  // 3 trailing bytes inside moov
    val ftyp = box("ftyp", "isom".toByteArray() + ByteArray(8))
    val mdat = box("mdat", ByteArray(8))

    val inputFile = File.createTempFile("test_mp4_trailing_", ".mp4")
    inputFile.writeBytes(ftyp + moov + mdat)
    val outputFile = File.createTempFile("test_mp4_trailing_out_", ".mp4")
    try {
      assertFailsWith<IllegalArgumentException> {
        stripVideoMetadata(inputFile.absolutePath, outputFile.absolutePath)
      }
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }

  // --- AVI silent break → throw (review #2, #3) ---

  @Test fun testAviTopLevelNegativeChunkSizeThrows() {
    // Top-level chunk with the high bit set in chunkSize. Previously silent-break, now throws.
    val data = byteArrayOf(
      0x52, 0x49, 0x46, 0x46,                                 // "RIFF"
      0x14, 0x00, 0x00, 0x00,                                 // file size - 8 = 20
      0x41, 0x56, 0x49, 0x20,                                 // "AVI "
      0x4A, 0x55, 0x4E, 0x4B,                                 // "JUNK" chunk
      0xFF.toByte(), 0xFF.toByte(), 0xFF.toByte(), 0xFF.toByte()  // chunkSize = -1 (top bit set)
    )
    val inputFile = File.createTempFile("test_avi_neg_", ".avi")
    inputFile.writeBytes(data)
    val outputFile = File.createTempFile("test_avi_neg_out_", ".avi")
    try {
      assertFailsWith<IllegalArgumentException> {
        stripAviMetadata(inputFile.absolutePath, outputFile.absolutePath)
      }
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }

  @Test fun testAviHdrlNegativeChunkSizeThrows() {
    // hdrl LIST containing an inner chunk with negative size. Previously silent-break,
    // now throws so subsequent _PMX/strd/IDIT metadata can't be silently dropped.
    val innerChunk = byteArrayOf(
      0x61, 0x76, 0x69, 0x68,                                 // "avih"
      0xFF.toByte(), 0xFF.toByte(), 0xFF.toByte(), 0xFF.toByte()  // chunkSize = -1
    )
    val hdrlListBody = "hdrl".toByteArray() + innerChunk
    val hdrlList = byteArrayOf(0x4C, 0x49, 0x53, 0x54) +      // "LIST"
      byteArrayOf(hdrlListBody.size.toByte(), 0, 0, 0) +      // size
      hdrlListBody
    val data = byteArrayOf(
      0x52, 0x49, 0x46, 0x46,                                 // "RIFF"
      (12 + hdrlList.size).toByte(), 0, 0, 0,                 // file size - 8
      0x41, 0x56, 0x49, 0x20                                  // "AVI "
    ) + hdrlList
    val inputFile = File.createTempFile("test_avi_hdrl_neg_", ".avi")
    inputFile.writeBytes(data)
    val outputFile = File.createTempFile("test_avi_hdrl_neg_out_", ".avi")
    try {
      assertFailsWith<IllegalArgumentException> {
        stripAviMetadata(inputFile.absolutePath, outputFile.absolutePath)
      }
    } finally {
      inputFile.delete(); outputFile.delete()
    }
  }
}
