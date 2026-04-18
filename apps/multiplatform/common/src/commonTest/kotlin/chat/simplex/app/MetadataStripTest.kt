package chat.simplex.app

import chat.simplex.common.platform.*
import java.io.File
import kotlin.test.*

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

  private fun loadResource(name: String): ByteArray =
    this::class.java.classLoader!!.getResourceAsStream("metadata-test-files/$name")!!.readBytes()

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

  private fun loadResourceToTempFile(name: String): File {
    val bytes = this::class.java.classLoader!!.getResourceAsStream("metadata-test-files/$name")!!.readBytes()
    val tmp = File.createTempFile("test_", "_$name")
    tmp.deleteOnExit()
    tmp.writeBytes(bytes)
    return tmp
  }

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

  private fun loadResourceToTempFile(name: String): File {
    val bytes = this::class.java.classLoader!!.getResourceAsStream("metadata-test-files/$name")!!.readBytes()
    val tmp = File.createTempFile("test_", "_$name")
    tmp.deleteOnExit()
    tmp.writeBytes(bytes)
    return tmp
  }

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

  private fun loadResourceToTempFile(name: String): File {
    val bytes = this::class.java.classLoader!!.getResourceAsStream("metadata-test-files/$name")!!.readBytes()
    val tmp = File.createTempFile("test_", "_$name")
    tmp.deleteOnExit()
    tmp.writeBytes(bytes)
    return tmp
  }

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

  private fun loadResourceToTempFile(name: String): File {
    val bytes = this::class.java.classLoader!!.getResourceAsStream("metadata-test-files/$name")!!.readBytes()
    val tmp = File.createTempFile("test_", "_$name")
    tmp.deleteOnExit()
    tmp.writeBytes(bytes)
    return tmp
  }

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

  private fun loadResource(name: String): ByteArray =
    this::class.java.classLoader!!.getResourceAsStream("metadata-test-files/$name")!!.readBytes()

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
