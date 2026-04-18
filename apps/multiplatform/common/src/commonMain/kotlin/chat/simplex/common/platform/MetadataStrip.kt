package chat.simplex.common.platform

private const val COPY_BUF_SIZE = 64 * 1024
// Max size for header structures loaded into memory (moov, hdrl, MKV Info/Tracks).
// Real headers are < 1MB; anything larger is malformed/malicious.
private const val MAX_HEADER_SIZE = 50L * 1024 * 1024

// --- GIF named constants ---
private const val GIF_TRAILER = 0x3B
private const val GIF_IMAGE_DESC = 0x2C
private const val GIF_EXT_INTRO = 0x21
private const val GIF_APP_EXT_LABEL = 0xFF
private const val GIF_GCE_LABEL = 0xF9
private const val GIF_APP_ID_OFFSET = 3
private const val GIF_APP_ID_LENGTH = 11

// --- MP4 named constants ---
/** mdhd language code for "und" (undetermined) — packed ISO 639-2/T: each char = (char - 0x60), 5 bits × 3 */
private const val MDHD_LANG_UND = 0x55C4

// --- MKV SeekHead element IDs ---
private val MKV_SEEK_ID = byteArrayOf(0x53.toByte(), 0xAB.toByte())
private val MKV_SEEK_POSITION_ID = byteArrayOf(0x53.toByte(), 0xAC.toByte())
private val MKV_SEEK_ENTRY_ID = byteArrayOf(0x4D.toByte(), 0xBB.toByte())

// --- Shared big-endian I/O ---

private fun readBE32(d: ByteArray, off: Int): Int =
  ((d[off].toInt() and 0xFF) shl 24) or ((d[off + 1].toInt() and 0xFF) shl 16) or
  ((d[off + 2].toInt() and 0xFF) shl 8) or (d[off + 3].toInt() and 0xFF)

private fun writeBE32(value: Int): ByteArray = byteArrayOf(
  ((value shr 24) and 0xFF).toByte(), ((value shr 16) and 0xFF).toByte(),
  ((value shr 8) and 0xFF).toByte(), (value and 0xFF).toByte()
)

// --- Stream copy helper ---

/** Copy [remaining] bytes from [raf] to [out] using [buf] as the transfer buffer. */
private fun streamCopy(raf: java.io.RandomAccessFile, out: java.io.OutputStream, remaining: Long, buf: ByteArray) {
  var left = remaining
  while (left > 0) {
    val toRead = minOf(left, buf.size.toLong()).toInt()
    raf.readFully(buf, 0, toRead)
    out.write(buf, 0, toRead)
    left -= toRead
  }
}

/** Copy [remaining] bytes from [raf] to [out] (RandomAccessFile) using [buf] as the transfer buffer. */
private fun streamCopy(raf: java.io.RandomAccessFile, out: java.io.RandomAccessFile, remaining: Long, buf: ByteArray) {
  var left = remaining
  while (left > 0) {
    val toRead = minOf(left, buf.size.toLong()).toInt()
    raf.readFully(buf, 0, toRead)
    out.write(buf, 0, toRead)
    left -= toRead
  }
}

// --- EBML VINT shared helper ---

/**
 * Returns the byte length of a VINT given its first (leading) byte.
 * Throws on an invalid leading byte (0x00).
 */
private fun vintByteLength(firstByte: Int): Int = when {
  firstByte and 0x80 != 0 -> 1
  firstByte and 0x40 != 0 -> 2
  firstByte and 0x20 != 0 -> 3
  firstByte and 0x10 != 0 -> 4
  firstByte and 0x08 != 0 -> 5
  firstByte and 0x04 != 0 -> 6
  firstByte and 0x02 != 0 -> 7
  firstByte and 0x01 != 0 -> 8
  else -> throw IllegalArgumentException("Invalid VINT leading byte: 0x${firstByte.toString(16)}")
}

/**
 * Strips metadata from GIF89a files while preserving animation.
 * Removes: Comment Extensions, Plain Text Extensions, non-animation Application Extensions (XMP, etc.)
 * Preserves: Header, LSD, GCT, Graphic Control Extensions, Image Descriptors, NETSCAPE2.0/ANIMEXTS1.0 app extensions, frame data.
 */
fun stripGifMetadata(data: ByteArray): ByteArray {
  if (data.size < 13) throw IllegalArgumentException("Invalid GIF: too short")
  val header = String(data, 0, 6)
  if (header != "GIF87a" && header != "GIF89a") throw IllegalArgumentException("Invalid GIF header: $header")

  val out = java.io.ByteArrayOutputStream(data.size)
  // Copy Header (6 bytes) + Logical Screen Descriptor (7 bytes)
  out.write(data, 0, 13)

  // Global Color Table
  val packed = data[10].toInt() and 0xFF
  val hasGCT = (packed and 0x80) != 0
  val gctSize = if (hasGCT) 3 * (1 shl ((packed and 0x07) + 1)) else 0
  if (hasGCT) {
    out.write(data, 13, gctSize)
  }

  var pos = 13 + gctSize

  while (pos < data.size) {
    val blockType = data[pos].toInt() and 0xFF

    when (blockType) {
      GIF_TRAILER -> {
        out.write(GIF_TRAILER)
        break
      }
      GIF_IMAGE_DESC -> {
        // Image Descriptor -- copy it + optional LCT + image data
        pos = copyImageDescriptor(data, pos, out)
      }
      GIF_EXT_INTRO -> {
        // Extension
        if (pos + 1 >= data.size) break
        val label = data[pos + 1].toInt() and 0xFF
        when (label) {
          GIF_GCE_LABEL -> {
            // Graphic Control Extension -- always keep
            pos = copyExtensionBlock(data, pos, out)
          }
          GIF_APP_EXT_LABEL -> {
            // Application Extension -- keep only NETSCAPE2.0 and ANIMEXTS1.0
            val appId = if (pos + GIF_APP_ID_OFFSET + GIF_APP_ID_LENGTH <= data.size)
              String(data, pos + GIF_APP_ID_OFFSET, GIF_APP_ID_LENGTH) else ""
            if (appId == "NETSCAPE2.0" || appId == "ANIMEXTS1.0") {
              pos = copyExtensionBlock(data, pos, out)
            } else {
              pos = skipExtensionBlock(data, pos)
            }
          }
          else -> {
            // Comment (0xFE), Plain Text (0x01), or unknown -- strip
            pos = skipExtensionBlock(data, pos)
          }
        }
      }
      else -> {
        // Unknown top-level block type -- fail closed on malformed input
        throw IllegalArgumentException("Unknown GIF block type 0x${blockType.toString(16)} at offset $pos")
      }
    }
  }

  // Ensure trailer is written
  val result = out.toByteArray()
  if (result.isEmpty() || result[result.size - 1] != GIF_TRAILER.toByte()) {
    out.write(GIF_TRAILER)
    return out.toByteArray()
  }
  return result
}

/** Copy an extension block (introducer + label + sub-blocks) to output. Returns new position. */
private fun copyExtensionBlock(data: ByteArray, start: Int, out: java.io.ByteArrayOutputStream): Int {
  // Write introducer (0x21) and label
  out.write(data[start].toInt() and 0xFF)
  out.write(data[start + 1].toInt() and 0xFF)
  var pos = start + 2
  // Copy sub-blocks until block terminator (0x00)
  while (pos < data.size) {
    val subBlockSize = data[pos].toInt() and 0xFF
    out.write(subBlockSize)
    if (subBlockSize == 0) {
      pos++
      break
    }
    out.write(data, pos + 1, subBlockSize)
    pos += 1 + subBlockSize
  }
  return pos
}

/** Skip an extension block. Returns new position after the block. */
private fun skipExtensionBlock(data: ByteArray, start: Int): Int {
  var pos = start + 2 // skip introducer + label
  while (pos < data.size) {
    val subBlockSize = data[pos].toInt() and 0xFF
    if (subBlockSize == 0) {
      pos++
      break
    }
    pos += 1 + subBlockSize
  }
  return pos
}

/** Copy an Image Descriptor block + optional Local Color Table + image data. Returns new position. */
private fun copyImageDescriptor(data: ByteArray, start: Int, out: java.io.ByteArrayOutputStream): Int {
  // Image Descriptor is 10 bytes (0x2C + 9 bytes)
  out.write(data, start, 10)
  var pos = start + 10

  // Check for Local Color Table
  val packed = data[start + 9].toInt() and 0xFF
  val hasLCT = (packed and 0x80) != 0
  if (hasLCT) {
    val lctSize = 3 * (1 shl ((packed and 0x07) + 1))
    out.write(data, pos, lctSize)
    pos += lctSize
  }

  // LZW Minimum Code Size
  out.write(data[pos].toInt() and 0xFF)
  pos++

  // Image data sub-blocks
  while (pos < data.size) {
    val subBlockSize = data[pos].toInt() and 0xFF
    out.write(subBlockSize)
    if (subBlockSize == 0) {
      pos++
      break
    }
    out.write(data, pos + 1, subBlockSize)
    pos += 1 + subBlockSize
  }
  return pos
}

/**
 * Strips metadata from WebP files while preserving animation.
 * Uses allowlist: only known structural chunks are kept, everything else is stripped.
 * Updates VP8X flags to reflect removed chunks.
 */
fun stripWebPMetadata(data: ByteArray): ByteArray {
  if (data.size < 12) throw IllegalArgumentException("Invalid WebP: too short")
  val riff = String(data, 0, 4)
  val webp = String(data, 8, 4)
  if (riff != "RIFF" || webp != "WEBP") throw IllegalArgumentException("Invalid WebP header")

  val out = java.io.ByteArrayOutputStream(data.size)
  out.write(data, 0, 12) // "RIFF" + size(4) + "WEBP"

  // Allowlist: only structural chunks needed for rendering
  val allowedChunks = setOf("VP8X", "VP8 ", "VP8L", "ANIM", "ANMF", "ALPH")
  // VP8X flags byte offset within output (relative to start of file): set when we see VP8X
  var vp8xOffset = -1
  var pos = 12
  var strippedAny = false

  while (pos + 8 <= data.size) {
    val fourCC = String(data, pos, 4)
    val chunkSize = readLE32(data, pos + 4)
    if (chunkSize < 0) throw IllegalArgumentException("Invalid WebP: negative chunk size at offset $pos")
    // Use Long to avoid overflow for odd chunkSize near Int.MAX_VALUE
    val paddedSize = if (chunkSize % 2 == 0) chunkSize.toLong() else chunkSize.toLong() + 1L
    val totalChunkSize = 8L + paddedSize

    if (pos + totalChunkSize > data.size) break

    if (fourCC !in allowedChunks) {
      strippedAny = true
      pos += totalChunkSize.toInt()
      continue
    }

    if (fourCC == "VP8X") {
      // +8 to skip the chunk header (fourCC + size), points to the flags byte in the output
      vp8xOffset = out.size() + 8
    }

    out.write(data, pos, minOf(totalChunkSize.toInt(), data.size - pos))
    pos += totalChunkSize.toInt()
  }

  val result = out.toByteArray()

  if (!strippedAny) return data

  writeLE32(result, 4, result.size - 8)

  // Clear all metadata-related VP8X flags
  if (vp8xOffset >= 0 && vp8xOffset < result.size) {
    var flags = result[vp8xOffset].toInt() and 0xFF
    flags = flags and 0x20.inv() // clear ICC bit (bit 5)
    flags = flags and 0x08.inv() // clear EXIF bit (bit 3)
    flags = flags and 0x04.inv() // clear XMP bit (bit 2)
    result[vp8xOffset] = flags.toByte()
  }

  return result
}

private fun readLE32(data: ByteArray, offset: Int): Int =
  (data[offset].toInt() and 0xFF) or
  ((data[offset + 1].toInt() and 0xFF) shl 8) or
  ((data[offset + 2].toInt() and 0xFF) shl 16) or
  ((data[offset + 3].toInt() and 0xFF) shl 24)

private fun writeLE32(data: ByteArray, offset: Int, value: Int) {
  data[offset] = (value and 0xFF).toByte()
  data[offset + 1] = ((value shr 8) and 0xFF).toByte()
  data[offset + 2] = ((value shr 16) and 0xFF).toByte()
  data[offset + 3] = ((value shr 24) and 0xFF).toByte()
}

/**
 * Strips metadata from MP4/MOV files using streaming I/O.
 * Only moov box (typically < 100KB) is loaded into memory.
 * mdat and other large boxes are stream-copied without buffering.
 *
 * MP4 box header layout (8 bytes):
 *   [0..3] size (big-endian uint32) — total box size including header
 *          size=0 means box extends to EOF
 *          size=1 means 64-bit extended size follows in bytes [8..15]
 *   [4..7] type (4 ASCII chars, e.g. "ftyp", "moov", "mdat")
 * For size=1, the actual size is in bytes [8..15] as a big-endian uint64,
 * and the data payload starts at offset 16 instead of 8.
 */
fun stripVideoMetadata(inputPath: String, outputPath: String) {
  val input = java.io.File(inputPath)
  if (!input.exists()) throw IllegalArgumentException("Input file does not exist: $inputPath")

  val metadataBoxes = setOf("udta", "meta", "uuid")
  val containerBoxes = setOf("moov", "trak", "mdia", "minf", "stbl", "dinf", "edts")
  val timestampBoxes = setOf("mvhd", "tkhd", "mdhd")

  fun zeroTimestamps(boxData: ByteArray, boxType: String): ByteArray {
    val copy = boxData.copyOf()
    if (copy.size < 12) return copy
    val version = copy[8].toInt() and 0xFF
    // Version 0 layout: [8] version, [9..11] flags, [12..15] creation_time, [16..19] modification_time
    if (version == 0 && copy.size >= 20) {
      for (i in 12..19) copy[i] = 0
      // mdhd: language field at offset [28..29]
      if (boxType == "mdhd" && copy.size >= 30) {
        copy[28] = ((MDHD_LANG_UND shr 8) and 0xFF).toByte()
        copy[29] = (MDHD_LANG_UND and 0xFF).toByte()
      }
    // Version 1 layout: [12..19] creation_time (64-bit), [20..27] modification_time (64-bit)
    } else if (version == 1 && copy.size >= 28) {
      for (i in 12..27) copy[i] = 0
      // mdhd v1: language field at offset [40..41]
      if (boxType == "mdhd" && copy.size >= 42) {
        copy[40] = ((MDHD_LANG_UND shr 8) and 0xFF).toByte()
        copy[41] = (MDHD_LANG_UND and 0xFF).toByte()
      }
    }
    return copy
  }

  // processContainer works on in-memory moov data where sizes fit in Int (moov is always small).
  // Metadata boxes are replaced with "free" boxes of the same size (not removed) to preserve
  // byte offsets. This is critical because stco/co64 tables contain absolute file offsets to
  // media samples in mdat — if moov changes size, those offsets become stale.
  // Max recursion depth matches the MP4 container hierarchy (moov > trak > mdia > minf > stbl etc.)
  val maxContainerDepth = 10

  fun processContainer(d: ByteArray, offset: Int, end: Int, depth: Int = 0): ByteArray {
    if (depth > maxContainerDepth) throw IllegalArgumentException("MP4 container nesting too deep (>$maxContainerDepth)")
    val containerOut = java.io.ByteArrayOutputStream(end - offset)
    var pos = offset
    while (pos + 8 <= end) {
      val boxSize = readBE32(d, pos)
      if (boxSize < 8 || pos.toLong() + boxSize.toLong() > end) break
      val boxType = String(d, pos + 4, 4, Charsets.US_ASCII)
      if (boxType in metadataBoxes) {
        // Replace metadata box with "free" of same size — preserves byte offsets
        containerOut.write(writeBE32(boxSize))
        containerOut.write("free".toByteArray(Charsets.US_ASCII))
        containerOut.write(ByteArray(boxSize - 8)) // zeroed padding
      } else if (boxType in containerBoxes) {
        // Recurse — children may contain metadata to replace
        val childrenBytes = processContainer(d, pos + 8, pos + boxSize, depth + 1)
        containerOut.write(writeBE32(8 + childrenBytes.size))
        containerOut.write(boxType.toByteArray(Charsets.US_ASCII))
        containerOut.write(childrenBytes)
      } else if (boxType in timestampBoxes) {
        containerOut.write(zeroTimestamps(d.copyOfRange(pos, pos + boxSize), boxType))
      } else {
        containerOut.write(d, pos, boxSize)
      }
      pos += boxSize
    }
    return containerOut.toByteArray()
  }

  val header = ByteArray(8)

  java.io.RandomAccessFile(input, "r").use { raf ->
    java.io.FileOutputStream(outputPath).use { fos ->
      val buf = ByteArray(COPY_BUF_SIZE)
      while (raf.filePointer + 8 <= raf.length()) {
        raf.readFully(header)
        // Convert to unsigned Long to handle sizes >= 2GB
        val rawSize = readBE32(header, 0).toLong() and 0xFFFFFFFFL
        val boxType = String(header, 4, 4, Charsets.US_ASCII)

        // Determine actual data size and header length
        val headerLen: Long
        val dataSize: Long
        when (rawSize) {
          0L -> {
            // size=0: box extends to end of file
            headerLen = 8L
            dataSize = raf.length() - raf.filePointer
          }
          1L -> {
            // size=1: 64-bit extended size in next 8 bytes
            val extBuf = ByteArray(8)
            raf.readFully(extBuf)
            val extSize = ((extBuf[0].toLong() and 0xFF) shl 56) or
              ((extBuf[1].toLong() and 0xFF) shl 48) or
              ((extBuf[2].toLong() and 0xFF) shl 40) or
              ((extBuf[3].toLong() and 0xFF) shl 32) or
              ((extBuf[4].toLong() and 0xFF) shl 24) or
              ((extBuf[5].toLong() and 0xFF) shl 16) or
              ((extBuf[6].toLong() and 0xFF) shl 8) or
              (extBuf[7].toLong() and 0xFF)
            headerLen = 16L
            dataSize = extSize - 16L
          }
          else -> {
            // Normal box: size includes the 8-byte header
            if (rawSize < 8L) break
            headerLen = 8L
            dataSize = rawSize - 8L
          }
        }

        if (boxType == "moov" || boxType in containerBoxes) {
          // Read moov into memory (typically < 100KB), process, write
          // For moov, dataSize must fit in Int (moov is always small)
          val totalBoxSize = (headerLen + dataSize).toInt()
          if (dataSize > MAX_HEADER_SIZE) throw IllegalArgumentException("MP4 moov box too large: $dataSize bytes")
          val moovData = ByteArray(totalBoxSize)
          // Copy original 8-byte header into moovData; processContainer reads children from offset 8
          System.arraycopy(header, 0, moovData, 0, 8)
          raf.readFully(moovData, 8, dataSize.toInt())
          val processed = processContainer(moovData, 8, totalBoxSize)
          fos.write(writeBE32(8 + processed.size))
          fos.write(boxType.toByteArray(Charsets.US_ASCII))
          fos.write(processed)
        } else if (boxType in metadataBoxes) {
          // Top-level metadata box (e.g. uuid with XMP/GPS from exiftool) —
          // replace with same-size "free" to preserve byte offsets for stco/co64
          fos.write(writeBE32((headerLen + dataSize).toInt()))
          fos.write("free".toByteArray(Charsets.US_ASCII))
          // Write zeroed payload in chunks (dataSize could be large)
          var left = dataSize + (headerLen - 8) // account for extended header bytes
          while (left > 0) {
            val toWrite = minOf(left, buf.size.toLong()).toInt()
            java.util.Arrays.fill(buf, 0, toWrite, 0)
            fos.write(buf, 0, toWrite)
            left -= toWrite
          }
          raf.seek(raf.filePointer + dataSize)
        } else {
          // Stream-copy large boxes (mdat, ftyp, etc.) without buffering
          // Write the original header bytes
          fos.write(header)
          if (rawSize == 1L) {
            // Write the 8-byte extended size (total box size as 64-bit BE)
            val totalSize = headerLen + dataSize
            val extBuf = ByteArray(8)
            var ts = totalSize
            for (i in 7 downTo 0) { extBuf[i] = (ts and 0xFF).toByte(); ts = ts shr 8 }
            fos.write(extBuf)
          }
          streamCopy(raf, fos, dataSize, buf)
        }
      }
    }
  }
}

/**
 * Strips metadata from AVI files using streaming I/O.
 * Only header chunks (hdrl, strl — typically < 50KB) loaded into memory.
 * movi (media data) and idx1 (index) are stream-copied.
 */
fun stripAviMetadata(inputPath: String, outputPath: String) {
  val input = java.io.File(inputPath)
  if (!input.exists()) throw IllegalArgumentException("Input file does not exist: $inputPath")

  // Chunks whose content should be zeroed (replaced with JUNK of same size).
  // We preserve sizes to maintain idx1 offset table integrity — AVI idx1 may
  // contain absolute file offsets, so shifting movi would corrupt seeking.
  val metadataChunks = setOf("_PMX", "strd")

  // Process small header chunks in memory — replace metadata with same-size JUNK
  val maxHeaderDepth = 10

  fun processHeaderChunks(d: ByteArray, start: Int, end: Int, output: java.io.ByteArrayOutputStream, depth: Int = 0) {
    if (depth > maxHeaderDepth) throw IllegalArgumentException("AVI header nesting too deep (>$maxHeaderDepth)")
    var p = start
    while (p + 8 <= end) {
      val fourCC = String(d, p, 4, Charsets.US_ASCII)
      val chunkSize = readLE32(d, p + 4)
      if (chunkSize < 0) break
      val paddedSize = if (chunkSize % 2 == 0) chunkSize.toLong() else chunkSize.toLong() + 1L
      val totalSizeLong = 8L + paddedSize
      if (totalSizeLong > Int.MAX_VALUE || p + totalSizeLong > end) break
      val totalSize = totalSizeLong.toInt()

      if (fourCC == "LIST" && p + 12 <= end) {
        val listType = String(d, p + 8, 4, Charsets.US_ASCII)
        if (listType == "strl" || listType == "hdrl") {
          val innerOut = java.io.ByteArrayOutputStream(totalSize)
          processHeaderChunks(d, p + 12, p + totalSize, innerOut, depth + 1)
          val innerBytes = innerOut.toByteArray()
          output.write("LIST".toByteArray(Charsets.US_ASCII))
          val sizeBytes = ByteArray(4); writeLE32(sizeBytes, 0, 4 + innerBytes.size)
          output.write(sizeBytes)
          output.write(listType.toByteArray(Charsets.US_ASCII))
          output.write(innerBytes)
        } else {
          output.write(d, p, totalSize)
        }
      } else if (fourCC in metadataChunks) {
        // Replace metadata chunk with JUNK of same size — preserves byte offsets
        output.write("JUNK".toByteArray(Charsets.US_ASCII))
        output.write(d, p + 4, 4) // keep original size field
        output.write(ByteArray(paddedSize.toInt())) // zeroed content
      } else {
        output.write(d, p, minOf(totalSize, end - p))
      }
      p += totalSize
    }
  }

  val header = ByteArray(12)
  java.io.RandomAccessFile(input, "r").use { raf ->
    // Read and validate RIFF header
    raf.readFully(header)
    val riff = String(header, 0, 4, Charsets.US_ASCII)
    val avi = String(header, 8, 4, Charsets.US_ASCII)
    if (riff != "RIFF" || avi != "AVI ") throw IllegalArgumentException("Invalid AVI header")

    // We need to track total output size to fix RIFF size at the end
    java.io.RandomAccessFile(outputPath, "rw").use { out ->
      out.write(header) // placeholder RIFF header

      val chunkHeader = ByteArray(8)
      val buf = ByteArray(COPY_BUF_SIZE)
      while (raf.filePointer + 8 <= raf.length()) {
        raf.readFully(chunkHeader)
        val fourCC = String(chunkHeader, 0, 4, Charsets.US_ASCII)
        val chunkSize = readLE32(chunkHeader, 4)
        if (chunkSize < 0) break
        // Use Long to avoid overflow for odd chunkSize near Int.MAX_VALUE
        val paddedSize = if (chunkSize % 2 == 0) chunkSize.toLong() else chunkSize.toLong() + 1L

        if (fourCC == "LIST" && raf.filePointer + 4 <= raf.length()) {
          val listTypeBytes = ByteArray(4)
          raf.readFully(listTypeBytes)
          val listType = String(listTypeBytes, Charsets.US_ASCII)

          if (listType == "INFO") {
            // Replace INFO list content with zeros (JUNK) — preserves file layout for idx1 offsets
            if (paddedSize > MAX_HEADER_SIZE) throw IllegalArgumentException("AVI INFO list too large: $paddedSize bytes")
            out.write("JUNK".toByteArray(Charsets.US_ASCII))
            out.write(chunkHeader, 4, 4) // keep original size
            raf.seek(raf.filePointer + paddedSize - 4L) // skip original content
            out.write(ByteArray(paddedSize.toInt())) // zeroed padding (includes the 4 list-type bytes)
          } else if (listType == "movi") {
            // Stream-copy movi (bulk media data)
            out.write(chunkHeader)
            out.write(listTypeBytes)
            streamCopy(raf, out, paddedSize - 4L, buf)
          } else if (listType == "hdrl" || listType == "strl") {
            // Read header list into memory (small), process, write
            if (paddedSize - 4L > MAX_HEADER_SIZE) throw IllegalArgumentException("AVI header list too large: ${paddedSize - 4L} bytes")
            val listData = ByteArray((paddedSize - 4L).toInt())
            raf.readFully(listData)
            val processed = java.io.ByteArrayOutputStream(listData.size)
            processHeaderChunks(listData, 0, listData.size, processed)
            val result = processed.toByteArray()
            out.write("LIST".toByteArray(Charsets.US_ASCII))
            val sizeBytes = ByteArray(4); writeLE32(sizeBytes, 0, 4 + result.size)
            out.write(sizeBytes)
            out.write(listTypeBytes)
            out.write(result)
          } else {
            // Other LIST types -- stream-copy
            out.write(chunkHeader)
            out.write(listTypeBytes)
            streamCopy(raf, out, paddedSize - 4L, buf)
          }
        } else if (fourCC in metadataChunks) {
          // Replace metadata chunk with JUNK of same size — preserves byte offsets
          if (paddedSize > MAX_HEADER_SIZE) throw IllegalArgumentException("AVI metadata chunk too large: $paddedSize bytes")
          out.write("JUNK".toByteArray(Charsets.US_ASCII))
          out.write(chunkHeader, 4, 4) // keep original size
          raf.seek(raf.filePointer + paddedSize)
          out.write(ByteArray(paddedSize.toInt())) // zeroed content
        } else {
          // Stream-copy (idx1, etc.)
          out.write(chunkHeader)
          streamCopy(raf, out, paddedSize, buf)
        }
      }

      // Fix RIFF size
      val fileSize = out.length()
      out.seek(4)
      val sizeBytes = ByteArray(4); writeLE32(sizeBytes, 0, (fileSize - 8).toInt())
      out.write(sizeBytes)
    }
  }
}

/**
 * Strips metadata from MKV/WebM files using streaming I/O.
 * Only header elements (SeekHead, Info, Tracks, Tags — typically < 50KB total)
 * are loaded into memory. Clusters (bulk media data) are stream-copied.
 * Rebuilds SeekHead with corrected offsets after stripping.
 */
fun stripMkvMetadata(inputPath: String, outputPath: String) {
  val input = java.io.File(inputPath)
  if (!input.exists()) throw IllegalArgumentException("Input file does not exist: $inputPath")

  val SEGMENT_ID = byteArrayOf(0x18.toByte(), 0x53.toByte(), 0x80.toByte(), 0x67.toByte())
  val SEGMENT_INFO_ID = byteArrayOf(0x15.toByte(), 0x49.toByte(), 0xA9.toByte(), 0x66.toByte())
  val TAGS_ID = byteArrayOf(0x12.toByte(), 0x54.toByte(), 0xC3.toByte(), 0x67.toByte())
  val SEEK_HEAD_ID = byteArrayOf(0x11.toByte(), 0x4D.toByte(), 0x9B.toByte(), 0x74.toByte())
  val VOID_ID = byteArrayOf(0xEC.toByte())
  val CLUSTER_ID = byteArrayOf(0x1F.toByte(), 0x43.toByte(), 0xB6.toByte(), 0x75.toByte())
  val CUES_ID = byteArrayOf(0x1C.toByte(), 0x53.toByte(), 0xBB.toByte(), 0x6B.toByte())

  // Cues are also removed because they contain CueClusterPosition offsets that become
  // stale after Tags/Void removal shifts Cluster positions. Players rebuild Cues on open.
  val removeElements = listOf(TAGS_ID, VOID_ID, CUES_ID)

  val metadataInfoElements = listOf(
    byteArrayOf(0x44.toByte(), 0x61.toByte()), // DateUTC
    byteArrayOf(0x7B.toByte(), 0xA9.toByte()), // Title
    byteArrayOf(0x4D.toByte(), 0x80.toByte()), // MuxingApp
    byteArrayOf(0x57.toByte(), 0x41.toByte()), // WritingApp
    byteArrayOf(0x73.toByte(), 0xA4.toByte()), // SegmentUUID
    byteArrayOf(0x73.toByte(), 0x84.toByte()), // SegmentFilename
    byteArrayOf(0x3C.toByte(), 0xB9.toByte(), 0x23.toByte()), // PrevUID
    byteArrayOf(0x3C.toByte(), 0x83.toByte(), 0xAB.toByte()), // PrevFilename
    byteArrayOf(0x3E.toByte(), 0xB9.toByte(), 0x23.toByte()), // NextUID
    byteArrayOf(0x3E.toByte(), 0x83.toByte(), 0xBB.toByte()), // NextFilename
    byteArrayOf(0x44.toByte(), 0x44.toByte())  // SegmentFamily
  )

  // Helper: read VINT element ID from RandomAccessFile, returns (byteLen, idBytes)
  fun rafReadVINTId(raf: java.io.RandomAccessFile): Pair<Int, ByteArray> {
    val first = raf.read()
    if (first < 0) return Pair(0, ByteArray(0))
    val len = vintByteLength(first)
    val idBytes = ByteArray(len)
    idBytes[0] = first.toByte()
    if (len > 1) raf.readFully(idBytes, 1, len - 1)
    return Pair(len, idBytes)
  }

  fun rafReadVINTSize(raf: java.io.RandomAccessFile): Pair<Int, Long> {
    val first = raf.read()
    if (first < 0) return Pair(0, 0)
    val len = vintByteLength(first)
    val mask = 0xFF shr len
    var value = (first and mask).toLong()
    for (i in 1 until len) {
      val b = raf.read()
      if (b < 0) throw java.io.EOFException("Truncated EBML VINT size at byte $i of $len")
      value = (value shl 8) or (b.toLong() and 0xFF)
    }
    // Check all-ones (unknown size)
    val maxVal = (1L shl (7 * len)) - 1
    if (value == maxVal) value = -1
    return Pair(len, value)
  }

  fun idMatchesAny(id: ByteArray, targets: List<ByteArray>): Boolean =
    targets.any { it.contentEquals(id) }

  data class HeaderChild(val id: ByteArray, val bytes: ByteArray)
  data class BulkChild(val fileOffset: Long, val totalSize: Long, val id: ByteArray)

  java.io.RandomAccessFile(input, "r").use { raf ->
    java.io.RandomAccessFile(outputPath, "rw").use { out ->

      // Read and copy EBML header
      val (_, ebmlId) = rafReadVINTId(raf)
      val (ebmlSzLen, ebmlSize) = rafReadVINTSize(raf)
      val ebmlTotalSize = ebmlId.size + ebmlSzLen + ebmlSize.toInt()
      raf.seek(0)
      val ebmlBytes = ByteArray(ebmlTotalSize)
      raf.readFully(ebmlBytes)
      out.write(ebmlBytes)

      // Read Segment header
      val segStart = raf.filePointer
      val (_, segId) = rafReadVINTId(raf)
      if (!segId.contentEquals(SEGMENT_ID)) {
        // Not a Segment -- copy rest of file
        raf.seek(segStart)
        val buf = ByteArray(COPY_BUF_SIZE)
        while (true) { val n = raf.read(buf); if (n <= 0) break; out.write(buf, 0, n) }
        return@use
      }
      val (segSzLen, segSize) = rafReadVINTSize(raf)
      val segDataStart = raf.filePointer
      val segDataEnd = if (segSize < 0) raf.length() else segDataStart + segSize

      // Scan Segment children
      val headerChildren = mutableListOf<HeaderChild>()
      val bulkChildren = mutableListOf<BulkChild>()

      while (raf.filePointer + 2 <= segDataEnd) {
        val childStart = raf.filePointer
        val (cidLen, cid) = rafReadVINTId(raf)
        if (cidLen == 0) break
        val (cszLen, csz) = rafReadVINTSize(raf)
        val chdrLen = cidLen + cszLen
        val dataSize = if (csz < 0) (segDataEnd - raf.filePointer) else csz

        when {
          cid.contentEquals(SEEK_HEAD_ID) -> {
            // Skip -- will rebuild
            raf.seek(raf.filePointer + dataSize)
          }
          idMatchesAny(cid, removeElements) -> {
            // Skip Tags, Void
            raf.seek(raf.filePointer + dataSize)
          }
          cid.contentEquals(SEGMENT_INFO_ID) -> {
            // Read small Info element into memory, process
            if (dataSize > MAX_HEADER_SIZE) throw IllegalArgumentException("MKV SegmentInfo too large: $dataSize bytes")
            val infoData = ByteArray(dataSize.toInt())
            raf.readFully(infoData)
            // Rebuild with header + processed children
            raf.seek(childStart)
            val fullHeader = ByteArray(chdrLen)
            raf.readFully(fullHeader)
            raf.seek(childStart + chdrLen + dataSize) // skip past
            val infoOut = java.io.ByteArrayOutputStream(chdrLen + infoData.size)
            infoOut.write(fullHeader)
            processSegmentInfo(infoData, 0, infoData.size, infoOut, metadataInfoElements)
            headerChildren.add(HeaderChild(cid, infoOut.toByteArray()))
          }
          cid.contentEquals(CLUSTER_ID) -> {
            // Bulk -- record file offset for stream copy
            bulkChildren.add(BulkChild(childStart, chdrLen + dataSize, cid))
            raf.seek(raf.filePointer + dataSize)
          }
          else -> {
            // Other small header elements (Tracks, etc.) -- read into memory
            if (dataSize > MAX_HEADER_SIZE) throw IllegalArgumentException("MKV header element too large: $dataSize bytes")
            raf.seek(childStart)
            val elemBytes = ByteArray((chdrLen + dataSize).toInt())
            raf.readFully(elemBytes)
            headerChildren.add(HeaderChild(cid, elemBytes))
          }
        }
      }

      // Rebuild SeekHead
      fun buildSeekEntry(targetId: ByteArray, position: Long): ByteArray {
        val entry = java.io.ByteArrayOutputStream()
        entry.write(MKV_SEEK_ID)
        entry.write(ebmlEncodeSize(targetId.size.toLong()))
        entry.write(targetId)
        val posBytes = ebmlEncodeUInt(position)
        entry.write(MKV_SEEK_POSITION_ID)
        entry.write(ebmlEncodeSize(posBytes.size.toLong()))
        entry.write(posBytes)
        val seekData = entry.toByteArray()
        val seek = java.io.ByteArrayOutputStream()
        seek.write(MKV_SEEK_ENTRY_ID)
        seek.write(ebmlEncodeSize(seekData.size.toLong()))
        seek.write(seekData)
        return seek.toByteArray()
      }

      data class SeekTarget(val id: ByteArray, val size: Long)
      // SeekHead should have one entry per element TYPE, not per instance.
      // Use distinctBy to keep only the first occurrence of each element ID.
      val seenIds = mutableSetOf<List<Byte>>()
      val allElements = (headerChildren.map { SeekTarget(it.id, it.bytes.size.toLong()) } +
                        bulkChildren.map { SeekTarget(it.id, it.totalSize) })
                        .filter { seenIds.add(it.id.toList()) }

      // Pre-compute SeekHead size with dummy offsets
      val dummyEntries = allElements.map { buildSeekEntry(it.id, 0) }
      val dummySize = dummyEntries.sumOf { it.size }
      var seekHeadOverhead = SEEK_HEAD_ID.size + ebmlEncodeSize(dummySize.toLong()).size + dummySize

      // Build SeekHead bytes for a given overhead estimate
      fun buildSeekHeadBytes(overhead: Int): ByteArray {
        var offset = overhead.toLong()
        val entries = java.io.ByteArrayOutputStream()
        for (elem in allElements) {
          entries.write(buildSeekEntry(elem.id, offset))
          offset += elem.size
        }
        val entriesData = entries.toByteArray()
        val result = java.io.ByteArrayOutputStream()
        result.write(SEEK_HEAD_ID)
        result.write(ebmlEncodeSize(entriesData.size.toLong()))
        result.write(entriesData)
        return result.toByteArray()
      }

      // Write Segment header + SeekHead + header children + stream-copy bulk children
      fun writeSegment(seekHeadBytes: ByteArray) {
        val totalSegContent = seekHeadBytes.size.toLong() +
          headerChildren.sumOf { it.bytes.size.toLong() } +
          bulkChildren.sumOf { it.totalSize }
        out.write(SEGMENT_ID)
        if (segSize < 0L) {
          // Original had unknown size — preserve it (write all-ones VINT of same byte length)
          val unknownSizeVint = ByteArray(segSzLen) { 0xFF.toByte() }
          out.write(unknownSizeVint)
        } else {
          out.write(ebmlEncodeSizeFixedLen(totalSegContent, segSzLen))
        }
        out.write(seekHeadBytes)
        for (child in headerChildren) out.write(child.bytes)
        val copyBuf = ByteArray(COPY_BUF_SIZE)
        for (bulk in bulkChildren) {
          raf.seek(bulk.fileOffset)
          streamCopy(raf, out, bulk.totalSize, copyBuf)
        }
        out.setLength(out.filePointer)
      }

      // Iteratively converge on correct SeekHead size (up to 3 passes)
      for (iteration in 0 until 3) {
        val seekHeadBytes = buildSeekHeadBytes(seekHeadOverhead)
        if (seekHeadBytes.size == seekHeadOverhead) {
          writeSegment(seekHeadBytes)
          return@use
        }
        seekHeadOverhead = seekHeadBytes.size
      }

      // Fallback: use last computed size (should not happen — 3 iterations is more than enough)
      writeSegment(buildSeekHeadBytes(seekHeadOverhead))
    }
  }
}

/** Read a VINT (variable-length integer) from EBML data. Returns (byteLength, value). */
private fun readVINT(data: ByteArray, offset: Int): Pair<Int, Long> {
  if (offset >= data.size) return Pair(1, 0)
  val first = data[offset].toInt() and 0xFF
  val len = vintByteLength(first)
  if (offset + len > data.size) return Pair(len, 0)
  // For size VINTs, mask off the length bits from first byte
  val mask = 0xFF shr len
  var value = (first and mask).toLong()
  for (i in 1 until len) {
    value = (value shl 8) or (data[offset + i].toLong() and 0xFF)
  }
  return Pair(len, value)
}

/** Read a VINT as an element ID (no masking — ID uses all bits including length marker). */
private fun readVINTId(data: ByteArray, offset: Int): Pair<Int, Long> {
  if (offset >= data.size) return Pair(1, 0)
  val first = data[offset].toInt() and 0xFF
  val len = vintByteLength(first)
  if (offset + len > data.size) return Pair(len, 0)
  var value = first.toLong()
  for (i in 1 until len) {
    value = (value shl 8) or (data[offset + i].toLong() and 0xFF)
  }
  return Pair(len, value)
}

/** Encode a value as EBML VINT size using minimum bytes. */
private fun ebmlEncodeSize(value: Long): ByteArray {
  val len = when {
    value < 0x7F -> 1            // 2^7 - 1
    value < 0x3FFF -> 2          // 2^14 - 1
    value < 0x1FFFFF -> 3        // 2^21 - 1
    value < 0x0FFFFFFF -> 4      // 2^28 - 1
    value < 0x07FFFFFFFF -> 5
    value < 0x03FFFFFFFFFF -> 6
    value < 0x01FFFFFFFFFFFF -> 7
    else -> 8
  }
  return ebmlEncodeSizeFixedLen(value, len)
}

/** Encode a value as EBML VINT size using exactly [byteLen] bytes. */
private fun ebmlEncodeSizeFixedLen(value: Long, byteLen: Int): ByteArray {
  val marker = 1L shl (8 * byteLen - byteLen)
  var encoded = value or marker
  val result = ByteArray(byteLen)
  for (i in byteLen - 1 downTo 0) {
    result[i] = (encoded and 0xFF).toByte()
    encoded = encoded shr 8
  }
  return result
}

/** Encode a non-negative integer as minimal big-endian bytes (for EBML uint elements). */
private fun ebmlEncodeUInt(value: Long): ByteArray {
  if (value == 0L) return byteArrayOf(0)
  var v = value
  var len = 0
  var tmp = v
  while (tmp > 0) { len++; tmp = tmp shr 8 }
  val result = ByteArray(len)
  for (i in len - 1 downTo 0) {
    result[i] = (v and 0xFF).toByte()
    v = v shr 8
  }
  return result
}

private fun matchesId(data: ByteArray, offset: Int, id: ByteArray): Boolean {
  if (offset + id.size > data.size) return false
  for (i in id.indices) {
    if (data[offset + i] != id[i]) return false
  }
  return true
}

private fun processSegmentInfo(
  data: ByteArray, start: Int, end: Int, out: java.io.ByteArrayOutputStream,
  metadataElements: List<ByteArray>
) {
  var pos = start
  while (pos + 2 <= end) {
    val (idLen, _) = readVINTId(data, pos)
    if (pos + idLen >= end) break
    val (sizeLen, elementSize) = readVINT(data, pos + idLen)
    val headerLen = idLen + sizeLen
    if (elementSize < 0 || pos + headerLen + elementSize.toInt() > end) {
      out.write(data, pos, end - pos)
      return
    }
    val totalSize = headerLen + elementSize.toInt()

    if (metadataElements.any { matchesId(data, pos, it) }) {
      // Write header but zero out the data payload
      out.write(data, pos, headerLen)
      out.write(ByteArray(elementSize.toInt())) // zeroed payload
    } else {
      out.write(data, pos, totalSize)
    }
    pos += totalSize
  }
}

private val VIDEO_EXTENSIONS = setOf("mp4", "mov", "m4v", "3gp", "mkv", "webm", "avi")

fun isVideoFile(fileName: String): Boolean {
  val ext = fileName.substringAfterLast('.', "").lowercase()
  return ext in VIDEO_EXTENSIONS
}

/** Route to the correct stripper based on file extension.
 *  [fileName] is the original filename (for extension detection).
 *  [inputPath] and [outputPath] may be temp file paths without meaningful extensions. */
fun stripVideoMetadataByExtension(fileName: String, inputPath: String, outputPath: String) {
  val ext = fileName.substringAfterLast('.', "").lowercase()
  when (ext) {
    "mp4", "mov", "m4v", "3gp" -> stripVideoMetadata(inputPath, outputPath)
    "mkv", "webm" -> stripMkvMetadata(inputPath, outputPath)
    "avi" -> stripAviMetadata(inputPath, outputPath)
    else -> throw IllegalArgumentException("Unsupported video format: $ext")
  }
}
