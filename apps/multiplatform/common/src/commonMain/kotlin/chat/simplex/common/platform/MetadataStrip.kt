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

private fun readBE64(d: ByteArray, off: Int): Long {
  var v = 0L
  for (j in 0 until 8) v = (v shl 8) or (d[off + j].toLong() and 0xFF)
  return v
}

private fun writeBE64(value: Long): ByteArray {
  val buf = ByteArray(8)
  var v = value
  for (i in 7 downTo 0) { buf[i] = (v and 0xFF).toByte(); v = v shr 8 }
  return buf
}

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

/** Write [count] zero bytes to [out] using [buf] (cleared once) to avoid large heap allocations. */
private fun writeZeros(out: java.io.RandomAccessFile, count: Long, buf: ByteArray) {
  java.util.Arrays.fill(buf, 0)
  var left = count
  while (left > 0) {
    val toWrite = minOf(left, buf.size.toLong()).toInt()
    out.write(buf, 0, toWrite)
    left -= toWrite
  }
}

/** Write [count] zero bytes to [out] using [buf] (cleared once) to avoid large heap allocations. */
private fun writeZeros(out: java.io.OutputStream, count: Long, buf: ByteArray) {
  java.util.Arrays.fill(buf, 0)
  var left = count
  while (left > 0) {
    val toWrite = minOf(left, buf.size.toLong()).toInt()
    out.write(buf, 0, toWrite)
    left -= toWrite
  }
}

/**
 * Stream-copy [remaining] bytes from [raf] to [out], zeroing any bytes that fall inside [ranges]
 * (file-absolute, inclusive, MUST be sorted by .first ascending). Used to scrub timed-metadata
 * sample bytes from mdat while preserving everything else (video, audio samples).
 * Forward cursor avoids O(buffers × ranges) DoS when attacker crafts millions of ranges.
 */
private fun streamCopyZeroingRanges(
  raf: java.io.RandomAccessFile, out: java.io.OutputStream,
  remaining: Long, buf: ByteArray, ranges: List<LongRange>
) {
  var left = remaining
  // Forward cursor: ranges sorted by .first, monotonically advance as filePointer moves.
  // Skips ranges that end before the current read window.
  var rangeCursor = 0
  while (left > 0) {
    val readStart = raf.filePointer
    val toRead = minOf(left, buf.size.toLong()).toInt()
    raf.readFully(buf, 0, toRead)
    val readEnd = readStart + toRead - 1
    // Advance cursor past ranges whose end is before this read window.
    while (rangeCursor < ranges.size && ranges[rangeCursor].last < readStart) rangeCursor++
    // Apply zeroing for ranges that overlap this window; stop at first range past window.
    var i = rangeCursor
    while (i < ranges.size) {
      val range = ranges[i]
      if (range.first > readEnd) break
      val zStart = maxOf(readStart, range.first)
      val zEnd = minOf(readEnd, range.last)
      if (zStart <= zEnd) {
        val bufOff = (zStart - readStart).toInt()
        val bufLen = (zEnd - zStart + 1).toInt()
        java.util.Arrays.fill(buf, bufOff, bufOff + bufLen, 0)
      }
      i++
    }
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
    if (13 + gctSize > data.size) throw IllegalArgumentException("Invalid GIF: GCT (size $gctSize) extends past file (size ${data.size})")
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

/** Walk GIF sub-blocks at [start], optionally copying each (size byte + data) to [out].
 *  Returns the position immediately after the 0x00 terminator, or end-of-data if truncated. */
private fun walkSubBlocks(data: ByteArray, start: Int, out: java.io.ByteArrayOutputStream?): Int {
  var pos = start
  while (pos < data.size) {
    val subBlockSize = data[pos].toInt() and 0xFF
    out?.write(subBlockSize)
    if (subBlockSize == 0) return pos + 1
    out?.write(data, pos + 1, subBlockSize)
    pos += 1 + subBlockSize
  }
  return pos
}

/** Copy an extension block (introducer + label + sub-blocks) to output. Returns new position. */
private fun copyExtensionBlock(data: ByteArray, start: Int, out: java.io.ByteArrayOutputStream): Int {
  out.write(data[start].toInt() and 0xFF)     // introducer (0x21)
  out.write(data[start + 1].toInt() and 0xFF) // label
  return walkSubBlocks(data, start + 2, out)
}

/** Skip an extension block. Returns new position after the block. */
private fun skipExtensionBlock(data: ByteArray, start: Int): Int =
  walkSubBlocks(data, start + 2, out = null)

/** Copy an Image Descriptor block + optional Local Color Table + image data. Returns new position. */
private fun copyImageDescriptor(data: ByteArray, start: Int, out: java.io.ByteArrayOutputStream): Int {
  // Image Descriptor is 10 bytes (0x2C + 9 bytes)
  if (start + 10 > data.size) throw IllegalArgumentException("Invalid GIF: truncated Image Descriptor at offset $start")
  out.write(data, start, 10)
  var pos = start + 10

  // Check for Local Color Table
  val packed = data[start + 9].toInt() and 0xFF
  val hasLCT = (packed and 0x80) != 0
  if (hasLCT) {
    val lctSize = 3 * (1 shl ((packed and 0x07) + 1))
    if (pos + lctSize > data.size) throw IllegalArgumentException("Invalid GIF: LCT (size $lctSize) extends past file at offset $pos")
    out.write(data, pos, lctSize)
    pos += lctSize
  }

  // LZW Minimum Code Size
  if (pos >= data.size) throw IllegalArgumentException("Invalid GIF: truncated before LZW min code size at offset $pos")
  out.write(data[pos].toInt() and 0xFF)
  pos++

  return walkSubBlocks(data, pos, out)
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

    // Fail closed: a chunk extending past EOF means either truncated input or malformed size.
    // Returning early would skip remaining (possibly metadata) chunks → privacy leak.
    if (pos + totalChunkSize > data.size) throw IllegalArgumentException("Invalid WebP: chunk extends past file at offset $pos")

    if (fourCC !in allowedChunks) {
      strippedAny = true
      pos += totalChunkSize.toInt()
      continue
    }

    if (fourCC == "VP8X") {
      // Per spec VP8X payload is fixed at 10 bytes. If it's smaller, our flags-byte write
      // at the post-loop fixup would land on the next chunk's FourCC and corrupt structure.
      if (chunkSize < 10) throw IllegalArgumentException("Invalid WebP: VP8X chunk too small ($chunkSize bytes, need 10)")
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

  val metadataBoxes = setOf("udta", "meta", "uuid", "cprt")
  val containerBoxes = setOf("moov", "trak", "mdia", "minf", "stbl", "dinf", "edts")
  val timestampBoxes = setOf("mvhd", "tkhd", "mdhd")
  // Timed metadata tracks (GPS telemetry, gyroscope, etc.) — identified by hdlr handler_type
  val timedMetadataHandlers = setOf("meta", "camm", "gpmd", "mett", "metx")

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

  // Check if a trak box contains a timed metadata track (GPS telemetry, gyroscope, etc.)
  // by scanning for mdia > hdlr and checking handler_type.
  // These tracks (Google CAMM, GoPro GPMF, generic meta/mett/metx) store frame-by-frame
  // location data as samples in mdat — a full GPS breadcrumb trail.
  fun isTimedMetadataTrack(d: ByteArray, trakChildStart: Int, trakEnd: Int): Boolean {
    var pos = trakChildStart
    while (pos + 8 <= trakEnd) {
      val size = readBE32(d, pos)
      if (size < 8 || pos.toLong() + size.toLong() > trakEnd) break
      val type = String(d, pos + 4, 4, Charsets.US_ASCII)
      if (type == "mdia") {
        // Scan mdia children for hdlr
        var mpos = pos + 8
        val mend = pos + size
        while (mpos + 8 <= mend) {
          val msize = readBE32(d, mpos)
          if (msize < 8 || mpos.toLong() + msize.toLong() > mend) break
          val mtype = String(d, mpos + 4, 4, Charsets.US_ASCII)
          // hdlr box: [8] version/flags, [12..15] pre_defined, [16..19] handler_type
          if (mtype == "hdlr" && msize >= 24) {
            val handlerType = String(d, mpos + 16, 4, Charsets.US_ASCII)
            return handlerType in timedMetadataHandlers
          }
          mpos += msize
        }
      }
      pos += size
    }
    return false
  }

  // Find a direct child box of [type] within [start, end) of [d]. Returns IntRange or null.
  fun findChildBox(d: ByteArray, start: Int, end: Int, type: String): IntRange? {
    var p = start
    while (p + 8 <= end) {
      val sz = readBE32(d, p)
      if (sz < 8 || p.toLong() + sz.toLong() > end) return null
      val t = String(d, p + 4, 4, Charsets.US_ASCII)
      if (t == type) return p until (p + sz)
      p += sz
    }
    return null
  }

  // Fail closed if the timed-metadata trak is missing a required child box: returning emptyList
  // would silently leave raw GPS/telemetry bytes in mdat unzeroed. [path] is the dotted parent
  // chain (e.g. "mdia>minf") for debugging; defaults to [type] for direct children.
  fun requireChildBox(d: ByteArray, start: Int, end: Int, type: String, path: String = type): IntRange =
    findChildBox(d, start, end, type)
      ?: throw IllegalArgumentException("Timed-metadata trak missing $path")

  // Extract the file-absolute byte ranges of a timed metadata track's samples in mdat,
  // by parsing stbl tables (stco/co64 for chunk offsets, stsc for samples-per-chunk, stsz for sizes).
  // Used to zero out raw GPS/telemetry bytes so they can't be recovered by forensic tools.
  // Fails closed (throws) on malformed stbl — caller has already identified this trak as
  // timed-metadata via hdlr, so returning emptyList would silently leave the raw bytes
  // in mdat unzeroed.
  fun extractTimedMetadataMdatRanges(d: ByteArray, trakStart: Int, trakEnd: Int): List<LongRange> {
    val mdia = requireChildBox(d, trakStart, trakEnd, "mdia")
    val minf = requireChildBox(d, mdia.first + 8, mdia.last + 1, "minf", "mdia>minf")
    val stbl = requireChildBox(d, minf.first + 8, minf.last + 1, "stbl", "mdia>minf>stbl")
    val stblStart = stbl.first + 8
    val stblEnd = stbl.last + 1

    // Chunk offsets: stco (32-bit) or co64 (64-bit). Use LongArray to avoid boxing
    // ~24 bytes/element × 12.5M entries (worst case under MAX_HEADER_SIZE).
    val stco = findChildBox(d, stblStart, stblEnd, "stco")
    val co64 = findChildBox(d, stblStart, stblEnd, "co64")
    val chunkOffsets: LongArray = when {
      stco != null -> {
        if (stco.last + 1 - stco.first < 16) throw IllegalArgumentException("Timed-metadata stco too short")
        val count = readBE32(d, stco.first + 12)
        if (count < 0 || stco.first + 16 + count.toLong() * 4 > stco.last + 1) throw IllegalArgumentException("Timed-metadata stco count invalid: $count")
        LongArray(count) { i -> readBE32(d, stco.first + 16 + i * 4).toLong() and 0xFFFFFFFFL }
      }
      co64 != null -> {
        if (co64.last + 1 - co64.first < 16) throw IllegalArgumentException("Timed-metadata co64 too short")
        val count = readBE32(d, co64.first + 12)
        if (count < 0 || co64.first + 16 + count.toLong() * 8 > co64.last + 1) throw IllegalArgumentException("Timed-metadata co64 count invalid: $count")
        LongArray(count) { i ->
          val v = readBE64(d, co64.first + 16 + i * 8)
          // File offsets are unsigned per spec; negative Long means top-bit-set → spec-invalid.
          if (v < 0) throw IllegalArgumentException("MP4 co64 entry has invalid offset: $v")
          v
        }
      }
      else -> throw IllegalArgumentException("Timed-metadata trak missing stco/co64")
    }
    if (chunkOffsets.isEmpty()) return emptyList()

    val stsc = requireChildBox(d, stblStart, stblEnd, "stsc")
    val stsz = requireChildBox(d, stblStart, stblEnd, "stsz")
    if (stsc.last + 1 - stsc.first < 16) throw IllegalArgumentException("Timed-metadata stsc too short")
    if (stsz.last + 1 - stsz.first < 20) throw IllegalArgumentException("Timed-metadata stsz too short")

    // stsc entries: first_chunk(4) + samples_per_chunk(4) + sample_description_index(4) = 12 bytes
    val stscCount = readBE32(d, stsc.first + 12)
    if (stscCount <= 0 || stsc.first + 16 + stscCount.toLong() * 12 > stsc.last + 1) throw IllegalArgumentException("Timed-metadata stsc count invalid: $stscCount")
    val stscFirstChunk = IntArray(stscCount)
    val stscSamplesPerChunk = IntArray(stscCount)
    for (i in 0 until stscCount) {
      val off = stsc.first + 16 + i * 12
      stscFirstChunk[i] = readBE32(d, off)
      stscSamplesPerChunk[i] = readBE32(d, off + 4)
    }

    val fixedSampleSize = readBE32(d, stsz.first + 12)
    val sampleCount = readBE32(d, stsz.first + 16)
    if (sampleCount < 0) throw IllegalArgumentException("Timed-metadata stsz sample_count negative")
    if (fixedSampleSize == 0 && stsz.first + 20 + sampleCount.toLong() * 4 > stsz.last + 1) {
      throw IllegalArgumentException("Timed-metadata stsz sample table overflows box")
    }

    val ranges = ArrayList<LongRange>(chunkOffsets.size)
    // Use Long to prevent overflow with attacker-controlled samples_per_chunk values.
    var sampleIdx = 0L
    // Forward-only cursor through stsc — chunkNum is monotonically increasing,
    // so we never need to rescan from the beginning. Prevents O(chunks × stscCount) DoS.
    var stscIdx = 0
    for (chunkIdx in chunkOffsets.indices) {
      val chunkNum = chunkIdx + 1 // 1-based per spec
      // Advance stscIdx as long as the next entry's first_chunk is still ≤ chunkNum.
      while (stscIdx + 1 < stscCount && stscFirstChunk[stscIdx + 1] <= chunkNum) {
        stscIdx++
      }
      val spc = stscSamplesPerChunk[stscIdx]
      // Negative Int (high bit set) would silently `continue` here, dropping the chunk's range
      // from zeroing. Attacker can set samples_per_chunk to 0x80000000 to bypass zeroing of
      // this chunk's GPS/telemetry bytes in mdat. Fail closed.
      if (spc < 0) throw IllegalArgumentException("MP4 stsc samples_per_chunk has high bit set: 0x${spc.toString(16)}")
      if (spc == 0) continue
      val remaining = (sampleCount.toLong() and 0xFFFFFFFFL) - sampleIdx
      if (remaining <= 0) break
      // spc must fit within remaining samples — otherwise stsc/stsz are inconsistent.
      // Failing closed prevents an attacker from setting spc[0]=huge to bypass zeroing
      // of chunks 1..N (their offsets stay in chunkOffsets but loop breaks early).
      if (spc.toLong() > remaining) {
        throw IllegalArgumentException("MP4 stsc samples_per_chunk ($spc) exceeds remaining samples ($remaining) at chunk $chunkNum")
      }
      val takeCount = spc
      val chunkBytes: Long = if (fixedSampleSize != 0) {
        takeCount.toLong() * (fixedSampleSize.toLong() and 0xFFFFFFFFL)
      } else {
        var sum = 0L
        for (s in 0 until takeCount) {
          sum += readBE32(d, stsz.first + 20 + ((sampleIdx + s).toInt()) * 4).toLong() and 0xFFFFFFFFL
        }
        sum
      }
      if (chunkBytes > 0) {
        val start = chunkOffsets[chunkIdx]
        ranges.add(start until (start + chunkBytes))
      }
      sampleIdx += spc.toLong()
      if (sampleIdx >= (sampleCount.toLong() and 0xFFFFFFFFL)) break
    }
    return ranges
  }

  // processContainer works on in-memory moov data where sizes fit in Int (moov is always small).
  // Metadata boxes are replaced with "free" boxes of the same size (not removed) to preserve
  // byte offsets. This is critical because stco/co64 tables contain absolute file offsets to
  // media samples in mdat — if moov changes size, those offsets become stale.
  // Max recursion depth matches the MP4 container hierarchy (moov > trak > mdia > minf > stbl etc.)
  val maxContainerDepth = 10

  // [zeroBuf] is a reusable buffer for writing zero padding — avoids allocating ByteArray(boxSize - 8)
  // for large metadata boxes (e.g. multi-MB GoPro telemetry traks).
  fun processContainer(
    d: ByteArray, offset: Int, end: Int,
    zeroBuf: ByteArray,
    depth: Int = 0
  ): ByteArray {
    if (depth > maxContainerDepth) throw IllegalArgumentException("MP4 container nesting too deep (>$maxContainerDepth)")
    val containerOut = java.io.ByteArrayOutputStream(end - offset)
    var pos = offset
    while (pos + 8 <= end) {
      val boxSize = readBE32(d, pos)
      // Fail closed: malformed box invalidates size preservation invariant for stco/co64.
      // Silently breaking would produce a smaller moov, shifting mdat — broken playback.
      if (boxSize < 8 || pos.toLong() + boxSize.toLong() > end) {
        throw IllegalArgumentException("Malformed MP4 box at offset $pos: size=$boxSize")
      }
      val boxType = String(d, pos + 4, 4, Charsets.US_ASCII)
      if (boxType in metadataBoxes ||
          (boxType == "trak" && isTimedMetadataTrack(d, pos + 8, pos + boxSize))) {
        // Replace metadata box (or timed-metadata trak) with "free" of same size — preserves byte offsets.
        // For timed metadata traks, the raw GPS/telemetry sample bytes in mdat are zeroed separately
        // via the prescan + streamCopyZeroingRanges path.
        containerOut.write(writeBE32(boxSize))
        containerOut.write("free".toByteArray(Charsets.US_ASCII))
        writeZeros(containerOut, (boxSize - 8).toLong(), zeroBuf)
      } else if (boxType in containerBoxes) {
        // Recurse — children may contain metadata to replace
        val childrenBytes = processContainer(d, pos + 8, pos + boxSize, zeroBuf, depth + 1)
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
    // Fail closed if any bytes remain — silently dropping them would shrink the container,
    // shifting absolute file offsets that stco/co64 in moov reference into mdat.
    if (pos != end) throw IllegalArgumentException("MP4 container has ${end - pos} trailing byte(s) at end of range $offset..$end")
    return containerOut.toByteArray()
  }

  val header = ByteArray(8)

  // Prescan: find moov, parse it, collect mdat byte ranges of any timed-metadata tracks.
  // We must do this BEFORE streaming, because mdat can appear BEFORE moov in non-faststart files —
  // and we need the ranges to zero the right bytes while streaming mdat through.
  val mdatZeroRanges = mutableListOf<LongRange>()
  java.io.RandomAccessFile(input, "r").use { raf ->
    while (raf.filePointer + 8 <= raf.length()) {
      val boxPos = raf.filePointer
      raf.readFully(header)
      val rs = readBE32(header, 0).toLong() and 0xFFFFFFFFL
      val ty = String(header, 4, 4, Charsets.US_ASCII)
      val (hLen, dSize) = when (rs) {
        0L -> 8L to (raf.length() - boxPos - 8L)
        1L -> {
          val eb = ByteArray(8)
          raf.readFully(eb)
          var v = 0L
          for (j in 0 until 8) v = (v shl 8) or (eb[j].toLong() and 0xFF)
          // Guard against malformed extended size that would cause a backward seek (infinite loop).
          if (v < 16L) throw IllegalArgumentException("MP4 extended box size too small: $v")
          16L to (v - 16L)
        }
        else -> if (rs < 8L) throw IllegalArgumentException("Malformed MP4 top-level box: rawSize=$rs") else 8L to (rs - 8L)
      }
      if (ty == "moov") {
        if (dSize < 0 || dSize > MAX_HEADER_SIZE) return@use
        val moovData = ByteArray((8 + dSize).toInt())
        raf.seek(boxPos); raf.readFully(moovData, 0, 8)
        raf.seek(boxPos + hLen); raf.readFully(moovData, 8, dSize.toInt())
        // Walk moov children looking for timed-metadata trak boxes
        fun walk(s: Int, e: Int, depth: Int) {
          if (depth > maxContainerDepth) return
          var p = s
          while (p + 8 <= e) {
            val sz = readBE32(moovData, p)
            if (sz < 8 || p.toLong() + sz.toLong() > e) return
            val t = String(moovData, p + 4, 4, Charsets.US_ASCII)
            if (t == "trak" && isTimedMetadataTrack(moovData, p + 8, p + sz)) {
              mdatZeroRanges.addAll(extractTimedMetadataMdatRanges(moovData, p + 8, p + sz))
            } else if (t in containerBoxes) {
              walk(p + 8, p + sz, depth + 1)
            }
            p += sz
          }
        }
        walk(8, (8 + dSize).toInt(), 0)
        return@use
      }
      raf.seek(boxPos + hLen + dSize)
    }
  }
  // Sort once by start offset so streamCopyZeroingRanges can use a forward cursor (O(B + R)
  // instead of O(B × R)). Per spec, stco/co64 offsets are usually already sorted, but the prescan
  // may have collected ranges from multiple timed-metadata traks that interleave.
  if (mdatZeroRanges.size > 1) mdatZeroRanges.sortBy { it.first }

  java.io.RandomAccessFile(input, "r").use { raf ->
    java.io.FileOutputStream(outputPath).use { fos ->
      val buf = ByteArray(COPY_BUF_SIZE)
      // Defense against engulfment attacks: an attacker can declare a top-level box (e.g. mdat)
      // with a lying large size that "engulfs" subsequent boxes, causing them to be stream-copied
      // verbatim. Requiring ftyp first and moov present catches the most damaging case where moov
      // itself is engulfed (file would be unplayable anyway, so we fail closed).
      var moovSeen = false
      var firstBox = true
      while (raf.filePointer + 8 <= raf.length()) {
        raf.readFully(header)
        // Convert to unsigned Long to handle sizes >= 2GB
        val rawSize = readBE32(header, 0).toLong() and 0xFFFFFFFFL
        val boxType = String(header, 4, 4, Charsets.US_ASCII)
        if (firstBox && boxType != "ftyp") {
          throw IllegalArgumentException("MP4 must start with ftyp, got '$boxType'")
        }
        firstBox = false
        if (boxType == "moov") moovSeen = true

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
            val extSize = readBE64(extBuf, 0)
            // Guard: extSize < 16 would yield negative dataSize → backward seek → infinite loop.
            if (extSize < 16L) throw IllegalArgumentException("MP4 extended box size too small: $extSize")
            headerLen = 16L
            dataSize = extSize - 16L
          }
          else -> {
            // Normal box: size includes the 8-byte header.
            // Throwing (vs. silent break) prevents an attacker-crafted file from being
            // sent with only the bytes up to the malformed box — including any preceding
            // mdat with raw GPS bytes that prescan didn't get a chance to zero.
            if (rawSize < 8L) throw IllegalArgumentException("Malformed MP4 top-level box: rawSize=$rawSize")
            headerLen = 8L
            dataSize = rawSize - 8L
          }
        }

        if (boxType == "moov") {
          // Read moov into memory (typically < 100KB), process, write.
          // Only "moov" is recognized at the top level — other container types
          // (trak, mdia, etc.) only appear inside moov in well-formed MP4s.
          if (dataSize > MAX_HEADER_SIZE) throw IllegalArgumentException("MP4 moov box too large: $dataSize bytes")
          val moovData = ByteArray((8 + dataSize).toInt())
          // Copy original 8-byte basic header; processContainer reads children from offset 8.
          // For 64-bit extended boxes, the 8-byte ext_size was already consumed into extBuf above.
          System.arraycopy(header, 0, moovData, 0, 8)
          raf.readFully(moovData, 8, dataSize.toInt())
          val processed = processContainer(moovData, 8, (8 + dataSize).toInt(), buf)
          if (rawSize == 1L) {
            // Preserve 64-bit extended header to maintain byte offsets (stco/co64)
            fos.write(writeBE32(1))
            fos.write(boxType.toByteArray(Charsets.US_ASCII))
            fos.write(writeBE64(16L + processed.size))
          } else {
            fos.write(writeBE32(8 + processed.size))
            fos.write(boxType.toByteArray(Charsets.US_ASCII))
          }
          fos.write(processed)
        } else if (boxType in metadataBoxes) {
          // Top-level metadata box (e.g. uuid with XMP/GPS from exiftool) —
          // replace with same-size "free" to preserve byte offsets for stco/co64.
          // Guard against attacker-controlled huge extSize (writeZeros would otherwise fill disk).
          if (dataSize > MAX_HEADER_SIZE) throw IllegalArgumentException("MP4 top-level metadata box too large: $dataSize bytes")
          fos.write(writeBE32((headerLen + dataSize).toInt()))
          fos.write("free".toByteArray(Charsets.US_ASCII))
          // Write zeroed payload (account for extended header bytes that aren't being preserved)
          writeZeros(fos, dataSize + (headerLen - 8), buf)
          raf.seek(raf.filePointer + dataSize)
        } else if (boxType == "moof" || boxType == "mfra") {
          // Fragmented MP4 — moof/mfra contain metadata and timed-metadata sample references
          // (traf > udta, traf > uuid, per-fragment GPS via tfhd/trun) that we don't currently process.
          // Fail closed rather than passing them through unstripped.
          throw IllegalArgumentException("Fragmented MP4 (moof/mfra) not supported by stripper")
        } else {
          // Stream-copy large boxes (mdat, ftyp, etc.) without buffering
          // Write the original header bytes
          fos.write(header)
          if (rawSize == 1L) {
            // Write the 8-byte extended size (total box size as 64-bit BE)
            fos.write(writeBE64(headerLen + dataSize))
          }
          // Apply zeroing to ALL stream-copied boxes when ranges are non-empty — not just mdat.
          // Attacker could craft an MP4 where timed-metadata stco points outside mdat
          // (into a top-level "free", unknown four-CC, etc.), bypassing mdat-only zeroing.
          if (mdatZeroRanges.isNotEmpty()) {
            streamCopyZeroingRanges(raf, fos, dataSize, buf, mdatZeroRanges)
          } else {
            streamCopy(raf, fos, dataSize, buf)
          }
        }
      }
      if (!moovSeen) {
        throw IllegalArgumentException("MP4 has no moov box at top level — possibly engulfed by a lying box size")
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
  val metadataChunks = setOf("_PMX", "strd", "IDIT", "ISMP", "JUNQ")
  // LIST types containing privacy-sensitive metadata (replaced with JUNK like INFO)
  val metadataListTypes = setOf("INFO", "exif", "ncdt", "hydt", "pntx")

  // Process small header chunks in memory — replace metadata with same-size JUNK
  val maxHeaderDepth = 10

  val zeroBuf = ByteArray(COPY_BUF_SIZE)
  fun processHeaderChunks(d: ByteArray, start: Int, end: Int, output: java.io.ByteArrayOutputStream, depth: Int = 0) {
    if (depth > maxHeaderDepth) throw IllegalArgumentException("AVI header nesting too deep (>$maxHeaderDepth)")
    var p = start
    while (p + 8 <= end) {
      val fourCC = String(d, p, 4, Charsets.US_ASCII)
      val chunkSize = readLE32(d, p + 4)
      // Fail closed: silent break would shorten the rewritten LIST and could skip past real
      // metadata chunks that follow a malformed one. Match the MKV-side fail-closed posture.
      if (chunkSize < 0) throw IllegalArgumentException("AVI header chunk '$fourCC' has negative size at offset $p: $chunkSize")
      val paddedSize = if (chunkSize % 2 == 0) chunkSize.toLong() else chunkSize.toLong() + 1L
      val totalSizeLong = 8L + paddedSize
      if (totalSizeLong > Int.MAX_VALUE || p + totalSizeLong > end) {
        throw IllegalArgumentException("AVI header chunk '$fourCC' at offset $p overruns container: totalSize=$totalSizeLong, remaining=${end - p}")
      }
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
        writeZeros(output, paddedSize, zeroBuf)
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
        // Fail closed: a silent break would truncate the output (RIFF size is rewritten at the end
        // to match), letting a crafted file pass through with arbitrary tail data silently dropped.
        if (chunkSize < 0) throw IllegalArgumentException("AVI top-level chunk '$fourCC' has negative size: $chunkSize")
        // Use Long to avoid overflow for odd chunkSize near Int.MAX_VALUE
        val paddedSize = if (chunkSize % 2 == 0) chunkSize.toLong() else chunkSize.toLong() + 1L

        if (fourCC == "LIST" && raf.filePointer + 4 <= raf.length()) {
          // LIST chunks must declare at least 4 bytes (the listType FourCC).
          // chunkSize < 4 would cause `streamCopy(..., paddedSize - 4L, ...)` below to be a no-op
          // (negative `remaining`) while we still consume 4 bytes of listType — leaving the file
          // pointer mis-aligned and potentially causing subsequent metadata chunks to be skipped.
          if (chunkSize < 4) throw IllegalArgumentException("AVI LIST chunk too small: chunkSize=$chunkSize")
          val listTypeBytes = ByteArray(4)
          raf.readFully(listTypeBytes)
          val listType = String(listTypeBytes, Charsets.US_ASCII)

          if (listType in metadataListTypes) {
            // Replace metadata list content with zeros (JUNK) — preserves file layout for idx1 offsets
            if (paddedSize > MAX_HEADER_SIZE) throw IllegalArgumentException("AVI metadata list too large: $paddedSize bytes")
            out.write("JUNK".toByteArray(Charsets.US_ASCII))
            out.write(chunkHeader, 4, 4) // keep original size
            raf.seek(raf.filePointer + paddedSize - 4L) // skip original content
            writeZeros(out, paddedSize, buf) // zeroed padding (includes the 4 list-type bytes)
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
          writeZeros(out, paddedSize, buf) // zeroed content
        } else {
          // Stream-copy (idx1, etc.)
          out.write(chunkHeader)
          streamCopy(raf, out, paddedSize, buf)
        }
      }

      // Truncate output to the written length (defensive — guards against output path reuse)
      out.setLength(out.filePointer)

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
      if (ebmlSize < 0 || ebmlSize > MAX_HEADER_SIZE) throw IllegalArgumentException("MKV EBML header too large: $ebmlSize bytes")
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
      // Defense against engulfment: if SegmentInfo or Tracks are missing, the file is malformed
      // or an attacker placed a Cluster with a lying size that engulfed them.
      val TRACKS_ID = byteArrayOf(0x16.toByte(), 0x54.toByte(), 0xAE.toByte(), 0x6B.toByte())
      var segmentInfoSeen = false
      var tracksSeen = false

      while (raf.filePointer + 2 <= segDataEnd) {
        val childStart = raf.filePointer
        val (cidLen, cid) = rafReadVINTId(raf)
        if (cidLen == 0) break
        val (cszLen, csz) = rafReadVINTSize(raf)
        val chdrLen = cidLen + cszLen
        // Reject all unknown-size elements at Segment scope. Boundary-scanning for the next
        // Level-1 ID is fundamentally vulnerable: attacker can embed Cluster/Tags/etc. ID byte
        // sequences inside the element's payload (e.g., inside SimpleBlock data), making the
        // scanner stop early and bulk-copy remaining metadata bytes as a fake new element.
        // This affects browser MediaRecorder live-streaming WebM, which is rare in chat uploads;
        // users can re-encode if needed.
        if (csz < 0) {
          throw IllegalArgumentException("MKV element with unknown size (id=${cid.joinToString("") { "%02x".format(it) }}) not supported")
        }
        val dataSize = csz

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
            processSegmentInfo(infoData, 0, infoData.size, infoOut, metadataInfoElements, ByteArray(COPY_BUF_SIZE))
            headerChildren.add(HeaderChild(cid, infoOut.toByteArray()))
            segmentInfoSeen = true
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
            if (cid.contentEquals(TRACKS_ID)) tracksSeen = true
          }
        }
      }
      if (!segmentInfoSeen || !tracksSeen) {
        throw IllegalArgumentException("MKV Segment missing required SegmentInfo or Tracks — possibly engulfed by a Cluster with lying size")
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

      // Iteratively converge on correct SeekHead size. In practice convergence happens
      // in 1-2 iterations since size changes are monotone. Fail closed if it doesn't —
      // writing a non-converged SeekHead would produce stale offsets and broken seeking.
      var converged = false
      for (iteration in 0 until 5) {
        val seekHeadBytes = buildSeekHeadBytes(seekHeadOverhead)
        if (seekHeadBytes.size == seekHeadOverhead) {
          writeSegment(seekHeadBytes)
          converged = true
          break
        }
        seekHeadOverhead = seekHeadBytes.size
      }
      if (!converged) throw IllegalStateException("MKV SeekHead size did not converge")
    }
  }
}

/** Read a VINT (variable-length integer) from EBML data. Returns (byteLength, value).
 *  All-ones data bits encode "unknown size" → returns -1, matching rafReadVINTSize. */
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
  // Check all-ones (unknown size sentinel)
  val maxVal = (1L shl (7 * len)) - 1
  if (value == maxVal) value = -1
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

/** Encode a value as EBML VINT size using exactly [byteLen] bytes. Throws if value doesn't fit. */
private fun ebmlEncodeSizeFixedLen(value: Long, byteLen: Int): ByteArray {
  val maxValue = (1L shl (7 * byteLen)) - 1
  if (value < 0 || value >= maxValue) {
    throw IllegalArgumentException("EBML VINT value $value does not fit in $byteLen bytes (max $maxValue)")
  }
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
  metadataElements: List<ByteArray>, zeroBuf: ByteArray
) {
  var pos = start
  while (pos + 2 <= end) {
    val (idLen, _) = readVINTId(data, pos)
    if (pos + idLen >= end) break
    val (sizeLen, elementSize) = readVINT(data, pos + idLen)
    val headerLen = idLen + sizeLen
    // Fail closed on unknown-size or oversized children — otherwise an attacker could put
    // an unknown-size element first, and the verbatim-write of the remainder would leak
    // subsequent DateUTC, SegmentUUID, etc.
    if (elementSize < 0) {
      throw IllegalArgumentException("MKV SegmentInfo child has unknown size at offset $pos")
    }
    // Compare as Long: elementSize.toInt() would truncate values > Int.MAX_VALUE,
    // letting an attacker-crafted huge VINT size pass the bounds check.
    if (elementSize > Int.MAX_VALUE || pos.toLong() + headerLen + elementSize > end) {
      throw IllegalArgumentException("MKV SegmentInfo child extends past element bounds at offset $pos")
    }
    val totalSize = headerLen + elementSize.toInt()

    if (metadataElements.any { matchesId(data, pos, it) }) {
      // Write header but zero out the data payload
      out.write(data, pos, headerLen)
      writeZeros(out, elementSize, zeroBuf)
    } else {
      out.write(data, pos, totalSize)
    }
    pos += totalSize
  }
}

// Single source of truth for which extensions map to which stripper. Adding a new format
// is one map entry; isVideoFile / isAnimImageFile and the by-extension dispatchers all
// derive from these tables. .m4a is MP4 audio-only (Android MediaRecorder.OutputFormat.MPEG_4).
private val VIDEO_STRIPPERS: Map<String, (String, String) -> Unit> = mapOf(
  "mp4" to ::stripVideoMetadata,
  "mov" to ::stripVideoMetadata,
  "m4v" to ::stripVideoMetadata,
  "3gp" to ::stripVideoMetadata,
  "m4a" to ::stripVideoMetadata,
  "mkv" to ::stripMkvMetadata,
  "webm" to ::stripMkvMetadata,
  "avi" to ::stripAviMetadata,
)

private val ANIM_IMAGE_STRIPPERS: Map<String, (ByteArray) -> ByteArray> = mapOf(
  "gif" to ::stripGifMetadata,
  "webp" to ::stripWebPMetadata,
)

private fun fileExt(fileName: String): String = fileName.substringAfterLast('.', "").lowercase()

fun isVideoFile(fileName: String): Boolean = fileExt(fileName) in VIDEO_STRIPPERS

fun isAnimImageFile(fileName: String): Boolean = fileExt(fileName) in ANIM_IMAGE_STRIPPERS

/** Strip metadata from animated-image bytes by file extension (case-insensitive). */
fun stripAnimImageMetadataByExtension(ext: String, rawBytes: ByteArray): ByteArray {
  val strip = ANIM_IMAGE_STRIPPERS[ext.lowercase()]
    ?: throw IllegalArgumentException("Unsupported animated image format: $ext")
  return strip(rawBytes)
}

/** Route to the correct stripper based on file extension.
 *  [fileName] is the original filename (for extension detection).
 *  [inputPath] and [outputPath] may be temp file paths without meaningful extensions. */
fun stripVideoMetadataByExtension(fileName: String, inputPath: String, outputPath: String) {
  val ext = fileExt(fileName)
  val strip = VIDEO_STRIPPERS[ext]
    ?: throw IllegalArgumentException("Unsupported video format: $ext")
  strip(inputPath, outputPath)
}
