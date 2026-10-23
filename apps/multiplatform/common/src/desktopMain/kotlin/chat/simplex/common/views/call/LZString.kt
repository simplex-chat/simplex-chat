package chat.simplex.common.views.call

// Port of lz-string compressToBase64/decompressFromBase64, operating on UTF-16 code units as the JS library does.
// Call peers (call.ts, iOS) exchange SDP and ICE candidates in this encoding.
internal object LZString {
  private const val KEY_BASE64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="
  private val BASE64_VALUES = IntArray(128) { -1 }.also { t -> KEY_BASE64.forEachIndexed { i, c -> t[c.code] = i } }
  private const val BASE64_BITS = 6
  private const val BASE64_RESET_VALUE = 32

  fun compressToBase64(input: String): String {
    val res = compress(input, BASE64_BITS) { KEY_BASE64[it] }
    return when (res.length % 4) {
      1 -> "$res==="
      2 -> "$res=="
      3 -> "$res="
      else -> res
    }
  }

  fun decompressFromBase64(input: String): String? {
    if (input.isEmpty()) return null
    return decompress(input.length, BASE64_RESET_VALUE) { i -> input[i].code.let { if (it < 128) BASE64_VALUES[it] else -1 } }
  }

  private class BitWriter(private val bitsPerChar: Int, private val charOf: (Int) -> Char) {
    val out = StringBuilder()
    private var value = 0
    private var position = 0

    fun write(bits: Int, v: Int) {
      var x = v
      repeat(bits) {
        value = (value shl 1) or (x and 1)
        if (position == bitsPerChar - 1) {
          position = 0
          out.append(charOf(value))
          value = 0
        } else {
          position++
        }
        x = x shr 1
      }
    }

    fun flush() {
      while (true) {
        value = value shl 1
        if (position == bitsPerChar - 1) {
          out.append(charOf(value))
          break
        }
        position++
      }
    }
  }

  private fun compress(input: String, bitsPerChar: Int, charOf: (Int) -> Char): String {
    val dictionary = HashMap<String, Int>()
    val toCreate = HashSet<String>()
    var w = ""
    var enlargeIn = 2
    var dictSize = 3
    var numBits = 2
    val writer = BitWriter(bitsPerChar, charOf)

    fun enlarge() {
      enlargeIn--
      if (enlargeIn == 0) {
        enlargeIn = 1 shl numBits
        numBits++
      }
    }

    fun emitW() {
      if (w in toCreate) {
        val code = w[0].code
        if (code < 256) {
          writer.write(numBits, 0)
          writer.write(8, code)
        } else {
          writer.write(numBits, 1)
          writer.write(16, code)
        }
        enlarge()
        toCreate.remove(w)
      } else {
        writer.write(numBits, dictionary.getValue(w))
      }
      enlarge()
    }

    for (ch in input) {
      val c = ch.toString()
      if (c !in dictionary) {
        dictionary[c] = dictSize++
        toCreate.add(c)
      }
      val wc = w + c
      if (wc in dictionary) {
        w = wc
      } else {
        emitW()
        dictionary[wc] = dictSize++
        w = c
      }
    }
    if (w.isNotEmpty()) emitW()
    writer.write(numBits, 2)
    writer.flush()
    return writer.out.toString()
  }

  private fun decompress(length: Int, resetValue: Int, valueAt: (Int) -> Int): String? {
    val dictionary = arrayListOf("0", "1", "2")
    var enlargeIn = 4
    var numBits = 3
    var dataVal = valueAt(0)
    var dataPosition = resetValue
    var dataIndex = 1

    fun readBits(n: Int): Int {
      var bits = 0
      var power = 1
      repeat(n) {
        val resb = dataVal and dataPosition
        dataPosition = dataPosition shr 1
        if (dataPosition == 0) {
          dataPosition = resetValue
          dataVal = if (dataIndex < length) valueAt(dataIndex) else 0
          dataIndex++
        }
        if (resb > 0) bits = bits or power
        power = power shl 1
      }
      return bits
    }

    val first = when (readBits(2)) {
      0 -> readBits(8).toChar().toString()
      1 -> readBits(16).toChar().toString()
      else -> return ""
    }
    dictionary.add(first)
    var w = first
    val result = StringBuilder(first)
    while (true) {
      if (dataIndex > length) return ""
      var c = readBits(numBits)
      when (c) {
        0, 1 -> {
          dictionary.add(readBits(if (c == 0) 8 else 16).toChar().toString())
          c = dictionary.size - 1
          enlargeIn--
        }
        2 -> return result.toString()
      }
      if (enlargeIn == 0) {
        enlargeIn = 1 shl numBits
        numBits++
      }
      val entry = when {
        c < dictionary.size -> dictionary[c]
        c == dictionary.size -> w + w[0]
        else -> return null
      }
      result.append(entry)
      dictionary.add(w + entry[0])
      enlargeIn--
      w = entry
      if (enlargeIn == 0) {
        enlargeIn = 1 shl numBits
        numBits++
      }
    }
  }
}
