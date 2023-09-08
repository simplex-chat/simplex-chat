package chat.simplex.common.model

import chat.simplex.common.platform.*
import kotlinx.serialization.*
import java.nio.ByteBuffer

@Serializable
sealed class WriteFileResult {
  @Serializable @SerialName("result") data class Result(val cryptoArgs: CryptoFileArgs): WriteFileResult()
  @Serializable @SerialName("error") data class Error(val writeError: String): WriteFileResult()
}

/*
 fun writeCryptoFile(path: String, data: ByteArray): CryptoFileArgs {
  val str = chatWriteFile(path, data)
  return when (val d = json.decodeFromString(WriteFileResult.serializer(), str)) {
    is WriteFileResult.Result -> d.cryptoArgs
    is WriteFileResult.Error -> throw Exception(d.writeError)
  }
}
* */

fun writeCryptoFile(path: String, data: ByteArray): CryptoFileArgs {
  val buffer = ByteBuffer.allocateDirect(data.size)
  buffer.put(data)
  buffer.rewind()
  val str = chatWriteFile(path, buffer)
  return when (val d = json.decodeFromString(WriteFileResult.serializer(), str)) {
    is WriteFileResult.Result -> d.cryptoArgs
    is WriteFileResult.Error -> throw Exception(d.writeError)
  }
}

fun readCryptoFile(path: String, cryptoArgs: CryptoFileArgs): ByteArray {
  val res: ByteArray = chatReadFile(path, cryptoArgs.fileKey, cryptoArgs.fileNonce)
  // If the first 10 bytes aren't zero, then the whole array is the data to be returned
  return if (
    res[0].toInt() == 0 &&
    res[1].toInt() == 0 &&
    res[2].toInt() == 0 &&
    res[3].toInt() == 0 &&
    res[4].toInt() == 0 &&
    res[5].toInt() == 0 &&
    res[6].toInt() == 0 &&
    res[7].toInt() == 0 &&
    res[8].toInt() == 0 &&
    res[9].toInt() == 0
    ) {
    // otherwise, first 10 elements should be skipped and copy of next elements will be made
    throw Exception(String(res.sliceArray(10..res.lastIndex)))
  } else {
    res
  }
}

fun encryptCryptoFile(fromPath: String, toPath: String): CryptoFileArgs {
  val str = chatEncryptFile(fromPath, toPath)
  val d = json.decodeFromString(WriteFileResult.serializer(), str)
  return when (d) {
    is WriteFileResult.Result -> d.cryptoArgs
    is WriteFileResult.Error -> throw Exception(d.writeError)
  }
}

fun decryptCryptoFile(fromPath: String, cryptoArgs: CryptoFileArgs, toPath: String) {
  val err = chatDecryptFile(fromPath, cryptoArgs.fileKey, cryptoArgs.fileNonce, toPath)
  if (err != "") {
    throw Exception(err)
  }
}
