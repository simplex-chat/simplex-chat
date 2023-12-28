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
  val ctrl = ChatController.ctrl ?: throw Exception("Controller is not initialized")
  val buffer = ByteBuffer.allocateDirect(data.size)
  buffer.put(data)
  buffer.rewind()
  val str = chatWriteFile(ctrl, path, buffer)
  return when (val d = json.decodeFromString(WriteFileResult.serializer(), str)) {
    is WriteFileResult.Result -> d.cryptoArgs
    is WriteFileResult.Error -> throw Exception(d.writeError)
  }
}

fun readCryptoFile(path: String, cryptoArgs: CryptoFileArgs): ByteArray {
  val res: Array<Any> = chatReadFile(path, cryptoArgs.fileKey, cryptoArgs.fileNonce)
  val status = (res[0] as Integer).toInt()
  val arr = res[1] as ByteArray
  if (status == 0) {
    return arr
  } else {
    throw Exception(String(arr))
  }
}

fun encryptCryptoFile(fromPath: String, toPath: String): CryptoFileArgs {
  val ctrl = ChatController.ctrl ?: throw Exception("Controller is not initialized")
  val str = chatEncryptFile(ctrl, fromPath, toPath)
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
