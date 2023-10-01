package chat.simplex.common.platform

actual val cryptor: CryptorInterface = object : CryptorInterface {
  override fun decryptData(data: ByteArray, iv: ByteArray, alias: String): String? {
    return String(data) // LALAL
  }

  override fun encryptText(text: String, alias: String): Pair<ByteArray, ByteArray> {
    return text.toByteArray() to text.toByteArray() // LALAL
  }

  override fun deleteKey(alias: String) {
    // LALAL
  }
}
