package chat.simplex.common.platform

enum class KeyStorage {
  StrongBox,
  TrustedEnvironment,
  Software,
}

interface CryptorInterface {
  fun decryptData(data: ByteArray, iv: ByteArray, alias: String): String?
  fun encryptText(text: String, alias: String): Pair<ByteArray, ByteArray>
  fun deleteKey(alias: String)
  fun keyStorage(alias: String): KeyStorage?
}

expect val cryptor: CryptorInterface
