package chat.simplex.common.platform

actual val cryptor: CryptorInterface =
  if (desktopPlatform.isLinux()) LinuxCryptor(LibsecretStore, ::showKeyringProblem) else object : CryptorInterface {
    override fun decryptData(data: ByteArray, iv: ByteArray, alias: String): String? {
      return String(data) // LALAL
    }

    override fun encryptText(text: String, alias: String): Pair<ByteArray, ByteArray> {
      return text.toByteArray() to text.toByteArray() // LALAL
    }

    override fun deleteKey(alias: String) {
      // LALAL
    }

    override fun keyStorage(alias: String): String? = null
  }
