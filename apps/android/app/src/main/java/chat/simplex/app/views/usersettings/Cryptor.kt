package chat.simplex.app.views.usersettings

import android.annotation.SuppressLint
import android.security.keystore.KeyGenParameterSpec
import android.security.keystore.KeyProperties
import java.security.KeyStore
import javax.crypto.*
import javax.crypto.spec.GCMParameterSpec

@SuppressLint("ObsoleteSdkInt")
internal class Cryptor {
  private var keyStore: KeyStore = KeyStore.getInstance("AndroidKeyStore").apply { load(null) }

  fun decryptData(data: ByteArray, iv: ByteArray, alias: String): String {
    val cipher: Cipher = Cipher.getInstance(TRANSFORMATION)
    val spec = GCMParameterSpec(128, iv)
    cipher.init(Cipher.DECRYPT_MODE, getSecretKey(alias), spec)
    return String(cipher.doFinal(data))
  }

  fun encryptText(text: String, alias: String): Pair<ByteArray, ByteArray> {
    val cipher: Cipher = Cipher.getInstance(TRANSFORMATION)
    cipher.init(Cipher.ENCRYPT_MODE, createSecretKey(alias))
    return Pair(cipher.doFinal(text.toByteArray(charset("UTF-8"))), cipher.iv)
  }

  fun deleteKey(alias: String) {
    if (!keyStore.containsAlias(alias)) return
    keyStore.deleteEntry(alias)
  }

  private fun createSecretKey(alias: String): SecretKey {
    if (keyStore.containsAlias(alias)) return getSecretKey(alias)
    val keyGenerator: KeyGenerator = KeyGenerator.getInstance(KEY_ALGORITHM, "AndroidKeyStore")
    keyGenerator.init(
      KeyGenParameterSpec.Builder(alias, KeyProperties.PURPOSE_ENCRYPT or KeyProperties.PURPOSE_DECRYPT)
        .setBlockModes(BLOCK_MODE)
        .setEncryptionPaddings(KeyProperties.ENCRYPTION_PADDING_NONE)
        .build()
    )
    return keyGenerator.generateKey()
  }

  private fun getSecretKey(alias: String): SecretKey {
    return (keyStore.getEntry(alias, null) as KeyStore.SecretKeyEntry).secretKey
  }

  companion object {
    private val KEY_ALGORITHM = KeyProperties.KEY_ALGORITHM_AES
    private val BLOCK_MODE = KeyProperties.BLOCK_MODE_GCM
    private val TRANSFORMATION = "AES/GCM/NoPadding"
  }
}
