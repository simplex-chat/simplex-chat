package chat.simplex.app.views.usersettings

import android.annotation.SuppressLint
import android.content.Context
import android.os.Build
import android.security.keystore.KeyGenParameterSpec
import android.security.keystore.KeyProperties
import android.util.Base64
import android.util.Log
import chat.simplex.app.SimplexApp
import chat.simplex.app.TAG
import java.security.KeyStore
import javax.crypto.*
import javax.crypto.spec.GCMParameterSpec

@SuppressLint("ObsoleteSdkInt")
internal class Cryptor {
  private var keyStore: KeyStore = KeyStore.getInstance("AndroidKeyStore").apply { load(null) }

  fun decryptData(alias: String, data: ByteArray, iv: ByteArray): String {
    val cipher: Cipher = Cipher.getInstance(TRANSFORMATION)
    val spec = GCMParameterSpec(128, iv)
    cipher.init(Cipher.DECRYPT_MODE, getSecretKey(alias), spec)
    return String(cipher.doFinal(data))
  }

  fun encryptText(alias: String, text: String): CryptorData {
    val cipher: Cipher = Cipher.getInstance(TRANSFORMATION)
    cipher.init(Cipher.ENCRYPT_MODE, createSecretKey(alias))
    return CryptorData(cipher.doFinal(text.toByteArray(charset("UTF-8"))), cipher.iv)
  }

  private fun createSecretKey(alias: String): SecretKey {
    if (keyStore.containsAlias(alias))
      return (keyStore.getEntry(alias, null) as KeyStore.SecretKeyEntry).secretKey
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
    private val KEY_ALGORITHM = if (Build.VERSION.SDK_INT >= 23) KeyProperties.KEY_ALGORITHM_AES else KeyProperties.KEY_ALGORITHM_RSA
    private val BLOCK_MODE = if (Build.VERSION.SDK_INT >= 23) KeyProperties.BLOCK_MODE_GCM else KeyProperties.BLOCK_MODE_ECB
    private val TRANSFORMATION = if (Build.VERSION.SDK_INT >= 23) "AES/GCM/NoPadding" else "RSA/ECB/NoPadding"

    fun test(alias: String = "test", text: String = "toEncrypt", usePreferences: Boolean = false) {
      val enc = Cryptor()
      Log.d(TAG, "Cryptor: all keys ${enc.keyStore.aliases().toList()}")
      val encrypted = enc.encryptText(alias, text)
      val prefs = SimplexApp.context.getSharedPreferences("chat.simplex.app.SIMPLEX_APP_PREFS", Context.MODE_PRIVATE)
      if (usePreferences) {
        prefs.edit().putString("encrypted_data", encrypted.dataAsString()).apply()
        prefs.edit().putString("encrypted_iv", encrypted.ivAsString()).apply()
      }
      Log.d(TAG, "Cryptor: encrypt ${Base64.encodeToString(encrypted.data, Base64.DEFAULT)}")
      Log.d(TAG, "Cryptor: all keys ${enc.keyStore.aliases().toList()}")
      if (usePreferences) {
        val fromPrefs = CryptorData.fromStrings(prefs.getString("encrypted_data", "")!!, prefs.getString("encrypted_iv", "")!!)
        Log.d(
          TAG, "Cryptor: decrypt ${
            enc.decryptData(
              alias,
              fromPrefs.data,
              fromPrefs.iv
            )
          }"
        )
      } else {
        Log.d(TAG, "Cryptor: decrypt ${enc.decryptData(alias, encrypted.data, encrypted.iv)}")
      }
      prefs.edit().remove("encrypted_data").apply()
      prefs.edit().remove("encrypted_iv").apply()
    }
  }
}

data class CryptorData(val data: ByteArray, val iv: ByteArray) {
  companion object {
    fun fromStrings(data: String, iv: String): CryptorData =
      CryptorData(Base64.decode(data, Base64.DEFAULT), Base64.decode(iv, Base64.DEFAULT))
  }

  fun dataAsString(): String = Base64.encodeToString(data, Base64.DEFAULT)

  fun ivAsString(): String = Base64.encodeToString(iv, Base64.DEFAULT)

  override fun equals(other: Any?): Boolean {
    if (this === other) return true
    if (javaClass != other?.javaClass) return false

    other as CryptorData

    if (!data.contentEquals(other.data)) return false
    if (!iv.contentEquals(other.iv)) return false

    return true
  }

  override fun hashCode(): Int {
    var result = data.contentHashCode()
    result = 31 * result + iv.contentHashCode()
    return result
  }
}
