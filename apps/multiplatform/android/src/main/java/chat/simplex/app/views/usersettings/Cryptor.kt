package chat.simplex.app.views.usersettings

import android.annotation.SuppressLint
import android.security.keystore.KeyGenParameterSpec
import android.security.keystore.KeyProperties
import android.util.Log
import chat.simplex.app.R
import chat.simplex.app.TAG
import chat.simplex.app.views.helpers.AlertManager
import chat.simplex.app.views.helpers.generalGetString
import java.security.KeyStore
import javax.crypto.*
import javax.crypto.spec.GCMParameterSpec

@SuppressLint("ObsoleteSdkInt")
internal class Cryptor {
  private var keyStore: KeyStore = KeyStore.getInstance("AndroidKeyStore").apply { load(null) }
  private var warningShown = false

  fun decryptData(data: ByteArray, iv: ByteArray, alias: String): String? {
    val secretKey = getSecretKey(alias)
    if (secretKey == null) {
      if (!warningShown) {
        // Repeated calls will not show the alert again
        warningShown = true
        AlertManager.shared.showAlertMsg(
          title = generalGetString(R.string.wrong_passphrase),
          text = generalGetString(R.string.restore_passphrase_not_found_desc)
        )
      }
      return null
    }
    val cipher: Cipher = Cipher.getInstance(TRANSFORMATION)
    val spec = GCMParameterSpec(128, iv)
    cipher.init(Cipher.DECRYPT_MODE, secretKey, spec)
    return runCatching { String(cipher.doFinal(data))}.onFailure { Log.e(TAG, "doFinal: ${it.stackTraceToString()}") }.getOrNull()
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

  private fun createSecretKey(alias: String): SecretKey? {
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

  private fun getSecretKey(alias: String): SecretKey? {
    return (keyStore.getEntry(alias, null) as? KeyStore.SecretKeyEntry)?.secretKey
  }

  companion object {
    private val KEY_ALGORITHM = KeyProperties.KEY_ALGORITHM_AES
    private val BLOCK_MODE = KeyProperties.BLOCK_MODE_GCM
    private val TRANSFORMATION = "AES/GCM/NoPadding"
  }
}
