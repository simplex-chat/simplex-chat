package chat.simplex.common.platform

import android.annotation.SuppressLint
import android.security.keystore.KeyGenParameterSpec
import android.security.keystore.KeyProperties
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.MR
import java.security.KeyStore
import javax.crypto.*
import javax.crypto.spec.GCMParameterSpec

actual val cryptor: CryptorInterface = Cryptor()

@SuppressLint("ObsoleteSdkInt")
internal class Cryptor: CryptorInterface {
  private var keyStore: KeyStore = KeyStore.getInstance("AndroidKeyStore").apply { load(null) }
  private var warningShown = false

  override fun decryptData(data: ByteArray, iv: ByteArray, alias: String): String? {
    val secretKey = getSecretKey(alias)
    if (secretKey == null) {
      if (!warningShown) {
        // Repeated calls will not show the alert again
        warningShown = true
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.wrong_passphrase),
          text = generalGetString(MR.strings.restore_passphrase_not_found_desc)
        )
      }
      return null
    }

    try {
      val cipher: Cipher = Cipher.getInstance(TRANSFORMATION)
      val spec = GCMParameterSpec(128, iv)
      cipher.init(Cipher.DECRYPT_MODE, secretKey, spec)
      return String(cipher.doFinal(data))
    } catch (e: Throwable) {
      Log.e(TAG, "cipher.init: ${e.stackTraceToString()}")
      val randomPassphrase = appPreferences.initialRandomDBPassphrase.get()
      AlertManager.shared.showAlertMsg(
        title = generalGetString(MR.strings.error_reading_passphrase),
        text = generalGetString(if (randomPassphrase) {
          MR.strings.restore_passphrase_can_not_be_read_desc
        } else {
          MR.strings.restore_passphrase_can_not_be_read_enter_manually_desc
        }
        )
          .plus("\n\n").plus(e.stackTraceToString())
      )
      if (randomPassphrase) {
        // do not allow to override initial random passphrase in case of such error
        throw e
      }
      return null
    }
  }

  override fun encryptText(text: String, alias: String): Pair<ByteArray, ByteArray> {
    val cipher: Cipher = Cipher.getInstance(TRANSFORMATION)
    cipher.init(Cipher.ENCRYPT_MODE, createSecretKey(alias))
    return Pair(cipher.doFinal(text.toByteArray(charset("UTF-8"))), cipher.iv)
  }

  override fun deleteKey(alias: String) {
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
