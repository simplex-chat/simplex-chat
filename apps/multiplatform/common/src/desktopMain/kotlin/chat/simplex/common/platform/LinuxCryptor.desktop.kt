package chat.simplex.common.platform

import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.DatabaseUtils
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.MR
import java.security.GeneralSecurityException
import java.security.SecureRandom
import java.util.Base64
import java.util.concurrent.ConcurrentHashMap
import javax.crypto.Cipher
import javax.crypto.SecretKey
import javax.crypto.spec.GCMParameterSpec
import javax.crypto.spec.SecretKeySpec

private const val KEY_ALGORITHM = "AES"
private const val TRANSFORMATION = "AES/GCM/NoPadding"
private const val KEY_BYTES = 32
private const val TAG_BITS = 128
private const val SECRET_LABEL_PREFIX = "SimpleX Chat: "
private const val SECRET_SERVICE_NAME = "Secret Service"

class SecretServiceAbsentException(message: String?) : Exception(message)

class SecretServiceException(message: String?) : Exception(message)

interface SecretStore {
  fun lookup(alias: String): String?
  fun store(alias: String, secret: String)
  fun clear(alias: String)
}

internal object LibsecretStore : SecretStore {
  override fun lookup(alias: String): String? {
    requireLoaded()
    return LinuxNative.nativeSecretLookup(alias)
  }

  override fun store(alias: String, secret: String) {
    requireLoaded()
    LinuxNative.nativeSecretStore(alias, SECRET_LABEL_PREFIX + alias, secret)
  }

  override fun clear(alias: String) {
    requireLoaded()
    LinuxNative.nativeSecretClear(alias)
  }

  private fun requireLoaded() {
    if (!LinuxNative.isLoaded) throw SecretServiceAbsentException("Linux native library is unavailable")
  }
}

enum class KeyringProblem {
  Unavailable,
  KeyMissing,
}

internal class LinuxCryptor(
  private val store: SecretStore,
  private val onProblem: (KeyringProblem) -> Unit,
) : CryptorInterface {
  private val keys = ConcurrentHashMap<String, SecretKey>()
  private val random = SecureRandom()

  override fun decryptData(data: ByteArray, iv: ByteArray, alias: String): String? {
    if (data.contentEquals(iv)) return String(data)
    val key = try {
      keys[alias] ?: readKey(alias)
    } catch (e: SecretServiceAbsentException) {
      onProblem(KeyringProblem.Unavailable)
      throw KeyUnavailableException(e.message)
    } catch (e: SecretServiceException) {
      onProblem(KeyringProblem.Unavailable)
      throw KeyUnavailableException(e.message)
    }
    if (key == null) {
      onProblem(KeyringProblem.KeyMissing)
      return null
    }
    return try {
      val cipher = Cipher.getInstance(TRANSFORMATION)
      cipher.init(Cipher.DECRYPT_MODE, key, GCMParameterSpec(TAG_BITS, iv))
      String(cipher.doFinal(data), Charsets.UTF_8)
    } catch (e: GeneralSecurityException) {
      Log.e(TAG, "Cannot decrypt $alias: ${e.message}")
      onProblem(KeyringProblem.KeyMissing)
      null
    }
  }

  override fun encryptText(text: String, alias: String): Pair<ByteArray, ByteArray> {
    val key = try {
      keys[alias] ?: readKey(alias) ?: createKey(alias)
    } catch (e: SecretServiceAbsentException) {
      Log.w(TAG, "Secret Service is absent, $alias is stored in settings: ${e.message}")
      null
    } catch (e: SecretServiceException) {
      Log.e(TAG, "Secret Service failed, $alias is stored in settings: ${e.message}")
      null
    }
    if (key == null) {
      val plaintext = text.toByteArray()
      return plaintext to plaintext
    }
    val cipher = Cipher.getInstance(TRANSFORMATION)
    cipher.init(Cipher.ENCRYPT_MODE, key)
    return cipher.doFinal(text.toByteArray(Charsets.UTF_8)) to cipher.iv
  }

  override fun deleteKey(alias: String) {
    keys.remove(alias)
    try {
      store.clear(alias)
    } catch (e: SecretServiceAbsentException) {
      Log.w(TAG, "Secret Service is absent, $alias is not cleared: ${e.message}")
    } catch (e: SecretServiceException) {
      Log.e(TAG, "Secret Service failed, $alias is not cleared: ${e.message}")
    }
  }

  override fun keyStorage(alias: String): String? = if (keys.containsKey(alias)) SECRET_SERVICE_NAME else null

  private fun readKey(alias: String): SecretKey? {
    val encoded = store.lookup(alias) ?: return null
    val bytes = try {
      Base64.getDecoder().decode(encoded)
    } catch (e: IllegalArgumentException) {
      Log.e(TAG, "Key $alias is malformed: ${e.message}")
      return null
    }
    if (bytes.size != KEY_BYTES) return null
    return SecretKeySpec(bytes, KEY_ALGORITHM).also { keys[alias] = it }
  }

  private fun createKey(alias: String): SecretKey {
    val bytes = ByteArray(KEY_BYTES).also(random::nextBytes)
    store.store(alias, Base64.getEncoder().encodeToString(bytes))
    return SecretKeySpec(bytes, KEY_ALGORITHM).also { keys[alias] = it }
  }
}

private val shownProblems = ConcurrentHashMap.newKeySet<KeyringProblem>()

internal fun showKeyringProblem(problem: KeyringProblem) {
  if (!shownProblems.add(problem)) return
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.keychain_error),
    text = generalGetString(
      when (problem) {
        KeyringProblem.Unavailable -> MR.strings.keyring_unavailable_desc
        KeyringProblem.KeyMissing -> MR.strings.keyring_key_not_found_desc
      }
    )
  )
}

internal fun migratePlaintextSecrets(
  items: List<DatabaseUtils.KeyStoreItem> = listOf(DatabaseUtils.ksDatabasePassword, DatabaseUtils.ksAppPassword, DatabaseUtils.ksSelfDestructPassword),
): Boolean {
  val plaintext = items.filter { it.isPlaintext() }
  plaintext.forEach { item -> item.get()?.let(item::set) }
  return plaintext.any { !it.isPlaintext() }
}
