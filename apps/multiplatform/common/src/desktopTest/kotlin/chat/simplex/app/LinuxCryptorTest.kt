package chat.simplex.app

import chat.simplex.common.platform.KeyUnavailableException
import chat.simplex.common.platform.KeyringProblem
import chat.simplex.common.platform.LinuxCryptor
import chat.simplex.common.platform.SecretServiceAbsentException
import chat.simplex.common.platform.SecretServiceException
import chat.simplex.common.platform.SecretStore
import java.util.Base64
import kotlin.test.Test
import kotlin.test.assertContentEquals
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertFalse
import kotlin.test.assertNotNull
import kotlin.test.assertNull
import kotlin.test.assertTrue

private const val ALIAS = "databasePassword"
private const val SECRET = "passphrase with ü and 🔑"

private enum class StoreState { Working, Absent, Failing }

private class FakeSecretStore : SecretStore {
  val secrets = mutableMapOf<String, String>()
  var state = StoreState.Working
  var lookups = 0

  override fun lookup(alias: String): String? {
    lookups++
    check()
    return secrets[alias]
  }

  override fun store(alias: String, secret: String) {
    check()
    secrets[alias] = secret
  }

  override fun clear(alias: String) {
    check()
    secrets.remove(alias)
  }

  private fun check() {
    when (state) {
      StoreState.Working -> {}
      StoreState.Absent -> throw SecretServiceAbsentException("no provider")
      StoreState.Failing -> throw SecretServiceException("prompt dismissed")
    }
  }
}

class LinuxCryptorTest {
  private val store = FakeSecretStore()
  private val problems = mutableListOf<KeyringProblem>()
  private fun cryptor() = LinuxCryptor(store) { problems.add(it) }

  @Test
  fun encryptedRecordRoundTripsAndKeepsOnlyCiphertext() {
    val (data, iv) = cryptor().encryptText(SECRET, ALIAS)
    assertFalse(data.contentEquals(iv), "an encrypted record must differ from the legacy layout")
    assertFalse(String(data, Charsets.UTF_8).contains("passphrase"), "the ciphertext must not contain the secret")
    assertEquals(SECRET, cryptor().decryptData(data, iv, ALIAS), "a new process reads the key from the store")
    assertEquals(listOf(), problems)
  }

  @Test
  fun keyIsStoredOncePerAlias() {
    val cryptor = cryptor()
    cryptor.encryptText("first", ALIAS)
    val key = store.secrets[ALIAS]
    cryptor.encryptText("second", ALIAS)
    cryptor().encryptText("third", ALIAS)
    assertEquals(key, store.secrets[ALIAS], "later writes reuse the stored key")
    assertEquals(32, Base64.getDecoder().decode(key).size)
  }

  @Test
  fun keyIsReadOncePerProcess() {
    val (data, iv) = cryptor().encryptText(SECRET, ALIAS)
    val cryptor = cryptor()
    store.lookups = 0
    repeat(3) { assertEquals(SECRET, cryptor.decryptData(data, iv, ALIAS)) }
    assertEquals(1, store.lookups)
  }

  @Test
  fun legacyRecordIsReadWithoutTheStore() {
    store.state = StoreState.Absent
    val legacy = SECRET.toByteArray()
    assertEquals(SECRET, cryptor().decryptData(legacy, legacy, ALIAS))
    assertEquals(listOf(), problems)
  }

  @Test
  fun absentStoreFallsBackToTheLegacyLayout() {
    store.state = StoreState.Absent
    val cryptor = cryptor()
    val (data, iv) = cryptor.encryptText(SECRET, ALIAS)
    assertContentEquals(data, iv)
    assertEquals(SECRET, cryptor.decryptData(data, iv, ALIAS))
    assertNull(cryptor.keyStorage(ALIAS), "settings show the passphrase as plaintext")
  }

  @Test
  fun failingStoreFallsBackToTheLegacyLayout() {
    store.state = StoreState.Failing
    val (data, iv) = cryptor().encryptText(SECRET, ALIAS)
    assertContentEquals(data, iv)
  }

  @Test
  fun encryptedRecordWithAbsentStoreIsUnavailable() {
    val (data, iv) = cryptor().encryptText(SECRET, ALIAS)
    store.state = StoreState.Absent
    assertFailsWith<KeyUnavailableException> { cryptor().decryptData(data, iv, ALIAS) }
    assertEquals(listOf(KeyringProblem.Unavailable), problems)
  }

  @Test
  fun encryptedRecordWithLockedStoreIsUnavailable() {
    val (data, iv) = cryptor().encryptText(SECRET, ALIAS)
    store.state = StoreState.Failing
    assertFailsWith<KeyUnavailableException> { cryptor().decryptData(data, iv, ALIAS) }
    assertEquals(listOf(KeyringProblem.Unavailable), problems)
  }

  @Test
  fun encryptedRecordWithoutKeyIsMissing() {
    val (data, iv) = cryptor().encryptText(SECRET, ALIAS)
    store.secrets.clear()
    assertNull(cryptor().decryptData(data, iv, ALIAS))
    assertEquals(listOf(KeyringProblem.KeyMissing), problems)
  }

  @Test
  fun encryptedRecordWithOtherKeyIsMissing() {
    val (data, iv) = cryptor().encryptText(SECRET, ALIAS)
    store.secrets[ALIAS] = Base64.getEncoder().encodeToString(ByteArray(32) { 7 })
    assertNull(cryptor().decryptData(data, iv, ALIAS))
    assertEquals(listOf(KeyringProblem.KeyMissing), problems)
  }

  @Test
  fun malformedKeyIsMissing() {
    val (data, iv) = cryptor().encryptText(SECRET, ALIAS)
    store.secrets[ALIAS] = "not base64 !"
    assertNull(cryptor().decryptData(data, iv, ALIAS))
    store.secrets[ALIAS] = Base64.getEncoder().encodeToString(ByteArray(16))
    assertNull(cryptor().decryptData(data, iv, ALIAS), "a key of a wrong size")
  }

  @Test
  fun deleteKeyRemovesItFromStoreAndMemory() {
    val cryptor = cryptor()
    val (data, iv) = cryptor.encryptText(SECRET, ALIAS)
    assertNotNull(cryptor.keyStorage(ALIAS))
    cryptor.deleteKey(ALIAS)
    assertTrue(ALIAS !in store.secrets)
    assertNull(cryptor.keyStorage(ALIAS))
    assertNull(cryptor.decryptData(data, iv, ALIAS))
  }

  @Test
  fun deleteKeyWithAbsentStoreDoesNotThrow() {
    val cryptor = cryptor()
    cryptor.encryptText(SECRET, ALIAS)
    store.state = StoreState.Absent
    cryptor.deleteKey(ALIAS)
    assertNull(cryptor.keyStorage(ALIAS))
  }

  @Test
  fun aliasesHaveSeparateKeys() {
    val cryptor = cryptor()
    val (data, iv) = cryptor.encryptText(SECRET, ALIAS)
    cryptor.encryptText(SECRET, "appPassword")
    assertTrue(store.secrets[ALIAS] != store.secrets["appPassword"])
    assertNull(cryptor.decryptData(data, iv, "appPassword"), "a record decrypted with another alias's key")
  }
}
