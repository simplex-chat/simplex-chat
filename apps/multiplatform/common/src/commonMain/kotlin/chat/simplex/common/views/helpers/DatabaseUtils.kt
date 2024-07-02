package chat.simplex.common.views.helpers

import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import kotlinx.serialization.*
import java.io.File
import java.security.SecureRandom

object DatabaseUtils {
  private val appPreferences: AppPreferences = ChatController.appPrefs

  private const val DATABASE_PASSWORD_ALIAS: String = "databasePassword"
  private const val APP_PASSWORD_ALIAS: String = "appPassword"
  private const val SELF_DESTRUCT_PASSWORD_ALIAS: String = "selfDestructPassword"

  val ksDatabasePassword = KeyStoreItem(DATABASE_PASSWORD_ALIAS, appPreferences.encryptedDBPassphrase, appPreferences.initializationVectorDBPassphrase)
  val ksAppPassword = KeyStoreItem(APP_PASSWORD_ALIAS, appPreferences.encryptedAppPassphrase, appPreferences.initializationVectorAppPassphrase)
  val ksSelfDestructPassword = KeyStoreItem(SELF_DESTRUCT_PASSWORD_ALIAS, appPreferences.encryptedSelfDestructPassphrase, appPreferences.initializationVectorSelfDestructPassphrase)

  class KeyStoreItem(private val alias: String, val passphrase: SharedPreference<String?>, val initVector: SharedPreference<String?>) {
    fun get(): String? {
      return cryptor.decryptData(
        passphrase.get()?.toByteArrayFromBase64ForPassphrase() ?: return null,
        initVector.get()?.toByteArrayFromBase64ForPassphrase() ?: return null,
        alias,
      )
    }

    fun set(key: String) {
      val data = cryptor.encryptText(key, alias)
      passphrase.set(data.first.toBase64StringForPassphrase())
      initVector.set(data.second.toBase64StringForPassphrase())
    }

    fun remove() {
      cryptor.deleteKey(alias)
      passphrase.set(null)
      initVector.set(null)
    }
  }

  private fun hasAtLeastOneDatabase(rootDir: String): Boolean =
    File(rootDir + File.separator + chatDatabaseFileName).exists() || File(rootDir + File.separator + agentDatabaseFileName).exists()

  fun hasOnlyOneDatabase(rootDir: String): Boolean =
    File(rootDir + File.separator + chatDatabaseFileName).exists() != File(rootDir + File.separator + agentDatabaseFileName).exists()

  fun useDatabaseKey(): String {
    Log.d(TAG, "useDatabaseKey ${appPreferences.storeDBPassphrase.get()}")
    var dbKey = ""
    val useKeychain = appPreferences.storeDBPassphrase.get()
    if (useKeychain) {
      if (!hasAtLeastOneDatabase(dataDir.absolutePath)) {
        dbKey = randomDatabasePassword()
        ksDatabasePassword.set(dbKey)
        appPreferences.initialRandomDBPassphrase.set(true)
      } else {
        dbKey = ksDatabasePassword.get() ?: ""
      }
    } else if (appPlatform.isDesktop && !hasAtLeastOneDatabase(dataDir.absolutePath)) {
      // In case of database was deleted by hand
      dbKey = randomDatabasePassword()
      ksDatabasePassword.set(dbKey)
      appPreferences.initialRandomDBPassphrase.set(true)
      appPreferences.storeDBPassphrase.set(true)
    }
    return dbKey
  }

  fun randomDatabasePassword(): String {
    val s = ByteArray(32)
    SecureRandom().nextBytes(s)
    return s.toBase64StringForPassphrase().replace("\n", "")
  }
}

@Serializable
sealed class DBMigrationResult {
  @Serializable @SerialName("ok") object OK: DBMigrationResult()
  @Serializable @SerialName("invalidConfirmation") object InvalidConfirmation: DBMigrationResult()
  @Serializable @SerialName("errorNotADatabase") data class ErrorNotADatabase(val dbFile: String): DBMigrationResult()
  @Serializable @SerialName("errorMigration") data class ErrorMigration(val dbFile: String, val migrationError: MigrationError): DBMigrationResult()
  @Serializable @SerialName("errorSQL") data class ErrorSQL(val dbFile: String, val migrationSQLError: String): DBMigrationResult()
  @Serializable @SerialName("errorKeychain") object ErrorKeychain: DBMigrationResult()
  @Serializable @SerialName("unknown") data class Unknown(val json: String): DBMigrationResult()
}

enum class MigrationConfirmation(val value: String) {
  YesUp("yesUp"),
  YesUpDown ("yesUpDown"),
  Error("error")
}

fun defaultMigrationConfirmation(appPrefs: AppPreferences): MigrationConfirmation =
  if (appPrefs.developerTools.get() && appPrefs.confirmDBUpgrades.get()) MigrationConfirmation.Error else MigrationConfirmation.YesUp

@Serializable
sealed class MigrationError {
  @Serializable @SerialName("upgrade") class Upgrade(val upMigrations: List<UpMigration>): MigrationError()
  @Serializable @SerialName("downgrade") class Downgrade(val downMigrations: List<String>): MigrationError()
  @Serializable @SerialName("migrationError") class Error(val mtrError: MTRError): MigrationError()
}

@Serializable
data class UpMigration(
  val upName: String,
  // val withDown: Boolean
)

@Serializable
sealed class MTRError {
  @Serializable @SerialName("noDown") class NoDown(val dbMigrations: List<String>): MTRError()
  @Serializable @SerialName("different") class Different(val appMigration: String, val dbMigration: String): MTRError()
}
