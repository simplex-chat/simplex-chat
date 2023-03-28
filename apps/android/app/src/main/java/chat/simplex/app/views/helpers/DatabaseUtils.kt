package chat.simplex.app.views.helpers

import android.util.Log
import chat.simplex.app.*
import chat.simplex.app.model.AppPreferences
import chat.simplex.app.views.usersettings.Cryptor
import kotlinx.serialization.*
import java.io.File
import java.security.SecureRandom

object DatabaseUtils {
  private val cryptor = Cryptor()

  private val appPreferences: AppPreferences by lazy {
    AppPreferences(SimplexApp.context)
  }

  private const val DATABASE_PASSWORD_ALIAS: String = "databasePassword"

  private fun hasDatabase(rootDir: String): Boolean =
    File(rootDir + File.separator + "files_chat.db").exists() && File(rootDir + File.separator + "files_agent.db").exists()

  fun getDatabaseKey(): String? {
    return cryptor.decryptData(
      appPreferences.encryptedDBPassphrase.get()?.toByteArrayFromBase64() ?: return null,
      appPreferences.initializationVectorDBPassphrase.get()?.toByteArrayFromBase64() ?: return null,
      DATABASE_PASSWORD_ALIAS,
    )
  }

  fun setDatabaseKey(key: String) {
    val data = cryptor.encryptText(key, DATABASE_PASSWORD_ALIAS)
    appPreferences.encryptedDBPassphrase.set(data.first.toBase64String())
    appPreferences.initializationVectorDBPassphrase.set(data.second.toBase64String())
  }

  fun removeDatabaseKey() {
    cryptor.deleteKey(DATABASE_PASSWORD_ALIAS)
    appPreferences.encryptedDBPassphrase.set(null)
    appPreferences.initializationVectorDBPassphrase.set(null)
  }

  fun useDatabaseKey(): String {
    Log.d(TAG, "useDatabaseKey ${appPreferences.storeDBPassphrase.get()}")
    var dbKey = ""
    val useKeychain = appPreferences.storeDBPassphrase.get()
    if (useKeychain) {
      if (!hasDatabase(SimplexApp.context.dataDir.absolutePath)) {
        dbKey = randomDatabasePassword()
        setDatabaseKey(dbKey)
        appPreferences.initialRandomDBPassphrase.set(true)
      } else {
        dbKey = getDatabaseKey() ?: ""
      }
    }
    return dbKey
  }

  private fun randomDatabasePassword(): String {
    val s = ByteArray(32)
    SecureRandom().nextBytes(s)
    return s.toBase64String().replace("\n", "")
  }
}

@Serializable
sealed class DBMigrationResult {
  @Serializable @SerialName("ok") object OK: DBMigrationResult()
  @Serializable @SerialName("invalidConfirmation") object InvalidConfirmation: DBMigrationResult()
  @Serializable @SerialName("errorNotADatabase") class ErrorNotADatabase(val dbFile: String): DBMigrationResult()
  @Serializable @SerialName("errorMigration") class ErrorMigration(val dbFile: String, val migrationError: MigrationError): DBMigrationResult()
  @Serializable @SerialName("errorSQL") class ErrorSQL(val dbFile: String, val migrationSQLError: String): DBMigrationResult()
  @Serializable @SerialName("errorKeychain") object ErrorKeychain: DBMigrationResult()
  @Serializable @SerialName("unknown") class Unknown(val json: String): DBMigrationResult()
}


enum class MigrationConfirmation(val value: String) {
  YesUp("yesUp"),
  YesUpDown ("yesUpDown"),
  Error("error")
}

fun defaultMigrationConfirmation(appPrefs: AppPreferences): MigrationConfirmation =
  if (appPrefs.confirmDBUpgrades.get()) MigrationConfirmation.Error else MigrationConfirmation.YesUp

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