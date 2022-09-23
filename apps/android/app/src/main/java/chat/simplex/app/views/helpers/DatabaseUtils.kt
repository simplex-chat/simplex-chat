package chat.simplex.app.views.helpers

import android.util.Log
import chat.simplex.app.*
import chat.simplex.app.model.AppPreferences
import chat.simplex.app.model.json
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

  fun migrateChatDatabase(useKey: String? = null): Triple<Boolean, DBMigrationResult, Long> {
    Log.d(TAG, "migrateChatDatabase ${appPreferences.storeDBPassphrase.get()}")
    val dbAbsolutePathPrefix = getFilesDirectory(SimplexApp.context)
    var dbKey = ""
    val useKeychain = appPreferences.storeDBPassphrase.get()
    if (useKey != null) {
      dbKey = useKey
    } else if (useKeychain) {
      if (!hasDatabase(SimplexApp.context.dataDir.absolutePath)) {
        dbKey = randomDatabasePassword()
        appPreferences.initialRandomDBPassphrase.set(true)
      } else {
        dbKey = getDatabaseKey() ?: ""
      }
    }
    Log.d(TAG, "migrateChatDatabase DB path: $dbAbsolutePathPrefix")
    val migrated: Array<Any> = chatMigrateInit(dbAbsolutePathPrefix, dbKey)
    val res: DBMigrationResult = kotlin.runCatching {
      json.decodeFromString<DBMigrationResult>(migrated[0] as String)
    }.getOrElse { DBMigrationResult.Unknown(migrated[0] as String) }
    val encrypted = dbKey != ""
    return Triple(encrypted, res, migrated[1] as Long)
  }

  private fun randomDatabasePassword(): String = ByteArray(32).apply { SecureRandom().nextBytes(this) }.toBase64String()
}

@Serializable
sealed class DBMigrationResult {
  @Serializable @SerialName("ok") object OK: DBMigrationResult()
  @Serializable @SerialName("errorNotADatabase") class ErrorNotADatabase(val dbFile: String): DBMigrationResult()
  @Serializable @SerialName("error") class Error(val dbFile: String, val migrationError: String): DBMigrationResult()
  @Serializable @SerialName("errorKeychain") object ErrorKeychain: DBMigrationResult()
  @Serializable @SerialName("unknown") class Unknown(val json: String): DBMigrationResult()
}