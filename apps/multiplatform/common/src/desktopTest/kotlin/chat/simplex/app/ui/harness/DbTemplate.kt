package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.DatabaseUtils.ksDatabasePassword
import java.io.File
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.StandardCopyOption

object DbTemplate {
  private val shared = File(UiEnv.runDir, "db-template")
  private val keyFile = File(shared, "key")

  val exists: Boolean get() = keyFile.isFile

  fun installIfPresent() {
    if (!exists) return
    dbFiles(shared).forEach { it.copyTo(File(dataDir, it.name), overwrite = true) }
    ksDatabasePassword.set(keyFile.readText())
    appPrefs.storeDBPassphrase.set(true)
    appPrefs.initialRandomDBPassphrase.set(true)
  }

  fun save() {
    val staging = File(UiEnv.jvmDir, "db-template")
    staging.deleteRecursively()
    staging.mkdirs()
    dbFiles(dataDir).forEach { it.copyTo(File(staging, it.name)) }
    File(staging, "key").writeText(ksDatabasePassword.get() ?: error("no database key after initChatController"))
    try {
      Files.move(staging.toPath(), shared.toPath(), StandardCopyOption.ATOMIC_MOVE)
    } catch (e: AtomicMoveNotSupportedException) {
      throw IllegalStateException("run directory and JVM directory must be on one filesystem", e)
    } catch (e: java.nio.file.FileSystemException) {
      if (!exists) throw e
      staging.deleteRecursively()
    }
  }

  private fun dbFiles(dir: File): List<File> =
    dir.listFiles { f -> f.isFile && (f.name.startsWith(chatDatabaseFileName) || f.name.startsWith(agentDatabaseFileName)) }.orEmpty().toList()
}
