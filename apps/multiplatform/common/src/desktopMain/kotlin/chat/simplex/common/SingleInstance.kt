package chat.simplex.common

import chat.simplex.common.platform.Log
import chat.simplex.common.platform.TAG
import chat.simplex.common.platform.dataDir
import chat.simplex.common.platform.desktopPlatform
import com.sun.jna.NativeLibrary
import java.io.IOException
import java.nio.channels.FileChannel
import java.nio.channels.FileLock
import java.nio.channels.OverlappingFileLockException
import java.nio.file.*
import java.nio.file.StandardCopyOption.ATOMIC_MOVE
import java.nio.file.StandardOpenOption.CREATE
import java.nio.file.StandardOpenOption.READ
import java.nio.file.StandardOpenOption.WRITE
import javax.swing.SwingUtilities
import kotlin.concurrent.thread

private var lockHandle: FileLock? = null
private var watcher: WatchService? = null

private const val SHOW_FILE = "simplex.show"
private const val SHOW_TMP_SUFFIX = ".tmp"
// Win32 ASFW_ANY: any process may take the foreground
private const val ASFW_ANY = -1

private val lockPath get() = dataDir.resolve("simplex.started").toPath()
private val showPath get() = dataDir.resolve(SHOW_FILE).toPath()

var singleInstanceLock = false
  private set

private sealed interface LockResult {
  class Acquired(val lock: FileLock) : LockResult
  object Taken : LockResult
  object Failed : LockResult
}

fun acquireSingleInstance(appLink: String?): Boolean {
  dataDir.mkdirs()
  when (val result = tryAcquireLock()) {
    is LockResult.Acquired -> {
      lockHandle = result.lock
      singleInstanceLock = true
      deleteShowFiles(dataDir.toPath())
      startShowFileWatcher()
      return true
    }
    LockResult.Failed -> {
      return true
    }
    LockResult.Taken -> {
      // Signal the primary and wait up to 1s for its watcher to consume the
      // signal. If still there after the wait, the primary is hung — let the
      // user decide.
      allowPrimaryForeground()
      signalRunningInstance(dataDir.toPath(), appLink)
      val deadline = System.currentTimeMillis() + 1000
      while (Files.exists(showPath) && System.currentTimeMillis() < deadline) {
        try { Thread.sleep(50) } catch (_: InterruptedException) { break }
      }
      if (!Files.exists(showPath)) return false
      val start = showSingleInstanceAlert()
      if (start) deleteShowFile()
      return start
    }
  }
}

private fun tryAcquireLock(): LockResult {
  val channel = try {
    FileChannel.open(lockPath, READ, WRITE, CREATE)
  } catch (e: IOException) {
    Log.w(TAG, "single-instance: cannot open lock file: ${e.message}")
    return LockResult.Failed
  }
  return try {
    val lock = channel.tryLock(0L, 1L, false)
    if (lock != null) {
      LockResult.Acquired(lock)
    } else {
      channel.close()
      LockResult.Taken
    }
  } catch (_: OverlappingFileLockException) {
    Log.w(TAG, "single-instance: overlapping lock in same JVM")
    LockResult.Failed
  } catch (e: IOException) {
    Log.w(TAG, "single-instance: tryLock failed: ${e.message}")
    channel.close(); LockResult.Failed
  }
}

private fun deleteShowFile() {
  try { Files.deleteIfExists(showPath) } catch (e: IOException) {
    Log.w(TAG, "single-instance: cannot delete show file: ${e.message}")
  }
}

// Stale files are left by a crash between writing a signal and consuming it.
internal fun deleteShowFiles(dir: Path) {
  try {
    Files.deleteIfExists(dir.resolve(SHOW_FILE))
    Files.newDirectoryStream(dir, "$SHOW_FILE*$SHOW_TMP_SUFFIX").use { tmps -> tmps.forEach(Files::deleteIfExists) }
  } catch (e: IOException) {
    Log.w(TAG, "single-instance: cannot delete show files: ${e.message}")
  }
}

// The signal may carry an app link, a bearer secret: the temp file is owner-only on POSIX,
// and the rename makes it appear whole, so the watcher never reads a partial write.
internal fun signalRunningInstance(dir: Path, appLink: String?) {
  var tmp: Path? = null
  try {
    tmp = Files.createTempFile(dir, SHOW_FILE, SHOW_TMP_SUFFIX)
    Files.write(tmp, (appLink ?: "").toByteArray(Charsets.UTF_8))
    Files.move(tmp, dir.resolve(SHOW_FILE), ATOMIC_MOVE)
  } catch (e: IOException) {
    Log.w(TAG, "single-instance: cannot signal running instance: ${e.message}")
    tmp?.let { try { Files.deleteIfExists(it) } catch (_: IOException) {} }
  }
}

internal fun takeSignal(file: Path): String? {
  val bytes = try {
    Files.newInputStream(file).use { it.readNBytes(MAX_APP_LINK_LENGTH + 1) }
  } catch (_: NoSuchFileException) {
    return null
  } catch (e: IOException) {
    Log.w(TAG, "single-instance: cannot read show file: ${e.message}")
    null
  } finally {
    try { Files.deleteIfExists(file) } catch (e: IOException) {
      Log.w(TAG, "single-instance: cannot delete show file: ${e.message}")
    }
  }
  return bytes?.toString(Charsets.UTF_8)?.takeIf(::isAcceptedAppLink)
}

// Windows lets only the foreground process raise a window; the browser that launched this
// process passed that right here, and this hands it on to the primary.
private fun allowPrimaryForeground() {
  if (!desktopPlatform.isWindows()) return
  try {
    NativeLibrary.getInstance("user32").getFunction("AllowSetForegroundWindow").invokeInt(arrayOf<Any>(ASFW_ANY))
  } catch (e: UnsatisfiedLinkError) {
    Log.w(TAG, "single-instance: AllowSetForegroundWindow unavailable: ${e.message}")
  }
}

private fun showSingleInstanceAlert(): Boolean {
  val title = chat.simplex.common.views.helpers.generalGetString(chat.simplex.res.MR.strings.another_instance_title)
  val message = chat.simplex.common.views.helpers.generalGetString(chat.simplex.res.MR.strings.another_instance_not_responding)
  val result = javax.swing.JOptionPane.showConfirmDialog(
    null, message, title,
    javax.swing.JOptionPane.YES_NO_OPTION,
    javax.swing.JOptionPane.WARNING_MESSAGE
  )
  return result == javax.swing.JOptionPane.YES_OPTION
}

private fun startShowFileWatcher() {
  if (watcher != null) return
  val ws = try {
    dataDir.toPath().fileSystem.newWatchService()
  } catch (e: IOException) {
    Log.w(TAG, "single-instance: WatchService failed: ${e.message}")
    return
  }
  dataDir.toPath().register(ws, StandardWatchEventKinds.ENTRY_CREATE)
  watcher = ws
  thread(name = "simplex-single-instance", isDaemon = true) {
    while (true) {
      val key = try { ws.take() } catch (_: ClosedWatchServiceException) { return@thread } catch (_: InterruptedException) { return@thread }
      for (event in key.pollEvents()) {
        if ((event.context() as? Path)?.fileName?.toString() == SHOW_FILE) {
          val appLink = takeSignal(showPath)
          SwingUtilities.invokeLater {
            showWindow()
            appLink?.let(::openDesktopAppLink)
          }
        }
      }
      if (!key.reset()) return@thread
    }
  }
}
