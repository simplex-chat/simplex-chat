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
import java.nio.file.attribute.FileTime
import java.time.Duration
import java.time.Instant
import javax.swing.SwingUtilities
import kotlin.concurrent.thread

private var lockHandle: FileLock? = null
private var watcher: WatchService? = null

internal const val SHOW_FILE = "simplex.show"
internal const val SHOW_TMP_SUFFIX = ".tmp"
internal const val TAKEN_SHOW_FILE = "$SHOW_FILE.taken$SHOW_TMP_SUFFIX"
// File times can lag the system clock by a clock tick, and by 2 s on FAT.
internal val FILE_TIME_TOLERANCE: Duration = Duration.ofSeconds(2)

private val lockPath get() = dataDir.resolve("simplex.started").toPath()
private val showPath get() = dataDir.resolve(SHOW_FILE).toPath()

var singleInstanceLock = false
  private set

internal class ShowSignal(val appLink: String?)

private sealed interface LockResult {
  class Acquired(val lock: FileLock) : LockResult
  object Taken : LockResult
  object Failed : LockResult
}

fun acquireSingleInstance(appLink: String?): Boolean {
  dataDir.mkdirs()
  val lockAttemptTime = FileTime.from(Instant.now())
  when (val result = tryAcquireLock()) {
    is LockResult.Acquired -> {
      lockHandle = result.lock
      singleInstanceLock = true
      deleteStaleSignalFiles(dataDir.toPath(), lockAttemptTime)
      return true
    }
    LockResult.Failed -> {
      return true
    }
    LockResult.Taken -> {
      if (desktopPlatform.isWindows()) allowPrimaryForeground()
      signalRunningInstance(dataDir.toPath(), appLink)
      // a signal still present after 1 s means the running instance is hung, so the user decides
      val deadline = System.currentTimeMillis() + 1000
      while (Files.exists(showPath) && System.currentTimeMillis() < deadline) {
        try { Thread.sleep(50) } catch (_: InterruptedException) { break }
      }
      if (!Files.exists(showPath)) return false
      val start = showSingleInstanceAlert()
      if (start) deleteSignalFile(showPath)
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

private fun deleteSignalFile(path: Path) {
  try { Files.deleteIfExists(path) } catch (e: IOException) {
    Log.w(TAG, "single-instance: cannot delete signal file: ${e.message}")
  }
}

internal fun deleteStaleSignalFiles(dir: Path, lockAttemptTime: FileTime) {
  val staleBefore = FileTime.from(lockAttemptTime.toInstant().minus(FILE_TIME_TOLERANCE))
  try {
    // only a running instance takes a signal, so a taken file at startup was left by a crash
    Files.deleteIfExists(dir.resolve(TAKEN_SHOW_FILE))
    Files.newDirectoryStream(dir, "$SHOW_FILE*").use { files ->
      files.forEach { file ->
        try {
          if (Files.getLastModifiedTime(file) < staleBefore) Files.deleteIfExists(file)
        } catch (_: NoSuchFileException) {
          // a second process renamed its temp file meanwhile
        }
      }
    }
  } catch (e: IOException) {
    Log.w(TAG, "single-instance: cannot delete stale signal files: ${e.message}")
  } catch (e: DirectoryIteratorException) {
    Log.w(TAG, "single-instance: cannot delete stale signal files: ${e.cause?.message}")
  }
}

// The temp file is owner-only on POSIX, as the link is a bearer secret, and the rename makes it appear whole.
internal fun signalRunningInstance(dir: Path, appLink: String?) {
  var tmp: Path? = null
  try {
    tmp = Files.createTempFile(dir, SHOW_FILE, SHOW_TMP_SUFFIX)
    Files.write(tmp, (appLink ?: "").toByteArray(Charsets.UTF_8))
    Files.move(tmp, dir.resolve(SHOW_FILE), ATOMIC_MOVE)
  } catch (e: IOException) {
    Log.w(TAG, "single-instance: cannot signal running instance: ${e.message}")
    tmp?.let(::deleteSignalFile)
    // an empty signal needs no free space and still brings the running instance forward
    try {
      Files.createFile(dir.resolve(SHOW_FILE))
    } catch (_: FileAlreadyExistsException) {
      // another process has already signalled
    } catch (createError: IOException) {
      Log.w(TAG, "single-instance: cannot create signal file: ${createError.message}")
    }
  }
}

// The signal is renamed before it is read, so a newer signal renamed over it meanwhile is not deleted unread.
internal fun takeSignal(dir: Path): ShowSignal? {
  val taken = dir.resolve(TAKEN_SHOW_FILE)
  try {
    Files.move(dir.resolve(SHOW_FILE), taken, ATOMIC_MOVE)
  } catch (_: NoSuchFileException) {
    return null
  } catch (e: IOException) {
    Log.w(TAG, "single-instance: cannot take signal file: ${e.message}")
    return ShowSignal(null)
  }
  val bytes = try {
    Files.newInputStream(taken).use { it.readNBytes(MAX_APP_LINK_BYTES + 1) }
  } catch (e: IOException) {
    Log.w(TAG, "single-instance: cannot read signal file: ${e.message}")
    null
  } finally {
    deleteSignalFile(taken)
  }
  return ShowSignal(bytes?.toString(Charsets.UTF_8)?.takeIf(::isAcceptedAppLink))
}

// Win32 ASFW_ANY lets every process take the foreground
private const val ASFW_ANY = -1

// Windows lets only the foreground process raise a window; the launching browser passed that right to this process.
private fun allowPrimaryForeground() {
  val allowed = try {
    NativeLibrary.getInstance("user32").getFunction("AllowSetForegroundWindow").invokeInt(arrayOf<Any>(ASFW_ANY)) != 0
  } catch (e: UnsatisfiedLinkError) {
    Log.w(TAG, "single-instance: AllowSetForegroundWindow unavailable: ${e.message}")
    return
  }
  if (!allowed) Log.w(TAG, "single-instance: AllowSetForegroundWindow refused")
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

fun startShowFileWatcher() {
  if (watcher != null) return
  val dir = dataDir.toPath()
  val ws = try {
    dir.fileSystem.newWatchService()
  } catch (e: IOException) {
    Log.w(TAG, "single-instance: WatchService failed: ${e.message}")
    return
  }
  dir.register(ws, StandardWatchEventKinds.ENTRY_CREATE)
  watcher = ws
  thread(name = "simplex-single-instance", isDaemon = true) {
    watchShowSignals(ws, dir) { signal ->
      SwingUtilities.invokeLater {
        if (signal.appLink != null) openDesktopAppLink(signal.appLink) else showWindow()
      }
    }
  }
}

// dir must already be registered with ws; a signal written before that raised no event, so it is taken first.
internal fun watchShowSignals(ws: WatchService, dir: Path, onSignal: (ShowSignal) -> Unit) {
  takeSignal(dir)?.let(onSignal)
  while (true) {
    val key = try { ws.take() } catch (_: ClosedWatchServiceException) { return } catch (_: InterruptedException) { return }
    for (event in key.pollEvents()) {
      if ((event.context() as? Path)?.fileName?.toString() == SHOW_FILE) takeSignal(dir)?.let(onSignal)
    }
    if (!key.reset()) return
  }
}
