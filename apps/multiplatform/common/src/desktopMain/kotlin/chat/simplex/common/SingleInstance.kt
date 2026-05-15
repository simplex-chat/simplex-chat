package chat.simplex.common

import chat.simplex.common.platform.Log
import chat.simplex.common.platform.TAG
import chat.simplex.common.platform.dataDir
import java.io.IOException
import java.nio.channels.FileChannel
import java.nio.channels.FileLock
import java.nio.channels.OverlappingFileLockException
import java.nio.file.*
import java.nio.file.StandardOpenOption.CREATE
import java.nio.file.StandardOpenOption.READ
import java.nio.file.StandardOpenOption.WRITE
import javax.swing.SwingUtilities
import kotlin.concurrent.thread

private var lockHandle: FileLock? = null
private var watcher: WatchService? = null

private val lockPath get() = dataDir.resolve("simplex.started").toPath()
private val showPath get() = dataDir.resolve("simplex.show").toPath()

var singleInstanceLock = false
  private set

private sealed interface LockResult {
  class Acquired(val lock: FileLock) : LockResult
  object Taken : LockResult
  object Failed : LockResult
}

fun acquireSingleInstance(): Boolean {
  dataDir.mkdirs()
  when (val result = tryAcquireLock()) {
    is LockResult.Acquired -> {
      lockHandle = result.lock
      singleInstanceLock = true
      deleteShowFile()
      return true
    }
    LockResult.Failed -> {
      return true
    }
    LockResult.Taken -> {
      if (Files.exists(showPath)) {
        val start = showSingleInstanceAlert()
        if (start) deleteShowFile()
        return start
      } else {
        val signalled = createShowFile()
        if (signalled) return false
        return showSingleInstanceAlert()
      }
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

private fun createShowFile(): Boolean =
  try { Files.createFile(showPath); true } catch (e: IOException) {
    Log.w(TAG, "single-instance: cannot create show file: ${e.message}")
    false
  }

fun startShowFileWatcher() {
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
        if ((event.context() as? Path)?.fileName?.toString() == "simplex.show") {
          deleteShowFile()
          SwingUtilities.invokeLater { showWindow() }
        }
      }
      if (!key.reset()) return@thread
    }
  }
}

fun stopShowFileWatcher() {
  watcher?.close()
  watcher = null
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
