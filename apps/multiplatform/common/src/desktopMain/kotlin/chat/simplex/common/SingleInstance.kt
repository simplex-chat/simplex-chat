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

// true = lock acquired (or lock unsupported - proceed without tray-minimize).
// false = another instance is running, signalled it to show, caller should exit.
var singleInstanceLock = false
  private set

fun acquireSingleInstance(): Boolean {
  dataDir.mkdirs()
  val channel: FileChannel = try {
    FileChannel.open(lockPath, READ, WRITE, CREATE)
  } catch (e: IOException) {
    Log.w(TAG, "single-instance: cannot open lock file: ${e.message}")
    return true
  }
  val held: FileLock? = try {
    channel.tryLock(0L, 1L, false)
  } catch (_: OverlappingFileLockException) {
    channel.close()
    return true
  } catch (e: IOException) {
    Log.w(TAG, "single-instance: tryLock failed: ${e.message}")
    channel.close()
    return true
  }
  if (held != null) {
    lockHandle = held
    singleInstanceLock = true
    try { Files.deleteIfExists(showPath) } catch (_: IOException) {}
    return true
  }
  // Lock taken by another instance
  channel.close()
  if (Files.exists(showPath)) {
    // Previous signal not picked up - running instance may be stuck
    return showSingleInstanceAlert()
  }
  try { Files.createFile(showPath) } catch (_: IOException) {}
  return false
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
          try { Files.deleteIfExists(showPath) } catch (_: IOException) {}
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
