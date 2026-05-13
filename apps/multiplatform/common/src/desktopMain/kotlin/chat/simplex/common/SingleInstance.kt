package chat.simplex.common

import chat.simplex.common.platform.Log
import chat.simplex.common.platform.TAG
import chat.simplex.common.platform.dataDir
import java.io.IOException
import java.net.InetAddress
import java.net.InetSocketAddress
import java.net.ServerSocket
import java.net.Socket
import java.nio.channels.FileChannel
import java.nio.channels.FileLock
import java.nio.channels.OverlappingFileLockException
import java.nio.charset.StandardCharsets
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import java.nio.file.StandardOpenOption.CREATE
import java.nio.file.StandardOpenOption.READ
import java.nio.file.StandardOpenOption.WRITE
import javax.swing.SwingUtilities
import kotlin.concurrent.thread

// Held for the process lifetime. Module-level var deliberately so the FileLock
// isn't garbage-collected — and FileLock itself pins its FileChannel, so a
// single reference here keeps both alive.
private var lockHandle: FileLock? = null

// Explicit IPv4 loopback. InetAddress.getLoopbackAddress() may return ::1 on
// dual-stack systems where IPv6 is preferred, and Windows Defender's loopback
// exemption is most reliable for the 127.0.0.0/8 family.
private val LOOPBACK: InetAddress = InetAddress.getByAddress(byteArrayOf(127, 0, 0, 1))

// Bound on the bytes we'll read from a signal — long enough for plausible
// future single-token commands, short enough that an adversarial same-UID
// process can't OOM us by streaming without a newline.
private const val MAX_SIGNAL_BYTES = 256

// Returns true if this process owns the single-instance lock (caller proceeds
// with normal startup). Returns false if another instance already owns it
// (caller must return from main without further init).
fun acquireSingleInstanceOrSignalAndExit(): Boolean {
  dataDir.mkdirs()
  val lockFile = dataDir.resolve("simplex.lock").toPath()
  val channel: FileChannel = try {
    FileChannel.open(lockFile, READ, WRITE, CREATE)
  } catch (e: IOException) {
    // Filesystem doesn't allow opening the lock file. Proceed without
    // single-instance enforcement — no worse than today's behaviour.
    Log.w(TAG, "single-instance: cannot open $lockFile: ${e.stackTraceToString()}")
    return true
  }
  val held: FileLock? = try {
    // Lock exactly one byte. Zero-arg tryLock() locks [0, Long.MAX_VALUE) and
    // is rejected by some SMB/NFS implementations (JDK-6674134).
    channel.tryLock(0L, 1L, false)
  } catch (e: OverlappingFileLockException) {
    // Unreachable by construction (function is called once at process start),
    // but if it ever fires the caller can't safely proceed as the singleton:
    // we don't know who holds the lock and we haven't started a listener.
    // Fail closed — close the channel and behave like a second instance.
    Log.w(TAG, "single-instance: overlapping lock on $lockFile")
    channel.close()
    return false
  } catch (e: IOException) {
    Log.w(TAG, "single-instance: tryLock failed on $lockFile: ${e.stackTraceToString()}")
    channel.close()
    return true
  }
  if (held == null) {
    channel.close()
    signalShowAndReturn()
    return false
  }
  lockHandle = held
  startSingleInstanceListener()
  return true
}

private const val LISTENER_THREAD_NAME = "simplex-single-instance"

private fun startSingleInstanceListener() {
  // Drop any stale simplex.port left by a previous primary BEFORE binding the
  // new ServerSocket. A second instance arriving between our lock acquisition
  // and our writePortFile() would otherwise read the old port and signal SHOW
  // to whatever process now owns it. With the file gone, that race exits the
  // signaller silently via readPortWithRetry returning null.
  try { Files.deleteIfExists(dataDir.resolve("simplex.port").toPath()) } catch (_: IOException) {}
  val server = try {
    ServerSocket(0, 0, LOOPBACK)
  } catch (e: IOException) {
    // Ephemeral port range starved (rare, observed behind some VPNs). Lock is
    // still held, so duplicate launches see the lock but no port to signal —
    // they retry and exit silently. Worst case: user clicks the tray icon.
    Log.w(TAG, "single-instance: ServerSocket bind failed: ${e.stackTraceToString()}")
    return
  }
  writePortFile(server.localPort)
  thread(name = LISTENER_THREAD_NAME, isDaemon = true) {
    while (true) {
      val socket = try {
        server.accept()
      } catch (e: IOException) {
        Log.w(TAG, "single-instance: accept() failed: ${e.stackTraceToString()}")
        return@thread
      }
      try {
        socket.soTimeout = 1000
        // Single bounded read: enough for any sensible command, capped so a
        // hostile client can't grow a buffer to OOM. Slow senders that drip
        // bytes lose, but our own signaller writes the full payload in one
        // call so this is fine in practice.
        val buf = ByteArray(MAX_SIGNAL_BYTES)
        val read = socket.getInputStream().read(buf)
        if (read > 0) {
          val line = String(buf, 0, read, StandardCharsets.UTF_8).substringBefore('\n').trimEnd('\r')
          Log.i(TAG, "single-instance: received $line")
          // Only SHOW is recognised today. Future commands (e.g. open-URL) will
          // extend this with new top-level branches; unknown lines are ignored.
          if (line == "SHOW") {
            SwingUtilities.invokeLater { showWindow() }
          }
        }
      } catch (e: IOException) {
        Log.w(TAG, "single-instance: read failed: ${e.stackTraceToString()}")
      } finally {
        try { socket.close() } catch (_: IOException) {}
      }
    }
  }
}

private fun signalShowAndReturn() {
  val port = readPortWithRetry() ?: return
  try {
    Socket().use { sock ->
      sock.connect(InetSocketAddress(LOOPBACK, port), 1000)
      val out = sock.getOutputStream()
      out.write("SHOW\n".toByteArray(StandardCharsets.UTF_8))
      out.flush()
    }
  } catch (e: IOException) {
    // First instance is starting, shutting down, or stuck. Doing nothing is
    // strictly less harmful than spawning a duplicate that will fail on the
    // SQLite lock. The stale-port-after-crash case lands here too — handled.
    Log.w(TAG, "single-instance: SHOW signal failed: ${e.stackTraceToString()}")
  }
}

private fun readPortWithRetry(): Int? {
  val portFile = dataDir.resolve("simplex.port").toPath()
  repeat(2) { attempt ->
    val raw = try {
      Files.readString(portFile, StandardCharsets.UTF_8).trim()
    } catch (e: IOException) {
      null
    }
    val parsed = raw?.toIntOrNull()
    if (parsed != null && parsed in 1..65535) return parsed
    // First-instance may still be writing the port during startup; one retry.
    if (attempt == 0) Thread.sleep(200)
  }
  return null
}

private fun writePortFile(port: Int) {
  val portFile = dataDir.resolve("simplex.port").toPath()
  // Files.createTempFile creates with O_CREAT|O_EXCL and a random name in
  // dataDir. A same-UID attacker can't pre-plant a symlink at this path to
  // make our subsequent write truncate their chosen target — they don't
  // know the random suffix, and EXCL refuses to open an existing path.
  val tmp = try {
    Files.createTempFile(dataDir.toPath(), "simplex.port.", ".tmp")
  } catch (e: IOException) {
    Log.w(TAG, "single-instance: createTempFile failed: ${e.stackTraceToString()}")
    return
  }
  var moved = false
  try {
    Files.writeString(tmp, port.toString(), StandardCharsets.UTF_8)
    try {
      Files.move(tmp, portFile, StandardCopyOption.ATOMIC_MOVE)
    } catch (e: AtomicMoveNotSupportedException) {
      // Exotic filesystem. Fall back to plain move; the reader retries on
      // parse failure so a brief window of a half-written port file is fine.
      Files.move(tmp, portFile, StandardCopyOption.REPLACE_EXISTING)
    }
    moved = true
  } catch (e: IOException) {
    Log.w(TAG, "single-instance: writing port file failed: ${e.stackTraceToString()}")
  } finally {
    if (!moved) try { Files.deleteIfExists(tmp) } catch (_: IOException) {}
  }
}
