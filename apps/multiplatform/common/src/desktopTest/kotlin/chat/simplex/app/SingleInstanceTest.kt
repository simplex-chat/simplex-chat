package chat.simplex.app

import java.nio.channels.FileChannel
import java.nio.channels.OverlappingFileLockException
import java.nio.file.Files
import java.nio.file.StandardOpenOption.CREATE
import java.nio.file.StandardOpenOption.READ
import java.nio.file.StandardOpenOption.WRITE
import kotlin.test.Test
import kotlin.test.assertFailsWith
import kotlin.test.assertNotNull

// Pins the JDK FileLock semantics the single-instance machinery relies on.
// Cross-process contention (the path that returns null) cannot be exercised
// from inside one JVM — within the same JVM, a second tryLock on an already
// locked region throws OverlappingFileLockException instead. The production
// code in SingleInstance.kt catches that exception and fails closed, so this
// pair of tests covers both observable JDK behaviours we depend on: the
// exception itself, and the release/reacquire round-trip.
class SingleInstanceTest {
  @Test
  fun overlappingLockOnSameRegionThrowsWithinOneJvm() = withTempLockDir { lockPath ->
    val first = FileChannel.open(lockPath, READ, WRITE, CREATE)
    val firstLock = first.tryLock(0L, 1L, false)
    assertNotNull(firstLock, "first acquirer must get the lock")

    val second = FileChannel.open(lockPath, READ, WRITE, CREATE)
    assertFailsWith<OverlappingFileLockException> {
      second.tryLock(0L, 1L, false)
    }
    second.close()

    firstLock.release()
    first.close()
  }

  @Test
  fun releasedLockCanBeReacquired() = withTempLockDir { lockPath ->
    val first = FileChannel.open(lockPath, READ, WRITE, CREATE)
    val firstLock = first.tryLock(0L, 1L, false)
    assertNotNull(firstLock)
    firstLock.release()
    first.close()

    val second = FileChannel.open(lockPath, READ, WRITE, CREATE)
    val secondLock = second.tryLock(0L, 1L, false)
    assertNotNull(secondLock, "after release, a fresh acquirer must succeed")
    secondLock.release()
    second.close()
  }

  // Creates a temp directory, runs the block with a lock-file path inside, and
  // cleans the directory afterwards. File.deleteOnExit() is unreliable for
  // non-empty directories — would leak a temp dir on every test run.
  private fun withTempLockDir(block: (java.nio.file.Path) -> Unit) {
    val tmp = Files.createTempDirectory("simplex-singleinstance-test")
    try {
      block(tmp.resolve("simplex.lock"))
    } finally {
      Files.walk(tmp).sorted(Comparator.reverseOrder()).forEach {
        try { Files.delete(it) } catch (_: java.io.IOException) {}
      }
    }
  }
}
