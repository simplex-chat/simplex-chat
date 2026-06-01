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

class SingleInstanceTest {
  @Test
  fun overlappingLockOnSameRegionThrowsWithinOneJvm() = withTempDir { dir ->
    val lockPath = dir.resolve("simplex.started")
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
  fun releasedLockCanBeReacquired() = withTempDir { dir ->
    val lockPath = dir.resolve("simplex.started")
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

  private fun withTempDir(block: (java.nio.file.Path) -> Unit) {
    val tmp = Files.createTempDirectory("simplex-singleinstance-test")
    try {
      block(tmp)
    } finally {
      Files.walk(tmp).sorted(Comparator.reverseOrder()).forEach {
        try { Files.delete(it) } catch (_: java.io.IOException) {}
      }
    }
  }
}
