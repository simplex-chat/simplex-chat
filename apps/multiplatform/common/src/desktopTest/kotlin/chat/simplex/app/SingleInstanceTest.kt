package chat.simplex.app

import chat.simplex.common.MAX_APP_LINK_LENGTH
import chat.simplex.common.deleteShowFiles
import chat.simplex.common.signalRunningInstance
import chat.simplex.common.takeSignal
import java.nio.channels.FileChannel
import java.nio.channels.OverlappingFileLockException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardWatchEventKinds
import java.nio.file.attribute.PosixFileAttributeView
import java.nio.file.attribute.PosixFilePermission
import java.util.concurrent.TimeUnit
import java.nio.file.StandardOpenOption.CREATE
import java.nio.file.StandardOpenOption.READ
import java.nio.file.StandardOpenOption.WRITE
import kotlin.test.Test
import kotlin.test.assertFailsWith
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNotNull
import kotlin.test.assertNull
import kotlin.test.assertTrue

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

  private val badgeLink = "simplexchat:/badge/code/SB0000000000000000000"

  @Test
  fun signalCarriesTheLinkAndIsTakenOnce() = withTempDir { dir ->
    signalRunningInstance(dir, badgeLink)
    val show = dir.resolve("simplex.show")
    assertTrue(Files.exists(show), "signal file must exist after signalling")
    assertEquals(listOf("simplex.show"), fileNames(dir), "no temp file may be left beside the signal")
    assertEquals(badgeLink, takeSignal(show))
    assertFalse(Files.exists(show), "taking the signal must delete it")
    assertNull(takeSignal(show), "a signal already taken carries nothing")
  }

  @Test
  fun signalWithoutLinkOnlyShowsTheWindow() = withTempDir { dir ->
    signalRunningInstance(dir, null)
    val show = dir.resolve("simplex.show")
    assertTrue(Files.exists(show))
    assertNull(takeSignal(show))
    assertFalse(Files.exists(show))
  }

  @Test
  fun laterSignalReplacesAnUntakenOne() = withTempDir { dir ->
    val second = "simplexchat:/badge/code/SB1111111111111111111"
    signalRunningInstance(dir, badgeLink)
    signalRunningInstance(dir, second)
    assertEquals(listOf("simplex.show"), fileNames(dir))
    assertEquals(second, takeSignal(dir.resolve("simplex.show")))
  }

  @Test
  fun signalIsReadableOnlyByItsOwner() = withTempDir { dir ->
    signalRunningInstance(dir, badgeLink)
    val view = Files.getFileAttributeView(dir.resolve("simplex.show"), PosixFileAttributeView::class.java)
      ?: return@withTempDir // not a POSIX file system
    assertEquals(
      setOf(PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE),
      view.readAttributes().permissions()
    )
  }

  @Test
  fun takeSignalDropsContentThatIsNotAnAppLink() = withTempDir { dir ->
    val show = dir.resolve("simplex.show")
    Files.writeString(show, "https://simplex.chat/contact#/?v=2-7")
    assertNull(takeSignal(show), "web link")
    assertFalse(Files.exists(show), "rejected signal must still be deleted")

    Files.writeString(show, "simplexchat:/badge/code/" + "A".repeat(MAX_APP_LINK_LENGTH))
    assertNull(takeSignal(show), "link over the length bound")
    assertFalse(Files.exists(show))
  }

  @Test
  fun deleteShowFilesRemovesStaleSignalsOnly() = withTempDir { dir ->
    Files.createFile(dir.resolve("simplex.started"))
    Files.createFile(dir.resolve("simplex.show"))
    Files.createFile(dir.resolve("simplex.show123.tmp"))
    Files.createFile(dir.resolve("other.tmp"))
    deleteShowFiles(dir)
    assertEquals(listOf("other.tmp", "simplex.started"), fileNames(dir))
  }

  @Test
  fun signalArrivesAsCreationOfTheShowFile() = withTempDir { dir ->
    dir.fileSystem.newWatchService().use { ws ->
      dir.register(ws, StandardWatchEventKinds.ENTRY_CREATE)
      signalRunningInstance(dir, badgeLink)
      val created = mutableListOf<String>()
      // macOS polls the directory, so a generous deadline; inotify and Windows report at once
      val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(15)
      while ("simplex.show" !in created && System.nanoTime() < deadline) {
        val key = ws.poll(1, TimeUnit.SECONDS) ?: continue
        key.pollEvents().mapNotNullTo(created) { (it.context() as? Path)?.fileName?.toString() }
        key.reset()
      }
      assertTrue("simplex.show" in created, "ENTRY_CREATE for simplex.show expected, got $created")
    }
  }

  private fun fileNames(dir: Path): List<String> =
    Files.list(dir).use { files -> files.map { it.fileName.toString() }.sorted().toList() }

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
