package chat.simplex.app

import chat.simplex.common.FILE_TIME_TOLERANCE
import chat.simplex.common.MAX_APP_LINK_BYTES
import chat.simplex.common.SHOW_FILE
import chat.simplex.common.SHOW_TMP_SUFFIX
import chat.simplex.common.ShowSignal
import chat.simplex.common.TAKEN_SHOW_FILE
import chat.simplex.common.deleteStaleSignalFiles
import chat.simplex.common.signalRunningInstance
import chat.simplex.common.takeSignal
import chat.simplex.common.watchShowSignals
import org.junit.Assume.assumeFalse
import org.junit.Assume.assumeTrue
import java.nio.channels.FileChannel
import java.nio.channels.OverlappingFileLockException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardOpenOption.CREATE
import java.nio.file.StandardOpenOption.READ
import java.nio.file.StandardOpenOption.WRITE
import java.nio.file.StandardWatchEventKinds
import java.nio.file.attribute.FileTime
import java.nio.file.attribute.PosixFileAttributeView
import java.nio.file.attribute.PosixFilePermission
import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit
import kotlin.concurrent.thread
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertFalse
import kotlin.test.assertNotNull
import kotlin.test.assertNull
import kotlin.test.assertTrue

// generous for a loaded build machine; the watcher normally delivers within milliseconds
private const val SIGNAL_WAIT_SECONDS = 15L

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

  @Test
  fun signalCarriesTheLinkAndIsTakenOnce() = withTempDir { dir ->
    signalRunningInstance(dir, BADGE_LINK)
    assertEquals(listOf(SHOW_FILE), fileNames(dir), "only the signal may exist after signalling")
    assertEquals(BADGE_LINK, takeSignal(dir)?.appLink)
    assertEquals(listOf(), fileNames(dir), "taking the signal must leave no file behind")
    assertNull(takeSignal(dir), "a signal already taken must not be taken again")
  }

  @Test
  fun signalWithoutLinkIsTakenWithNoLink() = withTempDir { dir ->
    signalRunningInstance(dir, null)
    val signal = takeSignal(dir)
    assertNotNull(signal, "an empty signal must still be taken")
    assertNull(signal.appLink, "an empty signal carries no link")
    assertEquals(listOf(), fileNames(dir), "taking the signal must leave no file behind")
    assertNull(takeSignal(dir), "a signal already taken must not be taken again")
  }

  @Test
  fun laterSignalReplacesAnUntakenOne() = withTempDir { dir ->
    signalRunningInstance(dir, BADGE_LINK)
    signalRunningInstance(dir, OTHER_BADGE_LINK)
    assertEquals(listOf(SHOW_FILE), fileNames(dir), "the later signal must replace the earlier one")
    assertEquals(OTHER_BADGE_LINK, takeSignal(dir)?.appLink)
  }

  @Test
  fun signalIsReadableOnlyByItsOwner() = withTempDir { dir ->
    signalRunningInstance(dir, BADGE_LINK)
    val view = Files.getFileAttributeView(dir.resolve(SHOW_FILE), PosixFileAttributeView::class.java)
    assumeTrue("POSIX file permissions are not supported here", view != null)
    assertEquals(
      setOf(PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE),
      view.readAttributes().permissions()
    )
  }

  @Test
  fun takeSignalReplacesATakenFileLeftByACrash() = withTempDir { dir ->
    Files.writeString(dir.resolve(TAKEN_SHOW_FILE), OTHER_BADGE_LINK)
    signalRunningInstance(dir, BADGE_LINK)
    assertEquals(BADGE_LINK, takeSignal(dir)?.appLink, "the new signal, not the leftover, must be read")
    assertEquals(listOf(), fileNames(dir), "taking the signal must leave no file behind")
  }

  @Test
  fun takeSignalAcceptsALinkOfExactlyTheBound() = withTempDir { dir ->
    val atBound = badgeLinkOfBytes(MAX_APP_LINK_BYTES)
    signalRunningInstance(dir, atBound)
    assertEquals(atBound, takeSignal(dir)?.appLink, "a link of exactly $MAX_APP_LINK_BYTES bytes")
  }

  @Test
  fun takeSignalDropsContentThatIsNotAnAppLink() = withTempDir { dir ->
    val show = dir.resolve(SHOW_FILE)
    fun assertTakenWithoutLink(case: String) {
      val signal = takeSignal(dir)
      assertNotNull(signal, "$case: rejected content is still a signal, so the window comes forward")
      assertNull(signal.appLink, case)
    }
    Files.writeString(show, "https://simplex.chat/contact#/?v=2-7")
    assertTakenWithoutLink("web link")
    assertEquals(listOf(), fileNames(dir), "a rejected signal must still be removed")

    Files.writeString(show, badgeLinkOfBytes(MAX_APP_LINK_BYTES + 1))
    assertTakenWithoutLink("link over the length bound")

    // 614 characters but 1214 bytes; a read cut at the bound would end on a whole character and decode to an accepted link
    Files.writeString(show, "simplexchat:/x" + "é".repeat(600))
    assertTakenWithoutLink("multi-byte link over the byte bound")
    assertEquals(listOf(), fileNames(dir), "a rejected signal must still be removed")
  }

  @Test
  fun deleteStaleSignalFilesRemovesOldTempAndTakenFilesOnly() = withTempDir { dir ->
    val lockAttemptTime = Instant.now().truncatedTo(ChronoUnit.SECONDS)
    val old = FileTime.from(lockAttemptTime.minusSeconds(3600))
    Files.setLastModifiedTime(Files.createFile(dir.resolve("simplex.started")), old)
    Files.setLastModifiedTime(Files.createFile(dir.resolve("simplex_v1_chat.db")), old)
    Files.setLastModifiedTime(Files.createFile(dir.resolve("other.tmp")), old)
    Files.setLastModifiedTime(Files.createFile(dir.resolve("${SHOW_FILE}123$SHOW_TMP_SUFFIX")), old)
    Files.createFile(dir.resolve("${SHOW_FILE}456$SHOW_TMP_SUFFIX"))
    Files.createFile(dir.resolve(TAKEN_SHOW_FILE))
    Files.createFile(dir.resolve(SHOW_FILE))
    deleteStaleSignalFiles(dir, FileTime.from(lockAttemptTime))
    assertEquals(
      listOf("other.tmp", SHOW_FILE, "${SHOW_FILE}456$SHOW_TMP_SUFFIX", "simplex.started", "simplex_v1_chat.db"),
      fileNames(dir),
      "an old temp file and a taken file must go; a fresh temp file is a signal being written, and the lock and database stay"
    )
  }

  @Test
  fun deleteStaleSignalFilesKeepsASignalWithinTheToleranceAndDropsAnOlderOne() = withTempDir { dir ->
    val lockAttemptTime = Instant.now().truncatedTo(ChronoUnit.SECONDS)
    val show = dir.resolve(SHOW_FILE)
    signalRunningInstance(dir, BADGE_LINK)
    Files.setLastModifiedTime(show, FileTime.from(lockAttemptTime.minus(FILE_TIME_TOLERANCE)))
    deleteStaleSignalFiles(dir, FileTime.from(lockAttemptTime))
    assertEquals(listOf(SHOW_FILE), fileNames(dir), "a signal exactly at the tolerance must be kept")

    Files.setLastModifiedTime(show, FileTime.from(lockAttemptTime.minusSeconds(1)))
    deleteStaleSignalFiles(dir, FileTime.from(lockAttemptTime))
    assertEquals(listOf(SHOW_FILE), fileNames(dir), "a signal one second behind the lock attempt must be kept")

    Files.setLastModifiedTime(show, FileTime.from(lockAttemptTime.minus(FILE_TIME_TOLERANCE).minusSeconds(1)))
    deleteStaleSignalFiles(dir, FileTime.from(lockAttemptTime))
    assertEquals(listOf(), fileNames(dir), "a signal older than the tolerance must be deleted")
  }

  @Test
  fun watcherTakesASignalWrittenBeforeItsWatch() = withTempDir { dir ->
    signalRunningInstance(dir, BADGE_LINK)
    watch(dir) { signals ->
      assertEquals(BADGE_LINK, signals.poll(SIGNAL_WAIT_SECONDS, TimeUnit.SECONDS)?.appLink, "a signal written before the watch raised no event")
    }
  }

  @Test
  fun watcherTakesEverySignalArrivingWhileItWatches() = withTempDir { dir ->
    assumeWatchServiceReportsEachCreation(dir)
    watch(dir) { signals ->
      // Each signal is written after the previous one arrived, so later ones can come only from watch events.
      signalRunningInstance(dir, BADGE_LINK)
      assertEquals(BADGE_LINK, signals.poll(SIGNAL_WAIT_SECONDS, TimeUnit.SECONDS)?.appLink, "first signal")
      signalRunningInstance(dir, OTHER_BADGE_LINK)
      assertEquals(OTHER_BADGE_LINK, signals.poll(SIGNAL_WAIT_SECONDS, TimeUnit.SECONDS)?.appLink, "second signal, seen as a creation")
      signalRunningInstance(dir, BADGE_LINK)
      assertEquals(BADGE_LINK, signals.poll(SIGNAL_WAIT_SECONDS, TimeUnit.SECONDS)?.appLink, "third signal, after the watch key was reset")
    }
  }

  @Test
  fun signalIsWrittenThroughATempFileBesideIt() = withTempDir { dir ->
    // a temp file elsewhere may sit on another file system, where the atomic move fails and the link is dropped
    assumeWatchServiceReportsEachCreation(dir)
    val created = dir.fileSystem.newWatchService().use { ws ->
      dir.register(ws, StandardWatchEventKinds.ENTRY_CREATE)
      signalRunningInstance(dir, BADGE_LINK)
      ws.poll(SIGNAL_WAIT_SECONDS, TimeUnit.SECONDS)?.pollEvents()?.map { it.context().toString() }.orEmpty()
    }
    assertTrue(
      created.any { it != TAKEN_SHOW_FILE && it.startsWith(SHOW_FILE) && it.endsWith(SHOW_TMP_SUFFIX) },
      "a temp file must be created in the signal directory, created: $created"
    )
  }

  // macOS polls the directory and keeps a taken name cached, so it misses short-lived files and reports a reused name as a modification
  private fun assumeWatchServiceReportsEachCreation(dir: Path) =
    assumeFalse("needs a watch service that reports each creation", dir.fileSystem.newWatchService().use { it.javaClass.simpleName == "PollingWatchService" })

  private fun watch(dir: Path, test: (signals: LinkedBlockingQueue<ShowSignal>) -> Unit) {
    val signals = LinkedBlockingQueue<ShowSignal>()
    val watcherThread = dir.fileSystem.newWatchService().use { ws ->
      dir.register(ws, StandardWatchEventKinds.ENTRY_CREATE)
      thread(isDaemon = true) { watchShowSignals(ws, dir) { signals.put(it) } }.also {
        test(signals)
        // a second, duplicate delivery would arrive within this wait
        assertNull(signals.poll(1, TimeUnit.SECONDS), "each signal is taken once")
      }
    }
    watcherThread.join(TimeUnit.SECONDS.toMillis(SIGNAL_WAIT_SECONDS))
    assertFalse(watcherThread.isAlive, "closing the watch service ends the watcher")
  }

  private fun fileNames(dir: Path): List<String> =
    Files.list(dir).use { files -> files.map { it.fileName.toString() }.sorted().toList() }
}
