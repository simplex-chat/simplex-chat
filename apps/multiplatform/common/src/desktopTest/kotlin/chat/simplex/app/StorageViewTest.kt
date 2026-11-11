package chat.simplex.app

import chat.simplex.common.views.usersettings.rootUsage
import java.nio.file.FileSystemException
import java.nio.file.Files
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull
import org.junit.Assume.assumeNoException

class StorageViewTest {
  @Test
  fun testOnlyFoldersOtherThanClosedFoldersOpen() = withTempDir { root ->
    val tempFiles = Files.createDirectory(root.resolve("temp_files"))
    val appFiles = Files.createDirectory(root.resolve("app_files"))
    Files.createFile(root.resolve("files_chat.db"))

    val entries = rootUsage(root, closedDirs = listOf(appFiles)).entries.associateBy { it.name }
    assertEquals(tempFiles, entries.getValue("temp_files").dir)
    assertNull(entries.getValue("app_files").dir)
    assertNull(entries.getValue("files_chat.db").dir)
  }

  @Test
  fun testSymlinkToFolderDoesNotOpen() = withTempDir { root ->
    val tempFiles = Files.createDirectory(root.resolve("temp_files"))
    try {
      Files.createSymbolicLink(root.resolve("lib"), tempFiles)
    } catch (e: FileSystemException) {
      assumeNoException("symbolic links cannot be created here", e)
      return@withTempDir
    }

    val entries = rootUsage(root, closedDirs = emptyList()).entries.associateBy { it.name }
    assertNull(entries.getValue("lib").dir)
  }
}
