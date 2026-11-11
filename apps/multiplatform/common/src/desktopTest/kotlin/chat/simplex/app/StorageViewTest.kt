package chat.simplex.app

import chat.simplex.common.views.usersettings.rootUsage
import java.nio.file.Files
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotNull
import kotlin.test.assertNull

class StorageViewTest {
  @Test
  fun testFolderEntryListsFolderContents() = withTempDir { root ->
    val tempFiles = Files.createDirectory(root.resolve("temp_files"))
    val rcvFolder = Files.createDirectory(tempFiles.resolve("20261110_224830_434191_rcv.xftp"))
    Files.write(rcvFolder.resolve("xftp.decrypted"), ByteArray(3000))
    Files.write(root.resolve("files_chat.db"), ByteArray(1000))

    val entries = rootUsage(root).entries.associateBy { it.name }
    assertNull(entries.getValue("files_chat.db").dir)
    val folder = assertNotNull(entries.getValue("temp_files").dir)
    val inner = rootUsage(folder).entries.single()
    assertEquals("20261110_224830_434191_rcv.xftp", inner.name)
    assertEquals(3000L, inner.bytes)
  }
}
