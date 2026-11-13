package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.platform.appFilesDir
import java.io.File
import java.util.UUID
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class FileCleanupTest {
  @Test
  fun testItemWithoutStoredFileRemovesNothing() {
    withAppFile { name, file ->
      ChatController.cleanupFile(fileItem(fileName = name, filePath = null))
      assertTrue(file.exists())
    }
  }

  @Test
  fun testItemStoredFileRemoved() {
    withAppFile { name, file ->
      ChatController.cleanupFile(fileItem(fileName = "received-$name", filePath = name))
      assertFalse(file.exists())
    }
  }

  private fun withAppFile(test: (String, File) -> Unit) {
    val name = "${UUID.randomUUID()}.pdf"
    val file = File(appFilesDir, name)
    appFilesDir.mkdirs()
    file.writeText("")
    try {
      test(name, file)
    } finally {
      file.delete()
    }
  }

  private fun fileItem(fileName: String, filePath: String?): AChatItem =
    AChatItem(
      ChatInfo.Direct.sampleData,
      ChatItem.getFileMsgContentSample(fileName = fileName)
        .copy(file = CIFile.getSample(fileName = fileName, filePath = filePath, fileStatus = CIFileStatus.RcvCancelled))
    )
}
