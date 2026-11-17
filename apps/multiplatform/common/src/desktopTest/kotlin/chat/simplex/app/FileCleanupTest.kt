package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.platform.appFilesDir
import chat.simplex.common.platform.chatSendCmdRetry
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import java.io.File
import java.util.UUID
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertNotNull
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

  @Test
  fun testCancelledFileNotAcceptedRemovesNothing() {
    withAppFile("test.pdf") { _, file ->
      val commands = Json.parseToJsonElement(File("../../../tests/fixtures/api/files/cancel-not-accepted.json").readText()).jsonObject.getValue("commands").jsonObject
      val sendCmd = chatSendCmdRetry
      chatSendCmdRetry = { _, cmd, _ -> commands.getValue(cmd).toString() }
      try {
        ChatController.cleanupFile(assertNotNull(runBlocking { ChatController.apiCancelFile(null, 1) }))
      } finally {
        chatSendCmdRetry = sendCmd
      }
      assertTrue(file.exists())
    }
  }

  private fun withAppFile(name: String = "${UUID.randomUUID()}.pdf", test: (String, File) -> Unit) {
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
