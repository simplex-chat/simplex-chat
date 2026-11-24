package chat.simplex.app

import androidx.compose.runtime.mutableStateOf
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.chatSendCmdRetry
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.migration.MigrationToState
import chat.simplex.common.views.migration.checkUserLink
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import kotlin.test.AfterTest
import kotlin.test.Test
import kotlin.test.assertEquals

class MigrationLinkTest {
  private val sendCmd = chatSendCmdRetry
  private val stage = appPrefs.migrationToStage.get()

  @AfterTest
  fun tearDown() {
    chatSendCmdRetry = sendCmd
    appPrefs.migrationToStage.set(stage)
    AlertManager.shared.hideAllAlerts()
  }

  @Test
  fun acceptsAFileLinkWithoutTheSlashAfterTheScheme() {
    val commands = Json.parseToJsonElement(fixtureText("files/download-info")).jsonObject.getValue("commands").jsonObject
    chatSendCmdRetry = { _, cmd, _ -> commands.getValue(cmd).toString() }
    val state = mutableStateOf<MigrationToState?>(MigrationToState.PasteOrScanLink)
    runBlocking { state.checkUserLink(LINK) }
    assertEquals(LINK, (state.value as? MigrationToState.Onion)?.link)
  }

  private companion object {
    const val LINK = "simplex:file#/?desc=DESCRIPTION&data=%7B%22networkConfig%22%3A%7B%22hostMode%22%3A%22onion%22%2C%22requiredHostMode%22%3Atrue%7D%7D"
  }
}
