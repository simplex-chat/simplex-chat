package chat.simplex.app

import androidx.compose.runtime.MutableState
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.input.key.Key
import androidx.compose.ui.semantics.SemanticsProperties
import androidx.compose.ui.test.ComposeUiTest
import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.SemanticsMatcher
import androidx.compose.ui.test.hasClickAction
import androidx.compose.ui.test.hasSetTextAction
import androidx.compose.ui.test.onNodeWithContentDescription
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.compose.ui.test.performKeyInput
import androidx.compose.ui.test.performTextClearance
import androidx.compose.ui.test.performTextInput
import androidx.compose.ui.test.performTextInputSelection
import androidx.compose.ui.test.performTextReplacement
import androidx.compose.ui.test.pressKey
import androidx.compose.ui.test.requestFocus
import androidx.compose.ui.test.runComposeUiTest
import androidx.compose.ui.text.TextRange
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.chatParseMarkdown
import chat.simplex.common.platform.chatSendCmdRetry
import chat.simplex.common.views.chat.ComposePreview
import chat.simplex.common.views.chat.ComposeState
import chat.simplex.common.views.chat.ComposeView
import chat.simplex.common.views.chat.processPickedFile
import chat.simplex.common.views.helpers.AlertManager
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.*
import java.nio.file.Path
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import kotlin.test.*

@OptIn(ExperimentalTestApi::class)
class LinkPreviewTest {
  private val sendCmd = chatSendCmdRetry
  private val parseMarkdown = chatParseMarkdown
  private val testPrefs = mapOf(
    appPrefs.privacyLinkPreviews to true,
    appPrefs.privacySanitizeLinks to false,
    appPrefs.developerTools to false,
    appPrefs.showLinkPreviewErrors to false,
    appPrefs.privacySaveLastDraft to true,
  )
  private val savedPrefs = testPrefs.keys.associateWith { it.get() }
  private val requests = mutableListOf<String>()
  private val unrecorded = mutableListOf<String>()
  private val recording = Json.parseToJsonElement(fixtureText("previews/link_preview")).jsonObject
  private val commands = recording.getValue("commands").jsonObject
  private val markdown = recording.getValue("markdown").jsonObject

  @BeforeTest
  fun setUp() {
    testPrefs.forEach { (pref, value) -> pref.set(value) }
    chatSendCmdRetry = { _, cmd, _ -> respond(cmd) }
    chatParseMarkdown = { text -> if (text.isEmpty()) "{}" else markdown[text]?.toString() ?: unrecordedRequest("markdown $text") }
  }

  @AfterTest
  fun tearDown() {
    AlertManager.shared.hideAllAlerts()
    ChatModel.chatsContext.chats.value.clear()
    ChatModel.chatsContext.chatItems.value.clear()
    ChatModel.presetTags.clear()
    ChatModel.chatId.value = null
    ChatModel.draft.value = null
    ChatModel.draftChatId.value = null
    chatSendCmdRetry = sendCmd
    chatParseMarkdown = parseMarkdown
    savedPrefs.forEach { (pref, value) -> pref.set(value) }
    assertEquals(listOf(), unrecorded.toList(), "requests without recorded responses")
  }

  @Test
  fun recordedPreviewIsReturned() {
    val (request, response) = recorded("result")
    val url = request.removePrefix(LINK_PREVIEW)
    val preview = runBlocking { ChatController.apiGetLinkPreview(url) }
    assertEquals(listOf(request), requests)
    assertEquals(url, preview?.uri)
    assertNotEquals("", recordedPreview(response).text("description"))
    assertEquals(listOf("title", "description", "image").map { recordedPreview(response).text(it) }, listOf(preview?.title, preview?.description, preview?.image))
  }

  @Test
  fun typedLinkShowsPreviewWithoutAlert() = composeView { composeState ->
    val (request, response) = recorded("result")
    onNode(hasSetTextAction()).performTextInput(request.removePrefix(LINK_PREVIEW))
    waitUntil(timeoutMillis = 10_000) { composeState.value.linkPreview != null }
    onNodeWithText(recordedPreview(response).text("title")).assertExists()
    assertEquals(listOf(request), requests)
    assertFalse(AlertManager.shared.hasAlertsShown())
  }

  @Test
  fun failedLinkShowsNoPreview() = composeView { composeState ->
    val (request) = recorded("error")
    onNode(hasSetTextAction()).performTextInput(request.removePrefix(LINK_PREVIEW))
    awaitFailedFetches(composeState, 1)
    assertEquals(listOf(request), requests)
    assertFalse(AlertManager.shared.hasAlertsShown())
  }

  @Test
  fun failedLinkIsNotFetchedAgain() = composeView { composeState ->
    val (request) = recorded("error")
    val field = onNode(hasSetTextAction())
    field.performTextInput(request.removePrefix(LINK_PREVIEW))
    awaitFailedFetches(composeState, 1)
    field.performTextInputSelection(TextRange.Zero)
    runCatching { waitUntil(timeoutMillis = 3_000) { synchronized(requests) { requests.size > 1 } } }
    assertEquals(listOf(request), requests)
  }

  @Test
  fun failedLinkIsFetchedAgainAfterTheTextIsCleared() = composeView { composeState ->
    val (request) = recorded("error")
    val field = onNode(hasSetTextAction())
    field.performTextInput(request.removePrefix(LINK_PREVIEW))
    awaitFailedFetches(composeState, 1)
    field.performTextClearance()
    field.performTextInput(request.removePrefix(LINK_PREVIEW))
    awaitFailedFetches(composeState, 2)
    assertEquals(listOf(request, request), requests)
  }

  @Test
  fun failedLinkIsFetchedAgainInAnotherChat() = composeView { composeState ->
    val (request) = recorded("error")
    val field = onNode(hasSetTextAction())
    field.performTextInput(request.removePrefix(LINK_PREVIEW))
    awaitFailedFetches(composeState, 1)
    ChatModel.chatId.value = "@2"
    waitForIdle()
    field.performTextInput(request.removePrefix(LINK_PREVIEW))
    awaitFailedFetches(composeState, 2)
    assertEquals(listOf(request, request), requests)
  }

  @Test
  fun fileAttachedBeforeTheFetchKeepsItsPreview() = composeView { composeState ->
    val (request) = recorded("result")
    onNode(hasSetTextAction()).performTextInput(request.removePrefix(LINK_PREVIEW))
    withTempDir { dir ->
      composeState.attachFile(dir)
      runCatching { waitUntil(timeoutMillis = 3_000) { synchronized(requests) { requests.isNotEmpty() } } }
      waitForIdle()
      assertTrue(composeState.value.preview is ComposePreview.FilePreview)
      assertEquals(listOf(), requests)
    }
  }

  @Test
  fun fileAttachedDuringTheFetchKeepsItsPreview() = composeView { composeState ->
    val (request) = recorded("result")
    val release = heldResponses()
    onNode(hasSetTextAction()).performTextInput(request.removePrefix(LINK_PREVIEW))
    waitUntil(timeoutMillis = 10_000) { synchronized(requests) { requests.isNotEmpty() } }
    withTempDir { dir ->
      composeState.attachFile(dir)
      release.countDown()
      runCatching { waitUntil(timeoutMillis = 3_000) { composeState.value.preview is ComposePreview.CLinkPreview } }
      assertTrue(composeState.value.preview is ComposePreview.FilePreview)
      assertEquals(listOf(request), requests)
    }
  }

  @Test
  fun linkDeletedBeforeTheFetchGetsNoPreview() = composeView { composeState ->
    val (request) = recorded("result")
    val field = onNode(hasSetTextAction())
    field.performTextInput(request.removePrefix(LINK_PREVIEW))
    field.performTextReplacement(TEXT_WITHOUT_LINK)
    runCatching { waitUntil(timeoutMillis = 3_000) { synchronized(requests) { requests.isNotEmpty() } } }
    waitForIdle()
    assertTrue(composeState.value.preview is ComposePreview.NoPreview)
    assertEquals(listOf(), requests)
  }

  @Test
  fun linkDeletedDuringTheFetchGetsNoPreview() = composeView { composeState ->
    val (request) = recorded("result")
    val release = heldResponses()
    val field = onNode(hasSetTextAction())
    field.performTextInput(request.removePrefix(LINK_PREVIEW))
    waitUntil(timeoutMillis = 10_000) { synchronized(requests) { requests.isNotEmpty() } }
    field.performTextReplacement(TEXT_WITHOUT_LINK)
    release.countDown()
    runCatching { waitUntil(timeoutMillis = 3_000) { composeState.value.linkPreview != null } }
    assertTrue(composeState.value.preview is ComposePreview.NoPreview)
    assertEquals(listOf(request), requests)
  }

  @Test
  fun linkReplacedDuringTheFetchKeepsPreviewOfTheNewLink() = composeView { composeState ->
    val (failed) = recorded("error")
    val (shown) = recorded("result")
    val release = heldResponses { it == failed }
    val field = onNode(hasSetTextAction())
    field.performTextInput(failed.removePrefix(LINK_PREVIEW))
    waitUntil(timeoutMillis = 10_000) { synchronized(requests) { requests.isNotEmpty() } }
    field.performTextReplacement(shown.removePrefix(LINK_PREVIEW))
    waitUntil(timeoutMillis = 10_000) { composeState.value.linkPreview != null }
    release.countDown()
    runCatching { waitUntil(timeoutMillis = 3_000) { composeState.value.linkPreview == null } }
    assertEquals(shown.removePrefix(LINK_PREVIEW), composeState.value.linkPreview?.uri)
    assertEquals(listOf(failed, shown), requests)
  }

  @Test
  fun linkRestoredAfterAnotherLinkGetsItsPreview() = composeView { composeState ->
    val (shown) = recorded("result")
    val (failed) = recorded("error")
    val field = onNode(hasSetTextAction())
    field.performTextInput(shown.removePrefix(LINK_PREVIEW))
    waitUntil(timeoutMillis = 10_000) { composeState.value.linkPreview != null }
    field.performTextReplacement(failed.removePrefix(LINK_PREVIEW))
    field.performTextReplacement(shown.removePrefix(LINK_PREVIEW))
    waitUntil(timeoutMillis = 10_000) { synchronized(requests) { requests.size == 2 } && composeState.value.linkPreview != null }
    assertEquals(listOf(shown, shown), requests)
  }

  @Test
  fun linkChangedAndRestoredBeforeTheFetchIsRequestedOnce() = composeView { composeState ->
    val (shown) = recorded("result")
    val (failed) = recorded("error")
    val release = heldResponses()
    val field = onNode(hasSetTextAction())
    field.performTextInput(shown.removePrefix(LINK_PREVIEW))
    field.performTextReplacement(failed.removePrefix(LINK_PREVIEW))
    field.performTextReplacement(shown.removePrefix(LINK_PREVIEW))
    waitUntil(timeoutMillis = 10_000) { synchronized(requests) { requests.isNotEmpty() } }
    runCatching { waitUntil(timeoutMillis = 3_000) { synchronized(requests) { requests.size > 1 } } }
    release.countDown()
    waitUntil(timeoutMillis = 10_000) { composeState.value.linkPreview != null }
    assertEquals(listOf(shown), requests)
  }

  @Test
  fun editedMessageKeepsItsPreviewWhenThePendingLinkResponds() = composeView { composeState ->
    val (request) = recorded("result")
    val release = heldResponses()
    onNode(hasSetTextAction()).performTextInput(request.removePrefix(LINK_PREVIEW))
    waitUntil(timeoutMillis = 10_000) { synchronized(requests) { requests.isNotEmpty() } }
    val note = recordedNote()
    val edited = assertIs<MsgContent.MCLink>(note.content.msgContent).preview
    composeState.value = ComposeState.edit(note, useLinkPreviews = true, previewsOn = true)
    release.countDown()
    runCatching { waitUntil(timeoutMillis = 3_000) { composeState.value.linkPreview !== edited } }
    assertSame(edited, composeState.value.linkPreview)
    assertEquals(listOf(request), requests)
  }

  @Test
  fun editedMessageIsSavedWithThePreviewOfItsSecondLink() = composeView(noteFolder()) {
    val note = editLastNote()
    onNodeWithText(assertIs<MsgContent.MCLink>(note.content.msgContent).preview.title).assertExists()
    onNodeWithContentDescription("Send Message").performClick()
    assertEquals(createdContent(), updatedContent())
  }

  @Test
  fun editedMessageKeepsThePreviewOfItsSecondLinkWhenTheTextChanges() = composeView(noteFolder()) { composeState ->
    val note = editLastNote()
    val edited = assertIs<MsgContent.MCLink>(note.content.msgContent).preview
    onNode(hasSetTextAction()).performTextInput(" ")
    runCatching { waitUntil(timeoutMillis = 3_000) { synchronized(requests) { requests.isNotEmpty() } } }
    assertSame(edited, composeState.value.linkPreview)
    assertEquals(listOf(), requests)
    onNodeWithContentDescription("Send Message").performClick()
    val updated = updatedContent()
    assertEquals(createdContent().getValue("preview"), updated.getValue("preview"))
    assertEquals("${note.content.text} ", updated.text("text"))
  }

  @Test
  fun failedLinkIsFetchedAgainAfterTheDeleteTextButton() = composeView { composeState ->
    val (request) = recorded("error")
    val field = onNode(hasSetTextAction())
    field.performTextInput(request.removePrefix(LINK_PREVIEW) + "\n\n\n")
    awaitFailedFetches(composeState, 1)
    onNode(hasClickAction() and !hasSetTextAction() and SemanticsMatcher.keyNotDefined(SemanticsProperties.ContentDescription)).performClick()
    field.performTextInput(request.removePrefix(LINK_PREVIEW))
    awaitFailedFetches(composeState, 2)
    assertEquals(listOf(request, request), requests)
  }

  @Test
  fun failedLinkIsNotFetchedAgainAfterAFileIsAttachedAndRemoved() = composeView { composeState ->
    val (request) = recorded("error")
    val field = onNode(hasSetTextAction())
    field.performTextInput(request.removePrefix(LINK_PREVIEW))
    awaitFailedFetches(composeState, 1)
    withTempDir { dir ->
      composeState.attachFile(dir)
      field.performTextInputSelection(TextRange.Zero)
      onNodeWithContentDescription("Cancel file preview").performClick()
      field.performTextInputSelection(TextRange(request.removePrefix(LINK_PREVIEW).length))
      runCatching { waitUntil(timeoutMillis = 3_000) { synchronized(requests) { requests.size > 1 } } }
      assertTrue(composeState.value.preview is ComposePreview.NoPreview)
      assertEquals(listOf(request), requests)
    }
  }

  @Test
  fun draftSavedDuringTheFetchGetsThePreviewWhenTheChatIsOpenedAgain() = composeView { composeState ->
    val (request) = recorded("result")
    val release = heldResponses()
    onNode(hasSetTextAction()).performTextInput(request.removePrefix(LINK_PREVIEW))
    waitUntil(timeoutMillis = 10_000) { synchronized(requests) { requests.isNotEmpty() } }
    ChatModel.chatId.value = "@2"
    waitForIdle()
    release.countDown()
    ChatModel.chatId.value = Chat.sampleData.id
    waitUntil(timeoutMillis = 10_000) { composeState.value.linkPreview != null }
    assertEquals(listOf(request, request), requests)
  }

  @Test
  fun messageWithFailedFirstLinkIsSentWithPreviewOfSecondLink() = composeView(noteFolder()) { composeState ->
    val (failed) = recorded("error")
    val (shown) = recorded("result")
    sendTwoLinks(composeState, failed, shown)
    assertEquals(listOf(failed, shown, createdItem()), requests)
  }

  @Test
  fun failedLinkIsFetchedAgainAfterTheMessageIsSent() = composeView(noteFolder()) { composeState ->
    val (failed) = recorded("error")
    val (shown) = recorded("result")
    sendTwoLinks(composeState, failed, shown)
    onNode(hasSetTextAction()).performTextInput(failed.removePrefix(LINK_PREVIEW))
    awaitFailedFetches(composeState, 4)
    assertEquals(listOf(failed, shown, createdItem(), failed), requests)
  }

  @Test
  fun failureShowsNoAlert() = failedPreview(developerTools = false, showLinkPreviewErrors = false) {
    assertTexts()
  }

  @Test
  fun failureShowsNoAlertWithDeveloperOptionsOnly() = failedPreview(developerTools = true, showLinkPreviewErrors = false) {
    assertTexts()
  }

  @Test
  fun failureShowsNoAlertWithoutDeveloperOptions() = failedPreview(developerTools = false, showLinkPreviewErrors = true) {
    assertTexts()
  }

  @Test
  fun failureShowsCoreErrorWithBothOptions() = failedPreview(developerTools = true, showLinkPreviewErrors = true) { message ->
    assertTexts("Link preview error", message, "OK")
  }

  private fun failedPreview(developerTools: Boolean, showLinkPreviewErrors: Boolean, test: ComposeUiTest.(message: String) -> Unit) = runComposeUiTest {
    appPrefs.developerTools.set(developerTools)
    appPrefs.showLinkPreviewErrors.set(showLinkPreviewErrors)
    setContent { AlertManager.shared.showInView() }
    val (request, response) = recorded("error")
    assertNull(runBlocking { ChatController.apiGetLinkPreview(request.removePrefix(LINK_PREVIEW)) })
    assertEquals(listOf(request), requests)
    waitForIdle()
    test(recordedMessage(response))
  }

  private fun ComposeUiTest.awaitFailedFetches(composeState: MutableState<ComposeState>, count: Int) =
    waitUntil(timeoutMillis = 10_000) { synchronized(requests) { requests.size == count } && composeState.value.preview is ComposePreview.NoPreview }

  private fun ComposeUiTest.assertTexts(vararg texts: String) {
    val shown = onAllNodes(SemanticsMatcher.keyIsDefined(SemanticsProperties.Text), useUnmergedTree = true)
      .fetchSemanticsNodes()
      .flatMap { node -> node.config[SemanticsProperties.Text].map { it.text } }
    assertEquals(texts.toList(), shown)
  }

  private fun ComposeUiTest.sendTwoLinks(composeState: MutableState<ComposeState>, failed: String, shown: String) {
    val field = onNode(hasSetTextAction())
    field.performTextInput("${failed.removePrefix(LINK_PREVIEW)} ${shown.removePrefix(LINK_PREVIEW)}")
    runCatching { waitUntil(timeoutMillis = 10_000) { composeState.value.linkPreview != null } }
    field.performTextInputSelection(TextRange.Zero)
    waitUntil(timeoutMillis = 10_000) { composeState.value.linkPreview != null }
    onNodeWithContentDescription("Send Message").performClick()
    waitUntil(timeoutMillis = 10_000) { composeState.value.message.text.isEmpty() }
  }

  private fun composeView(chat: Chat = Chat.sampleData, test: ComposeUiTest.(MutableState<ComposeState>) -> Unit) = runComposeUiTest {
    val composeState = mutableStateOf(ComposeState(useLinkPreviews = true))
    ChatModel.chatId.value = chat.id
    setContent {
      ComposeView(rhId = null, chatModel = ChatModel, ChatModel.chatsContext, chat, composeState, remember { mutableStateOf(false) }, remember { mutableStateOf(null) }, {}, remember { FocusRequester() })
      AlertManager.shared.showInView()
    }
    test(composeState)
  }

  private fun respond(cmd: String, release: CountDownLatch? = null): String {
    val request = recordedRequest(cmd)
    synchronized(requests) { requests += request ?: cmd }
    release?.await(10, TimeUnit.SECONDS)
    return request?.let { commands.getValue(it).toString() } ?: unrecordedRequest(cmd)
  }

  private fun recordedRequest(cmd: String): String? =
    if (cmd in commands) cmd
    else commands.keys.firstOrNull { it.startsWith(CREATE_ITEM) && cmd.startsWith(CREATE_ITEM) && createdMessages(it) == createdMessages(cmd) }

  private fun createdMessages(cmd: String): JsonElement = Json.parseToJsonElement(cmd.removePrefix(CREATE_ITEM))

  private fun createdItem(): String = commands.keys.single { it.startsWith(CREATE_ITEM) }

  private fun createdNote(): JsonObject =
    commands.getValue(createdItem()).jsonObject.getValue("result").jsonObject.getValue("chatItems").jsonArray.first().jsonObject

  private fun noteFolder(): Chat =
    Chat(remoteHostId = null, chatInfo = json.decodeFromJsonElement(ChatInfo.serializer(), createdNote().getValue("chatInfo")), chatItems = listOf())

  private fun recordedNote(): ChatItem = json.decodeFromJsonElement(ChatItem.serializer(), createdNote().getValue("chatItem"))

  private fun ComposeUiTest.editLastNote(): ChatItem {
    val note = recordedNote()
    ChatModel.chatsContext.chatItems.replaceAll(listOf(note))
    onNode(hasSetTextAction()).requestFocus().performKeyInput { pressKey(Key.DirectionUp) }
    return note
  }

  private fun createdContent(): JsonObject = createdMessages(createdItem()).jsonArray.single().jsonObject.getValue("msgContent").jsonObject

  private fun ComposeUiTest.updatedContent(): JsonObject {
    waitUntil(timeoutMillis = 10_000) { synchronized(unrecorded) { unrecorded.isNotEmpty() } }
    val update = synchronized(unrecorded) { unrecorded.single().also { unrecorded.clear() } }
    assertTrue(update.startsWith(UPDATE_ITEM), update)
    return Json.parseToJsonElement(update.removePrefix(UPDATE_ITEM)).jsonObject.getValue("msgContent").jsonObject
  }

  private fun heldResponses(held: (String) -> Boolean = { true }): CountDownLatch {
    val release = CountDownLatch(1)
    chatSendCmdRetry = { _, cmd, _ -> respond(cmd, release.takeIf { held(cmd) }) }
    return release
  }

  private fun MutableState<ComposeState>.attachFile(dir: Path) =
    processPickedFile(dir.resolve("notes.txt").toFile().apply { writeText("notes") }.toURI(), null)

  private fun recorded(outcome: String): Pair<String, JsonObject> =
    commands.entries
      .first { (cmd, response) -> cmd.startsWith(LINK_PREVIEW) && outcome in response.jsonObject }
      .let { (cmd, response) -> cmd to response.jsonObject }

  private fun recordedPreview(response: JsonObject): JsonObject =
    response.getValue("result").jsonObject.getValue("linkPreview").jsonObject

  private fun recordedMessage(response: JsonObject): String {
    val errorType = response.getValue("error").jsonObject.getValue("errorType").jsonObject
    assertEquals("linkPreview", errorType.text("type"))
    return errorType.text("message")
  }

  private fun JsonObject.text(key: String): String = getValue(key).jsonPrimitive.content

  private fun unrecordedRequest(request: String): String {
    synchronized(unrecorded) { unrecorded += request }
    return "{}"
  }

  private companion object {
    const val LINK_PREVIEW = "/_link preview "
    const val CREATE_ITEM = "/_create *1 json "
    const val UPDATE_ITEM = "/_update item *1 1 live=off json "
    const val TEXT_WITHOUT_LINK = "text without a link"
  }
}
