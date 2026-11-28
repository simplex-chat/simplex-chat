package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.platform.chatParseMarkdown
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.helpers.parseToMarkdown
import kotlinx.serialization.json.*
import java.net.URI
import kotlin.test.*

class LinkPreviewStateTest {
  private val parseMarkdown = chatParseMarkdown
  private val recording = Json.parseToJsonElement(fixtureText("previews/link_preview")).jsonObject
  private val commands = recording.getValue("commands").jsonObject
  private val markdown = recording.getValue("markdown").jsonObject
  private val preview = assertNotNull(recordedPreview(PAGE))
  private val file = ComposePreview.FilePreview("notes.txt", URI("file:///notes.txt"))
  private val empty = ComposeState(useLinkPreviews = true)
  private val note = recordedNote()

  @BeforeTest
  fun setUp() {
    chatParseMarkdown = { text -> if (text.isEmpty()) "{}" else markdown.getValue(text).toString() }
  }

  @AfterTest
  fun tearDown() {
    chatParseMarkdown = parseMarkdown
  }

  @Test
  fun emptyTextClearsCancelledLinks() {
    val failed = empty.typed(FAILED).fetched()
    assertEquals(setOf(FAILED), failed.cancelledLinks)
    val cleared = failed.typed("")
    assertEquals(setOf(), cleared.cancelledLinks)
    assertSame(ComposePreview.NoPreview, cleared.preview)
    val attached = failed.copy(preview = file).typed("")
    assertEquals(setOf(), attached.cancelledLinks)
    assertSame(file, attached.preview)
  }

  @Test
  fun sameLinkKeepsItsStatus() {
    val loading = empty.typed(TWO_LINKS).fetched()
    assertSame(loading.preview, loading.typed("$TWO_LINKS ").preview)
    val shown = loading.fetched()
    assertSame(shown.preview, shown.typed("$TWO_LINKS ").preview)
  }

  @Test
  fun anotherLinkIsRequested() {
    val loading = empty.typed(TEXT_WITHOUT_LINK).typed(PAGE)
    assertEquals(PAGE, loading.loadingLink?.uri)
    val replaced = loading.typed(FAILED)
    assertEquals(FAILED, replaced.loadingLink?.uri)
    assertNotEquals(loading.loadingLink?.request, replaced.loadingLink?.request)
    assertEquals(FAILED, loading.fetched().typed(FAILED).loadingLink?.uri)
  }

  @Test
  fun linkRemovedOrPreviewsOffRemovesThePreview() {
    val loading = empty.typed(PAGE)
    for (s in listOf(loading, loading.fetched())) {
      assertSame(ComposePreview.NoPreview, s.typed(TEXT_WITHOUT_LINK).preview)
      assertSame(ComposePreview.NoPreview, s.typed(PAGE, previewsOn = false).preview)
    }
  }

  @Test
  fun textChangeKeepsAttachmentAndCancelledLinks() {
    val attached = empty.typed(FAILED).fetched().copy(preview = file).typed(TWO_LINKS)
    assertSame(file, attached.preview)
    assertEquals(setOf(FAILED), attached.cancelledLinks)
  }

  @Test
  fun fetchedPreviewIsShown() {
    assertEquals(preview.uri, empty.typed(PAGE).fetched().linkPreview?.uri)
  }

  @Test
  fun responseKeepsTheSendInProgress() {
    assertTrue(empty.typed(PAGE).copy(inProgress = true).fetched().inProgress)
    assertTrue(empty.typed(TWO_LINKS).copy(inProgress = true).fetched().inProgress)
  }

  @Test
  fun failedLinkIsCancelledAndTheNextLinkIsRequested() {
    val next = empty.typed(TWO_LINKS).fetched()
    assertEquals(setOf(FAILED), next.cancelledLinks)
    assertEquals(PAGE, next.loadingLink?.uri)
    assertSame(ComposePreview.NoPreview, empty.typed(FAILED).fetched().preview)
  }

  @Test
  fun responseOfAnotherRequestChangesNothing() {
    val loading = empty.typed(PAGE)
    val request = assertNotNull(loading.loadingLink).request
    for (s in listOf(empty, loading.typed(TEXT_WITHOUT_LINK), loading.typed(FAILED), loading.fetched(), loading.copy(preview = file))) {
      assertSame(s, s.updateLinkPreview(LinkPreviewEvent.Fetched(request, preview), true))
      assertSame(s, s.updateLinkPreview(LinkPreviewEvent.Fetched(request, null), true))
    }
  }

  @Test
  fun cancelledLinkSelectsTheNextLink() {
    val next = empty.typed(TWO_LINKS).updateLinkPreview(LinkPreviewEvent.Cancelled, true)
    assertEquals(setOf(FAILED), next.cancelledLinks)
    assertEquals(PAGE, next.loadingLink?.uri)
    val none = next.fetched().updateLinkPreview(LinkPreviewEvent.Cancelled, true)
    assertEquals(setOf(FAILED, PAGE), none.cancelledLinks)
    assertSame(ComposePreview.NoPreview, none.preview)
  }

  @Test
  fun chatChangeClearsCancelledLinks() {
    val changed = empty.typed(TWO_LINKS).fetched().fetched().updateLinkPreview(LinkPreviewEvent.ChatChanged, true)
    assertEquals(setOf(), changed.cancelledLinks)
    assertEquals(FAILED, changed.loadingLink?.uri)
    val shown = empty.typed(PAGE).fetched()
    assertSame(shown.preview, shown.updateLinkPreview(LinkPreviewEvent.ChatChanged, true).preview)
  }

  @Test
  fun restoredStateRequestsItsLinkAgain() {
    val loading = empty.typed(TWO_LINKS).fetched()
    val restored = json.decodeFromString(ComposeState.serializer(), json.encodeToString(ComposeState.serializer(), loading))
    assertEquals(PAGE, restored.loadingLink?.uri)
    assertNotEquals(loading.loadingLink?.request, restored.loadingLink?.request)
    assertEquals(setOf(FAILED), restored.cancelledLinks)
  }

  @Test
  fun editedMessageKeepsThePreviewOfItsSecondLink() {
    val edited = note.edited()
    assertSame(assertIs<MsgContent.MCLink>(note.content.msgContent).preview, edited.linkPreview)
    assertEquals(setOf(FAILED), edited.cancelledLinks)
  }

  @Test
  fun editedMessageChangesAsTheMessageItWasSentFrom() {
    val composed = empty.typed(TWO_LINKS).fetched().fetched()
    val edited = note.edited()
    assertSameLinkPreview(composed, edited)
    for (text in TEXTS) {
      assertSameLinkPreview(composed.typed(text, previewsOn = false), edited.typed(text, previewsOn = false))
      for (next in TEXTS) {
        assertSameLinkPreview(composed.typed(text).typed(next), edited.typed(text).typed(next))
      }
    }
    assertSameLinkPreview(composed.updateLinkPreview(LinkPreviewEvent.Cancelled, true), edited.updateLinkPreview(LinkPreviewEvent.Cancelled, true))
  }

  @Test
  fun chatChangeKeepsThePreviewOfAnEditedMessage() {
    val edited = note.edited()
    for (s in listOf(edited, edited.typed("$TWO_LINKS "))) {
      val changed = s.updateLinkPreview(LinkPreviewEvent.ChatChanged, true)
      assertSame(s.preview, changed.preview)
      assertEquals(setOf(FAILED), changed.cancelledLinks)
    }
    val reopened = edited.updateLinkPreview(LinkPreviewEvent.Cancelled, true).updateLinkPreview(LinkPreviewEvent.ChatChanged, true)
    assertEquals(PAGE, reopened.loadingLink?.uri)
    assertEquals(setOf(FAILED), reopened.cancelledLinks)
  }

  @Test
  fun editedMessageWithoutTheLinkOfItsPreviewSelectsTheLinkOfItsText() {
    val moved = note.withText(FAILED).edited()
    assertEquals(FAILED, moved.loadingLink?.uri)
    assertEquals(setOf(), moved.cancelledLinks)
    assertSame(ComposePreview.NoPreview, note.withText(TEXT_WITHOUT_LINK).edited().preview)
  }

  @Test
  fun editedMessageHasNoPreviewWithPreviewsOff() {
    for (item in listOf(note, note.withText(FAILED))) {
      assertSame(ComposePreview.NoPreview, item.edited(previewsOn = false).preview)
    }
  }

  @Test
  fun textChangeAppliedTwiceEqualsOnce() {
    for (s in reachableStates()) {
      val once = s.updateLinkPreview(LinkPreviewEvent.TextChanged, true)
      assertEquals(once, once.updateLinkPreview(LinkPreviewEvent.TextChanged, true))
    }
  }

  @Test
  fun shownPreviewIsOfTheSelectedLink() {
    for (s in reachableStates()) {
      val shown = s.linkPreview ?: continue
      assertEquals(shown.uri, s.getMessageLinks(parseToMarkdown(s.message.text)).first)
    }
  }

  private fun reachableStates(): List<ComposeState> =
    (TEXTS.map { empty.typed(it) } + listOf(note, note.withText(FAILED), note.withText(TEXT_WITHOUT_LINK)).map { it.edited() })
      .flatMap { first -> generateSequence(first) { s -> s.loadingLink?.let { s.fetched() } }.toList() }
      .flatMap { s ->
        listOf(s, s.copy(preview = file), s.updateLinkPreview(LinkPreviewEvent.Cancelled, true), s.updateLinkPreview(LinkPreviewEvent.ChatChanged, true)) +
          TEXTS.map { s.typed(it) }
      }

  private fun ComposeState.typed(text: String, previewsOn: Boolean = true): ComposeState =
    copy(message = ComposeMessage(text), parsedMessage = parseToMarkdown(text) ?: FormattedText.plain(text))
      .updateLinkPreview(LinkPreviewEvent.TextChanged, previewsOn)

  private fun ComposeState.fetched(): ComposeState {
    val loading = assertNotNull(loadingLink)
    return updateLinkPreview(LinkPreviewEvent.Fetched(loading.request, recordedPreview(loading.uri)), true)
  }

  private fun assertSameLinkPreview(expected: ComposeState, actual: ComposeState) {
    assertEquals(expected.loadingLink?.uri, actual.loadingLink?.uri)
    assertEquals(expected.linkPreview?.uri, actual.linkPreview?.uri)
    assertEquals(expected.cancelledLinks, actual.cancelledLinks)
  }

  private fun ChatItem.edited(previewsOn: Boolean = true): ComposeState = ComposeState.edit(this, useLinkPreviews = true, previewsOn = previewsOn)

  private fun ChatItem.withText(text: String): ChatItem =
    copy(content = CIContent.SndMsgContent(MsgContent.MCLink(text, preview)), formattedText = parseToMarkdown(text))

  private fun recordedPreview(uri: String): LinkPreview? =
    commands.getValue("/_link preview $uri").jsonObject["result"]?.let { json.decodeFromJsonElement(LinkPreview.serializer(), it.jsonObject.getValue("linkPreview")) }

  private fun recordedNote(): ChatItem {
    val created = commands.entries.single { (cmd, _) -> cmd.startsWith("/_create *1 json ") }.value
    return json.decodeFromJsonElement(ChatItem.serializer(), created.jsonObject.getValue("result").jsonObject.getValue("chatItems").jsonArray.first().jsonObject.getValue("chatItem"))
  }

  private companion object {
    const val PAGE = "LINK_URL/page"
    const val FAILED = "LINK_URL/no-image"
    const val TWO_LINKS = "$FAILED $PAGE"
    const val TEXT_WITHOUT_LINK = "text without a link"
    val TEXTS = listOf(PAGE, FAILED, TWO_LINKS, "$TWO_LINKS ", TEXT_WITHOUT_LINK, "")
  }
}
