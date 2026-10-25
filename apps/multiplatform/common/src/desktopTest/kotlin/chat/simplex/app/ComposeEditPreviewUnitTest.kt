package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.helpers.UploadContent
import chat.simplex.common.views.helpers.getAppFileUri
import kotlinx.datetime.Clock
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertIs
import kotlin.test.assertSame
import kotlin.test.assertTrue

class ComposeEditPreviewUnitTest {
  private val chatLink = "https://simplex.chat/contact#/?v=2&smp=abc"

  private fun sent(mc: MsgContent, file: CIFile? = null): ChatItem =
    ChatItem(
      chatDir = CIDirection.DirectSnd(),
      meta = CIMeta.getSample(7, Clock.System.now(), mc.text),
      content = CIContent.SndMsgContent(mc),
      reactions = emptyList(),
      file = file
    )

  @Test
  fun textItemHasNoPreview() {
    assertIs<ComposePreview.NoPreview>(chatItemPreview(sent(MsgContent.MCText("hello"))))
  }

  @Test
  fun linkItemKeepsItsLinkPreview() {
    val preview = chatItemPreview(sent(MsgContent.MCLink("https://www.duckduckgo.com", LinkPreview.sampleData)))
    assertIs<ComposePreview.CLinkPreview>(preview)
    assertSame(LinkPreview.sampleData, preview.linkPreview)
  }

  @Test
  fun fileItemShowsFilePreviewOfStoredFile() {
    val preview = chatItemPreview(sent(MsgContent.MCFile(""), CIFile.getSample(fileName = "report.pdf")))
    assertIs<ComposePreview.FilePreview>(preview)
    assertEquals("report.pdf", preview.fileName)
    assertEquals(getAppFileUri("report.pdf"), preview.uri)
    assertTrue(preview.uri.path.endsWith("/report.pdf"))
  }

  @Test
  fun imageItemShowsMediaPreviewOfStoredFile() {
    val preview = chatItemPreview(sent(MsgContent.MCImage("caption", "data:image/jpg;base64,AA=="), CIFile.getSample(fileName = "photo.jpg")))
    assertIs<ComposePreview.MediaPreview>(preview)
    assertEquals(listOf("data:image/jpg;base64,AA=="), preview.images)
    assertEquals(listOf<UploadContent>(UploadContent.SimpleImage(getAppFileUri("photo.jpg"))), preview.content)
  }

  @Test
  fun voiceItemDividesDurationByThousand() {
    val preview = chatItemPreview(sent(MsgContent.MCVoice("", 30), CIFile.getSample(fileName = "voice.m4a")))
    assertEquals(ComposePreview.VoicePreview(voice = "voice.m4a", durationMs = 0, finished = true), preview)
    val long = chatItemPreview(sent(MsgContent.MCVoice("", 4500), CIFile.getSample(fileName = "voice.m4a")))
    assertEquals(ComposePreview.VoicePreview(voice = "voice.m4a", durationMs = 4, finished = true), long)
  }

  @Test
  fun reportAndChatLinkItemsHaveNoPreview() {
    assertIs<ComposePreview.NoPreview>(chatItemPreview(sent(MsgContent.MCReport("bad", ReportReason.Spam))))
    assertIs<ComposePreview.NoPreview>(chatItemPreview(sent(MsgContent.MCChat("Join\n$chatLink", MsgChatLink.Contact(chatLink, Profile.sampleData, business = false)))))
  }

  @Test
  fun editingTextItemStartsWithItsText() {
    val ci = sent(MsgContent.MCText("hello"))
    val state = ComposeState(editingItem = ci, useLinkPreviews = true)
    assertEquals("hello", state.message.text)
    assertTrue(state.editing)
    assertSame(ci, (state.contextItem as ComposeContextItem.EditingItem).chatItem)
    assertIs<ComposePreview.NoPreview>(state.preview)
    assertEquals(listOf("hello"), state.parsedMessage.map { it.text })
  }

  @Test
  fun editingChatLinkItemStripsTheTrailingLink() {
    val ci = sent(MsgContent.MCChat("Join\n$chatLink", MsgChatLink.Contact(chatLink, Profile.sampleData, business = false)))
    val state = ComposeState(editingItem = ci, useLinkPreviews = true)
    assertEquals("Join", state.message.text)
    assertIs<ComposePreview.NoPreview>(state.preview)
  }

  @Test
  fun editingChatLinkItemThatIsOnlyTheLinkStartsEmpty() {
    val ci = sent(MsgContent.MCChat(chatLink, MsgChatLink.Contact(chatLink, Profile.sampleData, business = false)))
    assertEquals("", ComposeState(editingItem = ci, useLinkPreviews = true).message.text)
  }

  @Test
  fun editingFileItemKeepsCaptionAndFilePreview() {
    val ci = sent(MsgContent.MCFile("notes"), CIFile.getSample(fileName = "report.pdf"))
    val state = ComposeState(editingItem = ci, useLinkPreviews = true)
    assertEquals("notes", state.message.text)
    assertIs<ComposePreview.FilePreview>(state.preview)
    assertTrue(state.attachmentDisabled)
  }

  @Test
  fun editingFileItemWithoutCaptionStartsEmptyNotWithFileName() {
    val ci = sent(MsgContent.MCFile(""), CIFile.getSample(fileName = "report.pdf"))
    assertEquals("", ComposeState(editingItem = ci, useLinkPreviews = true).message.text)
  }
}
