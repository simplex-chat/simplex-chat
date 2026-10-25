package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.chat.item.stripFormattedTextLink
import chat.simplex.common.views.chat.item.stripTextLink
import chat.simplex.common.views.helpers.UploadContent
import java.net.URI
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertSame
import kotlin.test.assertTrue

class ComposeStateUnitTest {
  private val fileUri = URI("file:/tmp/compose-unit/report.pdf")
  private val item = ChatItem.getSampleData(1, text = "earlier")
  private val filePreview = ComposePreview.FilePreview("report.pdf", fileUri)
  private val mediaPreview = ComposePreview.MediaPreview(listOf("data:image/jpg;base64,AA=="), listOf(UploadContent.SimpleImage(fileUri)))
  private val voicePreview = ComposePreview.VoicePreview("voice.m4a", 3000, true)
  private val chatLinkPreview = ComposePreview.ChatLinkPreview(MsgChatLink.Contact("simplex:/contact#abc", Profile.sampleData, business = false))
  private val linkPreview = ComposePreview.CLinkPreview(LinkPreview.sampleData)

  private fun state(
    text: String = "",
    preview: ComposePreview = ComposePreview.NoPreview,
    contextItem: ComposeContextItem = ComposeContextItem.NoContextItem,
    liveMessage: LiveMessage? = null,
    inProgress: Boolean = false,
    useLinkPreviews: Boolean = true
  ) = ComposeState(
    message = ComposeMessage(text),
    liveMessage = liveMessage,
    preview = preview,
    contextItem = contextItem,
    inProgress = inProgress,
    useLinkPreviews = useLinkPreviews
  )

  private fun live() = LiveMessage(item, typedMsg = "", sentMsg = "", sent = false)

  @Test
  fun sendDisabledForWhitespaceOnlyText() {
    assertFalse(state("   ").sendEnabled())
    assertFalse(state("").sendEnabled())
    assertFalse(state(" \n\t").sendEnabled())
  }

  @Test
  fun sendEnabledForText() {
    assertTrue(state("hi").sendEnabled())
    assertTrue(state("  hi  ").sendEnabled())
  }

  @Test
  fun sendEnabledForAttachmentPreviewsWithoutText() {
    assertTrue(state(preview = filePreview).sendEnabled())
    assertTrue(state(preview = mediaPreview).sendEnabled())
    assertTrue(state(preview = voicePreview).sendEnabled())
    assertTrue(state(preview = chatLinkPreview).sendEnabled())
  }

  @Test
  fun sendDisabledForLinkPreviewWithoutText() {
    assertFalse(state(preview = linkPreview).sendEnabled())
    assertFalse(state(preview = ComposePreview.CLinkPreview(null)).sendEnabled())
  }

  @Test
  fun sendDisabledWhileInProgress() {
    assertFalse(state("hi", inProgress = true).sendEnabled())
    assertFalse(state(preview = filePreview, inProgress = true).sendEnabled())
  }

  @Test
  fun sendEnabledWithoutTextForLiveMessageAndForwarding() {
    assertTrue(state(liveMessage = live()).sendEnabled())
    assertTrue(state(contextItem = ComposeContextItem.ForwardingItems(listOf(item), ChatInfo.Direct.sampleData)).sendEnabled())
  }

  @Test
  fun reportOtherNeedsTextOtherReasonsDoNot() {
    assertFalse(state(contextItem = ComposeContextItem.ReportedItem(item, ReportReason.Other)).sendEnabled())
    assertTrue(state("why", contextItem = ComposeContextItem.ReportedItem(item, ReportReason.Other)).sendEnabled())
    assertTrue(state(contextItem = ComposeContextItem.ReportedItem(item, ReportReason.Spam)).sendEnabled())
  }

  @Test
  fun quotingWithoutTextDoesNotEnableSend() {
    assertFalse(state(contextItem = ComposeContextItem.QuotedItem(item)).sendEnabled())
  }

  @Test
  fun emptyOnlyWithoutTextPreviewAndContext() {
    assertTrue(state("  ").empty)
    assertTrue(state("  ").whitespaceOnly)
    assertFalse(state("x").empty)
    assertFalse(state("x").whitespaceOnly)
    assertFalse(state(preview = linkPreview).empty)
    assertFalse(state(contextItem = ComposeContextItem.QuotedItem(item)).empty)
  }

  @Test
  fun linkPreviewAllowedFollowsSettingWithoutAttachments() {
    assertTrue(state().linkPreviewAllowed)
    assertFalse(state(useLinkPreviews = false).linkPreviewAllowed)
    assertTrue(state(preview = linkPreview).linkPreviewAllowed)
    assertFalse(state(preview = linkPreview, useLinkPreviews = false).linkPreviewAllowed)
  }

  @Test
  fun linkPreviewNotAllowedWithAttachments() {
    assertFalse(state(preview = filePreview).linkPreviewAllowed)
    assertFalse(state(preview = mediaPreview).linkPreviewAllowed)
    assertFalse(state(preview = voicePreview).linkPreviewAllowed)
    assertFalse(state(preview = chatLinkPreview).linkPreviewAllowed)
  }

  @Test
  fun linkPreviewReturnsOnlyLinkPreviewContent() {
    assertSame(LinkPreview.sampleData, state(preview = linkPreview).linkPreview)
    assertNull(state(preview = filePreview).linkPreview)
    assertNull(state().linkPreview)
  }

  @Test
  fun attachmentEnabledWithNoPreviewOrLinkPreview() {
    assertFalse(state().attachmentDisabled)
    assertFalse(state("hi").attachmentDisabled)
    assertFalse(state(preview = linkPreview).attachmentDisabled)
  }

  @Test
  fun attachmentDisabledWithAttachmentPreview() {
    assertTrue(state(preview = filePreview).attachmentDisabled)
    assertTrue(state(preview = mediaPreview).attachmentDisabled)
    assertTrue(state(preview = voicePreview).attachmentDisabled)
    assertTrue(state(preview = chatLinkPreview).attachmentDisabled)
  }

  @Test
  fun attachmentDisabledWhileEditingForwardingReportingLiveOrInProgress() {
    assertTrue(state(contextItem = ComposeContextItem.EditingItem(item)).attachmentDisabled)
    assertTrue(state(contextItem = ComposeContextItem.ForwardingItems(listOf(item), ChatInfo.Direct.sampleData)).attachmentDisabled)
    assertTrue(state(contextItem = ComposeContextItem.ReportedItem(item, ReportReason.Spam)).attachmentDisabled)
    assertTrue(state(liveMessage = live()).attachmentDisabled)
    assertTrue(state(inProgress = true).attachmentDisabled)
  }

  @Test
  fun attachmentAllowedWhileQuoting() {
    assertFalse(state(contextItem = ComposeContextItem.QuotedItem(item)).attachmentDisabled)
  }

  @Test
  fun stripTextLinkRemovesOnlyTrailingLink() {
    val link = "https://simplex.chat/contact#abc"
    assertEquals("", stripTextLink(link, link))
    assertEquals("msg", stripTextLink("msg\n$link", link))
    assertEquals("a\nb", stripTextLink("a\nb\n$link", link))
    assertEquals("see $link here", stripTextLink("see $link here", link))
    assertEquals("msg $link", stripTextLink("msg $link", link))
    assertEquals("msg", stripTextLink("msg", link))
  }

  @Test
  fun stripFormattedTextLinkKeepsListWhenLastIsNotTheLink() {
    val link = "https://simplex.chat/contact#abc"
    assertNull(stripFormattedTextLink(null, link))
    val empty = emptyList<FormattedText>()
    assertSame(empty, stripFormattedTextLink(empty, link))
    val other = listOf(FormattedText("hello"))
    assertSame(other, stripFormattedTextLink(other, link))
  }

  @Test
  fun stripFormattedTextLinkDropsLinkAndNewline() {
    val link = "https://simplex.chat/contact#abc"
    assertNull(stripFormattedTextLink(listOf(FormattedText(link, Format.Uri())), link))
    val stripped = stripFormattedTextLink(listOf(FormattedText("msg\n"), FormattedText(link, Format.Uri())), link)!!
    assertEquals(listOf("msg"), stripped.map { it.text })
    assertNull(stripped.single().format)
    assertNull(stripFormattedTextLink(listOf(FormattedText("\n"), FormattedText(link, Format.Uri())), link))
  }

  @Test
  fun stripFormattedTextLinkDropsPrecedingMarkup() {
    val link = "https://simplex.chat/contact#abc"
    val stripped = stripFormattedTextLink(
      listOf(FormattedText("msg"), FormattedText("\n", Format.Markup(BlockToken.LineBreak())), FormattedText(link, Format.Uri())),
      link
    )!!
    assertEquals(listOf("msg"), stripped.map { it.text })
  }

  @Test
  fun stripFormattedTextLinkKeepsPrecedingTextWithoutNewline() {
    val link = "https://simplex.chat/contact#abc"
    val stripped = stripFormattedTextLink(listOf(FormattedText("msg "), FormattedText(link, Format.Uri())), link)!!
    assertEquals(listOf("msg "), stripped.map { it.text })
  }
}
