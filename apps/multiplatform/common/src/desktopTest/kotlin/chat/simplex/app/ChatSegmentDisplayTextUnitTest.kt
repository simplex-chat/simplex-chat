package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.chat.item.segmentDisplayText
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.*
import kotlin.test.Test
import kotlin.test.assertEquals

class ChatSegmentDisplayTextUnitTest {
  private fun display(ft: FormattedText, mentions: Map<String, CIMention>? = null, linkMode: SimplexLinkMode = SimplexLinkMode.DESCRIPTION): String =
    segmentDisplayText(ft, mentions, linkMode)

  private fun mention(displayName: String, localAlias: String? = null): CIMention =
    CIMention("member-1", CIMentionMember(groupMemberId = 1, displayName = displayName, localAlias = localAlias, memberRole = GroupMemberRole.Member))

  private fun simplexLink(showText: String?): Format.SimplexLink =
    Format.SimplexLink(showText = showText, linkType = SimplexLinkType.contact, simplexUri = "simplex:/contact#abc", smpHosts = listOf("smp.example.com"))

  @Test
  fun styledSegmentsShowTheirTextWithoutMarkers() {
    assertEquals("bold", display(FormattedText("bold", Format.Bold())))
    assertEquals("italic", display(FormattedText("italic", Format.Italic())))
    assertEquals("strike", display(FormattedText("strike", Format.StrikeThrough())))
    assertEquals("code", display(FormattedText("code", Format.Snippet())))
    assertEquals("secret", display(FormattedText("secret", Format.Secret())))
    assertEquals("plain", display(FormattedText("plain")))
  }

  @Test
  fun escapeSegmentIsHidden() {
    assertEquals("", display(FormattedText("\\", Format.Escape())))
  }

  @Test
  fun commandShowsItsText() {
    assertEquals("/help", display(FormattedText("/help", Format.Command("help"))))
  }

  @Test
  fun hyperlinkShowsLinkTextOrTheRawText() {
    assertEquals("docs", display(FormattedText("[docs](https://example.com)", Format.HyperLink(showText = "docs", linkUri = "https://example.com"))))
    assertEquals("https://example.com", display(FormattedText("https://example.com", Format.HyperLink(showText = null, linkUri = "https://example.com"))))
  }

  @Test
  fun uriWithAsciiHostIsUnchanged() {
    assertEquals("https://example.com/path?q=1", display(FormattedText("https://example.com/path?q=1", Format.Uri())))
  }

  @Test
  fun uriMarksNonAsciiHostCharactersButNotThePath() {
    assertEquals("https://ex[U+0430]mple.com/\u043F\u0443\u0442\u044C", display(FormattedText("https://ex\u0430mple.com/\u043F\u0443\u0442\u044C", Format.Uri())))
  }

  @Test
  fun uriMarksInvisibleCharactersAnywhere() {
    assertEquals("https://example.com/a[U+200B]b", display(FormattedText("https://example.com/a\u200Bb", Format.Uri())))
  }

  @Test
  fun mentionOfKnownMemberShowsDisplayName() {
    val ft = FormattedText("@bob", Format.Mention("bob"))
    assertEquals("@robert", display(ft, mapOf("bob" to mention("robert"))))
  }

  @Test
  fun mentionOfMemberWithAliasShowsAliasAndName() {
    val ft = FormattedText("@bob", Format.Mention("bob"))
    assertEquals("@Bobby (robert)", display(ft, mapOf("bob" to mention("robert", localAlias = "Bobby"))))
    assertEquals("@robert", display(ft, mapOf("bob" to mention("robert", localAlias = ""))))
  }

  @Test
  fun mentionWithSpaceBeforeAtIsQuoted() {
    val ft = FormattedText("@bob", Format.Mention("bob"))
    assertEquals("@'bob @home'", display(ft, mapOf("bob" to mention("bob @home"))))
  }

  @Test
  fun mentionWithoutMemberUsesMentionedName() {
    val ft = FormattedText("@bob", Format.Mention("bob"))
    assertEquals("@bob", display(ft, mapOf("bob" to CIMention("member-1", null))))
  }

  @Test
  fun unresolvedMentionShowsRawText() {
    val ft = FormattedText("@carol", Format.Mention("carol"))
    assertEquals("@carol", display(ft, mapOf("bob" to mention("robert"))))
    assertEquals("@carol", display(ft, null))
  }

  @Test
  fun simplexLinkInDescriptionModeShowsLinkTypeAndHost() {
    val via = "(" + generalGetString(MR.strings.simplex_link_connection).format("smp.example.com") + ")"
    val ft = FormattedText("simplex:/contact#abc", simplexLink(showText = null))
    assertEquals("${generalGetString(MR.strings.simplex_link_contact)} $via", display(ft, linkMode = SimplexLinkMode.DESCRIPTION))
  }

  @Test
  fun simplexLinkInFullModeShowsRawLink() {
    val ft = FormattedText("simplex:/contact#abc", simplexLink(showText = null))
    assertEquals("simplex:/contact#abc", display(ft, linkMode = SimplexLinkMode.FULL))
  }

  @Test
  fun simplexLinkWithLinkTextShowsItWithHostInAnyMode() {
    val via = "(" + generalGetString(MR.strings.simplex_link_connection).format("smp.example.com") + ")"
    val ft = FormattedText("[Join](simplex:/contact#abc)", simplexLink(showText = "Join"))
    assertEquals("Join $via", display(ft, linkMode = SimplexLinkMode.FULL))
    assertEquals("Join $via", display(ft, linkMode = SimplexLinkMode.DESCRIPTION))
  }
}
