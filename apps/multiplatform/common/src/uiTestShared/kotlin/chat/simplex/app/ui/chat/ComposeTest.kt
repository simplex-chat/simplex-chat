package chat.simplex.app.ui.chat

import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextDecoration
import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.PREVIEW_IMAGE_DATA
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.becomeBotWithCommands
import chat.simplex.app.ui.harness.commandShown
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.contactOf
import chat.simplex.app.ui.harness.described
import chat.simplex.app.ui.harness.groupMemberId
import chat.simplex.app.ui.harness.ownGroup
import chat.simplex.app.ui.harness.ownGroupOfThree
import chat.simplex.app.ui.harness.styled
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertFalse
import org.junit.Test

class ComposeTest : UiTest() {
  @Test fun sendText_sendButton_peerReceives() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    poll(what = "the empty message field with the voice and live buttons and no send button") {
      exists(Ui.Chat.Message) && editableText(Ui.Chat.Message).isNullOrEmpty() &&
        exists(Ui.Chat.Live) && seesDescription("Record voice message") && !exists(Ui.Chat.Send)
    }
    typeInto(Ui.Chat.Message, "   ")
    poll(what = "a disabled send button for a blank message") { exists(Ui.Chat.Send) && !isEnabled(Ui.Chat.Send) && !exists(Ui.Chat.Live) }
    replaceText(Ui.Chat.Message, TEXT)
    click(Ui.Chat.Send)
    bob expectsMessage "alice> $TEXT"
    poll(what = "the message field cleared after Send") { editableText(Ui.Chat.Message).isNullOrEmpty() }
    chat.itemId(TEXT)
  }

  @Test fun compose_clearTextButton_clears() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    typeInto(Ui.Chat.Message, "one\ntwo\nthree")
    settle()
    assertFalse("the clear button is shown for three lines", exists(Ui.Chat.ClearText))
    typeInto(Ui.Chat.Message, "\nfour")
    poll(what = "the clear button for four lines") { exists(Ui.Chat.ClearText) }
    hideKeyboard()
    click(Ui.Chat.ClearText)
    poll(what = "the message field empty and the clear button gone") { editableText(Ui.Chat.Message).isNullOrEmpty() && !exists(Ui.Chat.ClearText) }
    chat.send("hello\nthere")
    bob expectsMessage "alice> hello"
    bob expects "there"
  }

  @Test fun markdown_sent_rendersFormatted_peerGetsMarkers() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    chat.send(MARKDOWN)
    bob expectsMessage "alice> $CLI_MARKDOWN"
    val id = chat.itemId(MARKDOWN)
    val item = Ui.Chat.Item(id)
    poll(what = "alice's bubble showing `bold italic strike code` without the markdown markers") {
      textsOf(item).let { t -> t.any { "bold italic strike code" in it } && t.none { m -> MARKERS.any { it in m } } }
    }
    check(styled(item, "bold") { it.fontWeight == FontWeight.Bold }) { "`bold` is not bold in alice's bubble" }
    check(styled(item, "italic") { it.fontStyle == FontStyle.Italic }) { "`italic` is not italic in alice's bubble" }
    check(styled(item, "strike") { it.textDecoration == TextDecoration.LineThrough }) { "`strike` is not struck through in alice's bubble" }
    check(styled(item, "code") { it.fontFamily == FontFamily.Monospace }) { "`code` is not monospace in alice's bubble" }
    bob sends "/? @alice *bold*"
    bob expectsMessage "alice> $CLI_MARKDOWN"
  }

  @Test fun linkPreview_received_rendersCard() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    val pageUrl = "http://127.0.0.1/page"
    val title = "Loopback page"
    val description = "A page served on the loopback address"
    bob sends "@alice see http://127.0.0.1/other"
    val plain = chat.itemId("see http://127.0.0.1/other")
    val aliceId = bob.contactId("alice")
    bob sends """/_send @$aliceId json [{"msgContent": {"type": "link", "text": "$pageUrl", "preview": {"uri": "$pageUrl", "title": "$title", "description": "$description", "image": "$PREVIEW_IMAGE_DATA"}}}]"""
    val card = chat.itemId(pageUrl)
    poll(what = "bob's link message with the preview card: image, title, description and URL") {
      "link preview image" in descriptionsOf(Ui.Chat.Item(card)) && textsOf(Ui.Chat.Item(card)).let { t -> title in t && description in t && pageUrl in t }
    }
    check("link preview image" !in descriptionsOf(Ui.Chat.Item(plain)) && title !in textsOf(Ui.Chat.Item(plain))) { "bob's plain text message with a link shows a preview card" }
  }

  @Test fun mention_maxReached_showsLimit() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath"); val dan = cli("dan")
    ownGroupOfThree(alice, bob, cath, dan)
    typeInto(Ui.Chat.Message, "@b")
    val bobRow = Ui.Chat.MentionMember(groupMemberId("bob"))
    val cathRow = Ui.Chat.MentionMember(groupMemberId("cath"))
    val danRow = Ui.Chat.MentionMember(groupMemberId("dan"))
    val rows = listOf(bobRow, cathRow, danRow)
    poll(what = "the mention picker listing bob and neither cath nor dan for `@b`") { exists(bobRow) && !exists(cathRow) && !exists(danRow) }
    click(bobRow)
    poll(what = "`@bob ` in the message field") { editableText(Ui.Chat.Message) == "@bob " }
    typeInto(Ui.Chat.Message, "@")
    poll(what = "bob, cath and dan offered and enabled for the second mention, without the limit message") { rows.all { exists(it) && isEnabled(it) } && !seesText(MENTION_LIMIT) }
    click(cathRow)
    poll(what = "`@bob @cath ` in the message field") { editableText(Ui.Chat.Message) == "@bob @cath " }
    typeInto(Ui.Chat.Message, "@")
    poll(what = "bob, cath and dan offered and enabled for the third mention, without the limit message") { rows.all { exists(it) && isEnabled(it) } && !seesText(MENTION_LIMIT) }
    click(danRow)
    poll(what = "`@bob @cath @dan ` in the message field") { editableText(Ui.Chat.Message) == "@bob @cath @dan " }
    typeInto(Ui.Chat.Message, "@")
    poll(what = "the limit message with every member disabled for a fourth mention") { seesText(MENTION_LIMIT) && rows.all { exists(it) && !isEnabled(it) } }
  }

  @Test fun mention_editAddsMention() = uiTest {
    val alice = app("alice"); val bob = cli("bob"); val cath = cli("cath")
    connect(bob); connect(cath)
    val chat = ownGroup(alice, "team", bob, cath)
    chat.send("hello")
    bob.eventuallyMessage("#team alice> hello", CONNECT_TIMEOUT_MS)
    cath.eventuallyMessage("#team alice> hello", CONNECT_TIMEOUT_MS)
    val id = chat.itemId("hello")
    check(chat.item(id)?.mentions.isNullOrEmpty()) { "`hello` mentions someone before the edit: ${chat.item(id)?.mentions}" }
    hideKeyboard()
    chat.menu("hello", "edit")
    poll(what = "the edit bar with `hello` in the field") { editableText(Ui.Chat.Message) == "hello" && exists(described("Cancel")) }
    replaceText(Ui.Chat.Message, "hello @b")
    val bobId = groupMemberId("bob")
    val cathId = groupMemberId("cath")
    poll(what = "the mention picker listing bob and not cath for `@b` while editing") { exists(Ui.Chat.MentionMember(bobId)) && !exists(Ui.Chat.MentionMember(cathId)) }
    click(Ui.Chat.MentionMember(bobId))
    poll(what = "`hello @bob ` in the message field") { editableText(Ui.Chat.Message) == "hello @bob " }
    typeInto(Ui.Chat.Message, "there")
    chat.sendComposed()
    bob.eventuallyMessage("#team alice> [edited] hello @bob there", CONNECT_TIMEOUT_MS)
    cath.eventuallyMessage("#team alice> [edited] hello @bob there", CONNECT_TIMEOUT_MS)
    poll(what = "alice's `hello` edited into `hello @bob there`, mentioning bob") {
      chat.item(id)?.let { it.content.text == "hello @bob there" && it.mentions?.get("bob")?.memberRef?.groupMemberId == bobId } == true &&
        textsOf(Ui.Chat.Item(id)).any { "hello @bob there" in it }
    }
  }

  @Test fun commandsMenu_slash_showsBotCommands() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    bob.becomeBotWithCommands()
    connect(bob)
    alice.chatList.open("bob")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's bot commands in alice's contact") { contactOf("bob").profile.preferences?.commands?.isNotEmpty() == true }
    waitFor(Ui.Chat.CommandsButton)
    check(!exists(Ui.Chat.Command("help")) && !exists(MORE)) { "the commands menu is open before `//` is clicked" }
    hideKeyboard()
    click(Ui.Chat.CommandsButton)
    poll(what = "the commands menu with Help, Order and More, without the commands inside More") {
      commandShown("help", "Help") && commandShown("order", "Order") && exists(MORE) && "More" in textsOf(MORE) &&
        !exists(Ui.Chat.Command("status")) && !exists(Ui.Chat.CommandsBack)
    }
    click(MORE)
    poll(what = "the More submenu with Status and Cost under a back row labelled More") {
      commandShown("status", "Status") && commandShown("cost", "Cost") && exists(Ui.Chat.CommandsBack) && "More" in textsOf(Ui.Chat.CommandsBack) &&
        !exists(Ui.Chat.Command("help")) && !exists(MORE)
    }
    click(Ui.Chat.CommandsBack)
    poll(what = "the top commands back after the back row") {
      exists(Ui.Chat.Command("help")) && exists(Ui.Chat.Command("order")) && exists(MORE) && !exists(Ui.Chat.Command("status")) && !exists(Ui.Chat.CommandsBack)
    }
    click(Ui.Chat.CommandsButton)
    poll(what = "the commands menu closed by `//`") { !exists(Ui.Chat.Command("help")) && !exists(MORE) }
    typeInto(Ui.Chat.Message, "/or")
    poll(what = "only Order listed for `/or`") {
      exists(Ui.Chat.Command("order")) && !exists(Ui.Chat.Command("help")) && !exists(MORE) && !exists(Ui.Chat.Command("status")) && !exists(Ui.Chat.Command("cost"))
    }
    replaceText(Ui.Chat.Message, "/st")
    poll(what = "only Status from the More submenu listed for `/st`") {
      exists(Ui.Chat.Command("status")) && !exists(Ui.Chat.Command("cost")) && !exists(Ui.Chat.Command("order")) && !exists(Ui.Chat.Command("help")) && !exists(Ui.Chat.CommandsBack)
    }
    replaceText(Ui.Chat.Message, "/")
    poll(what = "the top commands listed for `/`") {
      exists(Ui.Chat.Command("help")) && exists(Ui.Chat.Command("order")) && exists(MORE) && !exists(Ui.Chat.Command("status"))
    }
  }

  @Test fun commandsMenu_selectCommand_sendsToBot() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    bob.becomeBotWithCommands()
    connect(bob)
    val chat = alice.chatList.open("bob")
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's bot commands in alice's contact") { contactOf("bob").profile.preferences?.commands?.isNotEmpty() == true }
    hideKeyboard()
    click(Ui.Chat.CommandsButton)
    click(Ui.Chat.Command("order"))
    poll(what = "`/order <number>` in the message field and the commands menu closed") {
      editableText(Ui.Chat.Message) == "/order <number>" && !exists(Ui.Chat.Command("order")) && !exists(Ui.Chat.Command("help"))
    }
    replaceText(Ui.Chat.Message, "/order 7")
    chat.sendComposed()
    bob.expectsMessage("alice> /order 7", CONNECT_TIMEOUT_MS)
    hideKeyboard()
    click(Ui.Chat.CommandsButton)
    click(Ui.Chat.Command("help"))
    poll(what = "the message field empty and the commands menu closed after Help") {
      editableText(Ui.Chat.Message).isNullOrEmpty() && !exists(Ui.Chat.Command("help"))
    }
    bob.expectsMessage("alice> /help", CONNECT_TIMEOUT_MS)
    chat.itemId("/help")
  }

  private companion object {
    const val TEXT = "hello there 🙂"
    const val MARKDOWN = "*bold* _italic_ ~strike~ `code`"
    const val CLI_MARKDOWN = "bold italic strike `code`"
    val MARKERS = listOf("*bold*", "_italic_", "~strike~", "`code`")
    const val MENTION_LIMIT = "You can mention up to 3 members per message!"
    val MORE = Ui.Chat.CommandMenu("More")
  }
}
