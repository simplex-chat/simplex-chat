package chat.simplex.app.ui.desktop

import androidx.compose.ui.input.key.Key
import androidx.compose.ui.semantics.SemanticsActions
import androidx.compose.ui.test.performKeyInput
import androidx.compose.ui.test.performSemanticsAction
import androidx.compose.ui.test.pressKey
import androidx.compose.ui.test.withKeyDown
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.described
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.MsgContent
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiIdle
import chat.simplex.common.ui.UiKey
import com.sun.net.httpserver.HttpServer
import org.junit.Test
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import java.io.Closeable
import java.net.InetSocketAddress
import java.util.concurrent.CopyOnWriteArrayList
import javax.imageio.ImageIO

class ComposeDesktopTest : UiTest() {
  @Test fun multiline_shiftEnter_peerReceivesTwoLines() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val chat = alice.chatList.open("bob")
    typeInto(Ui.Chat.Message, "   ")
    pressEnter(Ui.Chat.Message)
    check(editableText(Ui.Chat.Message) == "   " && !isEnabled(Ui.Chat.Send)) { "Enter on a blank message changed the field to `${editableText(Ui.Chat.Message)}`" }
    replaceText(Ui.Chat.Message, "hello")
    pressShiftEnter(Ui.Chat.Message)
    poll(what = "a new line after `hello` in the message field") { editableText(Ui.Chat.Message) == "hello\n" }
    chat.send("there")
    bob expectsMessage "alice> hello"
    bob expects "there"
  }

  @Test fun linkPreview_enable_peerReceivesLink_cancelSendsText() = uiTest {
    PreviewServer().use { server ->
      val alice = app("alice"); val bob = cli("bob")
      connect(bob)
      showLinkPreviewsAlertViaApi()
      val chat = alice.chatList.open("bob")
      val cat = server.url("cat.png")
      typeInto(Ui.Chat.Message, cat)
      poll(what = "the `Enable link previews?` alert with `Enable`") { seesText("Enable link previews?") && exists(ENABLE) }
      check(server.requests().isEmpty()) { "the preview was fetched before it was enabled: ${server.requests()}" }
      click(ENABLE)
      poll(what = "the cat.png preview in the compose area") { seesText("cat.png") && exists(CANCEL_PREVIEW) && seesDescription("link preview image") }
      check(server.requests() == listOf("/cat.png")) { "preview requests: ${server.requests()}" }
      chat.sendComposed()
      bob expectsMessage "alice> $cat"
      val id = chat.itemId(cat)
      poll(what = "alice's sent message showing the cat.png card") {
        !exists(CANCEL_PREVIEW) && "cat.png" in textsOf(Ui.Chat.Item(id)) && "link preview image" in descriptionsOf(Ui.Chat.Item(id))
      }
      val aliceId = bob.contactId("alice")
      bob sends "/_get content types @$aliceId"
      bob expects "Chat content types: link"
      val dog = server.url("dog.png")
      typeInto(Ui.Chat.Message, "see $dog")
      poll(what = "the dog.png preview fetched without asking again") { seesText("dog.png") && exists(CANCEL_PREVIEW) }
      check(!seesText("Enable link previews?") && server.requests() == listOf("/cat.png", "/dog.png")) { "alert shown again or preview requests: ${server.requests()}" }
      click(CANCEL_PREVIEW)
      poll(what = "the dog.png preview removed") { !seesText("dog.png") && !exists(CANCEL_PREVIEW) }
      chat.sendComposed()
      bob expectsMessage "alice> see $dog"
      bob sends "/_get content types @$aliceId"
      bob expects "Chat content types: link, text"
    }
  }

  @Test fun linkPreview_firstTimeAlert_disable_noPreview() = uiTest {
    PreviewServer().use { server ->
      val alice = app("alice")
      showLinkPreviewsAlertViaApi()
      val notes = alice.chatList.open("Private notes")
      check(storedSetting(LINK_PREVIEWS) != "false") { "link previews are stored as off before the alert" }
      val cat = server.url("cat.png")
      typeInto(Ui.Chat.Message, cat)
      poll(what = "the `Enable link previews?` alert with `Disable`") { seesText("Enable link previews?") && exists(DISABLE) }
      click(DISABLE)
      poll(what = "the alert closed and link previews stored as off") {
        !seesText("Enable link previews?") && storedSetting(LINK_PREVIEWS) == "false" && storedSetting(LINK_PREVIEWS_ALERT) == "false"
      }
      awaitAppWork()
      check(server.requests().isEmpty() && !exists(CANCEL_PREVIEW)) { "a preview after Disable; requests: ${server.requests()}" }
      notes.sendComposed()
      val id = notes.itemId(cat)
      poll(what = "the note saved as text without a card") {
        notes.item(id)?.content?.msgContent is MsgContent.MCText && "link preview image" !in descriptionsOf(Ui.Chat.Item(id))
      }
      typeInto(Ui.Chat.Message, server.url("dog.png"))
      awaitAppWork()
      check(!seesText("Enable link previews?") && !exists(CANCEL_PREVIEW) && server.requests().isEmpty()) { "alert or preview for a link typed after Disable; requests: ${server.requests()}" }
    }
  }

  private fun showLinkPreviewsAlertViaApi() = appPrefs.privacyLinkPreviewsShowAlert.set(true)

  private fun UiScope.awaitAppWork() = poll(what = "the app to finish handling the typed link") { UiIdle.inFlight().isEmpty() }

  private fun UiScope.pressShiftEnter(key: UiKey) {
    val node = ui.onNode(matcher(key))
    ui.runOnUiThread { node.performSemanticsAction(SemanticsActions.RequestFocus) }
    ui.runOnUiThread { node.performKeyInput { withKeyDown(Key.ShiftLeft) { pressKey(Key.Enter) } } }
    ui.waitForIdle()
  }

  private class PreviewServer : Closeable {
    private val paths = CopyOnWriteArrayList<String>()
    private val server = HttpServer.create(InetSocketAddress("127.0.0.1", 0), 0).apply {
      createContext("/") { exchange ->
        paths += exchange.requestURI.path
        exchange.responseHeaders.add("Content-Type", "image/png")
        exchange.sendResponseHeaders(200, PNG.size.toLong())
        exchange.responseBody.use { it.write(PNG) }
      }
      start()
    }

    fun url(name: String) = "http://127.0.0.1:${server.address.port}/$name"

    fun requests(): List<String> = paths.toList()

    override fun close() = server.stop(0)
  }

  private companion object {
    val ENABLE = Ui.Alert.Option(Ui.AlertSource.Shared, "enable")
    val DISABLE = Ui.Alert.Option(Ui.AlertSource.Shared, "disable")
    val CANCEL_PREVIEW = described("cancel link preview")
    const val LINK_PREVIEWS = "PrivacyLinkPreviews"
    const val LINK_PREVIEWS_ALERT = "PrivacyLinkPreviewsShowAlert"
    val PNG: ByteArray = ByteArrayOutputStream().also { ImageIO.write(BufferedImage(16, 16, BufferedImage.TYPE_INT_RGB), "png", it) }.toByteArray()
  }
}
