package chat.simplex.app.ui.desktop

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiEnv
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.msgContent
import chat.simplex.app.ui.harness.yourPrivacy
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiIdle
import com.sun.net.httpserver.HttpServer
import kotlinx.serialization.json.jsonObject
import kotlinx.serialization.json.jsonPrimitive
import org.junit.Assert.assertEquals
import org.junit.Test
import java.io.File
import java.net.InetAddress
import java.net.InetSocketAddress
import java.util.concurrent.CopyOnWriteArrayList

class PrivacyDesktopTest : UiTest() {
  @Test fun privacy_linkPreviewsOff_noPreview() = uiTest {
    PreviewSite().use { site ->
      val alice = app("alice"); val bob = cli("bob")
      connect(bob)
      val aliceChat = "@${bob.contactId("alice")}"
      check(storedSetting(LINK_PREVIEWS) != "false") { "Send link previews is stored as off before it is turned off" }
      val privacy = yourPrivacy(alice)
      click(Ui.Privacy.LinkPreviews)
      poll(what = "Send link previews stored as off") { storedSetting(LINK_PREVIEWS) == "false" }
      privacy.close()
      val chat = alice.chatList.open("bob")
      val off = site.url("off.jpg")
      typeInto(Ui.Chat.Message, off)
      poll(what = "the app to finish handling the typed link") { UiIdle.inFlight().isEmpty() }
      check(!seesDescription(PREVIEW_IMAGE) && site.requests.isEmpty()) { "with previews off: preview shown ${seesDescription(PREVIEW_IMAGE)}, requests ${site.requests}" }
      chat.sendComposed()
      bob.expectsMessage("alice> $off", CONNECT_TIMEOUT_MS)
      assertEquals("text", bob.msgContent(aliceChat, off)?.get("type")?.jsonPrimitive?.content)
      val again = yourPrivacy(alice)
      click(Ui.Privacy.LinkPreviews)
      poll(what = "Send link previews stored as on") { storedSetting(LINK_PREVIEWS) == "true" }
      again.close()
      val on = site.url("on.jpg")
      typeInto(Ui.Chat.Message, on)
      poll(what = "the on.jpg link preview in the compose area") { seesDescription(PREVIEW_IMAGE) && seesText("on.jpg") }
      chat.sendComposed()
      bob.expectsMessage("alice> $on", CONNECT_TIMEOUT_MS)
      val content = bob.msgContent(aliceChat, on)
      assertEquals("link", content?.get("type")?.jsonPrimitive?.content)
      assertEquals("on.jpg", content?.get("preview")?.jsonObject?.get("title")?.jsonPrimitive?.content)
      assertEquals(listOf("/on.jpg"), site.requests.toList())
    }
  }

  @Test fun privacy_removeLinkTracking() = uiTest {
    PreviewSite().use { site ->
      val alice = app("alice"); val bob = cli("bob")
      connect(bob)
      val tracked = site.url(TRACKED_PAGE)
      val clean = site.url(CLEAN_PAGE)
      check(storedSetting(SANITIZE_LINKS) != "true") { "Remove link tracking is stored as on before it is turned on" }
      val chat = alice.chatList.open("bob")
      enterText(Ui.Chat.Message, tracked)
      poll(what = "the typed link kept with its preview, with Remove link tracking off") { editableText(Ui.Chat.Message) == tracked && seesDescription(PREVIEW_IMAGE) }
      chat.sendComposed()
      bob.expectsMessage("alice> $tracked", CONNECT_TIMEOUT_MS)
      assertEquals(listOf("/$TRACKED_PAGE"), site.requests.toList())
      val privacy = yourPrivacy(alice)
      click(Ui.Privacy.SanitizeLinks)
      poll(what = "Remove link tracking stored as on") { storedSetting(SANITIZE_LINKS) == "true" }
      privacy.close()
      enterText(Ui.Chat.Message, tracked)
      poll(what = "the tracking parameter removed from the typed link") { editableText(Ui.Chat.Message) == clean }
      poll(what = "the preview of the cleaned link fetched from the site") { site.requests.size >= 2 }
      chat.sendComposed()
      bob.expectsMessage("alice> $clean", CONNECT_TIMEOUT_MS)
      assertEquals(listOf("/$TRACKED_PAGE", "/$CLEAN_PAGE"), site.requests.toList())
    }
  }

  private class PreviewSite : AutoCloseable {
    val requests = CopyOnWriteArrayList<String>()
    private val image = File(UiEnv.repoRoot, "tests/fixtures/test.jpg").readBytes()
    private val server = HttpServer.create(InetSocketAddress(InetAddress.getByName(HOST), 0), 0).apply {
      createContext("/") { exchange ->
        requests.add(exchange.requestURI.toString())
        exchange.responseHeaders.add("Content-Type", "image/jpeg")
        exchange.sendResponseHeaders(200, image.size.toLong())
        exchange.responseBody.use { it.write(image) }
      }
      start()
    }

    fun url(name: String) = "http://$HOST:${server.address.port}/$name"

    override fun close() = server.stop(0)
  }

  private companion object {
    const val LINK_PREVIEWS = "PrivacyLinkPreviews"
    const val PREVIEW_IMAGE = "link preview image"
    const val NOTES = "Private notes"
    const val HOST = "127.0.0.1"
    const val SANITIZE_LINKS = "PrivacySanitizeLinks"
    const val TRACKED_PAGE = "page.jpg?q=chat&utm_source=app"
    const val CLEAN_PAGE = "page.jpg?q=chat"
  }
}
