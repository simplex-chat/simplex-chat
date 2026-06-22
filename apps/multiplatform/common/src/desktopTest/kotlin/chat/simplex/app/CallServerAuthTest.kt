package chat.simplex.app

import chat.simplex.common.views.call.startServer
import java.net.Socket
import kotlin.test.AfterTest
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotEquals

// Integration test for the desktop call server's token gate (the handle() enforcement),
// which the unit-level CallServerTokenTest does not exercise.
class CallServerAuthTest {
  private val token = "integration-test-token"
  // port = 0 binds a random free port, avoiding a clash with a real call server on SERVER_PORT
  private val server = startServer(onResponse = {}, port = 0, token = token)
  private val port get() = server.listeningPort

  @AfterTest
  fun tearDown() = server.stop()

  @Test
  fun testWebSocketUpgradeRejectedWithoutToken() {
    assertEquals(401, requestStatus(webSocketUpgrade(path = "/")))
  }

  @Test
  fun testWebSocketUpgradeRejectedWithWrongToken() {
    assertEquals(401, requestStatus(webSocketUpgrade(path = "/?token=wrong")))
  }

  @Test
  fun testWebSocketUpgradeAcceptedWithToken() {
    assertEquals(101, requestStatus(webSocketUpgrade(path = "/?token=$token")))
  }

  @Test
  fun testCallPageRejectedWithoutToken() {
    assertEquals(401, requestStatus(get(path = "/simplex/call/")))
  }

  @Test
  fun testCallPagePassesAuthGateWithToken() {
    // Resource serving may differ in the test classpath, so assert only that the auth gate was passed (not 401)
    assertNotEquals(401, requestStatus(get(path = "/simplex/call/?token=$token")))
  }

  private fun get(path: String): List<String> = listOf("GET $path HTTP/1.1", "Host: localhost:$port")

  private fun webSocketUpgrade(path: String): List<String> =
    listOf(
      "GET $path HTTP/1.1",
      "Host: localhost:$port",
      "Upgrade: websocket",
      "Connection: Upgrade",
      "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==",
      "Sec-WebSocket-Version: 13",
    )

  // Sends a raw HTTP request and returns the response status code from the status line.
  private fun requestStatus(requestLines: List<String>): Int =
    Socket("localhost", port).use { socket ->
      socket.soTimeout = 5000
      socket.getOutputStream().apply {
        write((requestLines.joinToString("\r\n") + "\r\n\r\n").toByteArray())
        flush()
      }
      val statusLine = socket.getInputStream().bufferedReader().readLine() ?: error("no response from call server")
      statusLine.split(" ")[1].toInt()
    }
}
