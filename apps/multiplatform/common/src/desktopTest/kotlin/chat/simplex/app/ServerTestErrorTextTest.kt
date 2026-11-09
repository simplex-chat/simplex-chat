package chat.simplex.app

import chat.simplex.common.model.*
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class ServerTestErrorTextTest {
  private val server = "smp://AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=@127.0.0.1:1"
  private val refused = "Network.Socket.connect: <socket: 168>: does not exist (Connection refused)"

  @Test
  fun serverTestFailureShowsTheError() {
    val failure = ProtocolTestFailure(ProtocolTestStep.Connect, AgentErrorType.BROKER(server, BrokerErrorType.NETWORK(NetworkError.ConnectError(refused))))
    val text = failure.localizedDescription
    assertTrue(text.endsWith(" BROKER NETWORK connectError $refused"), text)
  }

  @Test
  fun networkErrorShowsItsDetail() {
    assertEquals("NETWORK connectError $refused", BrokerErrorType.NETWORK(NetworkError.ConnectError(refused)).string)
    assertEquals("NETWORK tlsError HandshakeFailed", BrokerErrorType.NETWORK(NetworkError.TLSError("HandshakeFailed")).string)
    assertEquals("NETWORK unknownCAError", BrokerErrorType.NETWORK(NetworkError.UnknownCAError).string)
    assertEquals("NETWORK failedError", BrokerErrorType.NETWORK(NetworkError.FailedError).string)
    assertEquals("NETWORK timeoutError", BrokerErrorType.NETWORK(NetworkError.TimeoutError).string)
    assertEquals("NETWORK subscribeError timeout", BrokerErrorType.NETWORK(NetworkError.SubscribeError("timeout")).string)
  }

  @Test
  fun proxyErrorShowsTheRelayError() {
    val proxy = "smp://BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB=@proxy.example"
    val relay = "smp://CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC=@relay.example"
    assertEquals(
      "PROXY $proxy $relay ProxyProtocolError AUTH",
      AgentErrorType.PROXY(proxy, relay, ProxyClientError.ProxyProtocolError(SMPErrorType.AUTH())).string
    )
    assertEquals(
      "PROXY $proxy $relay ProxyResponseError QUOTA",
      AgentErrorType.PROXY(proxy, relay, ProxyClientError.ProxyResponseError(SMPErrorType.QUOTA())).string
    )
  }
}
