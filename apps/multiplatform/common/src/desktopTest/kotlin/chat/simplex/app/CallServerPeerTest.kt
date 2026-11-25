package chat.simplex.app

import chat.simplex.common.platform.desktopPlatform
import chat.simplex.common.views.call.ipv4Mapped
import chat.simplex.common.views.call.isConnectedByProcessUser
import chat.simplex.common.views.call.procNetAddress
import chat.simplex.common.views.call.procNetSocketUid
import java.net.InetAddress
import java.net.ServerSocket
import java.net.Socket
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNull
import kotlin.test.assertTrue

class CallServerPeerTest {
  private val loopback = byteArrayOf(127, 0, 0, 1)
  private val ipv6Loopback = ByteArray(15) + byteArrayOf(1)
  private val header = "  sl  local_address rem_address   st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode"

  @Test
  fun testProcNetAddressMatchesKernelFormat() {
    assertEquals("0100007F:C4DB", procNetAddress(loopback, 50395))
    assertEquals("00000000000000000000000001000000:0050", procNetAddress(ipv6Loopback, 80))
    assertEquals("0000000000000000FFFF00000100007F:9C40", procNetAddress(ipv4Mapped(loopback), 40000))
  }

  @Test
  fun testSocketUidIsReadFromTheClientRow() {
    val rows = listOf(
      header,
      "   0: 0100007F:C4DB 00000000:0000 0A 00000000:00000000 00:00000000 00000000  1000        0 101 1 0000000000000000 100 0 0 10 0",
      "   1: 0100007F:C4DB 0100007F:9C40 01 00000000:00000000 00:00000000 00000000  1000        0 102 1 0000000000000000 20 4 30 10 -1",
      "   2: 0100007F:9C40 0100007F:C4DB 01 00000000:00000000 00:00000000 00000000  1001        0 103 1 0000000000000000 20 4 30 10 -1",
    )

    assertEquals(1001, procNetSocketUid(rows, local = "0100007F:9C40", remote = "0100007F:C4DB"))
    assertEquals(1000, procNetSocketUid(rows, local = "0100007F:C4DB", remote = "0100007F:9C40"))
  }

  @Test
  fun testSocketUidIsNullForAbsentOrShortRows() {
    val rows = listOf(header, "   0: 0100007F:9C40 0100007F:C4DB 01")

    assertNull(procNetSocketUid(rows, local = "0100007F:9C40", remote = "0100007F:C4DB"))
    assertNull(procNetSocketUid(rows, local = "0100007F:9C41", remote = "0100007F:C4DB"))
  }

  @Test
  fun testLoopbackConnectionOfTheProcessUserIsAccepted() {
    if (!desktopPlatform.isLinux()) return
    ServerSocket(0, 1, InetAddress.getLoopbackAddress()).use { server ->
      Socket(InetAddress.getLoopbackAddress(), server.localPort).use {
        server.accept().use { accepted ->
          assertTrue(isConnectedByProcessUser(accepted), "client ${accepted.inetAddress}:${accepted.port} to ${accepted.localAddress}:${accepted.localPort}")
        }
      }
    }
  }
}
