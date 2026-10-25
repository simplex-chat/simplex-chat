package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.views.usersettings.networkAndServers.validPort
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertTrue

class NetworkProxyConfigUnitTest {

  @Test
  fun validPortsAreAccepted() {
    listOf("1", "80", "9050", "9999", "10000", "59999", "60000", "64999", "65000", "65499", "65500", "65529", "65530", "65535").forEach {
      assertTrue(validPort(it), it)
    }
  }

  @Test
  fun outOfRangeAndNonNumericPortsAreRejected() {
    listOf("65536", "65540", "65600", "66000", "70000", "99999", "100000", "", " ", "-1", "abc", "90 50", "9050a").forEach {
      assertFalse(validPort(it), it)
    }
  }

  @Test
  fun portZeroIsAccepted_currentBehaviour_item38() {
    assertTrue(validPort("0"))
    assertTrue(validPort("00000"))
  }

  @Test
  fun fiveDigitPortsWithLeadingZeroAreAcceptedOnlyWithDigitsUpToFive_currentBehaviour_item38() {
    assertTrue(validPort("05555"))
    assertFalse(validPort("06000"))
    assertFalse(validPort("09050"))
  }

  @Test
  fun defaultProxyIsDefaultPortOnly() {
    assertEquals(":9050", NetworkProxy().toProxyString())
  }

  @Test
  fun proxyHostAndPort() {
    assertEquals("127.0.0.1:9150", NetworkProxy(host = "127.0.0.1", port = 9150).toProxyString())
    assertEquals(":9150", NetworkProxy(port = 9150).toProxyString())
    assertEquals("127.0.0.1", NetworkProxy(host = "127.0.0.1").toProxyString())
    assertEquals("10.0.0.1", NetworkProxy(host = " 10.0.0.1 ").toProxyString())
  }

  @Test
  fun ipv6ProxyHostIsBracketed() {
    assertEquals("[::1]", NetworkProxy(host = "::1").toProxyString())
    assertEquals("[::1]", NetworkProxy(host = "[::1]").toProxyString())
    assertEquals("[fe80::1]:9150", NetworkProxy(host = "fe80::1", port = 9150).toProxyString())
  }

  @Test
  fun usernameAuthAddsTrimmedCredentials() {
    assertEquals("u:p@", NetworkProxy(username = " u ", password = " p ", auth = NetworkProxyAuth.USERNAME).toProxyString())
    assertEquals("u:p@proxy.example:1080", NetworkProxy(username = "u", password = "p", auth = NetworkProxyAuth.USERNAME, host = "proxy.example", port = 1080).toProxyString())
    assertEquals(":p@", NetworkProxy(password = "p", auth = NetworkProxyAuth.USERNAME).toProxyString())
  }

  @Test
  fun usernameAuthWithBlankCredentialsAddsOnlyAtSign() {
    assertEquals("@", NetworkProxy(username = " ", auth = NetworkProxyAuth.USERNAME).toProxyString())
    assertEquals("@127.0.0.1:9150", NetworkProxy(auth = NetworkProxyAuth.USERNAME, host = "127.0.0.1", port = 9150).toProxyString())
  }

  @Test
  fun isolateAuthIgnoresCredentials() {
    assertEquals(":9050", NetworkProxy(username = "u", password = "p", auth = NetworkProxyAuth.ISOLATE).toProxyString())
  }

  @Test
  fun withProxyUsesProxyStringOrDefault() {
    assertEquals("127.0.0.1:9150", NetCfg.defaults.withProxy(NetworkProxy(host = "127.0.0.1", port = 9150)).socksProxy)
    assertEquals(":9050", NetCfg.defaults.withProxy(null).socksProxy)
    assertNull(NetCfg.defaults.withProxy(null, default = null).socksProxy)
  }

  @Test
  fun onionHostsRoundTrip() {
    OnionHosts.entries.forEach {
      assertEquals(it, NetCfg.defaults.withOnionHosts(it).onionHosts)
    }
  }

  @Test
  fun withOnionHostsSetsHostMode() {
    val never = NetCfg.defaults.withOnionHosts(OnionHosts.NEVER)
    assertEquals(HostMode.Public to true, never.hostMode to never.requiredHostMode)
    val prefer = NetCfg.defaults.withOnionHosts(OnionHosts.PREFER)
    assertEquals(HostMode.OnionViaSocks to false, prefer.hostMode to prefer.requiredHostMode)
    val required = NetCfg.defaults.withOnionHosts(OnionHosts.REQUIRED)
    assertEquals(HostMode.OnionViaSocks to true, required.hostMode to required.requiredHostMode)
  }

  @Test
  fun withOnionHostsKeepsOtherSettings() {
    val cfg = NetCfg.proxyDefaults.copy(sessionMode = TransportSessionMode.Entity, smpProxyMode = SMPProxyMode.Never, tcpKeepAlive = null)
    assertEquals(cfg.copy(hostMode = HostMode.OnionViaSocks, requiredHostMode = true), cfg.withOnionHosts(OnionHosts.REQUIRED))
  }

  @Test
  fun onionHostsOfOtherHostModes() {
    assertEquals(OnionHosts.NEVER, NetCfg.defaults.copy(hostMode = HostMode.Public, requiredHostMode = true).onionHosts)
    assertEquals(OnionHosts.PREFER, NetCfg.defaults.copy(hostMode = HostMode.Public, requiredHostMode = false).onionHosts)
    assertEquals(OnionHosts.PREFER, NetCfg.defaults.copy(hostMode = HostMode.Onion, requiredHostMode = true).onionHosts)
    assertEquals(OnionHosts.PREFER, NetCfg.defaults.copy(hostMode = HostMode.Onion, requiredHostMode = false).onionHosts)
    assertEquals(OnionHosts.PREFER, NetCfg.defaults.onionHosts)
  }
}
