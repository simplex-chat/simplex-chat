package chat.simplex.common.helpers

import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.model.UserNetworkInfo
import chat.simplex.common.model.UserNetworkType
import chat.simplex.common.platform.*
import kotlinx.coroutines.runBlocking
import java.net.*
import kotlin.concurrent.thread

// JVM has no network change notifications, so the addresses the routing table would use for outgoing
// connections are polled instead - unlike the addresses of all interfaces, they change when the
// network the connections are made via changes, or when the address the connections are made from changes.
class NetworkObserver {
  // the network info and the addresses the core was last told about, plus the addresses added since
  // (sentAddresses null - nothing was accepted since the observer was last restarted; sentInfo is also cleared
  // after a suspend, so that the network is reported again), and the IPv6 address the connections were
  // made from (recorded with the last accepted report, or when IPv6 was added after it)
  @Volatile private var sentAddresses: Set<String>? = null
  @Volatile private var sentInfo: UserNetworkInfo? = null
  @Volatile private var sentAddress6: InetAddress? = null
  @Volatile private var observer: Thread? = null
  private var address6Polls = 0
  private var offlinePolls = 0

  @Synchronized fun restartNetworkObserver() {
    // the chat controller is created anew, and what was recorded, including a report the observer that is
    // stopping delivered to it while it was being replaced, is cleared, so the network info is reported to it again
    sentAddresses = null
    sentInfo = null
    sentAddress6 = null
    val newObserver = thread(start = false, isDaemon = true, name = "NetworkObserver") { observeNetwork() }
    // the observer that is stopping does not report, and it stops when it polls next
    observer = newObserver
    newObserver.start()
  }

  private fun observeNetwork() {
    while (observer === Thread.currentThread()) {
      try {
        pollNetwork()
      } catch (e: Throwable) {
        // the observer thread is not restarted until the chat controller is created anew
        Log.e(TAG, "Error observing network: ${e.stackTraceToString()}")
      }
      // the core holds sending while it is offline, so the network is checked more often then
      sleepDetectingSuspend(if (sentInfo?.online == false) OFFLINE_POLL_INTERVAL_MS else POLL_INTERVAL_MS)
    }
  }

  // only the sleeps are measured - a poll waiting for the core to answer must not look like a suspend,
  // as the report it makes is what keeps the core busy; the connections made before the computer was
  // suspended may be dead, so the network is reported again even with the same addresses
  private fun sleepDetectingSuspend(ms: Long) {
    val sleptAt = System.currentTimeMillis()
    Thread.sleep(ms)
    if (System.currentTimeMillis() - sleptAt > SUSPENDED_MS) resetSentInfo()
  }

  private fun pollNetwork() {
    // the addresses can be briefly unavailable, e.g. when the computer wakes up - it is not confirmed
    // when the core already has the network offline
    val addresses = localAddresses().ifEmpty {
      if (sentInfo?.online == false) emptySet() else { sleepDetectingSuspend(CONFIRM_OFFLINE_MS); localAddresses() }
    }
    val info = UserNetworkInfo(networkType = UserNetworkType.OTHER, online = addresses.isNotEmpty())
    if (!info.online && sentAddresses == null) return
    // the reported addresses are compared, so that the report that was not accepted is repeated,
    // and an added address (e.g. IPv6 configured after IPv4) does not affect the connections, a removed does
    val address6 = sourceAddress(PROBE_IPV6)
    val report = info != sentInfo || !addresses.containsAll(sentAddresses ?: emptySet()) || sentAddress6Removed(address6)
    if (report) sendNetworkInfo(info, addresses, address6) else setSentInfo(info, addresses, sentAddress6 ?: address6)
  }

  // the network prefix stays the same when an IPv6 temporary address is deleted, while the connections
  // made via it are dead; the address that is still used is not deleted, so it is not looked up, and the
  // replaced one is looked up every 6th poll (once a minute while online), as listing the interfaces can be
  // slow (Windows)
  private fun sentAddress6Removed(address6: InetAddress?): Boolean {
    val sent = sentAddress6 ?: return false
    if (sent == address6 || ++address6Polls % ADDRESS6_CHECK_POLLS != 0) return false
    return runCatching { NetworkInterface.getByInetAddress(sent) == null }.getOrDefault(false)
  }

  // while the core has the network offline and it is polled more often, the interfaces are listed
  // as often as while it is online
  private fun localAddresses(): Set<String> =
    setOfNotNull(localAddress(PROBE_IPV4), localAddress(PROBE_IPV6)).ifEmpty {
      if (sentInfo?.online == false && ++offlinePolls % OFFLINE_LIST_POLLS != 0) emptySet() else usableAddresses()
    }

  // without a route to the internet the network can still be used, e.g. via a proxy or a server in the
  // local network, so the addresses of the usable interfaces are compared instead of the chosen ones, with
  // a marker that is removed when the route returns - that is a change, while the addresses are not
  private fun usableAddresses(): Set<String> {
    val addresses = NetworkInterface.getNetworkInterfaces()?.asSequence()?.filter { runCatching { !it.isLoopback && it.isUp }.getOrDefault(false) }
      ?.flatMap { i -> i.inetAddresses.asSequence().filter { !it.isLoopbackAddress && !it.isLinkLocalAddress } }
      ?.map(::addressKey)?.toSet() ?: emptySet()
    return if (addresses.isEmpty()) addresses else addresses + NO_ROUTE
  }

  private fun localAddress(remoteHost: String): String? = sourceAddress(remoteHost)?.let(::addressKey)

  // a socket is used once - a reused one keeps the address it was bound to on the platforms
  // that do not reset it when the socket is disconnected
  private fun sourceAddress(remoteHost: String): InetAddress? =
    try {
      DatagramSocket().use { socket ->
        // connecting a UDP socket sends nothing, it only binds it to the address chosen by the routing table
        socket.connect(InetSocketAddress(InetAddress.getByName(remoteHost), PROBE_PORT))
        socket.localAddress?.takeIf { !it.isAnyLocalAddress && !it.isLinkLocalAddress }
      }
    } catch (e: Exception) {
      // there is no route to this address family, or a socket cannot be created
      null
    }

  // IPv6 temporary addresses are rotated within the same network, and the connections made via
  // the previous address remain valid until it is deleted, so only the network prefix is compared.
  private fun addressKey(address: InetAddress): String =
    if (address is Inet6Address) address.address.copyOf(8).joinToString(":") { it.toUByte().toString(16) }
    else address.hostAddress ?: ""

  // taken under the same lock as the observer restart, so that a report of the observer that is stopping
  // is either delivered and recorded before the restart resets what was recorded, or not delivered
  @Synchronized private fun sendNetworkInfo(info: UserNetworkInfo, addresses: Set<String>, address6: InetAddress?) {
    if (observer !== Thread.currentThread()) return
    Log.d(TAG, "Network changed: $info")
    val ctrl = controller.currentCtrl()
    val sent = ctrl != null && runBlocking { controller.apiSetNetworkInfo(info, ctrl) }
    if (sent) setSentInfo(info, addresses, address6)
  }

  @Synchronized private fun setSentInfo(info: UserNetworkInfo, addresses: Set<String>, address6: InetAddress?) {
    if (observer !== Thread.currentThread()) return
    sentAddresses = addresses
    sentInfo = info
    sentAddress6 = address6
    chatModel.networkInfo.value = info
  }

  @Synchronized private fun resetSentInfo() {
    if (observer === Thread.currentThread()) sentInfo = null
  }

  companion object {
    val shared = NetworkObserver()
    private const val POLL_INTERVAL_MS = 10_000L
    private const val OFFLINE_POLL_INTERVAL_MS = 2_000L
    private const val ADDRESS6_CHECK_POLLS = 6
    private const val OFFLINE_LIST_POLLS = 5
    private const val CONFIRM_OFFLINE_MS = 1_000L
    private const val SUSPENDED_MS = 25_000L
    private const val NO_ROUTE = "no route"
    // reserved for documentation, no packets are sent to them
    private const val PROBE_IPV4 = "192.0.2.1"
    private const val PROBE_IPV6 = "2001:db8::1"
    private const val PROBE_PORT = 9
  }
}
