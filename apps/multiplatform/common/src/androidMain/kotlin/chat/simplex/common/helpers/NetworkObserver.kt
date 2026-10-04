package chat.simplex.common.helpers

import android.net.*
import android.util.Log
import androidx.core.content.getSystemService
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.model.UserNetworkInfo
import chat.simplex.common.model.UserNetworkType
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.withBGApi
import kotlinx.coroutines.*
import kotlinx.coroutines.sync.*
import java.net.InetAddress
import java.util.concurrent.atomic.AtomicBoolean

class NetworkObserver {
  private var prevInfo: UserNetworkInfo? = null
  // The core reconnects to the servers on every online network event, so the event is also sent
  // when the network type is the same, but it is a different network or the addresses changed
  // (e.g., another Wi-Fi network or a new IP address) - connections via the previous network are dead.
  private var prevNetwork: Network? = null
  private var prevAddresses: Set<InetAddress>? = null
  // the last network info the core accepted and the network it was for (null - it was not accepted, or
  // nothing was sent to this chat controller and the core is online with the default network type)
  @Volatile private var sentInfo: UserNetworkInfo? = null
  @Volatile private var sentNetwork: Network? = null
  // incremented when the chat controller is created anew - the reports for the previous one are dropped
  @Volatile private var ctrlGeneration = 0
  private val sendMutex = Mutex()
  // the numbers of removed addresses of the current network and of those reported to the core: while they
  // differ the next online info must be sent even for the same network - the addresses cannot be compared
  // instead, as an address removed and assigned again also leaves the connections made via it dead
  @Volatile private var addressRemovals = 0
  @Volatile private var sentAddressRemovals = 0

  // When having both mobile and Wi-Fi networks enabled with Wi-Fi being active, then disabling Wi-Fi, network reports its offline (which is true)
  // but since it will be online after switching to mobile, there is no need to inform backend about such temporary change.
  // But if it will not be online after some seconds, report it and apply required measures
  private var noNetworkJob = Job() as Job
  private val networkCallback = object: ConnectivityManager.NetworkCallback() {
    override fun onCapabilitiesChanged(network: Network, networkCapabilities: NetworkCapabilities) = networkCapabilitiesChanged(network, networkCapabilities)
    override fun onLinkPropertiesChanged(network: Network, linkProperties: LinkProperties) = linkPropertiesChanged(network, linkProperties)
    override fun onLost(network: Network) = networkLost(network)
  }
  private val connectivityManager: ConnectivityManager? = androidAppContext.getSystemService()

  @Synchronized fun restartNetworkObserver() {
    prevInfo = null
    sentInfo = null
    ctrlGeneration++
    if (connectivityManager == null) {
      Log.e(TAG, "Connectivity manager is unavailable, network observer is disabled")
      val info = UserNetworkInfo(
        networkType = UserNetworkType.OTHER,
        online = true,
      )
      prevInfo = info
      setNetworkInfo(info, null)
      return
    }
    try {
      connectivityManager.unregisterNetworkCallback(networkCallback)
    } catch (e: Exception) {
      // do nothing
    }
    val activeNetwork = connectivityManager.activeNetwork
    val initialCapabilities = connectivityManager.getNetworkCapabilities(activeNetwork)
    if (activeNetwork != null && initialCapabilities != null) {
      networkCapabilitiesChanged(activeNetwork, initialCapabilities)
    } else {
      networkLost()
    }
    try {
      connectivityManager.registerDefaultNetworkCallback(networkCallback)
    } catch (e: Exception) {
      Log.e(TAG, "Error registering network callback: ${e.stackTraceToString()}")
    }
  }

  @Synchronized private fun networkCapabilitiesChanged(network: Network, capabilities: NetworkCapabilities) {
    connectivityManager ?: return
    val info = networkInfo(capabilities)
    if (prevInfo != info || (info.online && prevNetwork != network)) {
      prevInfo = info
      if (info.online) {
        prevNetwork = network
        // queried directly, rather than cached from onLinkPropertiesChanged, so it is for this network
        // regardless of which of the two callbacks fires first
        val addresses = connectivityManager.getLinkProperties(network)?.let { addresses(it) }
        // onLinkPropertiesChanged will not see a removal this snapshot absorbed, so it is counted here
        if (lost(prevAddresses, addresses)) addressRemovals++
        prevAddresses = addresses
      }
      setNetworkInfo(info, network)
    }
  }

  @Synchronized private fun linkPropertiesChanged(network: Network, linkProperties: LinkProperties) {
    if (network != prevNetwork) return
    val addresses = addresses(linkProperties)
    val prev = prevAddresses
    prevAddresses = addresses
    // the removal is counted even when the previous report failed, so that the next report includes it
    if (lost(prev, addresses)) {
      Log.d(TAG, "Network changed: addresses")
      addressRemovals++
      // prevInfo is null when the previous report failed; the info is read again rather than taken from
      // the one the core has, as that can be of another network
      val info = prevInfo ?: connectivityManager?.getNetworkCapabilities(network)?.let(::networkInfo)
      if (info != null && info.online) setNetworkInfo(info, network)
    }
  }

  @Synchronized private fun networkLost(network: Network? = null) {
    // the network that is replaced by another one is lost after it becomes the current network,
    // and the connections are made via it, so its loss is not reported
    if (network != null && prevNetwork != null && network != prevNetwork) return
    Log.d(TAG, "Network changed: lost")
    val none = UserNetworkInfo(networkType = UserNetworkType.NONE, false)
    prevInfo = none
    prevNetwork = null
    prevAddresses = null
    setNetworkInfo(none, null)
  }

  private fun setNetworkInfo(info: UserNetworkInfo, network: Network?) {
    val releaseWakeLock = getWakeLock(timeout = 180000)
    Log.d(TAG, "Network changed: $info")
    noNetworkJob.cancel()
    val ctrlGen = ctrlGeneration
    val sent = AtomicBoolean(false)
    val job = if (info.online) {
      withBGApi {
        sendMutex.withLock {
          val removals = addressRemovals
          // the core has the same network info for the network its connections were made via, e.g. the network
          // was reported offline less than 3 seconds ago and it was not sent - the connections are not affected
          if ((removals != sentAddressRemovals || sentInfo != info || network != sentNetwork) &&
            sendNetworkInfo(info, network, removals, ctrlGen)) sent.set(true)
        }
      }
    } else {
      withBGApi {
        delay(3000)
        // the report is not cancelled once it started, so that the core is not left with the info
        // the app did not record - the online event that cancelled it while it was resuming is
        // reported instead of it
        val cancelled = !isActive
        withContext(NonCancellable) {
          sendMutex.withLock {
            if (!cancelled && (sentInfo != info || network != sentNetwork)) sendNetworkInfo(info, network, addressRemovals, ctrlGen)
          }
        }
      }.also { noNetworkJob = it }
    }
    // the job that is cancelled before it starts does not run its body; when the network info is reported
    // the wake lock is held until it times out, so that the device does not sleep while the core reconnects
    job.invokeOnCompletion { if (!sent.get()) releaseWakeLock() }
  }

  private suspend fun sendNetworkInfo(info: UserNetworkInfo, network: Network?, removals: Int, ctrlGen: Int): Boolean {
    if (ctrlGen != ctrlGeneration) return false
    // the controller is passed, so that replacing it while the report is sent does not fail the command
    val ctrl = controller.currentCtrl() ?: return false.also { reportFailed(ctrlGen) }
    // a failed command must not be recorded as sent, and it throws when the response cannot be parsed
    val sent = try { controller.apiSetNetworkInfo(info, ctrl) } catch (e: Throwable) { Log.e(TAG, "Error reporting network info: ${e.stackTraceToString()}"); false }
    // when it was not sent the network info is reported again when the next event is received
    if (sent) setSentInfo(info, network, removals, ctrlGen) else reportFailed(ctrlGen)
    return sent
  }

  // taken under the lock restartNetworkObserver holds, so that a report for a replaced chat controller
  // does not change the state of the one that replaced it
  @Synchronized private fun setSentInfo(info: UserNetworkInfo, network: Network?, removals: Int, ctrlGen: Int) {
    if (ctrlGen != ctrlGeneration) return
    sentInfo = info
    sentNetwork = network
    sentAddressRemovals = removals
    chatModel.networkInfo.value = info
  }

  // what the core has is not known any more, so the next event is not deduplicated against it
  @Synchronized private fun reportFailed(ctrlGen: Int) {
    if (ctrlGen != ctrlGeneration) return
    prevInfo = null
    sentInfo = null
  }

  // one that disappeared means the connections via it are dead, and one that appeared when there were none
  // means the network can be used again; added ones (e.g. a rotated IPv6 temporary address) do not affect them
  private fun lost(prev: Set<InetAddress>?, addresses: Set<InetAddress>?): Boolean =
    prev != null && (addresses == null || !addresses.containsAll(prev) || (prev.isEmpty() && addresses.isNotEmpty()))

  // LinkAddress equality includes address lifetimes that change with every router advertisement
  private fun addresses(linkProperties: LinkProperties): Set<InetAddress> = linkProperties.linkAddresses.map { it.address }.toSet()

  private fun networkInfo(capabilities: NetworkCapabilities) = UserNetworkInfo(
    networkType = networkTypeFromCapabilities(capabilities),
    online = capabilities.hasCapability(NetworkCapabilities.NET_CAPABILITY_INTERNET) && capabilities.hasCapability(NetworkCapabilities.NET_CAPABILITY_VALIDATED),
  )

  private fun networkTypeFromCapabilities(capabilities: NetworkCapabilities): UserNetworkType = when {
    capabilities.hasTransport(NetworkCapabilities.TRANSPORT_ETHERNET) -> UserNetworkType.ETHERNET
    capabilities.hasTransport(NetworkCapabilities.TRANSPORT_WIFI) -> UserNetworkType.WIFI
    capabilities.hasTransport(NetworkCapabilities.TRANSPORT_CELLULAR) -> UserNetworkType.CELLULAR
    else -> UserNetworkType.OTHER
  }

  companion object {
    val shared = NetworkObserver()
  }
}
