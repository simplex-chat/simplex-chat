package chat.simplex.common.helpers

import android.net.*
import android.util.Log
import androidx.core.content.getSystemService
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.model.NetworkInfo
import chat.simplex.common.model.NetworkInfoType
import chat.simplex.common.platform.TAG
import chat.simplex.common.platform.androidAppContext
import chat.simplex.common.views.helpers.withBGApi
import kotlinx.coroutines.Job
import kotlinx.coroutines.delay

class NetworkObserver(private val onChange: (NetworkInfo) -> Unit) {
  private var prevInfo: NetworkInfo? = null

  private val networkCallback = object: ConnectivityManager.NetworkCallback() {
    override fun onCapabilitiesChanged(network: Network, networkCapabilities: NetworkCapabilities) = networkCapabilitiesChanged(networkCapabilities)
  }

  private val connectivityManager: ConnectivityManager? = androidAppContext.getSystemService()

  private fun start() {
    if (connectivityManager == null) {
      Log.e(TAG, "Connectivity manager is unavailable, network observer is disabled")
      val info = NetworkInfo(
        online = true,
        type = NetworkInfoType.OTHER,
        metered = false,
      )
      prevInfo = info
      onChange(info)
      return
    }
    try {
      connectivityManager.registerDefaultNetworkCallback(networkCallback)
    } catch (e: Exception) {
      Log.e(TAG, "Error registering network callback: ${e.stackTraceToString()}")
    }
  }

  private fun stop() {
    try {
      connectivityManager?.unregisterNetworkCallback(networkCallback)
    } catch (e: Exception) {
      Log.e(TAG, "Error unregistering network callback: ${e.stackTraceToString()}")
    }
  }

  private fun networkCapabilitiesChanged(capabilities: NetworkCapabilities) {
    connectivityManager ?: return
    val info = NetworkInfo(
      online = capabilities.hasCapability(NetworkCapabilities.NET_CAPABILITY_INTERNET) && capabilities.hasCapability(NetworkCapabilities.NET_CAPABILITY_VALIDATED),
      type = networkTypeFromCapabilities(capabilities),
      metered = !capabilities.hasCapability(NetworkCapabilities.NET_CAPABILITY_NOT_METERED),
    )
    if (prevInfo != info) {
      prevInfo = info
      onChange(info)
    }
  }

  private fun networkTypeFromCapabilities(capabilities: NetworkCapabilities): NetworkInfoType = when {
    capabilities.hasTransport(NetworkCapabilities.TRANSPORT_CELLULAR) &&
        !capabilities.hasCapability(NetworkCapabilities.NET_CAPABILITY_NOT_ROAMING) -> NetworkInfoType.ROAMING

    capabilities.hasTransport(NetworkCapabilities.TRANSPORT_CELLULAR) -> NetworkInfoType.CELLULAR
    capabilities.hasTransport(NetworkCapabilities.TRANSPORT_WIFI) -> NetworkInfoType.WIFI
    capabilities.hasTransport(NetworkCapabilities.TRANSPORT_ETHERNET) -> NetworkInfoType.ETHERNET
    else -> NetworkInfoType.OTHER
  }

  companion object {
    private var networkObserver: NetworkObserver? = null

    fun reinitNetworkObserver() {
      networkObserver?.stop()
      // When having both mobile and Wi-Fi networks enabled with Wi-Fi being active, then disabling Wi-Fi, network reports its offline (which is true)
      // but since it will be online after switching to mobile, there is no need to inform backend about such temporary change.
      // But if it will not be online after some seconds, report it and apply required measures
      var noNetworkJob = Job() as Job
      val observer = NetworkObserver { info ->
        Log.d(TAG, "Network changed: $info")
        noNetworkJob.cancel()
        if (info.online) {
          withBGApi {
            if (controller.hasChatCtrl()) {
              controller.apiSetNetworkInfo(info)
            }
          }
        } else {
          noNetworkJob = withBGApi {
            delay(3000)
            if (controller.hasChatCtrl()) {
              controller.apiSetNetworkInfo(info)
            }
          }
        }
      }
      observer.start()
      networkObserver = observer
    }
  }
}
