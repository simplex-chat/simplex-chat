package chat.simplex.common.helpers

import android.net.*
import android.util.Log
import androidx.core.content.getSystemService
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.model.UserNetworkInfo
import chat.simplex.common.model.UserNetworkType
import chat.simplex.common.platform.TAG
import chat.simplex.common.platform.androidAppContext
import chat.simplex.common.views.helpers.withBGApi
import kotlinx.coroutines.Job
import kotlinx.coroutines.delay

class NetworkObserver {
  private var prevInfo: UserNetworkInfo? = null

  // When having both mobile and Wi-Fi networks enabled with Wi-Fi being active, then disabling Wi-Fi, network reports its offline (which is true)
  // but since it will be online after switching to mobile, there is no need to inform backend about such temporary change.
  // But if it will not be online after some seconds, report it and apply required measures
  private var noNetworkJob = Job() as Job
  private val networkCallback = object: ConnectivityManager.NetworkCallback() {
    override fun onCapabilitiesChanged(network: Network, networkCapabilities: NetworkCapabilities) = networkCapabilitiesChanged(networkCapabilities)
  }
  private val connectivityManager: ConnectivityManager? = androidAppContext.getSystemService()

  fun restartNetworkObserver() {
    if (connectivityManager == null) {
      Log.e(TAG, "Connectivity manager is unavailable, network observer is disabled")
      val info = UserNetworkInfo(
        networkType = UserNetworkType.OTHER,
        online = true,
      )
      prevInfo = info
      setNetworkInfo(info)
      return
    }
    try {
      connectivityManager.unregisterNetworkCallback(networkCallback)
    } catch (e: Exception) {
      // do nothing
    }
    try {
      connectivityManager.registerDefaultNetworkCallback(networkCallback)
    } catch (e: Exception) {
      Log.e(TAG, "Error registering network callback: ${e.stackTraceToString()}")
    }
  }

  private fun networkCapabilitiesChanged(capabilities: NetworkCapabilities) {
    connectivityManager ?: return
    val info = UserNetworkInfo(
      networkType = networkTypeFromCapabilities(capabilities),
      online = capabilities.hasCapability(NetworkCapabilities.NET_CAPABILITY_INTERNET) && capabilities.hasCapability(NetworkCapabilities.NET_CAPABILITY_VALIDATED),
    )
    if (prevInfo != info) {
      prevInfo = info
      setNetworkInfo(info)
    }
  }

  private fun setNetworkInfo(info: UserNetworkInfo) {
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

  private fun networkTypeFromCapabilities(capabilities: NetworkCapabilities): UserNetworkType = when {
    capabilities.hasTransport(NetworkCapabilities.TRANSPORT_CELLULAR) -> UserNetworkType.CELLULAR
    capabilities.hasTransport(NetworkCapabilities.TRANSPORT_WIFI) -> UserNetworkType.WIFI
    capabilities.hasTransport(NetworkCapabilities.TRANSPORT_ETHERNET) -> UserNetworkType.ETHERNET
    else -> UserNetworkType.OTHER
  }

  companion object {
    val shared = NetworkObserver()
  }
}
