package chat.simplex.app

import chat.simplex.common.platform.androidAppContext
import chat.simplex.common.platform.androidPlayStoreCountry
import com.android.billingclient.api.*

// Requests the country of the Google Play account into [androidPlayStoreCountry].
// It stays null when Play is unavailable or the user is not signed in.
fun loadPlayStoreCountry() {
  val client = BillingClient.newBuilder(androidAppContext)
    .setListener { _, _ -> }
    .enablePendingPurchases(PendingPurchasesParams.newBuilder().enableOneTimeProducts().build())
    .build()
  client.startConnection(object : BillingClientStateListener {
    override fun onBillingSetupFinished(result: BillingResult) {
      if (result.responseCode != BillingClient.BillingResponseCode.OK) {
        client.endConnection()
        return
      }
      client.getBillingConfigAsync(GetBillingConfigParams.newBuilder().build()) { configResult, config ->
        if (configResult.responseCode == BillingClient.BillingResponseCode.OK) {
          androidPlayStoreCountry.value = config?.countryCode
        }
        client.endConnection()
      }
    }

    // The connection is only used for this one request, it is not retried
    override fun onBillingServiceDisconnected() = client.endConnection()
  })
}
