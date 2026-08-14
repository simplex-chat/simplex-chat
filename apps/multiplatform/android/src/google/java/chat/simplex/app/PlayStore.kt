package chat.simplex.app

import chat.simplex.common.platform.Log
import chat.simplex.common.platform.androidAppContext
import chat.simplex.common.platform.androidPlayStoreCountry
import chat.simplex.common.platform.mainActivity
import chat.simplex.common.views.badges.*
import com.android.billingclient.api.*
import kotlinx.coroutines.CompletableDeferred
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext

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

// One long-lived client for badges: ProductDetails obtained from it are passed back to it when the
// purchase is launched, and the purchase result arrives on its listener rather than as a return value.
// volatile: the listener is called on the main thread, the purchase runs on a background dispatcher
@Volatile private var badgeBillingClient: BillingClient? = null
@Volatile private var badgeProductDetails: Map<String, ProductDetails> = emptyMap()
@Volatile private var badgePurchase: CompletableDeferred<BadgePurchaseOutcome>? = null

suspend fun loadBadgeProducts(productIds: List<String>): List<BadgeProduct> {
  val client = connectedBadgeBillingClient()
  // every id is queried as both types - Play returns only the ones that match, so the product type
  // does not have to be inferred from the id
  val details = queryBadgeProducts(client, productIds, BillingClient.ProductType.INAPP) +
      queryBadgeProducts(client, productIds, BillingClient.ProductType.SUBS)
  badgeProductDetails = details.associateBy { it.productId }
  return details.mapNotNull { it.badgeProduct() }
}

suspend fun purchaseBadge(productId: String, invoiceId: String): BadgePurchaseOutcome {
  val activity = mainActivity.get() ?: throw BadgeStoreError.StoreUnavailable
  val client = connectedBadgeBillingClient()
  val details = badgeProductDetails[productId] ?: throw BadgeStoreError.ProductUnavailable(productId)
  val productParams = BillingFlowParams.ProductDetailsParams.newBuilder().setProductDetails(details)
  // subscriptions must state which base plan is bought, one-time products must not
  details.subscriptionOfferDetails?.firstOrNull()?.let { productParams.setOfferToken(it.offerToken) }
  val params = BillingFlowParams.newBuilder()
    .setProductDetailsParamsList(listOf(productParams.build()))
    .setObfuscatedAccountId(invoiceId)
    .build()
  val purchase = CompletableDeferred<BadgePurchaseOutcome>()
  badgePurchase = purchase
  try {
    val launched = withContext(Dispatchers.Main) { client.launchBillingFlow(activity, params) }
    if (launched.responseCode != BillingClient.BillingResponseCode.OK) {
      throw BadgeStoreError.BillingError(launched.responseCode, launched.debugMessage)
    }
    val outcome = purchase.await()
    if (outcome is BadgePurchaseOutcome.Purchased) finishBadgePurchase(client, details, outcome.receipt)
    return outcome
  } finally {
    badgePurchase = null
  }
}

private val badgePurchasesUpdatedListener = PurchasesUpdatedListener { result, purchases ->
  val pending = badgePurchase ?: return@PurchasesUpdatedListener
  when {
    result.responseCode == BillingClient.BillingResponseCode.OK && purchases != null ->
      pending.complete(badgePurchaseOutcome(purchases))
    result.responseCode == BillingClient.BillingResponseCode.USER_CANCELED ->
      pending.complete(BadgePurchaseOutcome.Cancelled)
    else ->
      pending.completeExceptionally(BadgeStoreError.BillingError(result.responseCode, result.debugMessage))
  }
}

private fun badgePurchaseOutcome(purchases: List<Purchase>): BadgePurchaseOutcome {
  val purchase = purchases.firstOrNull() ?: return BadgePurchaseOutcome.Cancelled
  if (purchase.purchaseState == Purchase.PurchaseState.PENDING) return BadgePurchaseOutcome.Pending
  return BadgePurchaseOutcome.Purchased(
    BadgeStoreReceipt(
      token = purchase.purchaseToken,
      productId = purchase.products.firstOrNull() ?: "",
      orderId = purchase.orderId,
      invoiceId = purchase.accountIdentifiers?.obfuscatedAccountId
    )
  )
}

private suspend fun connectedBadgeBillingClient(): BillingClient {
  badgeBillingClient?.let { if (it.isReady) return it }
  val client = BillingClient.newBuilder(androidAppContext)
    .setListener(badgePurchasesUpdatedListener)
    .enablePendingPurchases(PendingPurchasesParams.newBuilder().enableOneTimeProducts().build())
    .build()
  val connected = CompletableDeferred<BillingResult>()
  client.startConnection(object : BillingClientStateListener {
    override fun onBillingSetupFinished(result: BillingResult) {
      connected.complete(result)
    }

    override fun onBillingServiceDisconnected() {
      badgeBillingClient = null
      connected.complete(
        BillingResult.newBuilder().setResponseCode(BillingClient.BillingResponseCode.SERVICE_DISCONNECTED).build()
      )
    }
  })
  val result = connected.await()
  if (result.responseCode != BillingClient.BillingResponseCode.OK) {
    client.endConnection()
    throw BadgeStoreError.BillingError(result.responseCode, result.debugMessage)
  }
  badgeBillingClient = client
  return client
}

private suspend fun queryBadgeProducts(client: BillingClient, productIds: List<String>, productType: String): List<ProductDetails> {
  val params = QueryProductDetailsParams.newBuilder()
    .setProductList(
      productIds.map {
        QueryProductDetailsParams.Product.newBuilder().setProductId(it).setProductType(productType).build()
      }
    )
    .build()
  val queried = CompletableDeferred<List<ProductDetails>>()
  client.queryProductDetailsAsync(params) { result, productDetailsResult ->
    if (result.responseCode == BillingClient.BillingResponseCode.OK) {
      queried.complete(productDetailsResult.productDetailsList)
    } else {
      Log.w(TAG, "queryBadgeProducts: $productType query failed ${result.responseCode} ${result.debugMessage}")
      queried.complete(emptyList())
    }
  }
  return queried.await()
}

private fun ProductDetails.badgeProduct(): BadgeProduct? {
  oneTimePurchaseOfferDetails?.let {
    return BadgeProduct(productId, it.formattedPrice, it.priceAmountMicros, it.priceCurrencyCode)
  }
  // the last pricing phase is the recurring base price, earlier phases are trials and intro offers
  val phase = subscriptionOfferDetails?.firstOrNull()?.pricingPhases?.pricingPhaseList?.lastOrNull() ?: return null
  return BadgeProduct(productId, phase.formattedPrice, phase.priceAmountMicros, phase.priceCurrencyCode)
}

// nothing is delivered in this build, so the purchase is finished right away; once the service issues
// credentials it must only be finished after the credential is stored. One-time products are consumed
// so they can be bought again, subscriptions are acknowledged - Play refunds an unacknowledged
// purchase after 3 days.
private suspend fun finishBadgePurchase(client: BillingClient, details: ProductDetails, receipt: BadgeStoreReceipt) {
  val done = CompletableDeferred<BillingResult>()
  if (details.productType == BillingClient.ProductType.SUBS) {
    val params = AcknowledgePurchaseParams.newBuilder().setPurchaseToken(receipt.token).build()
    client.acknowledgePurchase(params) { done.complete(it) }
  } else {
    val params = ConsumeParams.newBuilder().setPurchaseToken(receipt.token).build()
    client.consumeAsync(params) { result, _ -> done.complete(result) }
  }
  val result = done.await()
  if (result.responseCode != BillingClient.BillingResponseCode.OK) {
    Log.e(TAG, "finishBadgePurchase: ${result.responseCode} ${result.debugMessage}")
  }
}
