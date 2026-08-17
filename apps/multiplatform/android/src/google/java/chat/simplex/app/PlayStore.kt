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
@Volatile private var badgeOffers: Map<BadgeStoreProductId, BadgeOffer> = emptyMap()
@Volatile private var badgePurchase: CompletableDeferred<BadgePurchaseOutcome>? = null

// offerToken is null for one-time products, which have no base plan to choose
private class BadgeOffer(
  val id: BadgeStoreProductId,
  val product: BadgeProduct,
  val details: ProductDetails,
  val offerToken: String?
)

suspend fun loadBadgeProducts(oneTimeIds: List<BadgeStoreProductId>, subscriptionIds: List<BadgeStoreProductId>): List<BadgeProduct> {
  val client = connectedBadgeBillingClient()
  val details = queryBadgeProducts(client, oneTimeIds, BillingClient.ProductType.INAPP) +
      queryBadgeProducts(client, subscriptionIds, BillingClient.ProductType.SUBS)
  val detailsByProductId = details.associateBy { it.productId }
  val ids = oneTimeIds + subscriptionIds
  val offers = ids.mapNotNull { id ->
    val productDetails = detailsByProductId[id.productId]
    if (productDetails == null) {
      Log.w(TAG, "loadBadgeProducts: Play returned no product ${id.productId}")
      null
    } else {
      productDetails.badgeOffer(id)
    }
  }
  if (offers.size < ids.size) {
    // a debug build's applicationIdSuffix is a common cause of Play not knowing the package
    Log.w(TAG, "loadBadgeProducts: ${offers.size} of ${ids.size} resolved - package ${androidAppContext.packageName}, country ${androidPlayStoreCountry.value ?: "none"}")
  }
  badgeOffers = offers.associateBy { it.id }
  return offers.map { it.product }
}

suspend fun purchaseBadge(id: BadgeStoreProductId, invoiceId: String): BadgePurchaseOutcome {
  val activity = mainActivity.get() ?: throw BadgeStoreError.StoreUnavailable
  val client = connectedBadgeBillingClient()
  val offer = badgeOffers[id] ?: throw BadgeStoreError.ProductUnavailable(id.productId)
  val details = offer.details
  val productParams = BillingFlowParams.ProductDetailsParams.newBuilder().setProductDetails(details)
  offer.offerToken?.let { productParams.setOfferToken(it) }
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

private suspend fun queryBadgeProducts(client: BillingClient, ids: List<BadgeStoreProductId>, productType: String): List<ProductDetails> {
  // durations of one subscription share a product id, so the same product is queried once
  val requested = ids.map { it.productId }.distinct()
  val params = QueryProductDetailsParams.newBuilder()
    .setProductList(
      requested.map {
        QueryProductDetailsParams.Product.newBuilder().setProductId(it).setProductType(productType).build()
      }
    )
    .build()
  val queried = CompletableDeferred<List<ProductDetails>>()
  client.queryProductDetailsAsync(params) { result, productDetailsResult ->
    if (result.responseCode == BillingClient.BillingResponseCode.OK) {
      queried.complete(productDetailsResult.productDetailsList)
    } else {
      Log.w(TAG, "queryBadgeProducts: $productType requested $requested, failed ${result.responseCode} ${result.debugMessage}")
      queried.complete(emptyList())
    }
  }
  return queried.await()
}

private fun ProductDetails.badgeOffer(id: BadgeStoreProductId): BadgeOffer? {
  if (id.basePlanId == null) {
    val purchase = oneTimePurchaseOfferDetails
    if (purchase == null) {
      Log.w(TAG, "badgeOffer: ${id.productId} has no one-time purchase price, type is $productType")
      return null
    }
    val product = BadgeProduct(id, purchase.formattedPrice, purchase.priceAmountMicros, purchase.priceCurrencyCode)
    return BadgeOffer(id, product, this, offerToken = null)
  }
  val offer = subscriptionOfferDetails?.firstOrNull { it.basePlanId == id.basePlanId }
  if (offer == null) {
    Log.w(TAG, "badgeOffer: ${id.productId} has no base plan ${id.basePlanId}, Play has ${subscriptionOfferDetails?.map { it.basePlanId }}")
    return null
  }
  val phase = offer.pricingPhases.pricingPhaseList.firstOrNull {
    it.recurrenceMode == ProductDetails.RecurrenceMode.INFINITE_RECURRING
  }
  if (phase == null) {
    Log.w(TAG, "badgeOffer: ${id.productId} base plan ${id.basePlanId} has no recurring price")
    return null
  }
  val product = BadgeProduct(id, phase.formattedPrice, phase.priceAmountMicros, phase.priceCurrencyCode)
  return BadgeOffer(id, product, this, offer.offerToken)
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
