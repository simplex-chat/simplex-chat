package chat.simplex.app

import chat.simplex.common.platform.Log
import chat.simplex.common.platform.androidAppContext
import chat.simplex.common.platform.androidPlayStoreCountry
import chat.simplex.common.platform.mainActivity
import chat.simplex.common.views.badges.*
import chat.simplex.common.views.helpers.withLongRunningApi
import com.android.billingclient.api.*
import kotlinx.coroutines.CompletableDeferred
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext

const val hasPlatformStore = true

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
  val productParams = BillingFlowParams.ProductDetailsParams.newBuilder().setProductDetails(offer.details)
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
    return purchase.await()
  } finally {
    badgePurchase = null
  }
}

// one-time purchases Play still holds: bought and not consumed, or awaiting a slow payment
suspend fun unfinishedBadgePurchases(): List<BadgePurchaseOutcome> {
  val client = connectedBadgeBillingClient()
  val params = QueryPurchasesParams.newBuilder().setProductType(BillingClient.ProductType.INAPP).build()
  val queried = CompletableDeferred<List<Purchase>>()
  client.queryPurchasesAsync(params) { result, purchases ->
    if (result.responseCode == BillingClient.BillingResponseCode.OK) {
      queried.complete(purchases)
    } else {
      queried.completeExceptionally(BadgeStoreError.BillingError(result.responseCode, result.debugMessage))
    }
  }
  return queried.await().mapNotNull(::badgePurchaseOutcome)
}

private val badgePurchasesUpdatedListener = PurchasesUpdatedListener { result, purchases ->
  val pending = badgePurchase
  if (pending == null) {
    // settled outside a purchase call, such as a slow payment completing
    if (result.responseCode == BillingClient.BillingResponseCode.OK && purchases != null) {
      withLongRunningApi { purchases.mapNotNull(::badgePurchaseOutcome).forEach { BadgeStore.reconcile(it) } }
    }
    return@PurchasesUpdatedListener
  }
  when {
    result.responseCode == BillingClient.BillingResponseCode.OK && purchases != null -> {
      val outcomes = purchases.mapNotNull(::badgePurchaseOutcome)
      pending.complete(outcomes.firstOrNull() ?: BadgePurchaseOutcome.Cancelled)
      withLongRunningApi { outcomes.drop(1).forEach { BadgeStore.reconcile(it) } }
    }
    result.responseCode == BillingClient.BillingResponseCode.USER_CANCELED ->
      pending.complete(BadgePurchaseOutcome.Cancelled)
    else ->
      pending.completeExceptionally(BadgeStoreError.BillingError(result.responseCode, result.debugMessage))
  }
}

private fun badgePurchaseOutcome(purchase: Purchase): BadgePurchaseOutcome? = when (purchase.purchaseState) {
  Purchase.PurchaseState.PENDING -> BadgePurchaseOutcome.Pending(purchase.accountIdentifiers?.obfuscatedAccountId)
  Purchase.PurchaseState.PURCHASED -> BadgePurchaseOutcome.Purchased(
    BadgeStoreReceipt(
      token = purchase.purchaseToken,
      productId = purchase.products.firstOrNull() ?: "",
      invoiceId = purchase.accountIdentifiers?.obfuscatedAccountId,
      acknowledged = purchase.isAcknowledged
    )
  )
  else -> null
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

// acknowledged without consuming, so Play stops its 3-day refund clock and still lists the purchase until it is finished
suspend fun acknowledgeBadgePurchase(receipt: BadgeStoreReceipt) {
  if (!receipt.acknowledged) acknowledge(connectedBadgeBillingClient(), receipt.token)
}

// consumed if one-time so it can be bought again, else acknowledged, as Play refunds an unacknowledged purchase
// after 3 days; decided by product id, as after a restart there is no ProductDetails for the purchase
suspend fun finishBadgePurchase(receipt: BadgeStoreReceipt) {
  val client = connectedBadgeBillingClient()
  if (receipt.productId in badgeOneTimeProductIds) {
    val done = CompletableDeferred<BillingResult>()
    val params = ConsumeParams.newBuilder().setPurchaseToken(receipt.token).build()
    client.consumeAsync(params) { result, _ -> done.complete(result) }
    checkBillingResult(done.await())
  } else {
    acknowledge(client, receipt.token)
  }
}

private suspend fun acknowledge(client: BillingClient, token: String) {
  val done = CompletableDeferred<BillingResult>()
  val params = AcknowledgePurchaseParams.newBuilder().setPurchaseToken(token).build()
  client.acknowledgePurchase(params) { done.complete(it) }
  checkBillingResult(done.await())
}

private fun checkBillingResult(result: BillingResult) {
  if (result.responseCode != BillingClient.BillingResponseCode.OK) {
    throw BadgeStoreError.BillingError(result.responseCode, result.debugMessage)
  }
}
