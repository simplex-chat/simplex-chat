package chat.simplex.common.views.badges

import androidx.compose.runtime.mutableStateOf
import chat.simplex.common.platform.*
import java.text.NumberFormat
import java.util.Currency
import java.util.UUID

// a subscription is one store product containing a base plan per duration, so a purchasable badge
// is identified by both; one-time products have no base plan
data class BadgeStoreProductId(val productId: String, val basePlanId: String? = null)

// TODO [badges] ids will come from app config and prices from the badge service catalog;
// hardcoded here so the Play Store integration can be tested before the purchase API lands.
fun badgeStoreProductId(level: BadgeLevel, period: BadgePeriod): BadgeStoreProductId = when (level) {
  BadgeLevel.Supporter -> when (period) {
    BadgePeriod.OneMonth -> BadgeStoreProductId("badge_supporter_01")
    BadgePeriod.Monthly -> BadgeStoreProductId("subscr_badge_supporter_01", "subscr-badge-supporter-month-02")
    BadgePeriod.Annual -> BadgeStoreProductId("subscr_badge_supporter_01", "subscr-badge-supporter-year-01")
  }
  BadgeLevel.Legend -> when (period) {
    BadgePeriod.OneMonth -> BadgeStoreProductId("badge_legend_01")
    BadgePeriod.Monthly -> BadgeStoreProductId("subscr_badge_legend_01", "subscr-badge-legend-month-01")
    BadgePeriod.Annual -> BadgeStoreProductId("subscr_badge_legend_01", "subscr-badge-legend-year-01")
  }
}

val badgeStoreProductIds: List<BadgeStoreProductId> = BadgeLevel.entries.flatMap { level ->
  BadgePeriod.entries.map { badgeStoreProductId(level, it) }
}

// TODO [badges] replaced by APIGetBadgeInvoice, which creates the invoice row and returns its id.
// Sent to Play as obfuscatedAccountId and echoed back on the purchase, which is how the service
// learns which invoice a store transaction settles.
fun newBadgeInvoiceId(): String = UUID.randomUUID().toString()

// what the platform store knows about one product; ProductDetails cannot cross into commonMain
data class BadgeProduct(
  val id: BadgeStoreProductId,
  val displayPrice: String,
  val priceMicros: Long,
  val currencyCode: String
)

sealed class BadgePrice {
  object Loading: BadgePrice()
  class Price(val price: String): BadgePrice()
  object Unavailable: BadgePrice()

  val canPurchase: Boolean
    get() = when (this) {
      is Price -> true
      is Loading, is Unavailable -> false
    }
}

data class BadgeStoreReceipt(
  // the token the badge service verifies with the Publisher API
  val token: String,
  val productId: String,
  val orderId: String?,
  val invoiceId: String?,
  // only set for test products - a real Play purchase has no environment to report
  val environment: String? = null
)

// TODO [badges] Play Billing has no offline product configuration. Set to true to price the screens
// and walk the purchase flow without Play Console products; the purchase is simulated and its
// receipt says so.
const val useBadgeTestProducts = false

private fun testProduct(level: BadgeLevel, period: BadgePeriod, priceMicros: Long) =
  BadgeProduct(badgeStoreProductId(level, period), "\$${priceMicros / 1_000_000}.00", priceMicros, "USD")

private val testBadgeProducts: List<BadgeProduct> = listOf(
  testProduct(BadgeLevel.Supporter, BadgePeriod.OneMonth, 7_000_000),
  testProduct(BadgeLevel.Supporter, BadgePeriod.Monthly, 7_000_000),
  testProduct(BadgeLevel.Supporter, BadgePeriod.Annual, 42_000_000),
  testProduct(BadgeLevel.Legend, BadgePeriod.OneMonth, 70_000_000),
  testProduct(BadgeLevel.Legend, BadgePeriod.Monthly, 70_000_000),
  testProduct(BadgeLevel.Legend, BadgePeriod.Annual, 420_000_000)
)

sealed class BadgePurchaseOutcome {
  class Purchased(val receipt: BadgeStoreReceipt): BadgePurchaseOutcome()
  object Pending: BadgePurchaseOutcome()
  object Cancelled: BadgePurchaseOutcome()
}

sealed class BadgeStoreError: Exception() {
  class ProductUnavailable(val productId: String): BadgeStoreError()
  class BillingError(val responseCode: Int, val debugMessage: String): BadgeStoreError()
  object StoreUnavailable: BadgeStoreError()

  override val message: String
    get() = when (this) {
      is ProductUnavailable -> "productUnavailable(productId: $productId)"
      is BillingError -> "billingError(responseCode: $responseCode, $debugMessage)"
      is StoreUnavailable -> "storeUnavailable"
    }
}

object BadgeStore {
  private enum class LoadState { NotLoaded, Loading, Loaded, Failed }

  private val state = mutableStateOf(LoadState.NotLoaded)
  // snapshot state so a composable reading only the products still recomposes when they arrive
  private val products = mutableStateOf<Map<BadgeStoreProductId, BadgeProduct>>(emptyMap())

  fun price(level: BadgeLevel, period: BadgePeriod): BadgePrice = when (state.value) {
    LoadState.NotLoaded, LoadState.Loading -> BadgePrice.Loading
    LoadState.Loaded, LoadState.Failed -> {
      val p = products.value[badgeStoreProductId(level, period)]
      if (p != null) BadgePrice.Price(compactPrice(p)) else BadgePrice.Unavailable
    }
  }

  fun annualSavings(level: BadgeLevel): Int? {
    val monthly = products.value[badgeStoreProductId(level, BadgePeriod.Monthly)] ?: return null
    val annual = products.value[badgeStoreProductId(level, BadgePeriod.Annual)] ?: return null
    val year = monthly.priceMicros * 12
    if (year <= 0 || annual.priceMicros >= year) return null
    val percent = Math.round((year - annual.priceMicros).toDouble() / year * 100).toInt()
    return if (percent > 0) percent else null
  }

  suspend fun load() {
    if (!startLoading()) return
    try {
      // TODO [badges] desktop and the foss build will price from the badge service catalog and pay
      // via Stripe/crypto instead of a store; only the google build reaches the platform store
      val loaded = if (useBadgeTestProducts) testBadgeProducts else platform.androidLoadBadgeProducts(
        oneTimeIds = badgeStoreProductIds.filter { it.basePlanId == null },
        subscriptionIds = badgeStoreProductIds.filter { it.basePlanId != null }
      )
      val byId = loaded.associateBy { it.id }
      val missing = badgeStoreProductIds.filter { !byId.containsKey(it) }
      if (missing.isNotEmpty()) {
        Log.w(TAG, "BadgeStore.load: no product returned for ${missing.joinToString(", ")}")
      }
      products.value = byId
      state.value = LoadState.Loaded
    } catch (e: Exception) {
      Log.e(TAG, "BadgeStore.load: ${e.stackTraceToString()}")
      state.value = LoadState.Failed
    }
  }

  suspend fun purchase(level: BadgeLevel, period: BadgePeriod, invoiceId: String): BadgePurchaseOutcome {
    val id = badgeStoreProductId(level, period)
    if (!products.value.containsKey(id)) throw BadgeStoreError.ProductUnavailable(id.productId)
    if (useBadgeTestProducts) {
      return BadgePurchaseOutcome.Purchased(
        BadgeStoreReceipt(
          token = "test-${UUID.randomUUID()}",
          productId = id.productId,
          orderId = null,
          invoiceId = invoiceId,
          environment = "test products"
        )
      )
    }
    return platform.androidPurchaseBadge(id, invoiceId)
  }

  private fun startLoading(): Boolean = when (state.value) {
    LoadState.NotLoaded, LoadState.Failed -> {
      state.value = LoadState.Loading
      true
    }
    LoadState.Loading, LoadState.Loaded -> false
  }
}

// drops the fraction from whole amounts ("$7", not "$7.00") in the product's own currency;
// BadgeProduct.displayPrice remains the exact form for views that need the cents
private fun compactPrice(product: BadgeProduct): String {
  if (product.priceMicros % 1_000_000L != 0L) return product.displayPrice
  return try {
    val format = NumberFormat.getCurrencyInstance()
    format.currency = Currency.getInstance(product.currencyCode)
    format.maximumFractionDigits = 0
    format.format(product.priceMicros / 1_000_000L)
  } catch (e: Exception) {
    product.displayPrice
  }
}
