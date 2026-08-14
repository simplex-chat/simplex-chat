package chat.simplex.common.views.badges

import androidx.compose.runtime.mutableStateOf
import chat.simplex.common.platform.*
import java.text.NumberFormat
import java.util.Currency
import java.util.UUID

// TODO [badges] product ids will come from app config and prices from the badge service catalog;
// hardcoded here so the Play Store integration can be tested before the purchase API lands.
fun badgeProductId(level: BadgeLevel, period: BadgePeriod): String = when (level) {
  BadgeLevel.Supporter -> when (period) {
    BadgePeriod.OneMonth -> "BADGE_SUPPORTER_01"
    BadgePeriod.Monthly -> "SUBSCR_BADGE_SUPPORTER_MONTH_01"
    BadgePeriod.Annual -> "SUBSCR_BADGE_SUPPORTER_YEAR_01"
  }
  BadgeLevel.Legend -> when (period) {
    BadgePeriod.OneMonth -> "BADGE_LEGEND_01"
    BadgePeriod.Monthly -> "SUBSCR_BADGE_LEGEND_MONTH_01"
    BadgePeriod.Annual -> "SUBSCR_BADGE_LEGEND_YEAR_01"
  }
}

val badgeProductIds: List<String> = BadgeLevel.entries.flatMap { level ->
  BadgePeriod.entries.map { badgeProductId(level, it) }
}

// TODO [badges] replaced by APIGetBadgeInvoice, which creates the invoice row and returns its id.
// Sent to Play as obfuscatedAccountId and echoed back on the purchase, which is how the service
// learns which invoice a store transaction settles.
fun newBadgeInvoiceId(): String = UUID.randomUUID().toString()

// what the platform store knows about one product; ProductDetails cannot cross into commonMain
data class BadgeProduct(
  val productId: String,
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

// TODO [badges] Play Billing has no offline configuration, so there is no analogue of the iOS
// .storekit file. Set to true to price the screens and walk the purchase flow without Play Console
// products; the purchase is simulated and its receipt says so.
const val useBadgeTestProducts = false

private fun testProduct(level: BadgeLevel, period: BadgePeriod, priceMicros: Long) =
  BadgeProduct(badgeProductId(level, period), "\$${priceMicros / 1_000_000}.00", priceMicros, "USD")

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
  // snapshot state, unlike the plain dictionary on iOS: Compose tracks reads per value, so a
  // composable that only reads the products would not recompose when they arrive
  private val products = mutableStateOf<Map<String, BadgeProduct>>(emptyMap())

  fun price(level: BadgeLevel, period: BadgePeriod): BadgePrice = when (state.value) {
    LoadState.NotLoaded, LoadState.Loading -> BadgePrice.Loading
    LoadState.Loaded, LoadState.Failed -> {
      val p = products.value[badgeProductId(level, period)]
      if (p != null) BadgePrice.Price(compactPrice(p)) else BadgePrice.Unavailable
    }
  }

  // percentage the annual subscription saves against 12 monthly payments
  fun annualSavings(level: BadgeLevel): Int? {
    val monthly = products.value[badgeProductId(level, BadgePeriod.Monthly)] ?: return null
    val annual = products.value[badgeProductId(level, BadgePeriod.Annual)] ?: return null
    val year = monthly.priceMicros * 12
    if (year <= 0 || annual.priceMicros >= year) return null
    val percent = Math.round((year - annual.priceMicros).toDouble() / year * 100).toInt()
    return if (percent > 0) percent else null
  }

  suspend fun load() {
    if (!startLoading()) return
    try {
      val loaded = if (useBadgeTestProducts) testBadgeProducts else platform.androidLoadBadgeProducts(badgeProductIds)
      val byId = loaded.associateBy { it.productId }
      val missing = badgeProductIds.filter { !byId.containsKey(it) }
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
    val productId = badgeProductId(level, period)
    if (!products.value.containsKey(productId)) throw BadgeStoreError.ProductUnavailable(productId)
    if (useBadgeTestProducts) {
      return BadgePurchaseOutcome.Purchased(
        BadgeStoreReceipt(
          token = "test-${UUID.randomUUID()}",
          productId = productId,
          orderId = null,
          invoiceId = invoiceId,
          environment = "test products"
        )
      )
    }
    return platform.androidPurchaseBadge(productId, invoiceId)
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
