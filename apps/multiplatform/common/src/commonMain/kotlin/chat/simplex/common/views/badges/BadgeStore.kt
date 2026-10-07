package chat.simplex.common.views.badges

import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.mutableStateOf
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import kotlinx.coroutines.CancellationException
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.NonCancellable
import kotlinx.coroutines.flow.MutableSharedFlow
import kotlinx.coroutines.withContext
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

// the only products sent to the badge service: nothing delivers a subscription yet
val badgeOneTimeProductIds: Set<String> = badgeStoreProductIds.filter { it.basePlanId == null }.map { it.productId }.toSet()

// subscriptions are for sale once renewals are delivered
val badgePeriodsForSale: List<BadgePeriod> = listOf(BadgePeriod.OneMonth)

// A subscription's id only: core mints a one-time purchase's, and a subscription is never sent to core.
// Sent to Play as obfuscatedAccountId and echoed back on the purchase.
fun newBadgeInvoiceId(): String = UUID.randomUUID().toString()

// the page's app flag rides in the fragment, which never reaches the service
// TEST ONLY: pointed at the dev deployment; restore the two lines below before merging
val badgePageUrl: String =
  if (appPlatform.isAndroid) "https://smp7.simplex.im/#/tier?app=true"
  else "https://smp7.simplex.im/#/tier?app=desktop"
// if (appPlatform.isAndroid) "https://badges.simplex.chat/#/tier?app=true"
// else "https://badges.simplex.chat/#/tier?app=desktop"

// where the store allows a link out to the badge page: the US for now, a set expected to widen.
// An unknown country does not count, as this decides whether the store sees a link out of the app.
@Composable
fun badgeBrowserAllowed(): Boolean {
  if (androidPlayStoreCountry.value == null) {
    LaunchedEffect(Unit) { platform.androidLoadPlayStoreCountry() }
  }
  return androidPlayStoreCountry.value == "US"
}

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
  val invoiceId: String?,
  val acknowledged: Boolean = false
)

// TODO [badges] Play Billing has no offline product configuration. Set to true to price the screens
// and walk the purchase flow without Play Console products; the purchase is simulated and its
// receipt says so.
const val useBadgeTestProducts = false

// test products stand in for a store, so the lane is offered without one
val badgeStoreAvailable: Boolean get() = useBadgeTestProducts || platform.androidHasPlatformStore

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
  class Pending(val invoiceId: String?): BadgePurchaseOutcome()
  object Cancelled: BadgePurchaseOutcome()
}

enum class BadgePurchaseState {
  Issuing,
  WaitingForApproval
}

sealed class BadgeStoreError: Exception() {
  class ProductUnavailable(val productId: String): BadgeStoreError()
  class BillingError(val responseCode: Int, val debugMessage: String): BadgeStoreError()
  object StoreUnavailable: BadgeStoreError()
  object NoActiveProfile: BadgeStoreError()
  class InvoiceRefused(val err: ChatError): BadgeStoreError()

  override val message: String
    get() = when (this) {
      is ProductUnavailable -> "productUnavailable(productId: $productId)"
      is BillingError -> "billingError(responseCode: $responseCode, $debugMessage)"
      is StoreUnavailable -> "storeUnavailable"
      is NoActiveProfile -> "noActiveProfile"
      is InvoiceRefused -> "invoiceRefused(${err.string})"
    }
}

object BadgeStore {
  private enum class LoadState { NotLoaded, Loading, Loaded, Failed }

  private val state = mutableStateOf(LoadState.NotLoaded)
  // snapshot state so a composable reading only the products still recomposes when they arrive
  private val products = mutableStateOf<Map<BadgeStoreProductId, BadgeProduct>>(emptyMap())
  // core's open store purchases for the profile they were read for: core knows whose a purchase is
  private val storePurchases = mutableStateOf<Triple<Long?, Long, List<OpenStorePurchase>>?>(null)
  // whether a store sheet this run opened has not returned
  private val buying = mutableStateOf(false)
  // by invoice id, so a pending purchase shows only under the profile whose record it names
  private val waitingForApproval = mutableStateOf<Set<String>>(emptySet())
  // set once presentUnfinished has read the store, or failed to: until then, a slow payment completed while
  // the app was closed, or a purchase it died before handing over, are both unknown, so canBuy refuses
  private val reconciledOnce = mutableStateOf(false)
  val refusals = MutableSharedFlow<ChatError>()

  fun purchaseState(userId: Long?): BadgePurchaseState? {
    if (!badgeStoreAvailable) return null
    val purchases = openStorePurchases(userId)
    return when {
      purchases.any { it.transactionRef != null } -> BadgePurchaseState.Issuing
      purchases.any { it.invoiceId?.let(waitingForApproval.value::contains) == true } -> BadgePurchaseState.WaitingForApproval
      else -> null
    }
  }

  fun creditError(userId: Long?): BadgeIssueFailure? =
    openStorePurchases(userId).firstNotNullOfOrNull { it.creditError }

  val checkingPurchases: Boolean get() = badgeStoreAvailable && !reconciledOnce.value

  fun canBuy(userId: Long?): Boolean =
    badgeStoreAvailable && reconciledOnce.value && !buying.value && purchaseState(userId) == null

  fun setStorePurchases(rhId: Long?, userId: Long, purchases: List<OpenStorePurchase>) {
    storePurchases.value = Triple(rhId, userId, purchases)
  }

  private fun openStorePurchases(userId: Long?): List<OpenStorePurchase> {
    val (rhId, readFor, purchases) = storePurchases.value ?: return emptyList()
    return if (rhId == chatModel.remoteHostId() && readFor == userId) purchases else emptyList()
  }

  fun price(level: BadgeLevel, period: BadgePeriod, compact: Boolean = true): BadgePrice = when (state.value) {
    LoadState.NotLoaded, LoadState.Loading -> BadgePrice.Loading
    LoadState.Loaded, LoadState.Failed -> {
      val p = products.value[badgeStoreProductId(level, period)]
      if (p != null) BadgePrice.Price(if (compact) compactPrice(p) else p.displayPrice) else BadgePrice.Unavailable
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

  // A one-time purchase has its core record before the store charges, and every store outcome reaches it.
  suspend fun purchase(level: BadgeLevel, period: BadgePeriod): BadgePurchaseOutcome {
    val id = badgeStoreProductId(level, period)
    if (!products.value.containsKey(id)) throw BadgeStoreError.ProductUnavailable(id.productId)
    // a subscription is never sent to core, so nothing would ever finish it later
    if (id.productId !in badgeOneTimeProductIds) {
      val outcome = storePurchase(id, newBadgeInvoiceId())
      if (outcome is BadgePurchaseOutcome.Purchased) finish(outcome.receipt)
      return outcome
    }
    val rhId = chatModel.remoteHostId()
    val userId = chatModel.currentUser.value?.userId ?: throw BadgeStoreError.NoActiveProfile
    val invoiceId = chatModel.controller.apiCreateBadgeInvoice(rhId, userId)
    withContext(Dispatchers.Main) { buying.value = true }
    chatModel.controller.loadBadgeState(rhId)
    try {
      val outcome = storePurchase(id, invoiceId)
      if (outcome is BadgePurchaseOutcome.Purchased) {
        handOver(outcome.receipt)
      }
      return outcome
    } finally {
      withContext(Dispatchers.Main + NonCancellable) { buying.value = false }
    }
  }

  private suspend fun storePurchase(id: BadgeStoreProductId, invoiceId: String): BadgePurchaseOutcome {
    val outcome = if (useBadgeTestProducts) {
      BadgePurchaseOutcome.Purchased(
        BadgeStoreReceipt(
          token = "test-${UUID.randomUUID()}",
          productId = id.productId,
          invoiceId = invoiceId
        )
      )
    } else {
      platform.androidPurchaseBadge(id, invoiceId)
    }
    if (outcome is BadgePurchaseOutcome.Pending && outcome.invoiceId != null) withContext(Dispatchers.Main) { waitingForApproval.value += outcome.invoiceId }
    return outcome
  }

  // Core holds the receipt and credits it; the purchase stays unfinished until core answers it credited
  // or refused, as an unfinished purchase is what the store re-delivers if anything is lost on the way.
  private suspend fun handOver(receipt: BadgeStoreReceipt) {
    val rhId = chatModel.remoteHostId()
    val userId = chatModel.currentUser.value?.userId ?: return
    try {
      when (val r = chatModel.controller.apiPurchaseBadge(rhId, userId, receipt.invoiceId, ServicePayment.Google(receipt.productId, receipt.token))) {
        is BadgePurchaseResult.Held -> {
          // core holds it durably now, so Play must not refund it after three days unacknowledged
          acknowledge(receipt)
          withContext(Dispatchers.Main) {
            // the answer is the owner's, which may be another profile and a hidden one
            if (chatModel.controller.activeUser(rhId, r.user)) {
              BadgeModel.set(rhId, r.user.userId, r.badgeState)
              setStorePurchases(rhId, r.user.userId, r.storePurchases)
            }
          }
        }
        is BadgePurchaseResult.Credited -> {
          withContext(Dispatchers.Main) {
            if (chatModel.controller.activeUser(rhId, r.user)) {
              BadgeModel.set(rhId, r.user.userId, r.badgeState)
              chatModel.updateUser(r.user)
              if (r.badgeState?.shown == true) appPrefs.supporterBannerShown.set(true)
            }
          }
          resolve(receipt, refusal = null)
        }
        is BadgePurchaseResult.Failed -> {
          Log.e(TAG, "BadgeStore.handOver: ${r.err?.string}")
          if (badgeReceiptRefused(r.err)) resolve(receipt, refusal = r.err)
        }
      }
    } catch (e: Exception) {
      if (e is CancellationException) throw e
      Log.e(TAG, "BadgeStore.handOver: ${e.stackTraceToString()}")
    }
  }

  private suspend fun resolve(receipt: BadgeStoreReceipt, refusal: ChatError?) {
    finish(receipt)
    if (refusal != null) {
      withContext(Dispatchers.Main) { refusals.emit(refusal) }
    }
  }

  // at launch, on return to the foreground, on a profile switch and when core resolves a purchase, never on a timer
  suspend fun presentUnfinished() {
    try {
      if (!useBadgeTestProducts && platform.androidHasPlatformStore) {
        try {
          val purchases = platform.androidUnfinishedBadgePurchases()
          // Play lists a purchase awaiting payment, so unlike on iOS the waiting state is re-found here
          withContext(Dispatchers.Main) { waitingForApproval.value = purchases.mapNotNull { (it as? BadgePurchaseOutcome.Pending)?.invoiceId }.toSet() }
          purchases.forEach { reconcile(it) }
        } catch (e: Exception) {
          if (e is CancellationException) throw e
          Log.e(TAG, "BadgeStore.presentUnfinished: ${e.message}")
        }
      }
      chatModel.controller.loadBadgeState(chatModel.remoteHostId())
    } finally {
      withContext(Dispatchers.Main + NonCancellable) { reconciledOnce.value = true }
    }
  }

  suspend fun reconcile(outcome: BadgePurchaseOutcome) {
    when (outcome) {
      is BadgePurchaseOutcome.Purchased ->
        if (outcome.receipt.productId !in badgeOneTimeProductIds) finish(outcome.receipt)
        else handOver(outcome.receipt)
      is BadgePurchaseOutcome.Pending ->
        if (outcome.invoiceId != null) withContext(Dispatchers.Main) { waitingForApproval.value += outcome.invoiceId }
      is BadgePurchaseOutcome.Cancelled -> {}
    }
  }

  private suspend fun acknowledge(receipt: BadgeStoreReceipt) {
    try {
      if (!useBadgeTestProducts) platform.androidAcknowledgeBadgePurchase(receipt)
    } catch (e: Exception) {
      // the next sweep hands the purchase over again, and acknowledges it then
      Log.e(TAG, "BadgeStore.acknowledge: ${e.message}")
    }
  }

  private suspend fun finish(receipt: BadgeStoreReceipt) {
    try {
      if (!useBadgeTestProducts) platform.androidFinishBadgePurchase(receipt)
    } catch (e: Exception) {
      // still unfinished: the store re-delivers it, and the next answer for it finishes it
      Log.e(TAG, "BadgeStore.finish: ${e.message}")
    }
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

// the codes core refuses a receipt with for good, after which the store may stop re-delivering it
private fun badgeReceiptRefused(err: ChatError?): Boolean {
  val redeemError = ((err as? ChatError.ChatErrorChat)?.errorType as? ChatErrorType.CEBadgeRedeemError)?.badgeRedeemError
  val code = (redeemError as? BadgeRedeemError.ServiceError)?.serviceError
  return code is BadgeServiceErrorCode.ReceiptInvalid || code is BadgeServiceErrorCode.ReceiptUsed
}
