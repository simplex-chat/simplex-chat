package chat.simplex.common.views.badges

import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.mutableStateOf
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.MR
import kotlinx.coroutines.CancellationException
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.NonCancellable
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
  val invoiceId: String?
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
  WaitingForApproval,
  Checking
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
  // purchases this run has started presenting and not finished - filled by claim, not by reading the
  // store, so it is empty at launch until the sweep reaches each one
  private val unfinished = mutableStateOf<Map<String, BadgeStoreReceipt>>(emptyMap())
  // core's open store purchases for the profile they were read for: core knows whose a purchase is
  private val storePurchases = mutableStateOf<Triple<Long?, Long, List<OpenStorePurchase>>?>(null)
  // invoices this run has an open store sheet for, and whose interactive presentation has not returned
  private val buying = mutableStateOf<Set<String>>(emptySet())
  // by invoice id, so a pending purchase shows only under the profile whose record it names
  private val waitingForApproval = mutableStateOf<Set<String>>(emptySet())
  // set when the first sweep has returned or failed, which is after every held purchase has been presented
  // over the network - until then a purchase made while the app was not running is unknown
  private val reconciledOnce = mutableStateOf(false)
  // whether the purchase the user started waits on the presentation, so its outcome is shown whoever claimed it;
  // read and written on the main thread only
  private val presenting = mutableMapOf<String, Boolean>()

  fun purchaseState(userId: Long?): BadgePurchaseState? {
    if (!badgeStoreAvailable) return null
    val purchases = openStorePurchases(userId)
    val held = unfinished.value.values.mapNotNull { it.invoiceId }.toSet()
    return when {
      purchases.any { it.invoiceId?.let(held::contains) == true } -> BadgePurchaseState.Issuing
      purchases.any { it.invoiceId?.let(waitingForApproval.value::contains) == true } -> BadgePurchaseState.WaitingForApproval
      purchases.any { it.transactionRef == null && it.invoiceId?.let(buying.value::contains) != true } -> BadgePurchaseState.Checking
      else -> null
    }
  }

  fun canBuy(userId: Long?): Boolean =
    badgeStoreAvailable && reconciledOnce.value && buying.value.isEmpty() && purchaseState(userId) == null

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
    withContext(Dispatchers.Main) { buying.value += invoiceId }
    chatModel.controller.loadBadgeState(rhId)
    try {
      val outcome = storePurchase(id, invoiceId)
      when (outcome) {
        // finished only once the service answers for it, as an unfinished purchase is what the store re-delivers
        is BadgePurchaseOutcome.Purchased -> presentPurchase(outcome.receipt, interactive = true)
        is BadgePurchaseOutcome.Cancelled -> closeInvoice(userId, invoiceId)
        is BadgePurchaseOutcome.Pending -> {}
      }
      return outcome
    } catch (e: Exception) {
      closeInvoice(userId, invoiceId)
      throw e
    } finally {
      withContext(Dispatchers.Main + NonCancellable) { buying.value -= invoiceId }
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

  // only the purchase the user started may alert: an answer can reveal a profile other than the one on screen
  private suspend fun presentPurchase(receipt: BadgeStoreReceipt, interactive: Boolean) {
    val rhId = chatModel.remoteHostId()
    val userId = chatModel.currentUser.value?.userId ?: return
    if (!claim(receipt, interactive)) return
    var alertText: String? = null
    var userWaiting = false
    try {
      when (val r = chatModel.controller.apiPurchaseBadge(rhId, userId, receipt.invoiceId, ServicePayment.Google(receipt.productId, receipt.token), retry = interactive)) {
        is BadgePurchaseResult.Redeemed -> {
          withContext(Dispatchers.Main) {
            // finished below whichever profile was credited, as it is paid for; only the profile on screen shows it
            if (chatModel.controller.activeUser(rhId, r.user)) {
              BadgeModel.set(rhId, r.user.userId, r.badgeState)
              chatModel.updateUser(r.user)
              if (r.badgeState?.shown == true) appPrefs.supporterBannerShown.set(true)
            }
          }
          finish(receipt)
        }
        is BadgePurchaseResult.Failed -> {
          Log.e(TAG, "BadgeStore.presentPurchase: ${r.err?.string}")
          val refused = badgeReceiptRefused(r.err)
          if (refused) finish(receipt)
          val text = chatModel.controller.redeemErrorText(r.err, purchase = true)
          alertText = if (refused || retryCannotCredit(r.err)) text else text + "\n\n" + generalGetString(MR.strings.badges_purchase_will_retry)
        }
        null -> {}
      }
    } catch (e: Exception) {
      if (e is CancellationException) throw e
      Log.e(TAG, "BadgeStore.presentPurchase: ${e.stackTraceToString()}")
      alertText = "${generalGetString(MR.strings.error_prefix)}: ${e.message ?: e}" + "\n\n" + generalGetString(MR.strings.badges_purchase_will_retry)
    } finally {
      withContext(NonCancellable) { chatModel.controller.loadBadgeState(chatModel.remoteHostId()) }
      userWaiting = withContext(Dispatchers.Main + NonCancellable) { presenting.remove(receipt.token) == true }
    }
    if (userWaiting && alertText != null) {
      AlertManager.shared.showAlertMsg(title = generalGetString(MR.strings.badges_purchase_error), text = alertText)
    }
  }

  // at launch, on return to the foreground and on a profile switch, never on a timer
  suspend fun presentUnfinished() {
    try {
      if (useBadgeTestProducts || !platform.androidHasPlatformStore) return
      val purchases = try {
        platform.androidUnfinishedBadgePurchases()
      } catch (e: Exception) {
        Log.e(TAG, "BadgeStore.presentUnfinished: ${e.message}")
        return
      }
      // Play lists a purchase awaiting payment, so unlike on iOS the waiting state is re-found here
      withContext(Dispatchers.Main) { waitingForApproval.value = purchases.mapNotNull { (it as? BadgePurchaseOutcome.Pending)?.invoiceId }.toSet() }
      val held = purchases.mapNotNull { (it as? BadgePurchaseOutcome.Purchased)?.receipt?.invoiceId }.toSet()
      purchases.forEach { reconcile(it) }
      closeAbandoned(held)
    } finally {
      withContext(Dispatchers.Main + NonCancellable) { reconciledOnce.value = true }
    }
  }

  // A record no held purchase names and no purchase here waits on. If the store still charges for one,
  // as when this runs between an invoice's creation and its purchase starting, the receipt reopens it.
  private suspend fun closeAbandoned(held: Set<String>) {
    val userId = chatModel.currentUser.value?.userId ?: return
    chatModel.controller.loadBadgeState(chatModel.remoteHostId())
    val abandoned = withContext(Dispatchers.Main) {
      openStorePurchases(userId)
        .mapNotNull { if (it.transactionRef == null) it.invoiceId else null }
        .filter { it !in held && it !in buying.value && it !in waitingForApproval.value }
    }
    abandoned.forEach { closeInvoice(userId, it) }
  }

  private suspend fun closeInvoice(userId: Long, invoiceId: String) {
    val rhId = chatModel.remoteHostId()
    try {
      chatModel.controller.apiCloseBadgeInvoice(rhId, userId, invoiceId)
    } catch (e: Exception) {
      Log.e(TAG, "BadgeStore.closeInvoice: ${e.message}")
    }
    chatModel.controller.loadBadgeState(rhId)
  }

  suspend fun reconcile(outcome: BadgePurchaseOutcome) {
    when (outcome) {
      is BadgePurchaseOutcome.Purchased ->
        if (outcome.receipt.productId !in badgeOneTimeProductIds) finish(outcome.receipt)
        else presentPurchase(outcome.receipt, interactive = false)
      is BadgePurchaseOutcome.Pending ->
        if (outcome.invoiceId != null) withContext(Dispatchers.Main) { waitingForApproval.value += outcome.invoiceId }
      is BadgePurchaseOutcome.Cancelled -> {}
    }
  }

  // one request per purchase: the purchase itself, launch, foreground and the store can each present it
  private suspend fun claim(receipt: BadgeStoreReceipt, interactive: Boolean): Boolean = withContext(Dispatchers.Main) {
    val waiting = presenting[receipt.token]
    if (waiting != null) {
      presenting[receipt.token] = waiting || interactive
      return@withContext false
    }
    presenting[receipt.token] = interactive
    unfinished.value += receipt.token to receipt
    // a slow payment that completes arrives as a purchase, which ends the wait
    if (receipt.invoiceId != null) waitingForApproval.value -= receipt.invoiceId
    true
  }

  private suspend fun finish(receipt: BadgeStoreReceipt) {
    try {
      if (!useBadgeTestProducts) platform.androidFinishBadgePurchase(receipt)
      withContext(Dispatchers.Main) { unfinished.value -= receipt.token }
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

// the only answers after which the receipt will never be credited, so the store may stop re-delivering it
private fun badgeReceiptRefused(err: ChatError?): Boolean {
  val redeemError = ((err as? ChatError.ChatErrorChat)?.errorType as? ChatErrorType.CEBadgeRedeemError)?.badgeRedeemError
  val code = (redeemError as? BadgeRedeemError.ServiceError)?.serviceError
  return code is BadgeServiceErrorCode.ReceiptInvalid || code is BadgeServiceErrorCode.ReceiptUsed
}

private fun retryCannotCredit(err: ChatError?): Boolean =
  when (val e = ((err as? ChatError.ChatErrorChat)?.errorType as? ChatErrorType.CEBadgeRedeemError)?.badgeRedeemError) {
    is BadgeRedeemError.BadgeActive, is BadgeRedeemError.ServiceNotConfigured -> true
    is BadgeRedeemError.ServiceError -> e.serviceError is BadgeServiceErrorCode.ProviderNotConfigured || e.serviceError is BadgeServiceErrorCode.ProductUnavailable
    else -> false
  }
