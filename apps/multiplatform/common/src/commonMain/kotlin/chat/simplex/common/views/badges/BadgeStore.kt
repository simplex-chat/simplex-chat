package chat.simplex.common.views.badges

import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.mutableStateOf
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.ModalManager
import chat.simplex.common.views.helpers.ModalViewId
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.*
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

val badgeProductIds: Set<String> = badgeStoreProductIds.map { it.productId }.toSet()

// a one-time product is acknowledged when core holds it and consumed when finished; a subscription is
// neither, as the service acknowledges it when it credits it, and Play refunds it if it never does
val badgeOneTimeProductIds: Set<String> = badgeStoreProductIds.filter { it.basePlanId == null }.map { it.productId }.toSet()

// the page's app flag rides in the fragment, which never reaches the service
fun badgePageUrl(linkReturns: Boolean): String =
  if (linkReturns) "https://badges.simplex.chat/#/tier?app=true"
  else "https://badges.simplex.chat/#/tier?app=desktop"
// the dev deployment, for testing store purchases: swap with the two lines above
// if (linkReturns) "https://smp7.simplex.im/#/tier?app=true"
// else "https://smp7.simplex.im/#/tier?app=desktop"

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
  val acknowledged: Boolean = false,
  // a subscription's, as Play lists it; null for a one-time product
  val autoRenewing: Boolean? = null
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
  object AlreadyOwned: BadgeStoreError()
  object StoreUnreachable: BadgeStoreError()
  object NotOnSale: BadgeStoreError()
  object StoreUnavailable: BadgeStoreError()
  object NoActiveProfile: BadgeStoreError()
  class ApiError(val err: ChatError): BadgeStoreError()

  override val message: String
    get() = when (this) {
      is ProductUnavailable -> "productUnavailable(productId: $productId)"
      is BillingError -> "billingError(responseCode: $responseCode, $debugMessage)"
      is AlreadyOwned -> "alreadyOwned"
      is StoreUnreachable -> "storeUnreachable"
      is NotOnSale -> "notOnSale"
      is StoreUnavailable -> "storeUnavailable"
      is NoActiveProfile -> "noActiveProfile"
      is ApiError -> "apiError(${err.string})"
    }
}

// the sentences iOS shows for the same store failures, in redeemErrorText
fun badgeStoreErrorText(e: Exception): String? = when (e) {
  is BadgeStoreError.AlreadyOwned -> generalGetString(MR.strings.badges_error_already_owned)
  is BadgeStoreError.StoreUnreachable -> generalGetString(MR.strings.badges_error_store_unreachable)
  is BadgeStoreError.NotOnSale -> generalGetString(MR.strings.badges_error_not_on_sale)
  else -> null
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
  // the sweep has no buyer waiting and runs at four triggers, so a failure that cannot clear is told once a run
  private var sweepFailureShown = false

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

  // Play gives a subscription bought again a new token, and refuses to sell one the account still has
  @Suppress("UNUSED_PARAMETER")
  fun forSale(period: BadgePeriod): Boolean = true

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

  // A purchase has its core record before the store charges, and every store outcome reaches it.
  suspend fun purchase(level: BadgeLevel, period: BadgePeriod): BadgePurchaseOutcome {
    val id = badgeStoreProductId(level, period)
    if (!products.value.containsKey(id)) throw BadgeStoreError.ProductUnavailable(id.productId)
    val rhId = chatModel.remoteHostId()
    val userId = chatModel.currentUser.value?.userId ?: throw BadgeStoreError.NoActiveProfile
    val invoiceId = chatModel.controller.apiCreateBadgeInvoice(rhId, userId)
    withContext(Dispatchers.Main) { buying.value = true }
    chatModel.controller.loadBadgeState(rhId)
    try {
      val outcome = storePurchase(id, invoiceId)
      if (outcome is BadgePurchaseOutcome.Purchased) {
        handOver(outcome.receipt, StoreReceiptOrigin.Purchased)
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
          // the dev service's mock reads a subscription's plan from its token, as Play's real ones name none
          token = if (id.basePlanId != null) "test.${id.basePlanId}.${UUID.randomUUID()}" else "test-${UUID.randomUUID()}",
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
  private suspend fun handOver(receipt: BadgeStoreReceipt, origin: StoreReceiptOrigin) {
    val rhId = chatModel.remoteHostId()
    val userId = chatModel.currentUser.value?.userId ?: return
    when (val r = chatModel.controller.apiPurchaseBadge(rhId, userId, receipt.invoiceId, origin, ServicePayment.Google(receipt.productId, receipt.token))) {
      is BadgePurchaseResult.Held -> {
        // core holds it durably now, so Play must not refund it after three days unacknowledged;
        // a subscription is acknowledged by the service when it credits it
        if (receipt.productId in badgeOneTimeProductIds) acknowledge(receipt)
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
        finish(receipt)
      }
      is BadgePurchaseResult.NotRecorded -> {
        // left unfinished, for the device that bought it, or for Restore to claim it here
      }
      // a refusal is announced from core's event for it, so only an unexpected answer reaches the buyer
      is BadgePurchaseResult.Failed ->
        if (badgeReceiptRefused(r.err)) {
          Log.e(TAG, "BadgeStore.handOver: ${r.err?.string}")
          finish(receipt)
          // Restore found this purchase, so it says why it was refused rather than that there was nothing to restore
          if (origin == StoreReceiptOrigin.Restored && r.err != null) throw BadgeStoreError.ApiError(r.err)
        } else {
          throw r.err?.let(BadgeStoreError::ApiError) ?: Exception("apiPurchaseBadge: unexpected response")
        }
    }
  }

  suspend fun storePurchaseCredited(rhId: Long?, user: UserRef, invoiceId: String) {
    withContext(Dispatchers.Main) {
      // not isLastModalOpen: the purchase screens pushed over the badges screen close once the badge appears
      val badgesOpen = ModalManager.start.hasModalOpen(ModalViewId.BADGES) || ModalManager.end.hasModalOpen(ModalViewId.BADGES)
      // an open badges screen is the notice only for the active profile, whose badge it shows
      if (!chatModel.controller.activeUser(rhId, user) || !badgesOpen) showBadgeAddedAlert(rhId, user)
    }
    storePurchaseResolved(rhId, user, invoiceId)
  }

  suspend fun storePurchaseRefused(rhId: Long?, user: UserRef, invoiceId: String, refusal: BadgeIssueFailure) {
    withContext(Dispatchers.Main) {
      if (chatModel.controller.activeUser(rhId, user) && openStorePurchases(user.userId).any { it.invoiceId == invoiceId }) {
        AlertManager.shared.showAlertMsg(title = generalGetString(MR.strings.badges_purchase_error), text = refusal.purchaseText)
      }
    }
    storePurchaseResolved(rhId, user, invoiceId)
  }

  private suspend fun storePurchaseResolved(rhId: Long?, user: UserRef, invoiceId: String) {
    // the row goes before the store is read, or the screen shows it open while the store answers;
    // the purchase is finished whoever owns it, as only the app can, while the cached row is the active profile's
    withContext(Dispatchers.Main) {
      val (readOn, readFor, purchases) = storePurchases.value ?: return@withContext
      if (chatModel.controller.activeUser(rhId, user) && readOn == rhId && readFor == user.userId) {
        storePurchases.value = Triple(readOn, readFor, purchases.filter { it.invoiceId != invoiceId })
      }
    }
    if (rhId != null || useBadgeTestProducts || !platform.androidHasPlatformStore) return
    try {
      platform.androidUnfinishedBadgePurchases()
        .firstNotNullOfOrNull { (it as? BadgePurchaseOutcome.Purchased)?.receipt?.takeIf { r -> r.invoiceId == invoiceId } }
        ?.let { finish(it) }
    } catch (e: Exception) {
      if (e is CancellationException) throw e
      // the purchase stays unfinished, and the next sweep hands it over and finishes it
      Log.e(TAG, "BadgeStore.storePurchaseResolved: ${e.message}")
    }
  }

  // at launch, on return to the foreground and on a profile switch, never on a timer
  suspend fun presentUnfinished() {
    try {
      if (!useBadgeTestProducts && platform.androidHasPlatformStore) {
        try {
          val purchases = platform.androidUnfinishedBadgePurchases()
          // Play lists a purchase awaiting payment, so unlike on iOS the waiting state is re-found here
          withContext(Dispatchers.Main) { waitingForApproval.value = purchases.mapNotNull { (it as? BadgePurchaseOutcome.Pending)?.invoiceId }.toSet() }
          purchases.forEach { reconcileListed(it) }
        } catch (e: Exception) {
          if (e is CancellationException) throw e
          Log.e(TAG, "BadgeStore.presentUnfinished: ${e.message}")
        }
      }
      chatModel.controller.loadBadgeState(chatModel.remoteHostId())
    } finally {
      withContext(Dispatchers.Main + NonCancellable) { reconciledOnce.value = true }
    }
    reportSubscriptionStates()
  }

  // Play lists a Google account's purchases on every device signed into it, so these are handed over as listed
  suspend fun reconcileListed(outcome: BadgePurchaseOutcome) {
    // the purchase stays unfinished, so the next sweep hands it over again
    try {
      reconcile(outcome, StoreReceiptOrigin.Listed)
    } catch (e: Exception) {
      if (e is CancellationException) throw e
      Log.e(TAG, "BadgeStore.reconcile: ${e.stackTraceToString()}")
      withContext(Dispatchers.Main) {
        if (!sweepFailureShown) {
          sweepFailureShown = true
          AlertManager.shared.showAlertMsg(
            title = generalGetString(MR.strings.badges_purchase_error),
            text = if (e is BadgeStoreError.ApiError) chatModel.controller.redeemErrorText(e.err, purchase = true)
              else badgeStoreErrorText(e) ?: "${generalGetString(MR.strings.error_prefix)}: ${e.message ?: e}"
          )
        }
      }
    }
  }

  private suspend fun reconcile(outcome: BadgePurchaseOutcome, origin: StoreReceiptOrigin) {
    when (outcome) {
      is BadgePurchaseOutcome.Purchased ->
        if (outcome.receipt.productId !in badgeProductIds) finish(outcome.receipt)
        else handOver(outcome.receipt, origin)
      is BadgePurchaseOutcome.Pending ->
        if (outcome.invoiceId != null) withContext(Dispatchers.Main) { waitingForApproval.value += outcome.invoiceId }
      is BadgePurchaseOutcome.Cancelled -> {}
    }
  }

  // The service asks the store only once the paid period ends, so a cancellation or a failed charge before
  // then is seen only here. Play lists no subscription on hold or expired, so it tells renewing or cancelled only.
  suspend fun reportSubscriptionStates() {
    // the store's purchases are this device's, so they are told to its own core
    if (chatModel.remoteHostId() != null || useBadgeTestProducts || !platform.androidHasPlatformStore) return
    try {
      val purchases = platform.androidUnfinishedBadgePurchases()
      if (purchases.none { (it as? BadgePurchaseOutcome.Purchased)?.receipt?.autoRenewing != null }) return
      load()
      for (outcome in purchases) {
        val receipt = (outcome as? BadgePurchaseOutcome.Purchased)?.receipt ?: continue
        val state = when (receipt.autoRenewing) {
          true -> BadgeSubscriptionState.Renewing
          false -> BadgeSubscriptionState.Cancelled
          null -> null
        }
        if (state != null) {
          chatModel.controller.apiSetBadgeSubscriptionStatus(null, state, ServicePayment.Google(receipt.productId, receipt.token))
        }
      }
    } catch (e: Exception) {
      if (e is CancellationException) throw e
      Log.e(TAG, "BadgeStore.reportSubscriptionStates: ${e.message}")
    }
  }

  // Play's page opens over the app, so the foreground sweep re-reads the subscription on return
  fun manageSubscriptions(badgeType: BadgeType) {
    val level = BadgeLevel.entries.firstOrNull { it.badgeType == badgeType } ?: return
    platform.androidManageBadgeSubscription(badgeStoreProductId(level, BadgePeriod.Monthly).productId)
  }

  // Hands over as restored whatever the store lists, so this profile claims it, also from another install or profile.
  // Answers whether a badge is shown, or a purchase is being added or awaits approval, afterwards.
  suspend fun restorePurchases(): Boolean {
    var handOverError: Exception? = null
    val purchases = if (useBadgeTestProducts) emptyList() else platform.androidUnfinishedBadgePurchases()
    for (outcome in purchases) {
      try {
        reconcile(outcome, StoreReceiptOrigin.Restored)
      } catch (e: Exception) {
        if (e is CancellationException) throw e
        Log.e(TAG, "BadgeStore.restorePurchases: ${e.stackTraceToString()}")
        if (handOverError == null) handOverError = e
      }
    }
    reportSubscriptionStates()
    val rhId = chatModel.remoteHostId()
    chatModel.controller.loadBadgeState(rhId)
    val restored = withContext(Dispatchers.Main) {
      val userId = chatModel.currentUser.value?.userId
      val badgeShown = BadgeModel.isCurrent(rhId, userId) && BadgeModel.badgeState.value?.shown == true
      badgeShown || purchaseState(userId) != null
    }
    if (!restored && handOverError != null) throw handOverError
    return restored
  }

  fun subscriptionPrice(badgeType: BadgeType, planMonths: Int): String? {
    val period = when (planMonths) {
      1 -> BadgePeriod.Monthly
      12 -> BadgePeriod.Annual
      else -> null
    }
    val level = BadgeLevel.entries.firstOrNull { it.badgeType == badgeType }
    if (period == null || level == null) return null
    return products.value[badgeStoreProductId(level, period)]?.displayPrice
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

// the codes core refuses a receipt with for good, after which the store may stop re-delivering it;
// a sweep that hands over a receipt refused while the app was not running learns it only from this answer
private fun badgeReceiptRefused(err: ChatError?): Boolean {
  val code = badgeServiceErrorCode(err)
  return code is BadgeServiceErrorCode.ReceiptInvalid || code is BadgeServiceErrorCode.ReceiptUsed || code is BadgeServiceErrorCode.TestPurchase || code is BadgeServiceErrorCode.TransferLimited
}

private fun badgeServiceErrorCode(err: ChatError?): BadgeServiceErrorCode? {
  val redeemError = ((err as? ChatError.ChatErrorChat)?.errorType as? ChatErrorType.CEBadgeRedeemError)?.badgeRedeemError
  return (redeemError as? BadgeRedeemError.ServiceError)?.serviceError
}
