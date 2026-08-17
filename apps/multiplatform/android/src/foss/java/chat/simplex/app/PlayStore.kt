package chat.simplex.app

import chat.simplex.common.platform.Log
import chat.simplex.common.views.badges.BadgeProduct
import chat.simplex.common.views.badges.BadgePurchaseOutcome
import chat.simplex.common.views.badges.BadgeStoreError
import chat.simplex.common.views.badges.BadgeStoreProductId

// Play Billing is only in the google flavor, so the Play country stays unknown here
fun loadPlayStoreCountry() {}

// No store in this flavor: no product is offered, so the purchase screen shows nothing to buy
// TODO [badges] this build pays via Stripe/crypto - the badge service catalog replaces these
@Suppress("UNUSED_PARAMETER")
suspend fun loadBadgeProducts(oneTimeIds: List<BadgeStoreProductId>, subscriptionIds: List<BadgeStoreProductId>): List<BadgeProduct> {
  Log.w(TAG, "loadBadgeProducts: no store in the foss build, use the google variant to buy from Play")
  return emptyList()
}

@Suppress("UNUSED_PARAMETER")
suspend fun purchaseBadge(id: BadgeStoreProductId, invoiceId: String): BadgePurchaseOutcome =
  throw BadgeStoreError.StoreUnavailable
