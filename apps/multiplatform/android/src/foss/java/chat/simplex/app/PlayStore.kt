package chat.simplex.app

import chat.simplex.common.views.badges.BadgeProduct
import chat.simplex.common.views.badges.BadgePurchaseOutcome
import chat.simplex.common.views.badges.BadgeStoreError

// Play Billing is only in the google flavor, so the Play country stays unknown here
fun loadPlayStoreCountry() {}

// No store in this flavor: no product is offered, so the purchase screen shows nothing to buy
@Suppress("UNUSED_PARAMETER")
suspend fun loadBadgeProducts(productIds: List<String>): List<BadgeProduct> = emptyList()

@Suppress("UNUSED_PARAMETER")
suspend fun purchaseBadge(productId: String, invoiceId: String): BadgePurchaseOutcome =
  throw BadgeStoreError.StoreUnavailable
