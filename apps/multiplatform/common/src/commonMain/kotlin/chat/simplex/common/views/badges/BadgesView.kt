package chat.simplex.common.views.badges

import androidx.compose.animation.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.remember
import chat.simplex.common.model.BadgeModel
import chat.simplex.common.model.BadgeState
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.ModalManager
import chat.simplex.common.views.helpers.ModalView
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.res.MR

@OptIn(ExperimentalAnimationApi::class)
@Composable
fun BadgesView(modalManager: ModalManager, close: () -> Unit) {
  val shownBadge = currentShownBadge()
  val unwindToDepth = remember { modalManager.openModalCount() }

  // the card look is a modal setting, so the modal is composed here to follow the screen shown
  ModalView(close, cardScreen = shownBadge != null) {
    AnimatedContent(
      targetState = Triple(shownBadge, BadgeStore.purchaseState(chatModel.currentUser.value?.userId), BadgeStore.checkingPurchases),
      transitionSpec = { fadeIn() with fadeOut() },
      contentKey = { (badgeState, purchaseState, checkingPurchases) -> Triple(badgeState != null, purchaseState, checkingPurchases) }
    ) { (badgeState, purchaseState, checkingPurchases) ->
      if (badgeState != null) {
        BadgesYourBadgeView(badgeState, modalManager)
      } else if (purchaseState != null) {
        // holds the purchase screens' slot, so a consumable cannot be bought twice
        BadgesPurchaseStateView(purchaseState.title, purchaseState.message, BadgeStore.creditError(chatModel.currentUser.value?.userId), onDismiss = close)
        LaunchedEffect(purchaseState) {
          BadgeStore.refusals.collect { refusal ->
            // only the issuing screen belongs to the active profile's held purchase, so a refusal shown there reads as its own
            if (purchaseState == BadgePurchaseState.Issuing) {
              AlertManager.shared.showAlertMsg(title = generalGetString(MR.strings.badges_purchase_error), text = chatModel.controller.redeemErrorText(refusal, purchase = true))
            }
          }
        }
      } else if (checkingPurchases) {
        BadgesPurchaseStateView(MR.strings.badges_checking_purchases_title, null, onDismiss = close)
      } else {
        BadgesSupportSimplexView(modalManager, unwindToDepth)
      }
    }
  }
}

fun currentShownBadge(): BadgeState? {
  if (!BadgeModel.isCurrent(chatModel.remoteHostId(), chatModel.currentUser.value?.userId)) return null
  val badgeState = BadgeModel.badgeState.value
  return if (badgeState != null && badgeState.shown) badgeState else null
}

// The badges modal shows the purchase state itself, so screens pushed over it have to go once a purchase
// appears. Only the top one is composed, so this runs there, closing them in one effect because a close per
// recomposition restarts showInView's transition, and stopping on a depth because closeModal defers removal.
// TODO [badges] ModalManager has no close-above and records no parentage; with either, the depth goes away.
@Composable
fun CloseWhenSupportGivesWay(modalManager: ModalManager, unwindToDepth: Int) {
  val gaveWay = BadgeStore.purchaseState(chatModel.currentUser.value?.userId) != null || currentShownBadge() != null
  LaunchedEffect(gaveWay) {
    if (gaveWay) while (modalManager.openModalCount() > unwindToDepth) modalManager.closeModal()
  }
}

// ModalManager.end, not start: every caller is in the chat, which on desktop is the right pane
fun openBadgesView() {
  ModalManager.end.showCustomModal { close -> BadgesView(ModalManager.end, close) }
}
