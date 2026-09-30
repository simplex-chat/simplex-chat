package chat.simplex.common.views.badges

import androidx.compose.animation.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import chat.simplex.common.model.BadgeModel
import chat.simplex.common.model.BadgeState
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.helpers.ModalManager
import chat.simplex.common.views.helpers.ModalView

@OptIn(ExperimentalAnimationApi::class)
@Composable
fun BadgesView(modalManager: ModalManager, close: () -> Unit) {
  val shownBadge: BadgeState? = run {
    if (!BadgeModel.isCurrent(chatModel.remoteHostId(), chatModel.currentUser.value?.userId)) return@run null
    val badgeState = BadgeModel.badgeState.value
    if (badgeState != null && badgeState.shown) badgeState else null
  }

  // the card look is a modal setting, so the modal is composed here to follow the screen shown
  ModalView(close, cardScreen = shownBadge != null) {
    AnimatedContent(
      targetState = shownBadge to BadgeStore.purchaseState(chatModel.currentUser.value?.userId),
      transitionSpec = { fadeIn() with fadeOut() },
      contentKey = { (badgeState, purchaseState) -> (badgeState != null) to purchaseState }
    ) { (badgeState, purchaseState) ->
      if (badgeState != null) {
        BadgesYourBadgeView(badgeState, modalManager)
      } else if (purchaseState != null) {
        // holds the purchase screens' slot, so a consumable cannot be bought twice
        BadgesPurchaseStateView(purchaseState)
      } else {
        BadgesSupportSimplexView(modalManager)
      }
    }
  }
}

// only the top modal is composed, so each purchase screen closes itself in turn, as iOS pops them
// when Support SimpleX gives way to the purchase in flight
@Composable
fun CloseWhenPurchaseInFlight(modalManager: ModalManager) {
  val inFlight = BadgeStore.purchaseState(chatModel.currentUser.value?.userId) != null
  LaunchedEffect(inFlight) {
    if (inFlight) modalManager.closeModal()
  }
}

// ModalManager.end, not start: every caller is in the chat, which on desktop is the right pane
fun openBadgesView() {
  ModalManager.end.showCustomModal { close -> BadgesView(ModalManager.end, close) }
}
