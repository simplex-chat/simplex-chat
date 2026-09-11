package chat.simplex.common.views.badges

import androidx.compose.animation.*
import androidx.compose.runtime.Composable
import chat.simplex.common.model.BadgeModel
import chat.simplex.common.model.BadgeState
import chat.simplex.common.platform.chatModel

@OptIn(ExperimentalAnimationApi::class)
@Composable
fun BadgesView() {
  val shownBadge: BadgeState? = run {
    if (!BadgeModel.isCurrent(chatModel.remoteHostId(), chatModel.currentUser.value?.userId)) return@run null
    val badgeState = BadgeModel.badgeState.value
    if (badgeState != null && badgeState.shown) badgeState else null
  }

  AnimatedContent(targetState = shownBadge, transitionSpec = { fadeIn() with fadeOut() }) { badgeState ->
    if (badgeState != null) {
      BadgesYourBadgeView(badgeState)
    } else {
      BadgesSupportSimplexView()
    }
  }
}
