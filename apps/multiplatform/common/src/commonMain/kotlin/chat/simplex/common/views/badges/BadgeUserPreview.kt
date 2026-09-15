package chat.simplex.common.views.badges

import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.BadgeInfo
import chat.simplex.common.model.BadgeStatus
import chat.simplex.common.model.LocalBadge
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.helpers.NameWithBadge
import chat.simplex.common.views.helpers.ProfileImage
import chat.simplex.res.MR
import kotlinx.datetime.Instant

@Composable
fun BadgeUserPreview(level: BadgeLevel, modifier: Modifier = Modifier, trailing: @Composable () -> Unit = {}) {
  val user = chatModel.currentUser.value
  val displayName = user?.displayName ?: stringResource(MR.strings.badges_preview_my_nickname)
  val previewBadge = LocalBadge(
    // fabricated for the preview: the status is given here, and NameBadge renders from it alone
    badge = BadgeInfo(badgeType = level.badgeType, badgeExpiry = Instant.DISTANT_FUTURE),
    status = BadgeStatus.Active
  )
  Column(modifier, horizontalAlignment = Alignment.CenterHorizontally, verticalArrangement = Arrangement.spacedBy(12.dp)) {
    ProfileImage(size = 128.dp, image = user?.image)
    Row(verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(6.dp)) {
      NameWithBadge(
        name = displayName,
        badge = previewBadge,
        style = MaterialTheme.typography.h1.copy(fontWeight = FontWeight.Normal),
        maxLines = 1,
        overflow = TextOverflow.Ellipsis
      )
      trailing()
    }
  }
}
