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

// User's avatar + name with a preview badge at the selected level, rendered via NameWithBadge
// so the preview matches how the badge appears on the name elsewhere. Callers can inject a
// picker affordance (e.g. a chevron) into the name row via `trailing`.
@Composable
fun BadgeUserPreview(level: BadgeLevel, modifier: Modifier = Modifier, trailing: @Composable () -> Unit = {}) {
  val user = chatModel.currentUser.value
  val displayName = user?.displayName ?: stringResource(MR.strings.badges_preview_my_nickname)
  val previewBadge = LocalBadge(
    badge = BadgeInfo(badgeType = level.badgeType),
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
