package chat.simplex.app.views.chat.item

import android.content.res.Configuration
import androidx.compose.foundation.clickable
import androidx.compose.foundation.isSystemInDarkTheme
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.SupervisedUserCircle
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.*
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*

@Composable
fun GroupInvitationItemView(
  ci: ChatItem,
  groupInvitation: CIGroupInvitation,
  memberRole: GroupMemberRole,
  joinGroup: (Long) -> Unit
) {
  val sent = ci.chatDir.sent

  fun groupInvitationAction() {
    when {
      !sent && groupInvitation.status == CIGroupInvitationStatus.Pending -> joinGroup(groupInvitation.groupId)
      else -> {}
    }
  }

  @Composable
  fun groupInvitationText() {
    when {
      sent -> Text(stringResource(R.string.you_sent_group_invitation))
      !sent && groupInvitation.status == CIGroupInvitationStatus.Pending -> Text(stringResource(R.string.group_invitation_tap_to_accept))
      !sent && groupInvitation.status == CIGroupInvitationStatus.Accepted -> Text(stringResource(R.string.accepted_group_invitation))
      !sent && groupInvitation.status == CIGroupInvitationStatus.Rejected -> Text(stringResource(R.string.rejected_group_invitation))
      !sent && groupInvitation.status == CIGroupInvitationStatus.Expired -> Text(stringResource(R.string.expired_group_invitation))
    }
  }

  fun accent(): Boolean {
    return !sent && groupInvitation.status == CIGroupInvitationStatus.Pending
  }

  Surface(
    Modifier.clickable(onClick = { groupInvitationAction() }),
    shape = RoundedCornerShape(18.dp),
    color = if (sent) SentColorLight else ReceivedColorLight,
  ) {
    Box (contentAlignment = Alignment.BottomEnd) {
      Column(
        Modifier.padding(4.dp),
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.spacedBy(4.dp)
      ) {
        val iconColor =
          if (accent()) MaterialTheme.colors.primary
          else
            if (isSystemInDarkTheme()) FileDark
            else FileLight

        Row {
          ProfileImage(size = 60.dp, icon = Icons.Filled.SupervisedUserCircle, color = iconColor)
          Spacer(Modifier.padding(horizontal = 4.dp))
          Column {
            Text(groupInvitation.groupProfile.displayName, style = MaterialTheme.typography.caption)
            Text(groupInvitation.groupProfile.fullName)
          }
        }
        Divider(Modifier.width(260.dp).padding(horizontal = 8.dp))
        Box(Modifier.padding(bottom = 22.dp)) {
          groupInvitationText()
        }
      }
      Box(Modifier.padding(bottom = 6.dp, end = 12.dp)) {
        CIMetaView(ci)
      }
    }
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  name = "Dark Mode"
)
@Composable
fun PendingGroupInvitationItemViewPreview() {
  SimpleXTheme {
    GroupInvitationItemView(
      ci = ChatItem.getGroupInvitationSample(),
      groupInvitation = CIGroupInvitation.getSample(),
      memberRole = GroupMemberRole.Admin,
      joinGroup = {}
    )
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  name = "Dark Mode"
)
@Composable
fun AcceptedGroupInvitationItemViewPreview() {
  SimpleXTheme {
    GroupInvitationItemView(
      ci = ChatItem.getGroupInvitationSample(),
      groupInvitation = CIGroupInvitation.getSample(status = CIGroupInvitationStatus.Accepted),
      memberRole = GroupMemberRole.Admin,
      joinGroup = {}
    )
  }
}
