package chat.simplex.app.views.chat.item

import android.content.res.Configuration
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.*
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*

@Composable
fun CIGroupInvitationView(
  ci: ChatItem,
  groupInvitation: CIGroupInvitation,
  memberRole: GroupMemberRole,
  chatIncognito: Boolean = false,
  joinGroup: (Long) -> Unit
) {
  val sent = ci.chatDir.sent
  val action = !sent && groupInvitation.status == CIGroupInvitationStatus.Pending

  @Composable
  fun groupInfoView() {
    val p = groupInvitation.groupProfile
    val iconColor =
      if (action) if (chatIncognito) Indigo else MaterialTheme.colors.primary
      else if (isInDarkTheme()) FileDark else FileLight

    Row(
      Modifier
        .defaultMinSize(minWidth = 220.dp)
        .padding(vertical = 4.dp)
        .padding(end = 2.dp)
    ) {
      ProfileImage(size = 60.dp, image = groupInvitation.groupProfile.image, icon = R.drawable.ic_supervised_user_circle_filled, color = iconColor)
      Spacer(Modifier.padding(horizontal = 3.dp))
      Column(
        Modifier.defaultMinSize(minHeight = 60.dp),
        verticalArrangement = Arrangement.Center
      ) {
        Text(p.displayName, style = MaterialTheme.typography.caption, fontWeight = FontWeight.Medium, maxLines = 2, overflow = TextOverflow.Ellipsis)
        if (p.fullName != "" && p.displayName != p.fullName) {
          Text(p.fullName, maxLines = 2, overflow = TextOverflow.Ellipsis)
        }
      }
    }
  }

  @Composable
  fun groupInvitationText() {
    when {
      sent -> Text(stringResource(R.string.you_sent_group_invitation))
      !sent && groupInvitation.status == CIGroupInvitationStatus.Pending -> Text(stringResource(R.string.you_are_invited_to_group))
      !sent && groupInvitation.status == CIGroupInvitationStatus.Accepted -> Text(stringResource(R.string.you_joined_this_group))
      !sent && groupInvitation.status == CIGroupInvitationStatus.Rejected -> Text(stringResource(R.string.you_rejected_group_invitation))
      !sent && groupInvitation.status == CIGroupInvitationStatus.Expired -> Text(stringResource(R.string.group_invitation_expired))
    }
  }

  val sentColor = CurrentColors.collectAsState().value.appColors.sentMessage
  val receivedColor = CurrentColors.collectAsState().value.appColors.receivedMessage
  Surface(
    modifier = if (action) Modifier.clickable(onClick = {
      joinGroup(groupInvitation.groupId)
    }) else Modifier,
    shape = RoundedCornerShape(18.dp),
    color = if (sent) sentColor else receivedColor,
  ) {
    Box(
      Modifier
        .width(IntrinsicSize.Min)
        .padding(vertical = 3.dp)
        .padding(start = 8.dp, end = 12.dp),
      contentAlignment = Alignment.BottomEnd
    ) {
      Column(
        Modifier
          .defaultMinSize(minWidth = 220.dp)
          .padding(bottom = 4.dp),
      ) {
        groupInfoView()
        Column(Modifier.padding(top = 2.dp, start = 5.dp)) {
          Divider(Modifier.fillMaxWidth().padding(bottom = 4.dp))
          if (action) {
            groupInvitationText()
            Text(stringResource(
              if (chatIncognito) R.string.group_invitation_tap_to_join_incognito else  R.string.group_invitation_tap_to_join),
              color = if (chatIncognito) Indigo else MaterialTheme.colors.primary)
          } else {
            Box(Modifier.padding(end = 48.dp)) {
              groupInvitationText()
            }
          }
        }
      }
      Text(
        ci.timestampText,
        color = MaterialTheme.colors.secondary,
        fontSize = 14.sp,
        modifier = Modifier.padding(start = 3.dp)
      )
    }
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  name = "Dark Mode"
)
@Composable
fun PendingCIGroupInvitationViewPreview() {
  SimpleXTheme {
    CIGroupInvitationView(
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
fun CIGroupInvitationViewAcceptedPreview() {
  SimpleXTheme {
    CIGroupInvitationView(
      ci = ChatItem.getGroupInvitationSample(),
      groupInvitation = CIGroupInvitation.getSample(status = CIGroupInvitationStatus.Accepted),
      memberRole = GroupMemberRole.Admin,
      joinGroup = {}
    )
  }
}

@Preview(showBackground = true)
@Composable
fun CIGroupInvitationViewLongNamePreview() {
  SimpleXTheme {
    CIGroupInvitationView(
      ci = ChatItem.getGroupInvitationSample(),
      groupInvitation = CIGroupInvitation.getSample(
        groupProfile = GroupProfile("group_with_a_really_really_really_long_name", "Group With A Really Really Really Long Name"),
        status = CIGroupInvitationStatus.Accepted
      ),
      memberRole = GroupMemberRole.Admin,
      joinGroup = {}
    )
  }
}
