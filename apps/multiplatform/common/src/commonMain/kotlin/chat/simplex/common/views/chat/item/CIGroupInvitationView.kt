package chat.simplex.common.views.chat.item

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.buildAnnotatedString
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.text.withStyle
import androidx.compose.ui.unit.dp
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.*
import chat.simplex.res.MR
import kotlinx.coroutines.delay

@Composable
fun CIGroupInvitationView(
  ci: ChatItem,
  groupInvitation: CIGroupInvitation,
  memberRole: GroupMemberRole,
  chatIncognito: Boolean = false,
  joinGroup: (Long, () -> Unit) -> Unit,
  timedMessagesTTL: Int?
) {
  val sent = ci.chatDir.sent
  val action = !sent && groupInvitation.status == CIGroupInvitationStatus.Pending
  val inProgress = remember { mutableStateOf(false) }
  var progressByTimeout by rememberSaveable { mutableStateOf(false) }
  LaunchedEffect(inProgress.value) {
    progressByTimeout = if (inProgress.value) {
      delay(1000)
      inProgress.value
    } else {
      false
    }
  }

  @Composable
  fun groupInfoView() {
    val p = groupInvitation.groupProfile
    val iconColor =
      if (action && !inProgress.value) if (chatIncognito) Indigo else MaterialTheme.colors.primary
      else if (isInDarkTheme()) FileDark else FileLight

    Row(
      Modifier
        .defaultMinSize(minWidth = 220.dp)
        .padding(vertical = 4.dp)
        .padding(end = 2.dp)
    ) {
      ProfileImage(size = 60.dp, image = groupInvitation.groupProfile.image, icon = MR.images.ic_supervised_user_circle_filled, color = iconColor)
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
  fun groupInvitationStr(): String {
    return when {
      sent -> stringResource(MR.strings.you_sent_group_invitation)
      else -> when(groupInvitation.status) {
        CIGroupInvitationStatus.Pending -> stringResource(MR.strings.you_are_invited_to_group)
        CIGroupInvitationStatus.Accepted -> stringResource(MR.strings.you_joined_this_group)
        CIGroupInvitationStatus.Rejected -> stringResource(MR.strings.you_rejected_group_invitation)
        CIGroupInvitationStatus.Expired -> stringResource(MR.strings.group_invitation_expired)
      }
    }
  }

  val sentColor = MaterialTheme.appColors.sentMessage
  val receivedColor = MaterialTheme.appColors.receivedMessage
  Surface(
    modifier = if (action && !inProgress.value) Modifier.clickable(onClick = {
      inProgress.value = true
      joinGroup(groupInvitation.groupId) { inProgress.value = false }
    }) else Modifier,
    shape = RoundedCornerShape(18.dp),
    color = if (sent) sentColor else receivedColor,
    contentColor = LocalContentColor.current
  ) {
    Box(
      Modifier
        .width(IntrinsicSize.Min)
        .padding(vertical = 3.dp)
        .padding(start = 8.dp, end = 12.dp),
      contentAlignment = Alignment.BottomEnd
    ) {
      Box(
        contentAlignment = Alignment.Center
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
              Text(groupInvitationStr())
              Text(
                buildAnnotatedString {
                  append(generalGetString(if (chatIncognito) MR.strings.group_invitation_tap_to_join_incognito else MR.strings.group_invitation_tap_to_join))
                  withStyle(reserveTimestampStyle) { append(reserveSpaceForMeta(ci.meta, timedMessagesTTL, encrypted = null, showStatus = false, showEdited = false)) }
                },
                color = if (inProgress.value)
                  MaterialTheme.colors.secondary
                else
                  if (chatIncognito) Indigo else MaterialTheme.colors.primary
              )
            } else {
              Text(
                buildAnnotatedString {
                  append(groupInvitationStr())
                  withStyle(reserveTimestampStyle) { append(reserveSpaceForMeta(ci.meta, timedMessagesTTL, encrypted = null, showStatus = false, showEdited = false)) }
                }
              )
            }
          }
        }

        if (progressByTimeout) {
          CircularProgressIndicator(
            Modifier.size(32.dp),
            color = if (isInDarkTheme()) FileDark else FileLight,
            strokeWidth = 3.dp
          )
        }
      }

      CIMetaView(ci, timedMessagesTTL, showStatus = false, showEdited = false)
    }
  }
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  name = "Dark Mode"
)*/
@Composable
fun PendingCIGroupInvitationViewPreview() {
  SimpleXTheme {
    CIGroupInvitationView(
      ci = ChatItem.getGroupInvitationSample(),
      groupInvitation = CIGroupInvitation.getSample(),
      memberRole = GroupMemberRole.Admin,
      joinGroup = { _, _ -> },
      timedMessagesTTL = null
    )
  }
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  name = "Dark Mode"
)*/
@Composable
fun CIGroupInvitationViewAcceptedPreview() {
  SimpleXTheme {
    CIGroupInvitationView(
      ci = ChatItem.getGroupInvitationSample(),
      groupInvitation = CIGroupInvitation.getSample(status = CIGroupInvitationStatus.Accepted),
      memberRole = GroupMemberRole.Admin,
      joinGroup = { _, _ -> },
      timedMessagesTTL = null
    )
  }
}

@Preview
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
      joinGroup = { _, _ -> },
      timedMessagesTTL = null
    )
  }
}
