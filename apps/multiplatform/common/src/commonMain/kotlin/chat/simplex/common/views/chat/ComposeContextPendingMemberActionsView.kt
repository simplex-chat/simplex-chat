package chat.simplex.common.views.chat

import SectionItemView
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.chat.group.removeMemberDialog
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext

@Composable
fun ComposeContextPendingMemberActionsView(
  rhId: Long?,
  groupInfo: GroupInfo,
  member: GroupMember
) {
  Column(
    Modifier
      .height(60.dp)
      .background(MaterialTheme.colors.surface)
  ) {
    Divider()

    Row(
      Modifier
        .fillMaxWidth(),
      horizontalArrangement = Arrangement.SpaceEvenly,
    ) {
      Column(
        Modifier
          .fillMaxWidth()
          .fillMaxHeight()
          .weight(1F)
          .clickable {
            removeMemberDialog(rhId, groupInfo, member, chatModel, close = { ModalManager.end.closeModal() })
          },
        verticalArrangement = Arrangement.Center,
        horizontalAlignment = Alignment.CenterHorizontally
      ) {
        Text(stringResource(MR.strings.remove_pending_member_button), color = Color.Red)
      }

      Column(
        Modifier
          .fillMaxWidth()
          .fillMaxHeight()
          .weight(1F)
          .clickable {
            acceptMemberDialog(rhId, groupInfo, member, close = { ModalManager.end.closeModal() })
          },
        verticalArrangement = Arrangement.Center,
        horizontalAlignment = Alignment.CenterHorizontally
      ) {
        Text(stringResource(MR.strings.accept_pending_member_button), color = MaterialTheme.colors.primary)
      }
    }
  }
}

fun acceptMemberDialog(rhId: Long?, groupInfo: GroupInfo, member: GroupMember, close: (() -> Unit)? = null) {
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = generalGetString(MR.strings.accept_pending_member_alert_title),
    text = generalGetString(MR.strings.accept_pending_member_alert_question),
    buttons = {
      Column {
        // Accept as member
        SectionItemView({
          AlertManager.shared.hideAlert()
          acceptMember(rhId, groupInfo, member, GroupMemberRole.Member, close)
        }) {
          Text(generalGetString(MR.strings.accept_pending_member_alert_confirmation_as_member), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        // Accept as observer
        SectionItemView({
          AlertManager.shared.hideAlert()
          acceptMember(rhId, groupInfo, member, GroupMemberRole.Observer, close)
        }) {
          Text(generalGetString(MR.strings.accept_pending_member_alert_confirmation_as_observer), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        // Cancel
        SectionItemView({
          AlertManager.shared.hideAlert()
        }) {
          Text(stringResource(MR.strings.cancel_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
      }
    }
  )
}

private fun acceptMember(rhId: Long?, groupInfo: GroupInfo, member: GroupMember, role: GroupMemberRole, close: (() -> Unit)?) {
  withBGApi {
    val r = chatModel.controller.apiAcceptMember(rhId, groupInfo.groupId, member.groupMemberId, role)
    if (r != null) {
      withContext(Dispatchers.Main) {
        chatModel.chatsContext.upsertGroupMember(rhId, groupInfo, r.second)
        chatModel.chatsContext.updateGroup(rhId, r.first)
      }
      withContext(Dispatchers.Main) {
        chatModel.secondaryChatsContext.value?.upsertGroupMember(rhId, groupInfo, r.second)
      }
    }
    close?.invoke()
  }
}
