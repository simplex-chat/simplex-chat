package chat.simplex.common.views.chat.group

import SectionBottomSpacer
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalClipboardManager
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.icerockdev.library.MR
import chat.simplex.common.model.*
import chat.simplex.common.platform.shareText
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.QRCode

@Composable
fun GroupLinkView(chatModel: ChatModel, groupInfo: GroupInfo, connReqContact: String?, memberRole: GroupMemberRole?, onGroupLinkUpdated: (Pair<String?, GroupMemberRole?>) -> Unit) {
  var groupLink by rememberSaveable { mutableStateOf(connReqContact) }
  val groupLinkMemberRole = rememberSaveable { mutableStateOf(memberRole) }
  var creatingLink by rememberSaveable { mutableStateOf(false) }
  fun createLink() {
    creatingLink = true
    withApi {
      val link = chatModel.controller.apiCreateGroupLink(groupInfo.groupId)
      if (link != null) {
        groupLink = link.first
        groupLinkMemberRole.value = link.second
        onGroupLinkUpdated(groupLink to groupLinkMemberRole.value)
      }
      creatingLink = false
    }
  }
  LaunchedEffect(Unit) {
    if (groupLink == null && !creatingLink) {
      createLink()
    }
  }
  val clipboard = LocalClipboardManager.current
  GroupLinkLayout(
    groupLink = groupLink,
    groupInfo,
    groupLinkMemberRole,
    creatingLink,
    createLink = ::createLink,
    share = { clipboard.shareText(groupLink ?: return@GroupLinkLayout) },
    updateLink = {
      val role = groupLinkMemberRole.value
      if (role != null) {
        withBGApi {
          val link = chatModel.controller.apiGroupLinkMemberRole(groupInfo.groupId, role)
          if (link != null) {
            groupLink = link.first
            groupLinkMemberRole.value = link.second
            onGroupLinkUpdated(groupLink to groupLinkMemberRole.value)
          }
        }
      }
    },
    deleteLink = {
      AlertManager.shared.showAlertDialog(
        title = generalGetString(MR.strings.delete_link_question),
        text = generalGetString(MR.strings.all_group_members_will_remain_connected),
        confirmText = generalGetString(MR.strings.delete_verb),
        onConfirm = {
          withApi {
            val r = chatModel.controller.apiDeleteGroupLink(groupInfo.groupId)
            if (r) {
              groupLink = null
              onGroupLinkUpdated(null to null)
            }
          }
        },
        destructive = true,
      )
    }
  )
  if (creatingLink) {
    ProgressIndicator()
  }
}

@Composable
fun GroupLinkLayout(
  groupLink: String?,
  groupInfo: GroupInfo,
  groupLinkMemberRole: MutableState<GroupMemberRole?>,
  creatingLink: Boolean,
  createLink: () -> Unit,
  share: () -> Unit,
  updateLink: () -> Unit,
  deleteLink: () -> Unit
) {
  Column(
    Modifier
      .verticalScroll(rememberScrollState()),
  ) {
    AppBarTitle(stringResource(MR.strings.group_link))
    Text(
      stringResource(MR.strings.you_can_share_group_link_anybody_will_be_able_to_connect),
      Modifier.padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING, bottom = 12.dp),
      lineHeight = 22.sp
    )
    Column(
      Modifier.fillMaxWidth(),
      horizontalAlignment = Alignment.CenterHorizontally,
      verticalArrangement = Arrangement.SpaceEvenly
    ) {
      if (groupLink == null) {
        SimpleButton(stringResource(MR.strings.button_create_group_link), icon = painterResource(MR.images.ic_add_link), disabled = creatingLink, click = createLink)
      } else {
        RoleSelectionRow(groupInfo, groupLinkMemberRole)
        var initialLaunch by remember { mutableStateOf(true) }
        LaunchedEffect(groupLinkMemberRole.value) {
          if (!initialLaunch) {
            updateLink()
          }
          initialLaunch = false
        }
        QRCode(groupLink, Modifier.aspectRatio(1f).padding(horizontal = DEFAULT_PADDING))
        Row(
          horizontalArrangement = Arrangement.spacedBy(10.dp),
          verticalAlignment = Alignment.CenterVertically,
          modifier = Modifier.padding(horizontal = DEFAULT_PADDING, vertical = 10.dp)
        ) {
          SimpleButton(
            stringResource(MR.strings.share_link),
            icon = painterResource(MR.images.ic_share),
            click = share
          )
          SimpleButton(
            stringResource(MR.strings.delete_link),
            icon = painterResource(MR.images.ic_delete),
            color = Color.Red,
            click = deleteLink
          )
        }
      }
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun RoleSelectionRow(groupInfo: GroupInfo, selectedRole: MutableState<GroupMemberRole?>, enabled: Boolean = true) {
  Row(
    Modifier.fillMaxWidth(),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.SpaceBetween
  ) {
    val values = listOf(GroupMemberRole.Member, GroupMemberRole.Observer).map { it to it.text }
    ExposedDropDownSettingRow(
      generalGetString(MR.strings.initial_member_role),
      values,
      selectedRole,
      icon = null,
      enabled = rememberUpdatedState(enabled)
    ) { selectedRole.value = it }
  }
}

@Composable
fun ProgressIndicator() {
  Box(
    Modifier.fillMaxSize(),
    contentAlignment = Alignment.Center
  ) {
    CircularProgressIndicator(
      Modifier
        .padding(horizontal = 2.dp)
        .size(30.dp),
      color = MaterialTheme.colors.secondary,
      strokeWidth = 2.5.dp
    )
  }
}
