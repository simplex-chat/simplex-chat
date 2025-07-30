package chat.simplex.common.views.chat.group

import SectionBottomSpacer
import SectionItemView
import SectionViewWithButton
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.text.style.TextAlign
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.*
import chat.simplex.common.views.usersettings.SettingsActionItem
import chat.simplex.res.MR

@Composable
fun GroupLinkView(
  chatModel: ChatModel,
  rhId: Long?,
  groupInfo: GroupInfo,
  groupLink: GroupLink?,
  onGroupLinkUpdated: ((GroupLink?) -> Unit)?,
  creatingGroup: Boolean = false,
  close: (() -> Unit)? = null
) {
  var groupLinkVar by rememberSaveable(stateSaver = GroupLink.nullableStateSaver) { mutableStateOf(groupLink) }
  val groupLinkMemberRole = rememberSaveable { mutableStateOf(groupLink?.acceptMemberRole) }
  var creatingLink by rememberSaveable { mutableStateOf(false) }
  val clipboard = LocalClipboardManager.current
  fun createLink() {
    creatingLink = true
    withBGApi {
      val link = chatModel.controller.apiCreateGroupLink(rhId, groupInfo.groupId)
      if (link != null) {
        groupLinkVar = link
        groupLinkMemberRole.value = link.acceptMemberRole
        onGroupLinkUpdated?.invoke(link)
      }
      creatingLink = false
    }
  }
  fun addShortLink(shareOnCompletion: Boolean = false) {
    creatingLink = true
    withBGApi {
      val link = chatModel.controller.apiAddGroupShortLink(rhId, groupInfo.groupId)
      if (link != null) {
        groupLinkVar = link
        groupLinkMemberRole.value = link.acceptMemberRole
        onGroupLinkUpdated?.invoke(link)
        if (shareOnCompletion) {
          clipboard.shareText(link.connLinkContact.simplexChatUri(short = true))
        }
      }
      creatingLink = false
    }
  }
  fun showAddShortLinkAlert(shareAddress: (() -> Unit)? = null) {
    AlertManager.shared.showAlertDialogButtonsColumn(
      title = generalGetString(MR.strings.share_group_profile_via_link),
      text = generalGetString(MR.strings.share_group_profile_via_link_alert_text),
      buttons = {
        Column {
          SectionItemView({
            AlertManager.shared.hideAlert()
            addShortLink(shareOnCompletion = shareAddress != null)
          }) {
            Text(
              generalGetString(MR.strings.share_profile_via_link_alert_confirm),
              Modifier.fillMaxWidth(),
              textAlign = TextAlign.Center,
              color = MaterialTheme.colors.primary
            )
          }

          if (shareAddress != null) {
            // Delete without notification
            SectionItemView({
              AlertManager.shared.hideAlert()
              shareAddress()
            }) {
              Text(
                generalGetString(MR.strings.share_old_link_alert_button),
                Modifier.fillMaxWidth(),
                textAlign = TextAlign.Center,
                color = MaterialTheme.colors.primary
              )
            }
          }
          // Cancel
          SectionItemView({
            AlertManager.shared.hideAlert()
          }) {
            Text(
              stringResource(MR.strings.cancel_verb),
              Modifier.fillMaxWidth(),
              textAlign = TextAlign.Center,
              color = MaterialTheme.colors.primary
            )
          }
        }
      }
    )
  }
  LaunchedEffect(Unit) {
    if (groupLink == null && !creatingLink) {
      createLink()
    }
  }
  GroupLinkLayout(
    groupLink = groupLinkVar,
    groupInfo,
    groupLinkMemberRole,
    creatingLink,
    createLink = ::createLink,
    showAddShortLinkAlert = ::showAddShortLinkAlert,
    updateLink = {
      val role = groupLinkMemberRole.value
      if (role != null) {
        withBGApi {
          val link = chatModel.controller.apiGroupLinkMemberRole(rhId, groupInfo.groupId, role)
          if (link != null) {
            groupLinkVar = link
            groupLinkMemberRole.value = link.acceptMemberRole
            onGroupLinkUpdated?.invoke(link)
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
          withBGApi {
            val r = chatModel.controller.apiDeleteGroupLink(rhId, groupInfo.groupId)
            if (r) {
              groupLinkVar = null
              onGroupLinkUpdated?.invoke(null)
            }
          }
        },
        destructive = true,
      )
    },
    creatingGroup = creatingGroup,
    close = close
  )
  if (creatingLink) {
    ProgressIndicator()
  }
}

@Composable
fun GroupLinkLayout(
  groupLink: GroupLink?,
  groupInfo: GroupInfo,
  groupLinkMemberRole: MutableState<GroupMemberRole?>,
  creatingLink: Boolean,
  createLink: () -> Unit,
  showAddShortLinkAlert: ((() -> Unit)?) -> Unit,
  updateLink: () -> Unit,
  deleteLink: () -> Unit,
  creatingGroup: Boolean = false,
  close: (() -> Unit)? = null
) {
  @Composable
  fun ContinueButton(close: () -> Unit) {
    SimpleButton(
      stringResource(MR.strings.continue_to_next_step),
      icon = painterResource(MR.images.ic_check),
      click = close
    )
  }

  ColumnWithScrollBar {
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
        Row(
          horizontalArrangement = Arrangement.spacedBy(10.dp),
          verticalAlignment = Alignment.CenterVertically,
          modifier = Modifier.padding(horizontal = DEFAULT_PADDING, vertical = 10.dp)
        ) {
          SimpleButton(stringResource(MR.strings.button_create_group_link), icon = painterResource(MR.images.ic_add_link), disabled = creatingLink, click = createLink)
          if (creatingGroup && close != null) {
            ContinueButton(close)
          }
        }
      } else {
        RoleSelectionRow(groupInfo, groupLinkMemberRole)
        var initialLaunch by remember { mutableStateOf(true) }
        LaunchedEffect(groupLinkMemberRole.value) {
          if (!initialLaunch) {
            updateLink()
          }
          initialLaunch = false
        }
        val showShortLink = remember { mutableStateOf(true) }
        Spacer(Modifier.height(DEFAULT_PADDING_HALF))
        if (groupLink.connLinkContact.connShortLink == null) {
          SimpleXCreatedLinkQRCode(groupLink.connLinkContact, short = false)
        } else {
          SectionViewWithButton(titleButton = { ToggleShortLinkButton(showShortLink) }) {
            SimpleXCreatedLinkQRCode(groupLink.connLinkContact, short = showShortLink.value)
          }
        }
        Row(
          horizontalArrangement = Arrangement.spacedBy(10.dp),
          verticalAlignment = Alignment.CenterVertically,
          modifier = Modifier.padding(horizontal = DEFAULT_PADDING, vertical = 10.dp)
        ) {
          val clipboard = LocalClipboardManager.current
          SimpleButton(
            stringResource(MR.strings.share_link),
            icon = painterResource(MR.images.ic_share),
            click = {
              if (groupLink.shouldBeUpgraded) {
                showAddShortLinkAlert {
                  clipboard.shareText(groupLink.connLinkContact.simplexChatUri(short = showShortLink.value))
                }
              } else {
                clipboard.shareText(groupLink.connLinkContact.simplexChatUri(short = showShortLink.value))
              }
            }
          )
          if (creatingGroup && close != null) {
            ContinueButton(close)
          } else {
            SimpleButton(
              stringResource(MR.strings.delete_link),
              icon = painterResource(MR.images.ic_delete),
              color = Color.Red,
              click = deleteLink
            )
          }
        }
        if (groupLink.shouldBeUpgraded) {
          AddShortLinkButton(text = stringResource(MR.strings.upgrade_group_link)) {
            showAddShortLinkAlert(null)
          }
        }
      }
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun AddShortLinkButton(text: String, onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_add),
    text,
    onClick,
    iconColor = MaterialTheme.colors.primary,
    textColor = MaterialTheme.colors.primary,
  )
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
