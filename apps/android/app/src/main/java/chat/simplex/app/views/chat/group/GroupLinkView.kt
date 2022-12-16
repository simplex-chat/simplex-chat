package chat.simplex.app.views.chat.group

import androidx.compose.foundation.layout.*
import androidx.compose.material.CircularProgressIndicator
import androidx.compose.material.Text
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.GroupInfo
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.newchat.QRCode

@Composable
fun GroupLinkView(chatModel: ChatModel, groupInfo: GroupInfo, connReqContact: String?, onGroupLinkUpdated: (String?) -> Unit) {
  var groupLink by rememberSaveable { mutableStateOf(connReqContact) }
  var creatingLink by rememberSaveable { mutableStateOf(false) }
  val cxt = LocalContext.current
  fun createLink() {
    creatingLink = true
    withApi {
      groupLink = chatModel.controller.apiCreateGroupLink(groupInfo.groupId)
      onGroupLinkUpdated(groupLink)
      creatingLink = false
    }
  }
  LaunchedEffect(Unit) {
    if (groupLink == null && !creatingLink) {
      createLink()
    }
  }
  GroupLinkLayout(
    groupLink = groupLink,
    creatingLink,
    createLink = ::createLink,
    share = { shareText(cxt, groupLink ?: return@GroupLinkLayout) },
    deleteLink = {
      AlertManager.shared.showAlertMsg(
        title = generalGetString(R.string.delete_link_question),
        text = generalGetString(R.string.all_group_members_will_remain_connected),
        confirmText = generalGetString(R.string.delete_verb),
        onConfirm = {
          withApi {
            val r = chatModel.controller.apiDeleteGroupLink(groupInfo.groupId)
            if (r) {
              groupLink = null
              onGroupLinkUpdated(null)
            }
          }
        }
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
  creatingLink: Boolean,
  createLink: () -> Unit,
  share: () -> Unit,
  deleteLink: () -> Unit
) {
  Column(
    Modifier.padding(horizontal = DEFAULT_PADDING),
    horizontalAlignment = Alignment.Start,
    verticalArrangement = Arrangement.Top
  ) {
    AppBarTitle(stringResource(R.string.group_link), false)
    Text(
      stringResource(R.string.you_can_share_group_link_anybody_will_be_able_to_connect),
      Modifier.padding(bottom = 12.dp),
      lineHeight = 22.sp
    )
    Column(
      Modifier.fillMaxWidth(),
      horizontalAlignment = Alignment.CenterHorizontally,
      verticalArrangement = Arrangement.SpaceEvenly
    ) {
      if (groupLink == null) {
        SimpleButton(stringResource(R.string.button_create_group_link), icon = Icons.Outlined.AddLink, disabled = creatingLink, click = createLink)
      } else {
        QRCode(groupLink, Modifier.weight(1f, fill = false).aspectRatio(1f))
        Row(
          horizontalArrangement = Arrangement.spacedBy(10.dp),
          verticalAlignment = Alignment.CenterVertically,
          modifier = Modifier.padding(vertical = 10.dp)
        ) {
          SimpleButton(
            stringResource(R.string.share_link),
            icon = Icons.Outlined.Share,
            click = share
          )
          SimpleButton(
            stringResource(R.string.delete_link),
            icon = Icons.Outlined.Delete,
            color = Color.Red,
            click = deleteLink
          )
        }
      }
    }
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
      color = HighOrLowlight,
      strokeWidth = 2.5.dp
    )
  }
}
