package chat.simplex.app.views.chat.item

import android.content.Context
import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Edit
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.UriHandler
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chat.ComposeContextItem
import chat.simplex.app.views.chat.ComposeState
import chat.simplex.app.views.helpers.*
import kotlinx.datetime.Clock

@Composable
fun ChatItemView(
  user: User,
  cItem: ChatItem,
  composeState: MutableState<ComposeState>,
  cxt: Context,
  uriHandler: UriHandler? = null,
  showMember: Boolean = false,
  deleteMessage: (Long, CIDeleteMode) -> Unit,
  receiveFile: (Long) -> Unit
) {
  val sent = cItem.chatDir.sent
  val alignment = if (sent) Alignment.CenterEnd else Alignment.CenterStart
  val showMenu = remember { mutableStateOf(false) }
  Box(
    modifier = Modifier
      .padding(bottom = 4.dp)
      .fillMaxWidth(),
    contentAlignment = alignment,
  ) {
    Column(Modifier.combinedClickable(onLongClick = { showMenu.value = true }, onClick = {})) {
      if (cItem.isMsgContent) {
        if (cItem.file == null && cItem.quotedItem == null && isShortEmoji(cItem.content.text)) {
          EmojiItemView(cItem)
        } else {
          FramedItemView(user, cItem, uriHandler, showMember = showMember, showMenu, receiveFile)
        }
      } else if (cItem.isDeletedContent) {
        DeletedItemView(cItem, showMember = showMember)
      }
      if (cItem.isMsgContent) {
        DropdownMenu(
          expanded = showMenu.value,
          onDismissRequest = { showMenu.value = false },
          Modifier.width(220.dp)
        ) {
          ItemAction(stringResource(R.string.reply_verb), Icons.Outlined.Reply, onClick = {
            composeState.value = composeState.value.copy(contextItem = ComposeContextItem.QuotedItem(cItem))
            showMenu.value = false
          })
          ItemAction(stringResource(R.string.share_verb), Icons.Outlined.Share, onClick = {
            shareText(cxt, cItem.content.text)
            showMenu.value = false
          })
          ItemAction(stringResource(R.string.copy_verb), Icons.Outlined.ContentCopy, onClick = {
            copyText(cxt, cItem.content.text)
            showMenu.value = false
          })
          if (cItem.chatDir.sent && cItem.meta.editable) {
            ItemAction(stringResource(R.string.edit_verb), Icons.Filled.Edit, onClick = {
              composeState.value = ComposeState(editingItem = cItem)
              showMenu.value = false
            })
          }
          ItemAction(
            stringResource(R.string.delete_verb),
            Icons.Outlined.Delete,
            onClick = {
              showMenu.value = false
              deleteMessageAlertDialog(cItem, deleteMessage = deleteMessage)
            },
            color = Color.Red
          )
        }
      }
    }
  }
}

@Composable
private fun ItemAction(text: String, icon: ImageVector, onClick: () -> Unit, color: Color = MaterialTheme.colors.onBackground) {
  DropdownMenuItem(onClick) {
    Row {
      Text(
        text,
        modifier = Modifier
          .fillMaxWidth()
          .weight(1F),
        color = color
      )
      Icon(icon, text, tint = color)
    }
  }
}

fun deleteMessageAlertDialog(chatItem: ChatItem, deleteMessage: (Long, CIDeleteMode) -> Unit) {
  AlertManager.shared.showAlertDialogButtons(
    title = generalGetString(R.string.delete_message__question),
    text = generalGetString(R.string.delete_message_cannot_be_undone_warning),
    buttons = {
      Row(
        Modifier
          .fillMaxWidth()
          .padding(horizontal = 8.dp, vertical = 2.dp),
        horizontalArrangement = Arrangement.End,
      ) {
        Button(onClick = {
          deleteMessage(chatItem.id, CIDeleteMode.cidmInternal)
          AlertManager.shared.hideAlert()
        }) { Text(stringResource(R.string.for_me_only)) }
        if (chatItem.meta.editable) {
          Spacer(Modifier.padding(horizontal = 4.dp))
          Button(onClick = {
            deleteMessage(chatItem.id, CIDeleteMode.cidmBroadcast)
            AlertManager.shared.hideAlert()
          }) { Text(stringResource(R.string.for_everybody)) }
        }
      }
    }
  )
}

@Preview
@Composable
fun PreviewChatItemView() {
  SimpleXTheme {
    ChatItemView(
      User.sampleData,
      ChatItem.getSampleData(
        1, CIDirection.DirectSnd(), Clock.System.now(), "hello"
      ),
      composeState = remember { mutableStateOf(ComposeState()) },
      cxt = LocalContext.current,
      deleteMessage = { _, _ -> },
      receiveFile = {}
    )
  }
}

@Preview
@Composable
fun PreviewChatItemViewDeletedContent() {
  SimpleXTheme {
    ChatItemView(
      User.sampleData,
      ChatItem.getDeletedContentSampleData(),
      composeState = remember { mutableStateOf(ComposeState()) },
      cxt = LocalContext.current,
      deleteMessage = { _, _ -> },
      receiveFile = {}
    )
  }
}
