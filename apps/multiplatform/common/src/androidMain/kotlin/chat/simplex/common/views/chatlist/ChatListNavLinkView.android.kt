package chat.simplex.common.views.chatlist

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.Divider
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.scale
import androidx.compose.ui.unit.dp
import chat.simplex.common.platform.onRightClick
import chat.simplex.common.views.helpers.*

@Composable
actual fun ChatListNavLinkLayout(
  chatLinkPreview: @Composable () -> Unit,
  click: () -> Unit,
  dropdownMenuItems: (@Composable () -> Unit)?,
  showMenu: MutableState<Boolean>,
  disabled: Boolean,
  selectedChat: State<Boolean>,
  nextChatSelected: State<Boolean>,
  oneHandUI: State<Boolean>
) {
  var modifier = Modifier.fillMaxWidth()

  if (oneHandUI.value) {
    modifier = modifier.scale(scaleX = 1f, scaleY = -1f)
  }

  if (!disabled) modifier = modifier
    .combinedClickable(onClick = click, onLongClick = { showMenu.value = true })
    .onRightClick { showMenu.value = true }
  Box(modifier) {
    Row(
      modifier = Modifier
        .fillMaxWidth()
        .padding(start = 8.dp, top = 8.dp, end = 12.dp, bottom = 8.dp),
      verticalAlignment = Alignment.Top
    ) {
      chatLinkPreview()
    }
    if (dropdownMenuItems != null) {
      DefaultDropdownMenu(showMenu, dropdownMenuItems = dropdownMenuItems)
    }
  }
  Divider(Modifier.padding(horizontal = 8.dp))
}
