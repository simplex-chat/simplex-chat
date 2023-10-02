package chat.simplex.common.views.chatlist

import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.InteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.material.Divider
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.drawscope.ContentDrawScope
import androidx.compose.ui.unit.dp
import chat.simplex.common.views.helpers.*

private object NoIndication : Indication {
  private object NoIndicationInstance : IndicationInstance {
    override fun ContentDrawScope.drawIndication() {
      drawContent()
    }
  }
  @Composable
  override fun rememberUpdatedInstance(interactionSource: InteractionSource): IndicationInstance {
    return NoIndicationInstance
  }
}

@Composable
actual fun ChatListNavLinkLayout(
  chatLinkPreview: @Composable () -> Unit,
  click: () -> Unit,
  dropdownMenuItems: (@Composable () -> Unit)?,
  showMenu: MutableState<Boolean>,
  stopped: Boolean,
  selectedChat: State<Boolean>
) {
  var modifier = Modifier.fillMaxWidth()
  if (!stopped) modifier = modifier
    .background(color = if (selectedChat.value) MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.95f) else Color.Unspecified)
    .combinedClickable(onClick = click, onLongClick = { showMenu.value = true })
    .onRightClick { showMenu.value = true }
  CompositionLocalProvider(
    LocalIndication provides if (selectedChat.value && !stopped) NoIndication else LocalIndication.current
  ) {
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
  }
  Divider()
}
