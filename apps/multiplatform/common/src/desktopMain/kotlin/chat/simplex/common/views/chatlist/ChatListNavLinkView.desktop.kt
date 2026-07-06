package chat.simplex.common.views.chatlist

import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.InteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.drawscope.ContentDrawScope
import androidx.compose.ui.node.DelegatableNode
import androidx.compose.ui.node.DrawModifierNode
import androidx.compose.ui.unit.dp
import chat.simplex.common.platform.onRightClick
import chat.simplex.common.ui.theme.CurrentColors
import chat.simplex.common.ui.theme.DefaultTheme
import chat.simplex.common.ui.theme.oklch
import chat.simplex.common.views.helpers.*

object NoIndication : IndicationNodeFactory {
  // Should be as a class, not an object. Otherwise, crash
  private class NoIndicationInstance : Modifier.Node(), DrawModifierNode {
    override fun ContentDrawScope.draw() { drawContent() }
  }
  override fun create(interactionSource: InteractionSource): DelegatableNode = NoIndicationInstance()
  override fun hashCode(): Int = -1
  override fun equals(other: Any?) = other === this
}

@Composable
actual fun ChatListNavLinkLayout(
  chatLinkPreview: @Composable () -> Unit,
  click: () -> Unit,
  dropdownMenuItems: (@Composable () -> Unit)?,
  showMenu: MutableState<Boolean>,
  disabled: Boolean,
  selectedChat: State<Boolean>,
  nextChatSelected: State<Boolean>,
) {
  var modifier = Modifier.fillMaxWidth()
  if (!disabled) modifier = modifier
    .combinedClickable(onClick = click, onLongClick = { showMenu.value = true })
    .onRightClick { showMenu.value = true }
  CompositionLocalProvider(
    LocalIndication provides if (selectedChat.value && !disabled) NoIndication else LocalIndication.current
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
      if (selectedChat.value) {
        Box(Modifier.matchParentSize().background(MaterialTheme.colors.onBackground.copy(0.05f)))
      }
      if (dropdownMenuItems != null) {
        DefaultDropdownMenu(showMenu, dropdownMenuItems = dropdownMenuItems)
      }
    }
  }
  val activeThemeBase = CurrentColors.collectAsState().value.base
  // SIMPLEX uses a flat slightly-lighter-than-bg divider; other themes get the Material default.
  if (activeThemeBase == DefaultTheme.SIMPLEX) {
    val simplexDividerColor = oklch(0.2104f, 0.0407f, 276.40f) // sRGB #131729
    if (selectedChat.value || nextChatSelected.value) Divider(color = simplexDividerColor)
    else Divider(Modifier.padding(horizontal = 8.dp), color = simplexDividerColor)
  } else if (selectedChat.value || nextChatSelected.value) {
    Divider()
  } else {
    Divider(Modifier.padding(horizontal = 8.dp))
  }
}
