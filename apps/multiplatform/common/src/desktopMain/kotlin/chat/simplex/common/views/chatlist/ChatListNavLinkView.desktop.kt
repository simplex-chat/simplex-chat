package chat.simplex.common.views.chatlist

import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.InteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusDirection
import androidx.compose.ui.focus.onFocusChanged
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.drawscope.ContentDrawScope
import androidx.compose.ui.input.key.*
import androidx.compose.ui.node.DelegatableNode
import androidx.compose.ui.node.DrawModifierNode
import androidx.compose.ui.platform.LocalFocusManager
import androidx.compose.ui.semantics.selected
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.unit.dp
import androidx.compose.foundation.shape.RoundedCornerShape
import chat.simplex.common.tokens
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.onRightClick
import chat.simplex.common.platform.desktopOnHovered
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
  val density = remember { appPrefs.desktopChatDensity.state }.value.tokens()
  val focusManager = LocalFocusManager.current
  var focused by remember { mutableStateOf(false) }
  var hovered by remember { mutableStateOf(false) }
  val rowShape = RoundedCornerShape(9.dp)
  var modifier = Modifier
    .padding(horizontal = 7.dp, vertical = 2.dp)
    .fillMaxWidth()
    .clip(rowShape)
    .background(
      when {
        selectedChat.value -> MaterialTheme.colors.primary.copy(alpha = 0.18f)
        focused -> MaterialTheme.colors.primary.copy(alpha = 0.10f)
        hovered -> MaterialTheme.colors.onBackground.copy(alpha = 0.07f)
        else -> Color.Transparent
      }
    )
    .then(
      if (focused) Modifier.border(1.dp, MaterialTheme.colors.primary.copy(alpha = 0.55f), rowShape)
      else Modifier
    )
    .alpha(if (disabled) 0.55f else 1f)
  if (!disabled) modifier = modifier
    .combinedClickable(onClick = click, onLongClick = { showMenu.value = true })
    .onRightClick { showMenu.value = true }
    .onFocusChanged { focused = it.isFocused }
    .desktopOnHovered { hovered = it }
    .focusable()
    .onPreviewKeyEvent { event ->
      if (event.type != KeyEventType.KeyDown) return@onPreviewKeyEvent false
      when (event.key) {
        Key.Enter, Key.NumPadEnter, Key.Spacebar -> {
          click()
          true
        }
        Key.DirectionUp -> focusManager.moveFocus(FocusDirection.Up)
        Key.DirectionDown -> focusManager.moveFocus(FocusDirection.Down)
        else -> false
      }
    }
    .semantics { selected = selectedChat.value }
  CompositionLocalProvider(
    LocalIndication provides if (selectedChat.value && !disabled) NoIndication else LocalIndication.current
  ) {
    Box(modifier) {
      Row(
        modifier = Modifier
          .fillMaxWidth()
          .padding(start = 10.dp, top = density.chatRowVerticalPadding, end = 12.dp, bottom = density.chatRowVerticalPadding),
        verticalAlignment = Alignment.Top
      ) {
        chatLinkPreview()
      }
      if (selectedChat.value) {
        Box(
          Modifier
            .align(Alignment.CenterStart)
            .padding(start = 3.dp)
            .width(3.dp)
            .height(28.dp)
            .clip(RoundedCornerShape(2.dp))
            .background(MaterialTheme.colors.primary)
        )
      }
      if (dropdownMenuItems != null) {
        DefaultDropdownMenu(showMenu, dropdownMenuItems = dropdownMenuItems)
      }
    }
  }
}
