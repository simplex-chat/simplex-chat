package chat.simplex.common.views.chatlist

import SectionDivider
import SectionItemView
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.InteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.SolidColor
import androidx.compose.ui.graphics.drawscope.ContentDrawScope
import androidx.compose.ui.node.DelegatableNode
import androidx.compose.ui.node.DrawModifierNode
import androidx.compose.ui.text.TextRange
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.unit.dp
import chat.simplex.common.platform.onRightClick
import chat.simplex.common.ui.theme.CurrentColors
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.chat.item.isShortEmoji
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource

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
  if (selectedChat.value || nextChatSelected.value) {
    Divider()
  } else {
    SectionDivider()
  }
}

@Composable
actual fun ChatTagInput(name: MutableState<String>, showError: State<Boolean>, emoji: MutableState<String?>) {
  SectionItemView(padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
    EmojiInput(emoji)
    ChatListNameTextField(name, showError = showError)
  }
}

@Composable
private fun EmojiInput(
  emoji: MutableState<String?>
) {
  TextField(
    value = emoji.value?.let { TextFieldValue(it) } ?: TextFieldValue(""),
    onValueChange = { newValue ->
      val limitedText = newValue.text.takeIf { it.isNotEmpty() }?.substring(0, newValue.text.offsetByCodePoints(0, 1)) ?: ""
      if (isShortEmoji(limitedText)) {
        emoji.value = limitedText
      } else {
        emoji.value = null
      }
    },
    singleLine = true,
    maxLines = 1,
    modifier = Modifier
      .size(60.dp)
      .padding(4.dp),
    placeholder = {
      Icon(
        painter = painterResource(MR.images.ic_add_reaction),
        contentDescription = null,
        tint = MaterialTheme.colors.secondary
      )
    },
    shape = RoundedCornerShape(8.dp),
    colors = TextFieldDefaults.textFieldColors(
      backgroundColor = Color.Unspecified,
      focusedIndicatorColor = MaterialTheme.colors.secondary.copy(alpha = 0.6f),
      unfocusedIndicatorColor = CurrentColors.value.colors.secondary.copy(alpha = 0.3f),
      cursorColor = MaterialTheme.colors.secondary,
    ),
  )
}
