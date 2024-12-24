package chat.simplex.common.views.chatlist

import SectionItemView
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.unit.dp
import chat.simplex.common.ui.theme.CurrentColors
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.chat.item.isShortEmoji
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource

@Composable
actual fun ChatTagInput(name: MutableState<String>, showError: State<Boolean>, emoji: MutableState<String?>) {
  SectionItemView(padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
    SingleEmojiInput(emoji)
    ChatListNameTextField(name, showError = showError)
  }
}

@Composable
private fun SingleEmojiInput(
  emoji: MutableState<String?>
) {
  TextField(
    value = emoji.value?.let { TextFieldValue(it) } ?: TextFieldValue(""),
    onValueChange = { newValue ->
      if (newValue.text == emoji.value) return@TextField
      val newValueClamped = newValue.text.replace(emoji.value ?: "", "")
      emoji.value = if (isShortEmoji(newValueClamped)) newValueClamped else null
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
