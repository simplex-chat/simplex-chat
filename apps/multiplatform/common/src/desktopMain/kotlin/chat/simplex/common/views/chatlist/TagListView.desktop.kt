package chat.simplex.common.views.chatlist

import SectionItemView
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.TextRange
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.ui.theme.*
import chat.simplex.common.ui.theme.ThemeManager.colorFromReadableHex
import chat.simplex.common.views.chat.item.isHeartEmoji
import chat.simplex.common.views.chat.item.isShortEmoji
import chat.simplex.common.views.helpers.toDp
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource

@Composable
actual fun ChatTagInput(name: MutableState<String>, showError: State<Boolean>, emoji: MutableState<String?>) {
  SectionItemView(padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
    SingleEmojiInput(emoji)
    TagListNameTextField(name, showError = showError)
  }
}

@Composable
private fun SingleEmojiInput(
  emoji: MutableState<String?>
) {
  val state = remember { mutableStateOf(TextFieldValue(emoji.value ?: "")) }
  val colors = TextFieldDefaults.textFieldColors(
    textColor = if (isHeartEmoji(emoji.value ?: "")) Color(0xffD63C31) else MaterialTheme.colors.onPrimary,
    backgroundColor = Color.Unspecified,
    focusedIndicatorColor = MaterialTheme.colors.secondary.copy(alpha = 0.6f),
    unfocusedIndicatorColor = CurrentColors.value.colors.secondary.copy(alpha = 0.3f),
    cursorColor = MaterialTheme.colors.secondary,
  )
  TextField(
    value = state.value,
    onValueChange = { newValue ->
      if (newValue.text == emoji.value) {
        state.value = newValue
        return@TextField
      }
      val newValueClamped = newValue.text.replace(emoji.value ?: "", "")
      val isEmoji = isShortEmoji(newValueClamped)
      emoji.value = if (isEmoji) newValueClamped else null
      state.value = if (isEmoji) newValue else TextFieldValue()
    },
    singleLine = true,
    modifier = Modifier
      .padding(4.dp)
      .size(width = TextFieldDefaults.MinHeight.value.sp.toDp(), height = TextFieldDefaults.MinHeight),
    textStyle = LocalTextStyle.current.copy(fontFamily = EmojiFont, textAlign = TextAlign.Center),
    placeholder = {
      Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
        Icon(
          painter = painterResource(MR.images.ic_add_reaction),
          contentDescription = null,
          tint = MaterialTheme.colors.secondary
        )
      }
    },
    colors = colors,
  )
}
