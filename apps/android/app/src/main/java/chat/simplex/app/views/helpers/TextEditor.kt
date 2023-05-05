package chat.simplex.app.views.helpers

import android.util.Log
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.*
import androidx.compose.ui.graphics.*
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.input.KeyboardCapitalization
import androidx.compose.ui.text.input.VisualTransformation
import androidx.compose.ui.unit.*
import chat.simplex.app.TAG
import chat.simplex.app.chatParseMarkdown
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.item.MarkdownText
import com.google.accompanist.insets.navigationBarsWithImePadding
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.serialization.Serializable
import java.lang.Exception

@Composable
fun TextEditor(
  value: MutableState<String>,
  modifier: Modifier,
  placeholder: String? = null,
  contentPadding: PaddingValues = PaddingValues(horizontal = DEFAULT_PADDING),
  isValid: (String) -> Boolean = { true },
  focusRequester: FocusRequester? = null
) {
  var valid by rememberSaveable { mutableStateOf(true) }
  var focused by rememberSaveable { mutableStateOf(false) }
  val strokeColor by remember {
    derivedStateOf {
      if (valid) {
        if (focused) {
          CurrentColors.value.colors.secondary.copy(alpha = 0.6f)
        } else {
          CurrentColors.value.colors.secondary.copy(alpha = 0.3f)
        }
      } else Color.Red
    }
  }
  Box(
    Modifier
      .fillMaxWidth()
      .padding(contentPadding)
      .heightIn(min = 52.dp),
//      .border(border = BorderStroke(1.dp, strokeColor), shape = RoundedCornerShape(26.dp)),
    contentAlignment = Alignment.Center,
  ) {
    val modifier = modifier
      .fillMaxWidth()
      .navigationBarsWithImePadding()
      .onFocusChanged { focused = it.isFocused }

    BasicTextField(
      value = value.value,
      onValueChange = { value.value = it },
      modifier = if (focusRequester == null) modifier else modifier.focusRequester(focusRequester),
      textStyle = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground, lineHeight = 22.sp),
      keyboardOptions = KeyboardOptions(
        capitalization = KeyboardCapitalization.None,
        autoCorrect = false
      ),
      singleLine = false,
      maxLines = 5,
      cursorBrush = SolidColor(MaterialTheme.colors.secondary),
      decorationBox = @Composable { innerTextField ->
        TextFieldDefaults.TextFieldDecorationBox(
          value = value.value,
          innerTextField = innerTextField,
          placeholder = if (placeholder != null) {{ Text(placeholder, style = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.secondary, lineHeight = 22.sp)) }} else null,
          contentPadding = PaddingValues(),
          label = null,
          visualTransformation = VisualTransformation.None,
          leadingIcon = null,
          trailingIcon = null,
          singleLine = false,
          enabled = true,
          isError = false,
          interactionSource = remember { MutableInteractionSource() },
        )
      }
    )
  }
  LaunchedEffect(Unit) {
    snapshotFlow { value.value }
      .distinctUntilChanged()
      .collect {
        valid = isValid(it)
      }
  }
}

@Serializable
data class ParsedFormattedText(
  val formattedText: List<FormattedText>? = null
)

fun parseToMarkdown(text: String): List<FormattedText>? {
  val formatted = chatParseMarkdown(text)
  return try {
    json.decodeFromString(ParsedFormattedText.serializer(), formatted).formattedText
  } catch (e: Exception) {
    Log.e(TAG, "Failed to parse into markdown: " + e.stackTraceToString())
    null
  }
}
