package chat.simplex.common.views.helpers

import androidx.compose.foundation.background
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.defaultMinSize
import androidx.compose.foundation.shape.ZeroCornerSize
import androidx.compose.foundation.text.*
import androidx.compose.material.*
import androidx.compose.material.TextFieldDefaults.indicatorLine
import androidx.compose.material.TextFieldDefaults.textFieldWithLabelPadding
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.focus.focusRequester
import androidx.compose.ui.graphics.*
import androidx.compose.ui.platform.LocalFocusManager
import androidx.compose.ui.platform.LocalSoftwareKeyboardController
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.*
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.platform.*
import chat.simplex.res.MR
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.filter
import kotlinx.coroutines.launch

@Composable
fun SearchTextField(
  modifier: Modifier,
  alwaysVisible: Boolean,
  searchText: MutableState<TextFieldValue> = rememberSaveable(stateSaver = TextFieldValue.Saver) { mutableStateOf(TextFieldValue("")) },
  placeholder: String = stringResource(MR.strings.search_verb),
  enabled: Boolean = true,
  trailingContent: @Composable (() -> Unit)? = null,
  onValueChange: (String) -> Unit
) {
  val focusRequester = remember { FocusRequester() }
  val focusManager = LocalFocusManager.current
  val keyboard = LocalSoftwareKeyboardController.current

  if (!alwaysVisible) {
    LaunchedEffect(Unit) {
      focusRequester.requestFocus()
      delay(200)
      keyboard?.show()
    }
  }
  if (appPlatform.isAndroid) {
    LaunchedEffect(Unit) {
      val modalCountOnOpen = ModalManager.start.modalCount.value
      launch {
        snapshotFlow { ModalManager.start.modalCount.value }
          .filter { it > modalCountOnOpen }
          .collect {
            keyboard?.hide()
          }
      }
    }
    KeyChangeEffect(chatModel.chatId.value) {
      if (chatModel.chatId.value != null) {
        delay(300)
        keyboard?.hide()
      }
    }
  }

  DisposableEffect(Unit) {
    onDispose {
      if (searchText.value.text.isNotEmpty()) onValueChange("")
    }
  }

  val colors = TextFieldDefaults.textFieldColors(
    backgroundColor = Color.Unspecified,
    textColor = MaterialTheme.colors.onBackground,
    focusedIndicatorColor = Color.Unspecified,
    unfocusedIndicatorColor = Color.Unspecified,
    disabledIndicatorColor = Color.Unspecified,
    placeholderColor = MaterialTheme.colors.secondary,
  )
  val shape = MaterialTheme.shapes.small.copy(bottomEnd = ZeroCornerSize, bottomStart = ZeroCornerSize)
  val interactionSource = remember { MutableInteractionSource() }
  BasicTextField(
    value = searchText.value,
    modifier = modifier
      .background(colors.backgroundColor(enabled).value, shape)
      .indicatorLine(enabled, false, interactionSource, colors)
      .focusRequester(focusRequester)
      .defaultMinSize(
        minWidth = TextFieldDefaults.MinWidth,
        minHeight = TextFieldDefaults.MinHeight
      ),
    onValueChange = {
      searchText.value = it
      onValueChange(it.text)
    },
    enabled = rememberUpdatedState(enabled).value,
    cursorBrush = SolidColor(colors.cursorColor(false).value),
    visualTransformation = VisualTransformation.None,
    keyboardOptions = KeyboardOptions(imeAction = ImeAction.Search),
    singleLine = true,
    textStyle = TextStyle(
      color = MaterialTheme.colors.onBackground,
      fontWeight = FontWeight.Normal,
      fontSize = 15.sp
    ),
    interactionSource = interactionSource,
    decorationBox = @Composable { innerTextField ->
      TextFieldDefaults.TextFieldDecorationBox(
        value = searchText.value.text,
        innerTextField = innerTextField,
        placeholder = {
          Text(placeholder, maxLines = 1, overflow = TextOverflow.Ellipsis)
        },
        trailingIcon = if (searchText.value.text.isNotEmpty()) {{
          IconButton({
            if (alwaysVisible) {
              keyboard?.hide()
              focusManager.clearFocus()
            }
            searchText.value = TextFieldValue("");
            onValueChange("")
          }) {
            Icon(painterResource(MR.images.ic_close), stringResource(MR.strings.icon_descr_close_button), tint = MaterialTheme.colors.primary,)
          }
        }} else trailingContent,
        singleLine = true,
        enabled = enabled,
        interactionSource = interactionSource,
        contentPadding = textFieldWithLabelPadding(start = 0.dp, end = 0.dp),
        visualTransformation = VisualTransformation.None,
        colors = colors
      )
    }
  )
}
