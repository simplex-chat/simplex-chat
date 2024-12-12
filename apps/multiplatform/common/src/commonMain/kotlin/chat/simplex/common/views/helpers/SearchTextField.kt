package chat.simplex.common.views.helpers

import androidx.compose.foundation.background
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
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
import androidx.compose.ui.text.input.*
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
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
  reducedCloseButtonPadding: Dp = 0.dp,
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
        // Delay is needed here because when ChatView is being opened and keyboard is hiding, bottom sheet (to choose attachment) is visible on a screen
        delay(300)
        keyboard?.hide()
      }
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
  val textStyle = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground)
  // sizing is done differently on Android and desktop in order to have the same height of search and compose view on desktop
  // see PlatformTextField.desktop + SendMsgView
  val padding = if (appPlatform.isAndroid) PaddingValues() else PaddingValues(top = 3.dp, bottom = 4.dp)
  BasicTextField(
    value = searchText.value,
    modifier = modifier
      .background(colors.backgroundColor(enabled).value, shape)
      .indicatorLine(enabled, false, interactionSource, colors)
      .focusRequester(focusRequester)
      .padding(padding)
      .defaultMinSize(
        minWidth = TextFieldDefaults.MinWidth,
        minHeight = if (appPlatform.isAndroid) TextFieldDefaults.MinHeight else 0.dp
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
    textStyle = textStyle,
    interactionSource = interactionSource,
    decorationBox = @Composable { innerTextField ->
      TextFieldDefaults.TextFieldDecorationBox(
        value = searchText.value.text,
        innerTextField = innerTextField,
        placeholder = {
          Text(placeholder, style = textStyle.copy(color = MaterialTheme.colors.secondary), maxLines = 1, overflow = TextOverflow.Ellipsis)
        },
        trailingIcon = if (searchText.value.text.isNotEmpty()) {{
          IconButton({
            if (alwaysVisible) {
              keyboard?.hide()
              focusManager.clearFocus()
            }
            searchText.value = TextFieldValue("");
            onValueChange("")
          }, Modifier.offset(x = reducedCloseButtonPadding)) {
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
