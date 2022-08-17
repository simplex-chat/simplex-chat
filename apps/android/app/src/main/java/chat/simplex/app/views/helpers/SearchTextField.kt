package chat.simplex.app.views.helpers

import androidx.compose.foundation.background
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.defaultMinSize
import androidx.compose.foundation.shape.ZeroCornerSize
import androidx.compose.foundation.text.*
import androidx.compose.material.*
import androidx.compose.material.TextFieldDefaults.indicatorLine
import androidx.compose.material.TextFieldDefaults.textFieldWithLabelPadding
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Close
import androidx.compose.runtime.*
import androidx.compose.ui.ExperimentalComposeUiApi
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.focus.focusRequester
import androidx.compose.ui.graphics.*
import androidx.compose.ui.platform.LocalSoftwareKeyboardController
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.ImeAction
import androidx.compose.ui.text.input.VisualTransformation
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import kotlinx.coroutines.delay

@OptIn(ExperimentalComposeUiApi::class)
@Composable
fun SearchTextField(modifier: Modifier, placeholder: String, onValueChange: (String) -> Unit) {
  var searchText by remember { mutableStateOf("") }
  val focusRequester = remember { FocusRequester() }
  val keyboard = LocalSoftwareKeyboardController.current

  LaunchedEffect(Unit) {
    focusRequester.requestFocus()
    delay(200)
    keyboard?.show()
  }

  val enabled = true
  val colors = TextFieldDefaults.textFieldColors(
    backgroundColor = Color.Unspecified,
    textColor = MaterialTheme.colors.onBackground,
    focusedIndicatorColor = Color.Unspecified,
    unfocusedIndicatorColor = Color.Unspecified,
  )
  val shape = MaterialTheme.shapes.small.copy(bottomEnd = ZeroCornerSize, bottomStart = ZeroCornerSize)
  val interactionSource = remember { MutableInteractionSource() }
  BasicTextField(
    value = searchText,
    modifier = modifier
      .background(colors.backgroundColor(enabled).value, shape)
      .indicatorLine(enabled, false, interactionSource, colors)
      .focusRequester(focusRequester)
      .defaultMinSize(
        minWidth = TextFieldDefaults.MinWidth,
        minHeight = TextFieldDefaults.MinHeight
      ),
    onValueChange = {
      searchText = it
      onValueChange(it)
    },
    cursorBrush = SolidColor(colors.cursorColor(false).value),
    visualTransformation = VisualTransformation.None,
    keyboardOptions = KeyboardOptions(imeAction = ImeAction.Search),
    singleLine = true,
    textStyle = TextStyle(
      color = MaterialTheme.colors.onBackground,
      fontWeight = FontWeight.Normal,
      fontSize = 16.sp
    ),
    interactionSource = interactionSource,
    decorationBox = @Composable { innerTextField ->
      TextFieldDefaults.TextFieldDecorationBox(
        value = searchText,
        innerTextField = innerTextField,
        placeholder = {
          Text(placeholder)
        },
        trailingIcon = if (searchText.isNotEmpty()) {{
          IconButton({ searchText = ""; onValueChange("") }) {
            Icon(Icons.Default.Close, stringResource(R.string.icon_descr_close_button), tint = MaterialTheme.colors.primary,)
          }
        }} else null,
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
