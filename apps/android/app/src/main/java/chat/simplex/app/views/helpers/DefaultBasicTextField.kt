package chat.simplex.app.views.helpers

import androidx.compose.foundation.background
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.defaultMinSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.shape.ZeroCornerSize
import androidx.compose.foundation.text.*
import androidx.compose.material.*
import androidx.compose.material.TextFieldDefaults.indicatorLine
import androidx.compose.runtime.*
import androidx.compose.ui.ExperimentalComposeUiApi
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.*
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.SolidColor
import androidx.compose.ui.platform.LocalSoftwareKeyboardController
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.*
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.views.database.PassphraseStrength
import chat.simplex.app.views.database.validKey
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.collect
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.launch

@OptIn(ExperimentalComposeUiApi::class)
@Composable
fun DefaultBasicTextField(
  modifier: Modifier,
  initialValue: String,
  placeholder: (@Composable () -> Unit)? = null,
  leadingIcon: (@Composable () -> Unit)? = null,
  focus: Boolean = false,
  color: Color = MaterialTheme.colors.onBackground,
  textStyle: TextStyle = TextStyle.Default,
  selectTextOnFocus: Boolean = false,
  keyboardOptions: KeyboardOptions = KeyboardOptions(imeAction = ImeAction.Done),
  keyboardActions: KeyboardActions = KeyboardActions(),
  onValueChange: (String) -> Unit,
) {
  val state = remember {
    mutableStateOf(TextFieldValue(initialValue))
  }
  val focusRequester = remember { FocusRequester() }
  val keyboard = LocalSoftwareKeyboardController.current

  LaunchedEffect(Unit) {
    if (!focus) return@LaunchedEffect
    delay(300)
    focusRequester.requestFocus()
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
    value = state.value,
    modifier = modifier
      .background(colors.backgroundColor(enabled).value, shape)
      .indicatorLine(enabled, false, interactionSource, colors)
      .focusRequester(focusRequester)
      .onFocusChanged { focusState ->
        if (focusState.isFocused && selectTextOnFocus) {
          val text = state.value.text
          state.value = state.value.copy(
            selection = TextRange(0, text.length)
          )
        }
      }
      .defaultMinSize(
        minWidth = TextFieldDefaults.MinWidth,
        minHeight = TextFieldDefaults.MinHeight
      ),
    onValueChange = {
      state.value = it
      onValueChange(it.text)
    },
    cursorBrush = SolidColor(colors.cursorColor(false).value),
    visualTransformation = VisualTransformation.None,
    keyboardOptions = keyboardOptions,
    keyboardActions = KeyboardActions(onDone = {
      keyboard?.hide()
      keyboardActions.onDone?.invoke(this)
    }),
    singleLine = true,
    textStyle = textStyle.copy(
      color = color,
      fontWeight = FontWeight.Normal,
      fontSize = 16.sp
    ),
    interactionSource = interactionSource,
    decorationBox = @Composable { innerTextField ->
      TextFieldDefaults.TextFieldDecorationBox(
        value = state.value.text,
        innerTextField = innerTextField,
        placeholder = placeholder,
        singleLine = true,
        enabled = enabled,
        leadingIcon = leadingIcon,
        interactionSource = interactionSource,
        contentPadding = TextFieldDefaults.textFieldWithLabelPadding(start = 0.dp, end = 0.dp),
        visualTransformation = VisualTransformation.None,
        colors = colors
      )
    }
  )
}

@OptIn(ExperimentalComposeUiApi::class)
@Composable
fun DefaultConfigurableTextField(
  state: MutableState<TextFieldValue>,
  placeholder: String,
  modifier: Modifier = Modifier,
  showPasswordStrength: Boolean = false,
  isValid: (String) -> Boolean,
  keyboardActions: KeyboardActions = KeyboardActions(),
  keyboardType: KeyboardType = KeyboardType.Text,
  dependsOn: State<Any?>? = null,
) {
  var valid by remember { mutableStateOf(validKey(state.value.text)) }
  var showKey by remember { mutableStateOf(false) }
  val icon = if (valid) {
    if (showKey) painterResource(R.drawable.ic_visibility_off_filled) else painterResource(R.drawable.ic_visibility_filled)
  } else painterResource(R.drawable.ic_error)
  val iconColor = if (valid) {
    if (showPasswordStrength && state.value.text.isNotEmpty()) PassphraseStrength.check(state.value.text).color else MaterialTheme.colors.secondary
  } else Color.Red
  val keyboard = LocalSoftwareKeyboardController.current
  val keyboardOptions = KeyboardOptions(
    imeAction = if (keyboardActions.onNext != null) ImeAction.Next else ImeAction.Done,
    autoCorrect = keyboardType != KeyboardType.Password,
    keyboardType = keyboardType
  )
  val enabled = true
  val colors = TextFieldDefaults.textFieldColors(
    backgroundColor = Color.Unspecified,
    textColor = MaterialTheme.colors.onBackground,
    focusedIndicatorColor = Color.Unspecified,
    unfocusedIndicatorColor = Color.Unspecified,
  )
  val color = MaterialTheme.colors.onBackground
  val shape = MaterialTheme.shapes.small.copy(bottomEnd = ZeroCornerSize, bottomStart = ZeroCornerSize)
  val interactionSource = remember { MutableInteractionSource() }
  BasicTextField(
    value = state.value,
    modifier = modifier
      .fillMaxWidth()
      .background(colors.backgroundColor(enabled).value, shape)
      .indicatorLine(enabled, false, interactionSource, colors)
      .defaultMinSize(
        minWidth = TextFieldDefaults.MinWidth,
        minHeight = TextFieldDefaults.MinHeight
      ),
    onValueChange = {
      state.value = it
    },
    cursorBrush = SolidColor(colors.cursorColor(false).value),
    visualTransformation = if (showKey || keyboardType != KeyboardType.Password)
      VisualTransformation.None
    else
      VisualTransformation { TransformedText(AnnotatedString(it.text.map { "*" }.joinToString(separator = "")), OffsetMapping.Identity) },
    keyboardOptions = keyboardOptions,
    keyboardActions = KeyboardActions(onDone = {
      keyboard?.hide()
      keyboardActions.onDone?.invoke(this)
    }),
    singleLine = true,
    textStyle = TextStyle.Default.copy(
      color = color,
      fontWeight = FontWeight.Normal,
      fontSize = 16.sp
    ),
    interactionSource = interactionSource,
    decorationBox = @Composable { innerTextField ->
      TextFieldDefaults.TextFieldDecorationBox(
        value = state.value.text,
        innerTextField = innerTextField,
        placeholder = { Text(placeholder, color = MaterialTheme.colors.secondary) },
        singleLine = true,
        enabled = enabled,
        isError = !valid,
        trailingIcon = {
          if (keyboardType == KeyboardType.Password || !valid) {
            IconButton({ showKey = !showKey }) {
              Icon(icon, null, tint = iconColor)
            }
          }
        },
        interactionSource = interactionSource,
        contentPadding = TextFieldDefaults.textFieldWithLabelPadding(start = 0.dp, end = 0.dp),
        visualTransformation = VisualTransformation.None,
        colors = colors
      )
    }
  )
  LaunchedEffect(Unit) {
    launch {
      snapshotFlow { state.value }
        .distinctUntilChanged()
        .collect {
          valid = isValid(it.text)
        }
    }
    launch {
      snapshotFlow { dependsOn?.value }
        .distinctUntilChanged()
        .collect {
          valid = isValid(state.value.text)
        }
    }
  }
}
