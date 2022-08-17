package chat.simplex.app.views.helpers

import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Close
import androidx.compose.runtime.*
import androidx.compose.ui.ExperimentalComposeUiApi
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.focus.focusRequester
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalSoftwareKeyboardController
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.ImeAction
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

  TextField(
    searchText,
    onValueChange = {
      searchText = it
      onValueChange(it)
    },
    modifier.focusRequester(focusRequester),
    placeholder = {
      Text(placeholder)
    },
    trailingIcon = if (searchText.isNotEmpty()) {{
      IconButton({ searchText = ""; onValueChange("") }) {
        Icon(Icons.Default.Close, stringResource(R.string.icon_descr_close_button), tint = MaterialTheme.colors.primary,)
      }
    }} else null,
    keyboardOptions =  KeyboardOptions(imeAction = ImeAction.Search),
    singleLine = true,
    textStyle = TextStyle(
      fontWeight = FontWeight.Normal,
      fontSize = 16.sp
    ),
    colors = TextFieldDefaults.textFieldColors(
      backgroundColor = Color.Unspecified,
      textColor = MaterialTheme.colors.onBackground,
      focusedIndicatorColor = Color.Unspecified,
      unfocusedIndicatorColor = Color.Unspecified,
    )
  )
}