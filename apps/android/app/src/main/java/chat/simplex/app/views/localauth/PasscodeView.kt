package chat.simplex.app.views.localauth

import android.content.res.Configuration
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.Done
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.DEFAULT_PADDING
import chat.simplex.app.ui.theme.SimpleButton
import chat.simplex.app.views.helpers.*

@Composable
fun PasscodeView(
  m: ChatModel,
  passcode: MutableState<String>,
  title: String,
  reason: String? = null,
  submitLabel: String,
  submitEnabled: ((String) -> Boolean)? = null,
  submit: () -> Unit,
  cancel: () -> Unit,
) {
  @Composable
  fun VerticalLayout() {
    Column(Modifier.padding(horizontal = DEFAULT_PADDING), horizontalAlignment = Alignment.CenterHorizontally) {
      Text(title, Modifier.padding(top = 10.dp), style = MaterialTheme.typography.h6)
      if (reason != null) {
        Text(reason, Modifier.padding(top = 5.dp), style = MaterialTheme.typography.subtitle1)
      }
      PasscodeEntry(m, passcode, true)
      Row(Modifier.padding(top = 30.dp)) {
        SimpleButton(generalGetString(R.string.cancel_verb), icon = Icons.Default.Close, click = cancel)
        Spacer(Modifier.size(20.dp))
        SimpleButton(submitLabel, icon = Icons.Default.Done, disabled = submitEnabled?.invoke(passcode.value) == false || passcode.value.length < 4, click = submit)
      }
    }
  }

  @Composable
  fun HorizontalLayout() {
    Row(Modifier.padding(horizontal = DEFAULT_PADDING), horizontalArrangement = Arrangement.Center) {
      Column(Modifier, horizontalAlignment = Alignment.CenterHorizontally) {
        Text(title, Modifier.padding(top = 10.dp), style = MaterialTheme.typography.h6)
        if (reason != null) {
          Text(reason, Modifier.padding(top = 5.dp), style = MaterialTheme.typography.subtitle1)
        }
        PasscodeEntry(m, passcode, false)
      }
      Column(Modifier.align(Alignment.CenterVertically).padding(top = if (reason != null) 30.dp else 0.dp, start = 30.dp)) {
        SimpleButton(generalGetString(R.string.cancel_verb), icon = Icons.Default.Close, click = cancel)
        Spacer(Modifier.size(30.dp))
        SimpleButton(submitLabel, icon = Icons.Default.Done, disabled = submitEnabled?.invoke(passcode.value) == false || passcode.value.length < 4, click = submit)
      }
    }
  }

  if (LocalContext.current.resources.configuration.orientation == Configuration.ORIENTATION_PORTRAIT) {
    VerticalLayout()
  } else {
    HorizontalLayout()
  }
}
