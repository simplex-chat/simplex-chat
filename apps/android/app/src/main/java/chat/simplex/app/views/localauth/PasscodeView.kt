package chat.simplex.app.views.localauth

import android.content.res.Configuration
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.ui.theme.DEFAULT_PADDING
import chat.simplex.app.ui.theme.SimpleButton
import chat.simplex.app.views.helpers.*

@Composable
fun PasscodeView(
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
    Column(
      horizontalAlignment = Alignment.CenterHorizontally,
      verticalArrangement = Arrangement.SpaceEvenly
    ) {
      Column(horizontalAlignment = Alignment.CenterHorizontally) {
        Text(title, style = MaterialTheme.typography.h1)
        if (reason != null) {
          Text(reason, Modifier.padding(top = 5.dp), style = MaterialTheme.typography.subtitle1)
        }
      }
      PasscodeEntry(passcode, true)
      Row {
        SimpleButton(generalGetString(R.string.cancel_verb), icon = painterResource(R.drawable.ic_close), click = cancel)
        Spacer(Modifier.size(20.dp))
        SimpleButton(submitLabel, icon = painterResource(R.drawable.ic_done_filled), disabled = submitEnabled?.invoke(passcode.value) == false || passcode.value.length < 4, click = submit)
      }
    }
  }

  @Composable
  fun HorizontalLayout() {
    Row(Modifier.padding(horizontal = DEFAULT_PADDING), horizontalArrangement = Arrangement.Center) {
      Column(
        Modifier.padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING, top = DEFAULT_PADDING * 4),
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.SpaceBetween
      ) {
        Column(horizontalAlignment = Alignment.CenterHorizontally) {
          Text(title, style = MaterialTheme.typography.h1)
          if (reason != null) {
            Text(reason, Modifier.padding(top = 5.dp), style = MaterialTheme.typography.subtitle1)
          }
        }
        PasscodeEntry(passcode, false)
      }

      Column(
        Modifier.padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING, top = DEFAULT_PADDING * 4),
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.SpaceBetween
      ) {
        // Just to fill space to correctly calculate the height
        Column {
          Text("", style = MaterialTheme.typography.h1)
          if (reason != null) {
            Text("", Modifier.padding(top = 5.dp), style = MaterialTheme.typography.subtitle1)
          }
          PasscodeView(remember { mutableStateOf("") })
        }
        BoxWithConstraints {
          val s = minOf(maxWidth, maxHeight) / 3.5f
          Column(
            Modifier.padding(start = 30.dp).height(s * 3),
            verticalArrangement = Arrangement.SpaceEvenly
          ) {
            SimpleButton(generalGetString(R.string.cancel_verb), icon = painterResource(R.drawable.ic_close), click = cancel)
            SimpleButton(submitLabel, icon = painterResource(R.drawable.ic_done_filled), disabled = submitEnabled?.invoke(passcode.value) == false || passcode.value.length < 4, click = submit)
          }
        }
      }
    }
  }

  if (LocalContext.current.resources.configuration.orientation == Configuration.ORIENTATION_PORTRAIT) {
    VerticalLayout()
  } else {
    HorizontalLayout()
  }
}
