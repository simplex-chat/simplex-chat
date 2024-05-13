package chat.simplex.common.views.localauth

import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.*
import androidx.compose.ui.input.key.*
import dev.icerock.moko.resources.compose.painterResource
import androidx.compose.ui.unit.dp
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.chat.group.ProgressIndicator
import chat.simplex.common.views.helpers.SimpleButton
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR

@Composable
fun PasscodeView(
  passcode: MutableState<String>,
  title: String,
  reason: String? = null,
  submitLabel: String,
  submitEnabled: ((String) -> Boolean)? = null,
  buttonsEnabled: State<Boolean> = remember { mutableStateOf(true) },
  submit: () -> Unit,
  cancel: () -> Unit,
) {
  val focusRequester = remember { FocusRequester() }

  @Composable
  fun Modifier.handleKeyboard(): Modifier {
    val numbers = remember {
      arrayOf(
        Key.Zero, Key.One, Key.Two, Key.Three, Key.Four, Key.Five, Key.Six, Key.Seven, Key.Eight, Key.Nine,
        Key.NumPad0, Key.NumPad1, Key.NumPad2, Key.NumPad3, Key.NumPad4, Key.NumPad5, Key.NumPad6, Key.NumPad7, Key.NumPad8, Key.NumPad9
      )
    }
    return onPreviewKeyEvent {
      if (it.key in numbers && it.type == KeyEventType.KeyDown) {
        if (passcode.value.length < 16) {
          passcode.value += numbers.indexOf(it.key) % 10
        }
        true
      } else if (it.key == Key.Backspace && it.type == KeyEventType.KeyDown && (it.isCtrlPressed || it.isMetaPressed)) {
        passcode.value = ""
        true
      } else if (it.key == Key.Backspace && it.type == KeyEventType.KeyDown) {
        passcode.value = passcode.value.dropLast(1)
        true
      } else if ((it.key == Key.Enter || it.key == Key.NumPadEnter) && it.type == KeyEventType.KeyUp) {
        if ((submitEnabled?.invoke(passcode.value) != false && passcode.value.length >= 4)) {
          submit()
        }
        true
      } else {
        false
      }
    }
  }

  @Composable
  fun VerticalLayout() {
    Column(
      Modifier.handleKeyboard().focusRequester(focusRequester),
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
      Row(Modifier.heightIn(min = 70.dp), verticalAlignment = Alignment.CenterVertically) {
        SimpleButton(generalGetString(MR.strings.cancel_verb), icon = painterResource(MR.images.ic_close), disabled = !buttonsEnabled.value, click = cancel)
        Spacer(Modifier.size(20.dp))
        SimpleButton(submitLabel, icon = painterResource(MR.images.ic_done_filled), disabled = submitEnabled?.invoke(passcode.value) == false || passcode.value.length < 4 || !buttonsEnabled.value, click = submit)
      }
    }
  }

  @Composable
  fun HorizontalLayout() {
    Row(Modifier.padding(horizontal = DEFAULT_PADDING).handleKeyboard().focusRequester(focusRequester), horizontalArrangement = Arrangement.Center) {
      Column(
        Modifier.padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING, top = DEFAULT_PADDING),
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
        Modifier.padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING, top = DEFAULT_PADDING),
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
            SimpleButton(generalGetString(MR.strings.cancel_verb), icon = painterResource(MR.images.ic_close), disabled = !buttonsEnabled.value, click = cancel)
            SimpleButton(submitLabel, icon = painterResource(MR.images.ic_done_filled), disabled = submitEnabled?.invoke(passcode.value) == false || passcode.value.length < 4 || !buttonsEnabled.value, click = submit)
          }
        }
      }
    }
  }

  if (windowOrientation() == WindowOrientation.PORTRAIT || appPlatform.isDesktop) {
    VerticalLayout()
  } else {
    HorizontalLayout()
  }
  if (!buttonsEnabled.value) {
    ProgressIndicator()
  }
  val view = LocalMultiplatformView()
  LaunchedEffect(Unit) {
    hideKeyboard(view, true)
    focusRequester.requestFocus()
    // Disallow to steal a focus by clicking on buttons or using Tab
    focusRequester.captureFocus()
  }
}
