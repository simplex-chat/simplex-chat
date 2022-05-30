package chat.simplex.app.views.usersettings

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextDecoration
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight

@Composable
fun CallSettingsView(m: ChatModel) {
  CallSettingsLayout(
    webrtcPolicyRelay = m.controller.appPrefs.webrtcPolicyRelay,
    callOnLockScreen = m.controller.appPrefs.callOnLockScreen
  )
}

@Composable
fun CallSettingsLayout(
  webrtcPolicyRelay: Preference<Boolean>,
  callOnLockScreen: Preference<CallOnLockScreen>,
) {
  Column(
    Modifier.fillMaxWidth(),
    horizontalAlignment = Alignment.Start,
    verticalArrangement = Arrangement.spacedBy(8.dp)
  ) {
    val lockCallState = remember { mutableStateOf(callOnLockScreen.get()) }
    Text(
      stringResource(R.string.your_calls),
      Modifier.padding(bottom = 24.dp),
      style = MaterialTheme.typography.h1
    )
    SharedPreferenceToggle(stringResource(R.string.connect_calls_via_relay), webrtcPolicyRelay)
    Column {
      Text(stringResource(R.string.call_on_lock_screen))
      Row {
        SharedPreferenceRadioButton(stringResource(R.string.no_call_on_lock_screen), lockCallState, callOnLockScreen, CallOnLockScreen.DISABLE)
        Spacer(Modifier.fillMaxWidth().weight(1f))
        SharedPreferenceRadioButton(stringResource(R.string.show_call_on_lock_screen), lockCallState, callOnLockScreen, CallOnLockScreen.SHOW)
        Spacer(Modifier.fillMaxWidth().weight(1f))
        SharedPreferenceRadioButton(stringResource(R.string.accept_call_on_lock_screen), lockCallState, callOnLockScreen, CallOnLockScreen.ACCEPT)
      }
    }
  }
}

@Composable
fun SharedPreferenceToggle(
  text: String,
  preference: Preference<Boolean>
) {
  var preferenceState by remember { mutableStateOf(preference.get()) }
  Row(Modifier.fillMaxWidth(), verticalAlignment = Alignment.CenterVertically) {
    Text(text, Modifier.padding(end = 24.dp))
    Spacer(Modifier.fillMaxWidth().weight(1f))
    Switch(
      checked = preferenceState,
      onCheckedChange = {
        preference.set(it)
        preferenceState = it
      },
      colors = SwitchDefaults.colors(
        checkedThumbColor = MaterialTheme.colors.primary,
        uncheckedThumbColor = HighOrLowlight
      ),
      modifier = Modifier.padding(end = 6.dp)
    )
  }
}

@Composable
fun <T>SharedPreferenceRadioButton(text: String, prefState: MutableState<T>, preference: Preference<T>, value: T) {
  Row(verticalAlignment = Alignment.CenterVertically) {
    Text(text)
    val colors = RadioButtonDefaults.colors(selectedColor = MaterialTheme.colors.primary)
    RadioButton(selected = prefState.value == value, colors = colors, onClick = {
      preference.set(value)
      prefState.value = value
    })
  }
}
