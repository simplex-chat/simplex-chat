package chat.simplex.app.views.usersettings

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight

@Composable
fun CallSettingsView(m: ChatModel) {
  CallSettingsLayout(
    webrtcPolicyRelay = m.controller.prefWebrtcPolicyRelay,
    acceptCallsFromLockScreen = m.controller.prefAcceptCallsFromLockScreen
  )
}

@Composable
fun CallSettingsLayout(
  webrtcPolicyRelay: Preference<Boolean>,
  acceptCallsFromLockScreen: Preference<Boolean>,
) {
  Column(
    Modifier.fillMaxWidth(),
    horizontalAlignment = Alignment.Start,
    verticalArrangement = Arrangement.spacedBy(8.dp)
  ) {
    Text(
      stringResource(R.string.call_settings),
      Modifier.padding(bottom = 24.dp),
      style = MaterialTheme.typography.h1
    )
    SharedPreferenceToggle(stringResource(R.string.connect_calls_via_relay), webrtcPolicyRelay)
    SharedPreferenceToggle(stringResource(R.string.accept_calls_from_lock_screen), acceptCallsFromLockScreen)
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
    )
  }
}
