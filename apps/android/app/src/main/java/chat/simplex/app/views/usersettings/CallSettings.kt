package chat.simplex.app.views.usersettings

import SectionDivider
import SectionItemView
import SectionView
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.views.helpers.*

@Composable
fun CallSettingsView(m: ChatModel,
  showModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
) {
  CallSettingsLayout(
    webrtcPolicyRelay = m.controller.appPrefs.webrtcPolicyRelay,
    callOnLockScreen = m.controller.appPrefs.callOnLockScreen,
    editIceServers = showModal { RTCServersView(m) }
  )
}

@Composable
fun CallSettingsLayout(
  webrtcPolicyRelay: SharedPreference<Boolean>,
  callOnLockScreen: SharedPreference<CallOnLockScreen>,
  editIceServers: () -> Unit,
) {
  Column(
    Modifier.fillMaxWidth(),
    horizontalAlignment = Alignment.Start,
    verticalArrangement = Arrangement.spacedBy(8.dp)
  ) {
    AppBarTitle(stringResource(R.string.your_calls))
    val lockCallState = remember { mutableStateOf(callOnLockScreen.get()) }
    SectionView(stringResource(R.string.settings_section_title_settings)) {
      SectionItemView() {
        SharedPreferenceToggle(stringResource(R.string.connect_calls_via_relay), webrtcPolicyRelay)
      }
      SectionDivider()

      val enabled = remember { mutableStateOf(true) }
      SectionItemView { LockscreenOpts(lockCallState, enabled, onSelected = { callOnLockScreen.set(it); lockCallState.value = it }) }
      SectionDivider()
      SectionItemView(editIceServers) { Text(stringResource(R.string.webrtc_ice_servers)) }
    }
  }
}

@Composable
private fun LockscreenOpts(lockscreenOpts: State<CallOnLockScreen>, enabled: State<Boolean>, onSelected: (CallOnLockScreen) -> Unit) {
  val values = remember {
    CallOnLockScreen.values().map {
      when (it) {
        CallOnLockScreen.DISABLE -> it to generalGetString(R.string.no_call_on_lock_screen)
        CallOnLockScreen.SHOW -> it to generalGetString(R.string.show_call_on_lock_screen)
        CallOnLockScreen.ACCEPT -> it to generalGetString(R.string.accept_call_on_lock_screen)
      }
    }
  }
  ExposedDropDownSettingRow(
    generalGetString(R.string.call_on_lock_screen),
    values,
    lockscreenOpts,
    icon = null,
    enabled = enabled,
    onSelected = onSelected
  )
}

@Composable
fun SharedPreferenceToggle(
  text: String,
  preference: SharedPreference<Boolean>,
  preferenceState: MutableState<Boolean>? = null,
  onChange: ((Boolean) -> Unit)? = null,
  ) {
  val prefState = preferenceState ?: remember { mutableStateOf(preference.get()) }
  Row(Modifier.fillMaxWidth(), verticalAlignment = Alignment.CenterVertically) {
    Text(text, Modifier.padding(end = 24.dp))
    Spacer(Modifier.fillMaxWidth().weight(1f))
    Switch(
      checked = prefState.value,
      onCheckedChange = {
        preference.set(it)
        prefState.value = it
        onChange?.invoke(it)
      },
      colors = SwitchDefaults.colors(
        checkedThumbColor = MaterialTheme.colors.primary,
        uncheckedThumbColor = HighOrLowlight
      )
    )
  }
}

@Composable
fun SharedPreferenceToggleWithIcon(
  text: String,
  icon: ImageVector,
  stopped: Boolean = false,
  onClickInfo: () -> Unit,
  preference: SharedPreference<Boolean>,
  preferenceState: MutableState<Boolean>? = null
) {
  val prefState = preferenceState ?: remember { mutableStateOf(preference.get()) }
  Row(Modifier.fillMaxWidth(), verticalAlignment = Alignment.CenterVertically) {
    Text(text, Modifier.padding(end = 4.dp))
    Icon(
      icon,
      null,
      Modifier.clickable(onClick = onClickInfo),
      tint = MaterialTheme.colors.primary
    )
    Spacer(Modifier.fillMaxWidth().weight(1f))
    Switch(
      checked = prefState.value,
      onCheckedChange = {
        preference.set(it)
        prefState.value = it
      },
      colors = SwitchDefaults.colors(
        checkedThumbColor = MaterialTheme.colors.primary,
        uncheckedThumbColor = HighOrLowlight
      ),
      enabled = !stopped
    )
  }
}

@Composable
fun <T>SharedPreferenceRadioButton(text: String, prefState: MutableState<T>, preference: SharedPreference<T>, value: T) {
  Row(verticalAlignment = Alignment.CenterVertically) {
    Text(text)
    val colors = RadioButtonDefaults.colors(selectedColor = MaterialTheme.colors.primary)
    RadioButton(selected = prefState.value == value, colors = colors, onClick = {
      preference.set(value)
      prefState.value = value
    })
  }
}
