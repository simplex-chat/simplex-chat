package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import com.icerockdev.library.MR
import chat.simplex.common.model.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.ChatModel

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
    Modifier.fillMaxWidth().verticalScroll(rememberScrollState()),
    verticalArrangement = Arrangement.spacedBy(8.dp)
  ) {
    AppBarTitle(stringResource(MR.strings.your_calls))
    val lockCallState = remember { mutableStateOf(callOnLockScreen.get()) }
    SectionView(stringResource(MR.strings.settings_section_title_settings)) {
      SectionItemView(editIceServers) { Text(stringResource(MR.strings.webrtc_ice_servers)) }

      val enabled = remember { mutableStateOf(true) }
      LockscreenOpts(lockCallState, enabled, onSelected = { callOnLockScreen.set(it); lockCallState.value = it })
      SettingsPreferenceItem(null, stringResource(MR.strings.always_use_relay), webrtcPolicyRelay)
    }
    SectionTextFooter(
      if (remember { webrtcPolicyRelay.state }.value) {
        generalGetString(MR.strings.relay_server_protects_ip)
      } else {
        generalGetString(MR.strings.relay_server_if_necessary)
      }
    )
    SectionBottomSpacer()
  }
}

@Composable
private fun LockscreenOpts(lockscreenOpts: State<CallOnLockScreen>, enabled: State<Boolean>, onSelected: (CallOnLockScreen) -> Unit) {
  val values = remember {
    CallOnLockScreen.values().map {
      when (it) {
        CallOnLockScreen.DISABLE -> it to generalGetString(MR.strings.no_call_on_lock_screen)
        CallOnLockScreen.SHOW -> it to generalGetString(MR.strings.show_call_on_lock_screen)
        CallOnLockScreen.ACCEPT -> it to generalGetString(MR.strings.accept_call_on_lock_screen)
      }
    }
  }
  ExposedDropDownSettingRow(
    generalGetString(MR.strings.call_on_lock_screen),
    values,
    lockscreenOpts,
    icon = null,
    enabled = enabled,
    onSelected = onSelected
  )
}

@Composable
fun SharedPreferenceToggle(
  preference: SharedPreference<Boolean>,
  enabled: Boolean = true,
  onChange: ((Boolean) -> Unit)? = null,
) {
  DefaultSwitch(
    enabled = enabled,
    checked = remember { preference.state }.value,
    onCheckedChange = {
      preference.set(it)
      onChange?.invoke(it)
    },
  )
}

@Composable
fun SharedPreferenceToggleWithIcon(
  text: String,
  icon: Painter,
  stopped: Boolean = false,
  onClickInfo: () -> Unit,
  preference: SharedPreference<Boolean>,
  preferenceState: MutableState<Boolean>? = null
) {
  val prefState = preferenceState ?: remember { mutableStateOf(preference.get()) }
  Row(Modifier.fillMaxWidth(), verticalAlignment = Alignment.CenterVertically) {
    Text(text, Modifier.padding(end = 4.dp), color = if (stopped) MaterialTheme.colors.secondary else Color.Unspecified)
    Icon(
      icon,
      null,
      Modifier.clickable(onClick = onClickInfo),
      tint = MaterialTheme.colors.primary
    )
    Spacer(Modifier.fillMaxWidth().weight(1f))
    DefaultSwitch(
      checked = prefState.value,
      onCheckedChange = {
        preference.set(it)
        prefState.value = it
      },
      enabled = !stopped
    )
  }
}

@Composable
fun SharedPreferenceToggleWithIcon(
  text: String,
  icon: Painter,
  onClickInfo: () -> Unit,
  checked: Boolean,
  onCheckedChange: (Boolean) -> Unit,
) {
  Row(Modifier.fillMaxWidth(), verticalAlignment = Alignment.CenterVertically) {
    Text(text, Modifier.padding(end = 4.dp))
    Icon(
      icon,
      null,
      Modifier.clickable(onClick = onClickInfo),
      tint = MaterialTheme.colors.primary
    )
    Spacer(Modifier.fillMaxWidth().weight(1f))
    DefaultSwitch(
      checked = checked,
      onCheckedChange = onCheckedChange,
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
