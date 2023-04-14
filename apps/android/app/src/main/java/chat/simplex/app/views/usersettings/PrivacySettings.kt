package chat.simplex.app.views.usersettings

import SectionDivider
import SectionItemView
import SectionSpacer
import SectionTextFooter
import SectionView
import android.view.WindowManager
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Lock
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import androidx.fragment.app.FragmentActivity
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimplexGreen
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.helpers.DatabaseUtils.ksAppPassword
import chat.simplex.app.views.localauth.SetAppPasscodeView

enum class LAMode {
  SYSTEM,
  PASSCODE;

  val text: String
    get() = when (this) {
      SYSTEM -> generalGetString(R.string.la_mode_system)
      PASSCODE -> generalGetString(R.string.la_mode_passcode)
    }
}

@Composable
fun PrivacySettingsView(
  chatModel: ChatModel,
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  setPerformLA: (Boolean, FragmentActivity) -> Unit
) {
  Column(
    Modifier.fillMaxWidth(),
    horizontalAlignment = Alignment.Start
  ) {
    val simplexLinkMode = chatModel.controller.appPrefs.simplexLinkMode
    AppBarTitle(stringResource(R.string.your_privacy))
    SectionView(stringResource(R.string.settings_section_title_device)) {
      ChatLockItem(chatModel, showSettingsModal, setPerformLA)
      SectionDivider()
      val context = LocalContext.current
      SettingsPreferenceItem(Icons.Outlined.VisibilityOff, stringResource(R.string.protect_app_screen), chatModel.controller.appPrefs.privacyProtectScreen) { on ->
        if (on) {
          (context as? FragmentActivity)?.window?.setFlags(
            WindowManager.LayoutParams.FLAG_SECURE,
            WindowManager.LayoutParams.FLAG_SECURE
          )
        } else {
          (context as? FragmentActivity)?.window?.clearFlags(WindowManager.LayoutParams.FLAG_SECURE)
        }
      }
    }
    SectionSpacer()

    SectionView(stringResource(R.string.settings_section_title_chats)) {
      SettingsPreferenceItem(Icons.Outlined.Image, stringResource(R.string.auto_accept_images), chatModel.controller.appPrefs.privacyAcceptImages)
      SectionDivider()
      SettingsPreferenceItem(Icons.Outlined.TravelExplore, stringResource(R.string.send_link_previews), chatModel.controller.appPrefs.privacyLinkPreviews)
      SectionDivider()
      SectionItemView {
        SimpleXLinkOptions(chatModel.simplexLinkMode, onSelected = {
          simplexLinkMode.set(it)
          chatModel.simplexLinkMode.value = it
        })
      }
    }
    if (chatModel.simplexLinkMode.value == SimplexLinkMode.BROWSER) {
      SectionTextFooter(stringResource(R.string.simplex_link_mode_browser_warning))
    }
  }
}

@Composable
private fun SimpleXLinkOptions(simplexLinkModeState: State<SimplexLinkMode>, onSelected: (SimplexLinkMode) -> Unit) {
  val values = remember {
    SimplexLinkMode.values().map {
      when (it) {
        SimplexLinkMode.DESCRIPTION -> it to generalGetString(R.string.simplex_link_mode_description)
        SimplexLinkMode.FULL -> it to generalGetString(R.string.simplex_link_mode_full)
        SimplexLinkMode.BROWSER -> it to generalGetString(R.string.simplex_link_mode_browser)
      }
    }
  }
  ExposedDropDownSettingRow(
    generalGetString(R.string.simplex_link_mode),
    values,
    simplexLinkModeState,
    icon = null,
    enabled = remember { mutableStateOf(true) },
    onSelected = onSelected
  )
}

private val laDelays = listOf(10, 30, 60, 180, 0)

@Composable
fun SimplexLockView(
  chatModel: ChatModel,
  currentLAMode: SharedPreference<LAMode>,
  setPerformLA: (Boolean, FragmentActivity) -> Unit
) {
  val performLA = remember { chatModel.performLA }
  val laMode = remember { chatModel.controller.appPrefs.laMode.state }
  val laLockDelay = remember { chatModel.controller.appPrefs.laLockDelay }
  val showChangePasscode = remember { derivedStateOf { performLA.value && currentLAMode.state.value == LAMode.PASSCODE } }
  val activity = LocalContext.current as FragmentActivity

  fun resetLAEnabled(onOff: Boolean) {
    chatModel.controller.appPrefs.performLA.set(onOff)
    chatModel.performLA.value = onOff
  }

  fun disableUnavailableLA() {
    resetLAEnabled(false)
    currentLAMode.set(LAMode.SYSTEM)
    laUnavailableInstructionAlert()
  }

  fun toggleLAMode(toLAMode: LAMode) {
    authenticate(
      if (toLAMode == LAMode.SYSTEM) {
        generalGetString(R.string.la_enter_app_passcode)
      } else {
        generalGetString(R.string.chat_lock)
      },
      generalGetString(R.string.change_lock_mode), activity
    ) { laResult ->
      when (laResult) {
        is LAResult.Error -> {
          laFailedAlert()
        }
        is LAResult.Failed -> { /* Can be called multiple times on every failure */ }
        LAResult.Success -> {
          when (toLAMode) {
            LAMode.SYSTEM -> {
              authenticate(generalGetString(R.string.auth_enable_simplex_lock), promptSubtitle = "", activity, toLAMode) { laResult ->
                when (laResult) {
                  LAResult.Success -> {
                    currentLAMode.set(toLAMode)
                    ksAppPassword.remove()
                    laTurnedOnAlert()
                  }
                  is LAResult.Unavailable, is LAResult.Error -> {
                    laFailedAlert()
                  }
                  is LAResult.Failed -> { /* Can be called multiple times on every failure */ }
                }
              }
            }
            LAMode.PASSCODE -> {
              ModalManager.shared.showCustomModal { close ->
                Surface(Modifier.fillMaxSize(), color = MaterialTheme.colors.background) {
                  SetAppPasscodeView(
                    submit = {
                      laLockDelay.set(30)
                      currentLAMode.set(toLAMode)
                      passcodeAlert(generalGetString(R.string.passcode_set))
                    },
                    cancel = {},
                    close
                  )
                }
              }
            }
          }
        }
        is LAResult.Unavailable -> disableUnavailableLA()
      }
    }
  }

  fun changeLAPassword() {
    authenticate(generalGetString(R.string.la_current_app_passcode), generalGetString(R.string.la_change_app_passcode), activity) { laResult ->
      when (laResult) {
        LAResult.Success -> {
          ModalManager.shared.showCustomModal { close ->
            Surface(Modifier.fillMaxSize(), color = MaterialTheme.colors.background) {
              SetAppPasscodeView(
                submit = {
                  passcodeAlert(generalGetString(R.string.passcode_changed))
                }, cancel = {
                  passcodeAlert(generalGetString(R.string.passcode_not_changed))
                }, close
              )
            }
          }
        }
        is LAResult.Error -> laFailedAlert()
        is LAResult.Failed -> {}
        is LAResult.Unavailable -> disableUnavailableLA()
      }
    }
  }

  Column(
    Modifier.fillMaxWidth(),
    horizontalAlignment = Alignment.Start
  ) {
    AppBarTitle(stringResource(R.string.chat_lock))
    SectionView {
      EnableLock(performLA) { performLAToggle ->
        performLA.value = performLAToggle
        chatModel.controller.appPrefs.laNoticeShown.set(true)
        if (performLAToggle) {
          when (currentLAMode.state.value) {
            LAMode.SYSTEM -> {
              setPerformLA(true, activity)
            }
            LAMode.PASSCODE -> {
              ModalManager.shared.showCustomModal { close ->
                Surface(Modifier.fillMaxSize(), color = MaterialTheme.colors.background) {
                  SetAppPasscodeView(
                    submit = {
                      laLockDelay.set(30)
                      chatModel.controller.appPrefs.performLA.set(true)
                      passcodeAlert(generalGetString(R.string.passcode_set))
                    },
                    cancel = {
                      resetLAEnabled(false)
                    }, close
                  )
                }
              }
            }
          }
        } else {
          setPerformLA(false, activity)
        }
      }
      SectionDivider()
      SectionItemView {
        LockModeSelector(laMode) { newLAMode ->
          if (laMode.value == newLAMode) return@LockModeSelector
          if (chatModel.controller.appPrefs.performLA.get()) {
            toggleLAMode(newLAMode)
          } else {
            currentLAMode.set(newLAMode)
          }
        }
      }

      if (performLA.value) {
        SectionDivider()
        SectionItemView {
          LockDelaySelector(remember { laLockDelay.state }) { laLockDelay.set(it) }
        }
        if (showChangePasscode.value && laMode.value == LAMode.PASSCODE) {
          SectionDivider()
          SectionItemView({ changeLAPassword() }) {
            Text(generalGetString(R.string.la_change_app_passcode))
          }
        }
      }
    }
  }
}

@Composable
private fun EnableLock(performLA: MutableState<Boolean>, onCheckedChange: (Boolean) -> Unit) {
  SectionItemView {
    Row(verticalAlignment = Alignment.CenterVertically) {
      Text(
        stringResource(R.string.enable_lock), Modifier
          .padding(end = 24.dp)
          .fillMaxWidth()
          .weight(1F)
      )
      Switch(
        checked = performLA.value,
        onCheckedChange = onCheckedChange,
        colors = SwitchDefaults.colors(
          checkedThumbColor = MaterialTheme.colors.primary,
          uncheckedThumbColor = HighOrLowlight
        )
      )
    }
  }
}

@Composable
private fun LockModeSelector(state: State<LAMode>, onSelected: (LAMode) -> Unit) {
  val values by remember { mutableStateOf(LAMode.values().map { it to it.text }) }
  ExposedDropDownSettingRow(
    generalGetString(R.string.lock_mode),
    values,
    state,
    icon = null,
    enabled = remember { mutableStateOf(true) },
    onSelected = onSelected
  )
}

@Composable
private fun LockDelaySelector(state: State<Int>, onSelected: (Int) -> Unit) {
  val delays = remember { if (laDelays.contains(state.value)) laDelays else listOf(state.value) + laDelays }
  val values by remember { mutableStateOf(delays.map { it to laDelayText(it) }) }
  ExposedDropDownSettingRow(
    generalGetString(R.string.lock_after),
    values,
    state,
    icon = null,
    enabled = remember { mutableStateOf(true) },
    onSelected = onSelected
  )
}

private fun laDelayText(t: Int): String {
  val m = t / 60
  val s = t % 60
  return if (t == 0) {
    generalGetString(R.string.la_immediately)
  } else if (m == 0 || s != 0) {
    // there are no options where both minutes and seconds are needed
    generalGetString(R.string.la_seconds).format(s)
  } else {
    generalGetString(R.string.la_minutes).format(m)
  }
}

private fun passcodeAlert(title: String) {
  AlertManager.shared.showAlertMsg(
    title = title,
    text = generalGetString(R.string.la_please_remember_to_store_password)
  )
}
