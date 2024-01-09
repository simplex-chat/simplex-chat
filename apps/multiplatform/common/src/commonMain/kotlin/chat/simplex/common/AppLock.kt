package chat.simplex.common

import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material.*
import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.Modifier
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.localauth.SetAppPasscodeView
import chat.simplex.common.views.usersettings.*
import chat.simplex.res.MR
import kotlinx.coroutines.*

object AppLock {
  /**
   * We don't want these values to be bound to Activity lifecycle since activities are changed often, for example, when a user
   * clicks on new message in notification. In this case savedInstanceState will be null (this prevents restoring the values)
   * See [SimplexService.onTaskRemoved] for another part of the logic which nullifies the values when app closed by the user
   * */
  val userAuthorized = mutableStateOf<Boolean?>(null)
  val enteredBackground = mutableStateOf<Long?>(null)

  // Remember result and show it after orientation change
  val laFailed = mutableStateOf(false)

  fun clearAuthState() {
    userAuthorized.value = null
    enteredBackground.value = null
  }

  fun showLANotice(laNoticeShown: SharedPreference<Boolean>) {
    Log.d(TAG, "showLANotice")
    if (!laNoticeShown.get()) {
      laNoticeShown.set(true)
      AlertManager.shared.showAlertDialog(
        title = generalGetString(MR.strings.la_notice_title_simplex_lock),
        text = generalGetString(MR.strings.la_notice_to_protect_your_information_turn_on_simplex_lock_you_will_be_prompted_to_complete_authentication_before_this_feature_is_enabled),
        confirmText = generalGetString(MR.strings.la_notice_turn_on),
        onConfirm = {
          laNoticeShown.set(true)
          withBGApi { // to remove this call, change ordering of onConfirm call in AlertManager
            if (appPlatform.isAndroid) {
              showChooseLAMode()
            } else {
              AlertManager.shared.hideAlert()
              setPasscode()
            }
          }
        },
        onDismiss = {
          AlertManager.shared.hideAlert()
        }
      )
    }
  }

  private fun showChooseLAMode() {
    Log.d(TAG, "showLANotice")
    AlertManager.shared.showAlertDialogStacked(
      title = generalGetString(MR.strings.la_lock_mode),
      text = null,
      confirmText = generalGetString(MR.strings.la_lock_mode_passcode),
      dismissText = generalGetString(MR.strings.la_lock_mode_system),
      onConfirm = {
        AlertManager.shared.hideAlert()
        setPasscode()
      },
      onDismiss = {
        AlertManager.shared.hideAlert()
        initialEnableLA()
      }
    )
  }

  private fun initialEnableLA() {
    val m = ChatModel
    val appPrefs = ChatController.appPrefs
    appPrefs.laMode.set(LAMode.default)
    authenticate(
      generalGetString(MR.strings.auth_enable_simplex_lock),
      generalGetString(MR.strings.auth_confirm_credential),
      completed = { laResult ->
        when (laResult) {
          LAResult.Success -> {
            m.performLA.value = true
            appPrefs.performLA.set(true)
            laTurnedOnAlert()
          }
          is LAResult.Failed -> { /* Can be called multiple times on every failure */ }
          is LAResult.Error -> {
            m.performLA.value = false
            appPrefs.performLA.set(false)
            laFailedAlert()
          }
          is LAResult.Unavailable -> {
            m.performLA.value = false
            appPrefs.performLA.set(false)
            m.showAdvertiseLAUnavailableAlert.value = true
          }
        }
      }
    )
  }

  private fun setPasscode() {
    val appPrefs = ChatController.appPrefs
    ModalManager.fullscreen.showCustomModal { close ->
      Surface(Modifier.fillMaxSize(), color = MaterialTheme.colors.background, contentColor = LocalContentColor.current) {
        SetAppPasscodeView(
          submit = {
            ChatModel.performLA.value = true
            appPrefs.performLA.set(true)
            appPrefs.laMode.set(LAMode.PASSCODE)
            laTurnedOnAlert()
          },
          cancel = {
            ChatModel.performLA.value = false
            appPrefs.performLA.set(false)
            laPasscodeNotSetAlert()
          },
          close = close
        )
      }
    }
  }

  fun setAuthState() {
    userAuthorized.value = !ChatController.appPrefs.performLA.get()
  }

  fun runAuthenticate() {
    val m = ChatModel
    setAuthState()
    if (userAuthorized.value == false) {
      // To make Main thread free in order to allow to Compose to show blank view that hiding content underneath of it faster on slow devices
      CoroutineScope(Dispatchers.Default).launch {
        delay(50)
        withContext(Dispatchers.Main) {
          authenticate(
            if (m.controller.appPrefs.laMode.get() == LAMode.SYSTEM)
              generalGetString(MR.strings.auth_unlock)
            else
              generalGetString(MR.strings.la_enter_app_passcode),
            if (m.controller.appPrefs.laMode.get() == LAMode.SYSTEM)
              generalGetString(MR.strings.auth_log_in_using_credential)
            else
              generalGetString(MR.strings.auth_unlock),
            selfDestruct = true,
            completed = { laResult ->
              when (laResult) {
                LAResult.Success ->
                  userAuthorized.value = true
                is LAResult.Failed -> { /* Can be called multiple times on every failure */ }
                is LAResult.Error -> {
                  laFailed.value = true
                  if (m.controller.appPrefs.laMode.get() == LAMode.PASSCODE) {
                    laFailedAlert()
                  }
                }
                is LAResult.Unavailable -> {
                  userAuthorized.value = true
                  m.performLA.value = false
                  m.controller.appPrefs.performLA.set(false)
                  laUnavailableTurningOffAlert()
                }
              }
            }
          )
        }
      }
    }
  }

  fun setPerformLA(on: Boolean) {
    ChatController.appPrefs.laNoticeShown.set(true)
    if (on) {
      enableLA()
    } else {
      disableLA()
    }
  }

  private fun enableLA() {
    val m = ChatModel
    authenticate(
      if (m.controller.appPrefs.laMode.get() == LAMode.SYSTEM)
        generalGetString(MR.strings.auth_enable_simplex_lock)
      else
        generalGetString(MR.strings.new_passcode),
      if (m.controller.appPrefs.laMode.get() == LAMode.SYSTEM)
        generalGetString(MR.strings.auth_confirm_credential)
      else
        "",
      completed = { laResult ->
        val prefPerformLA = m.controller.appPrefs.performLA
        when (laResult) {
          LAResult.Success -> {
            m.performLA.value = true
            prefPerformLA.set(true)
            laTurnedOnAlert()
          }
          is LAResult.Failed -> { /* Can be called multiple times on every failure */ }
          is LAResult.Error -> {
            m.performLA.value = false
            prefPerformLA.set(false)
            laFailedAlert()
          }
          is LAResult.Unavailable -> {
            m.performLA.value = false
            prefPerformLA.set(false)
            laUnavailableInstructionAlert()
          }
        }
      }
    )
  }

  private fun disableLA() {
    val m = ChatModel
    authenticate(
      if (m.controller.appPrefs.laMode.get() == LAMode.SYSTEM)
        generalGetString(MR.strings.auth_disable_simplex_lock)
      else
        generalGetString(MR.strings.la_enter_app_passcode),
      if (m.controller.appPrefs.laMode.get() == LAMode.SYSTEM)
        generalGetString(MR.strings.auth_confirm_credential)
      else
        generalGetString(MR.strings.auth_disable_simplex_lock),
      completed = { laResult ->
        val prefPerformLA = m.controller.appPrefs.performLA
        val selfDestructPref = m.controller.appPrefs.selfDestruct
        when (laResult) {
          LAResult.Success -> {
            m.performLA.value = false
            prefPerformLA.set(false)
            DatabaseUtils.ksAppPassword.remove()
            selfDestructPref.set(false)
            DatabaseUtils.ksSelfDestructPassword.remove()
          }
          is LAResult.Failed -> { /* Can be called multiple times on every failure */ }
          is LAResult.Error -> {
            m.performLA.value = true
            prefPerformLA.set(true)
            laFailedAlert()
          }
          is LAResult.Unavailable -> {
            m.performLA.value = false
            prefPerformLA.set(false)
            laUnavailableTurningOffAlert()
          }
        }
      }
    )
  }

  fun elapsedRealtime(): Long = System.nanoTime() / 1_000_000

  fun recheckAuthState() {
    val enteredBackgroundVal = enteredBackground.value
    val delay = ChatController.appPrefs.laLockDelay.get()
    if (enteredBackgroundVal == null || elapsedRealtime() - enteredBackgroundVal >= delay * 1000) {
      if (userAuthorized.value != false) {
        /** [runAuthenticate] will be called in [MainScreen] if needed. Making like this prevents double showing of passcode on start */
        setAuthState()
      } else if (!ChatModel.activeCallViewIsVisible.value) {
        runAuthenticate()
      }
    }
  }
  fun appWasHidden() {
    enteredBackground.value = elapsedRealtime()
  }
}
