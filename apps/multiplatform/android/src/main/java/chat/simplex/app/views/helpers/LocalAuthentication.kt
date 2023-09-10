package chat.simplex.app.views.helpers

import android.os.Build.VERSION.SDK_INT
import androidx.activity.compose.BackHandler
import androidx.biometric.BiometricManager
import androidx.biometric.BiometricManager.Authenticators.*
import androidx.biometric.BiometricPrompt
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Surface
import androidx.compose.ui.Modifier
import androidx.core.content.ContextCompat
import androidx.fragment.app.FragmentActivity
import chat.simplex.app.R
import chat.simplex.app.SimplexApp
import chat.simplex.app.views.helpers.DatabaseUtils.ksAppPassword
import chat.simplex.app.views.localauth.LocalAuthView
import chat.simplex.app.views.usersettings.LAMode
import chat.simplex.res.MR

sealed class LAResult {
  object Success: LAResult()
  class Error(val errString: CharSequence): LAResult()
  class Failed(val errString: CharSequence? = null): LAResult()
  class Unavailable(val errString: CharSequence? = null): LAResult()
}

data class LocalAuthRequest (
  val title: String?,
  val reason: String,
  val password: String,
  val selfDestruct: Boolean,
  val completed: (LAResult) -> Unit
) {
  companion object {
    val sample = LocalAuthRequest(generalGetString(MR.strings.la_enter_app_passcode), generalGetString(MR.strings.la_authenticate), "", selfDestruct = false) { }
  }
}

fun authenticate(
  promptTitle: String,
  promptSubtitle: String,
  selfDestruct: Boolean = false,
  usingLAMode: LAMode = SimplexApp.context.chatModel.controller.appPrefs.laMode.get(),
  completed: (LAResult) -> Unit
) {
  val activity = SimplexApp.context.mainActivity.get() ?: return completed(LAResult.Error(""))
  when (usingLAMode) {
    LAMode.SYSTEM -> when {
      SDK_INT in 28..29 ->
        // KeyguardManager.isDeviceSecure()? https://developer.android.com/training/sign-in/biometric-auth#declare-supported-authentication-types
        authenticateWithBiometricManager(promptTitle, promptSubtitle, activity, completed, BIOMETRIC_WEAK or DEVICE_CREDENTIAL)
      SDK_INT > 29 ->
        authenticateWithBiometricManager(promptTitle, promptSubtitle, activity, completed, BIOMETRIC_STRONG or DEVICE_CREDENTIAL)
      else -> completed(LAResult.Unavailable())
    }
    LAMode.PASSCODE -> {
      val password = ksAppPassword.get() ?: return completed(LAResult.Unavailable(generalGetString(MR.strings.la_no_app_password)))
      ModalManager.shared.showPasscodeCustomModal { close ->
        BackHandler {
          close()
          completed(LAResult.Error(generalGetString(MR.strings.authentication_cancelled)))
        }
        Surface(Modifier.fillMaxSize(), color = MaterialTheme.colors.background) {
          LocalAuthView(SimplexApp.context.chatModel, LocalAuthRequest(promptTitle, promptSubtitle, password, selfDestruct && SimplexApp.context.chatModel.controller.appPrefs.selfDestruct.get()) {
            close()
            completed(it)
          })
        }
      }
    }
  }
}

private fun authenticateWithBiometricManager(
  promptTitle: String,
  promptSubtitle: String,
  activity: FragmentActivity,
  completed: (LAResult) -> Unit,
  authenticators: Int
) {
  val biometricManager = BiometricManager.from(activity)
  when (biometricManager.canAuthenticate(authenticators)) {
    BiometricManager.BIOMETRIC_SUCCESS -> {
      val executor = ContextCompat.getMainExecutor(activity)
      val biometricPrompt = BiometricPrompt(
        activity,
        executor,
        object: BiometricPrompt.AuthenticationCallback() {
          override fun onAuthenticationError(
            errorCode: Int,
            errString: CharSequence
          ) {
            super.onAuthenticationError(errorCode, errString)
            completed(LAResult.Error(errString))
          }

          override fun onAuthenticationSucceeded(
            result: BiometricPrompt.AuthenticationResult
          ) {
            super.onAuthenticationSucceeded(result)
            completed(LAResult.Success)
          }

          override fun onAuthenticationFailed() {
            super.onAuthenticationFailed()
            completed(LAResult.Failed())
          }
        }
      )
      val promptInfo = BiometricPrompt.PromptInfo.Builder()
        .setTitle(promptTitle)
        .setSubtitle(promptSubtitle)
        .setAllowedAuthenticators(authenticators)
        .setConfirmationRequired(false)
        .build()
      biometricPrompt.authenticate(promptInfo)
    }
    else -> completed(LAResult.Unavailable())
  }
}

fun laTurnedOnAlert() = AlertManager.shared.showAlertMsg(
  generalGetString(MR.strings.auth_simplex_lock_turned_on),
  generalGetString(MR.strings.auth_you_will_be_required_to_authenticate_when_you_start_or_resume)
)

fun laPasscodeNotSetAlert() = AlertManager.shared.showAlertMsg(
  generalGetString(MR.strings.lock_not_enabled),
  generalGetString(MR.strings.you_can_turn_on_lock)
)

fun laFailedAlert() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.la_auth_failed),
    text = generalGetString(MR.strings.la_could_not_be_verified)
  )
}

fun laUnavailableInstructionAlert() = AlertManager.shared.showAlertMsg(
  generalGetString(MR.strings.auth_unavailable),
  generalGetString(MR.strings.auth_device_authentication_is_not_enabled_you_can_turn_on_in_settings_once_enabled)
)

fun laUnavailableTurningOffAlert() = AlertManager.shared.showAlertMsg(
  generalGetString(MR.strings.auth_unavailable),
  generalGetString(MR.strings.auth_device_authentication_is_disabled_turning_off)
)
