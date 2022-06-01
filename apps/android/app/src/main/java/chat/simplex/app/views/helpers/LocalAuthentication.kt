package chat.simplex.app.views.helpers

import android.content.Context
import android.os.Build.VERSION.SDK_INT
import android.widget.Toast
import androidx.biometric.BiometricManager
import androidx.biometric.BiometricManager.Authenticators.*
import androidx.biometric.BiometricPrompt
import androidx.core.content.ContextCompat
import androidx.fragment.app.FragmentActivity
import chat.simplex.app.R

sealed class LAResult {
  object Success: LAResult()
  class Error(val errString: CharSequence): LAResult()
  object Failed: LAResult()
  object Unavailable: LAResult()
}

fun authenticate(
  promptTitle: String,
  promptSubtitle: String,
  activity: FragmentActivity,
  completed: (LAResult) -> Unit
) {
  when {
    SDK_INT in 28..29 ->
      // KeyguardManager.isDeviceSecure()? https://developer.android.com/training/sign-in/biometric-auth#declare-supported-authentication-types
      authenticateWithBiometricManager(promptTitle, promptSubtitle, activity, completed, BIOMETRIC_WEAK or DEVICE_CREDENTIAL)
    SDK_INT > 29 ->
      authenticateWithBiometricManager(promptTitle, promptSubtitle, activity, completed, BIOMETRIC_STRONG or DEVICE_CREDENTIAL)
    else ->
      completed(LAResult.Unavailable)
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
            completed(LAResult.Failed)
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
    else -> {
      completed(LAResult.Unavailable)
    }
  }
}

fun laTurnedOnAlert() = AlertManager.shared.showAlertMsg(
  generalGetString(R.string.auth_simplex_lock_turned_on),
  generalGetString(R.string.auth_you_will_be_required_to_authenticate_when_you_start_or_resume)
)

fun laErrorToast(context: Context, errString: CharSequence) = Toast.makeText(
  context,
  if (errString.isNotEmpty()) String.format(generalGetString(R.string.auth_error_w_desc), errString) else generalGetString(R.string.auth_error),
  Toast.LENGTH_SHORT
).show()

fun laFailedToast(context: Context) = Toast.makeText(
  context,
  generalGetString(R.string.auth_failed),
  Toast.LENGTH_SHORT
).show()

fun laUnavailableInstructionAlert() = AlertManager.shared.showAlertMsg(
  generalGetString(R.string.auth_unavailable),
  generalGetString(R.string.auth_device_authentication_is_not_enabled_you_can_turn_on_in_settings_once_enabled)
)

fun laUnavailableTurningOffAlert() = AlertManager.shared.showAlertMsg(
  generalGetString(R.string.auth_unavailable),
  generalGetString(R.string.auth_device_authentication_is_disabled_turning_off)
)
