package chat.simplex.app.views.helpers

import android.content.Context
import android.widget.Toast
import androidx.biometric.BiometricManager
import androidx.biometric.BiometricManager.Authenticators.BIOMETRIC_STRONG
import androidx.biometric.BiometricManager.Authenticators.DEVICE_CREDENTIAL
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
  val biometricManager = BiometricManager.from(activity)
  when (biometricManager.canAuthenticate(BIOMETRIC_STRONG or DEVICE_CREDENTIAL)) {
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
        .setAllowedAuthenticators(BIOMETRIC_STRONG or DEVICE_CREDENTIAL)
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
  generalGetString(R.string.auth_turned_on),
  generalGetString(R.string.auth_turned_on_desc)
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
  generalGetString(R.string.auth_unavailable_instruction_desc)
)

fun laUnavailableTurningOffAlert() = AlertManager.shared.showAlertMsg(
  generalGetString(R.string.auth_unavailable),
  generalGetString(R.string.auth_unavailable_turning_off_desc)
)
