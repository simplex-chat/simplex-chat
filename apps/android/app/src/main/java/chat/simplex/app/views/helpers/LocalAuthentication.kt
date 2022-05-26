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
  object Failed: LAResult()
  object Unavailable: LAResult()
}

fun authenticationAvailable(activity: FragmentActivity): Boolean {
  val biometricManager = BiometricManager.from(activity)
  return when (biometricManager.canAuthenticate(BIOMETRIC_STRONG or DEVICE_CREDENTIAL)) {
    BiometricManager.BIOMETRIC_SUCCESS -> true
    else -> false
  }
}

fun authenticate(
  promptTitle: String,
  promptSubtitle: String,
  activity: FragmentActivity,
  context: Context,
  onLAResult: (laResult: LAResult) -> Unit
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
            Toast.makeText(
              context,
              if (errString.isNotEmpty()) String.format(generalGetString(R.string.auth_error_w_desc), errString) else generalGetString(R.string.auth_error),
              Toast.LENGTH_SHORT
            ).show()
            onLAResult(LAResult.Failed)
          }

          override fun onAuthenticationSucceeded(
            result: BiometricPrompt.AuthenticationResult
          ) {
            super.onAuthenticationSucceeded(result)
            onLAResult(LAResult.Success)
          }

          override fun onAuthenticationFailed() {
            super.onAuthenticationFailed()
            Toast.makeText(
              context,
              generalGetString(R.string.auth_failed),
              Toast.LENGTH_SHORT
            ).show()
            onLAResult(LAResult.Failed)
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
      AlertManager.shared.showAlertMsg(
        generalGetString(R.string.auth_unavailable),
        generalGetString(R.string.auth_unavailable_desc)
      )
      onLAResult(LAResult.Unavailable)
    }
  }
}

fun laTurnedOnAlert() = AlertManager.shared.showAlertMsg(
  generalGetString(R.string.auth_turned_on),
  generalGetString(R.string.auth_turned_on_desc)
)