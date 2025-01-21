package chat.simplex.common.views.helpers

import android.os.Build.VERSION.SDK_INT
import androidx.biometric.BiometricManager
import androidx.biometric.BiometricManager.Authenticators.*
import androidx.biometric.BiometricPrompt
import androidx.core.content.ContextCompat
import androidx.fragment.app.FragmentActivity
import chat.simplex.common.platform.mainActivity
import chat.simplex.common.views.usersettings.LAMode

actual fun authenticate(
  promptTitle: String,
  promptSubtitle: String,
  selfDestruct: Boolean,
  usingLAMode: LAMode,
  oneTime: Boolean,
  completed: (LAResult) -> Unit
) {
  val activity = mainActivity.get() ?: return completed(LAResult.Error(""))
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
      authenticateWithPasscode(promptTitle, promptSubtitle, selfDestruct, oneTime, completed)
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
