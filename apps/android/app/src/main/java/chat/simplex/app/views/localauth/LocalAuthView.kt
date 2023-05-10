package chat.simplex.app.views.localauth

import android.util.Log
import androidx.compose.runtime.Composable
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.res.stringResource
import chat.simplex.app.*
import chat.simplex.app.model.*
import chat.simplex.app.views.database.deleteChatAsync
import chat.simplex.app.views.database.stopChatAsync
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.helpers.DatabaseUtils.ksSelfDestructPassword
import chat.simplex.app.views.helpers.DatabaseUtils.ksAppPassword
import chat.simplex.app.views.onboarding.OnboardingStage
import kotlinx.coroutines.delay

@Composable
fun LocalAuthView(m: ChatModel, authRequest: LocalAuthRequest) {
  val passcode = rememberSaveable { mutableStateOf("") }
  PasscodeView(passcode, authRequest.title ?: stringResource(R.string.la_enter_app_passcode), authRequest.reason, stringResource(R.string.submit_passcode),
    submit = {
      val sdPassword = ksSelfDestructPassword.get()
      if (sdPassword == passcode.value && authRequest.selfDestruct) {
        deleteStorageAndRestart(m, sdPassword) { r ->
          authRequest.completed(r)
        }
      } else {
        val r: LAResult = if (passcode.value == authRequest.password) LAResult.Success else LAResult.Error(generalGetString(R.string.incorrect_passcode))
        authRequest.completed(r)
      }
    },
    cancel = {
      authRequest.completed(LAResult.Error(generalGetString(R.string.authentication_cancelled)))
    })
}

private fun deleteStorageAndRestart(m: ChatModel, password: String, completed: (LAResult) -> Unit) {
  withBGApi {
    try {
      stopChatAsync(m)
      deleteChatAsync(m)
      ksAppPassword.set(password)
      ksSelfDestructPassword.remove()
      m.controller.ntfManager.cancelAllNotifications()
      val selfDestructPref = m.controller.appPrefs.selfDestruct
      val displayNamePref = m.controller.appPrefs.selfDestructDisplayName
      val displayName = displayNamePref.get()
      selfDestructPref.set(false)
      displayNamePref.set(null)
      m.chatDbChanged.value = true
      m.chatDbStatus.value = null
      try {
        SimplexApp.context.initChatController(startChat = true)
      } catch (e: Exception) {
        Log.d(TAG, "initializeChat ${e.stackTraceToString()}")
      }
      m.chatDbChanged.value = false
      if (m.currentUser.value != null) {
        return@withBGApi
      }
      var profile: Profile? = null
      if (!displayName.isNullOrEmpty()) {
        profile = Profile(displayName = displayName, fullName = "")
      }
      val createdUser = m.controller.apiCreateActiveUser(profile, pastTimestamp = true)
      m.currentUser.value = createdUser
      m.controller.appPrefs.onboardingStage.set(OnboardingStage.OnboardingComplete)
      m.onboardingStage.value = OnboardingStage.OnboardingComplete
      if (createdUser != null) {
        m.controller.startChat(createdUser)
      }
      ModalManager.shared.closeModals()
      AlertManager.shared.hideAlert()
      completed(LAResult.Success)
    } catch (e: Exception) {
      completed(LAResult.Error(generalGetString(R.string.incorrect_passcode)))
    }
  }
}
