package chat.simplex.common.views.localauth

import androidx.compose.runtime.Composable
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.saveable.rememberSaveable
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.views.database.deleteChatAsync
import chat.simplex.common.views.database.stopChatAsync
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.helpers.DatabaseUtils.ksSelfDestructPassword
import chat.simplex.common.views.helpers.DatabaseUtils.ksAppPassword
import chat.simplex.common.views.onboarding.OnboardingStage
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.Profile
import chat.simplex.common.platform.*
import chat.simplex.res.MR

@Composable
fun LocalAuthView(m: ChatModel, authRequest: LocalAuthRequest) {
  val passcode = rememberSaveable { mutableStateOf("") }
  PasscodeView(passcode, authRequest.title ?: stringResource(MR.strings.la_enter_app_passcode), authRequest.reason, stringResource(MR.strings.submit_passcode),
    submit = {
      val sdPassword = ksSelfDestructPassword.get()
      if (sdPassword == passcode.value && authRequest.selfDestruct) {
        deleteStorageAndRestart(m, sdPassword) { r ->
          authRequest.completed(r)
        }
      } else {
        val r: LAResult = if (passcode.value == authRequest.password) LAResult.Success else LAResult.Error(generalGetString(MR.strings.incorrect_passcode))
        authRequest.completed(r)
      }
    },
    cancel = {
      authRequest.completed(LAResult.Error(generalGetString(MR.strings.authentication_cancelled)))
    })
}

private fun deleteStorageAndRestart(m: ChatModel, password: String, completed: (LAResult) -> Unit) {
  withBGApi {
    try {
      stopChatAsync(m)
      deleteChatAsync(m)
      ksAppPassword.set(password)
      ksSelfDestructPassword.remove()
      ntfManager.cancelAllNotifications()
      val selfDestructPref = m.controller.appPrefs.selfDestruct
      val displayNamePref = m.controller.appPrefs.selfDestructDisplayName
      val displayName = displayNamePref.get()
      selfDestructPref.set(false)
      displayNamePref.set(null)
      m.chatDbChanged.value = true
      m.chatDbStatus.value = null
      try {
        initChatController(startChat = true)
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
      val createdUser = m.controller.apiCreateActiveUser(null, profile, pastTimestamp = true)
      m.currentUser.value = createdUser
      m.controller.appPrefs.onboardingStage.set(OnboardingStage.OnboardingComplete)
      if (createdUser != null) {
        m.controller.startChat(createdUser)
      }
      ModalManager.fullscreen.closeModals()
      AlertManager.shared.hideAllAlerts()
      AlertManager.privacySensitive.hideAllAlerts()
      completed(LAResult.Success)
    } catch (e: Exception) {
      completed(LAResult.Error(generalGetString(MR.strings.incorrect_passcode)))
    }
  }
}
