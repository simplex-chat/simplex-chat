package chat.simplex.app.views.usersettings

import SectionDivider
import SectionItemView
import SectionSpacer
import SectionTextFooter
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*

@Composable
fun PreferencesView(m: ChatModel, user: User, close: () -> Unit,) {
  var preferences by rememberSaveable(stateSaver = serializableSaver()) { mutableStateOf(user.fullPreferences) }
  var currentPreferences by rememberSaveable(stateSaver = serializableSaver()) { mutableStateOf(preferences) }

  fun savePrefs(afterSave: () -> Unit = {}) {
    withApi {
      val newProfile = user.profile.toProfile().copy(preferences = preferences.toPreferences())
      val updatedProfile = m.controller.apiUpdateProfile(newProfile)
      if (updatedProfile != null) {
        val updatedUser = user.copy(
          profile = updatedProfile.toLocalProfile(user.profile.profileId),
          fullPreferences = preferences
        )
        currentPreferences = preferences
        m.currentUser.value = updatedUser
      }
      afterSave()
    }
  }
  ModalView(
    close = {
      if (preferences == currentPreferences) close()
      else  showUnsavedChangesAlert({ savePrefs(close) }, close)
    },
    background = if (isInDarkTheme()) MaterialTheme.colors.background else SettingsBackgroundLight
  ) {
    PreferencesLayout(
      preferences,
      currentPreferences,
      applyPrefs = { preferences = it },
      reset = { preferences = currentPreferences },
      savePrefs = ::savePrefs,
    )
  }
}

@Composable
private fun PreferencesLayout(
  preferences: FullChatPreferences,
  currentPreferences: FullChatPreferences,
  applyPrefs: (FullChatPreferences) -> Unit,
  reset: () -> Unit,
  savePrefs: () -> Unit,
) {
  Column(
    Modifier.fillMaxWidth().verticalScroll(rememberScrollState()),
    horizontalAlignment = Alignment.Start,
  ) {
    AppBarTitle(stringResource(R.string.your_preferences))
    val allowFullDeletion = remember(preferences) { mutableStateOf(preferences.fullDelete.allow) }
    FeatureSection(ChatFeature.FullDelete, allowFullDeletion) {
      applyPrefs(preferences.copy(fullDelete = ChatPreference(allow = it)))
    }
    SectionSpacer()
    val allowVoice = remember(preferences) { mutableStateOf(preferences.voice.allow) }
    FeatureSection(ChatFeature.Voice, allowVoice) {
      applyPrefs(preferences.copy(voice = ChatPreference(allow = it)))
    }
    SectionSpacer()
    ResetSaveButtons(
      reset = reset,
      save = savePrefs,
      disabled = preferences == currentPreferences
    )
  }
}

@Composable
private fun FeatureSection(feature: ChatFeature, allowFeature: State<FeatureAllowed>, onSelected: (FeatureAllowed) -> Unit) {
  SectionView {
    SectionItemView {
      ExposedDropDownSettingRow(
        feature.text,
        FeatureAllowed.values().map { it to it.text },
        allowFeature,
        icon = feature.icon,
        onSelected = onSelected
      )
    }
  }
  SectionTextFooter(feature.allowDescription(allowFeature.value))
}

@Composable
private fun ResetSaveButtons(reset: () -> Unit, save: () -> Unit, disabled: Boolean) {
  SectionView {
    SectionItemView(reset, disabled = disabled) {
      Text(stringResource(R.string.reset_verb), color = if (disabled) HighOrLowlight else MaterialTheme.colors.primary)
    }
    SectionDivider()
    SectionItemView(save, disabled = disabled) {
      Text(stringResource(R.string.save_and_notify_contacts), color = if (disabled) HighOrLowlight else MaterialTheme.colors.primary)
    }
  }
}

private fun showUnsavedChangesAlert(save: () -> Unit, revert: () -> Unit) {
  AlertManager.shared.showAlertDialogStacked(
    title = generalGetString(R.string.save_preferences_question),
    confirmText = generalGetString(R.string.save_and_notify_contacts),
    dismissText = generalGetString(R.string.exit_without_saving),
    onConfirm = save,
    onDismiss = revert,
  )
}
