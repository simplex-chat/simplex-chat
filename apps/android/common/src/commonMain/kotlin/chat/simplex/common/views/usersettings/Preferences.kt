package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.stringResource
import com.icerockdev.library.MR
import chat.simplex.common.model.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.User

@Composable
fun PreferencesView(m: ChatModel, user: User, close: () -> Unit,) {
  var preferences by rememberSaveable(stateSaver = serializableSaver()) { mutableStateOf(user.fullPreferences) }
  var currentPreferences by rememberSaveable(stateSaver = serializableSaver()) { mutableStateOf(preferences) }

  fun savePrefs(afterSave: () -> Unit = {}) {
    withApi {
      val newProfile = user.profile.toProfile().copy(preferences = preferences.toPreferences())
      val updatedProfile = m.controller.apiUpdateProfile(newProfile)
      if (updatedProfile != null) {
        m.updateCurrentUser(updatedProfile, preferences)
        currentPreferences = preferences
      }
      afterSave()
    }
  }
  ModalView(
    close = {
      if (preferences == currentPreferences) close()
      else  showUnsavedChangesAlert({ savePrefs(close) }, close)
    },
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
  ) {
    AppBarTitle(stringResource(MR.strings.your_preferences))
    val timedMessages = remember(preferences) { mutableStateOf(preferences.timedMessages.allow) }
    TimedMessagesFeatureSection(timedMessages) {
      applyPrefs(preferences.copy(timedMessages = TimedMessagesPreference(allow = if (it) FeatureAllowed.YES else FeatureAllowed.NO)))
    }
    SectionDividerSpaced(true, maxBottomPadding = false)
    val allowFullDeletion = remember(preferences) { mutableStateOf(preferences.fullDelete.allow) }
    FeatureSection(ChatFeature.FullDelete, allowFullDeletion) {
      applyPrefs(preferences.copy(fullDelete = SimpleChatPreference(allow = it)))
    }
    SectionDividerSpaced(true, maxBottomPadding = false)
//    val allowReactions = remember(preferences) { mutableStateOf(preferences.reactions.allow) }
//    FeatureSection(ChatFeature.Reactions, allowReactions) {
//      applyPrefs(preferences.copy(reactions = SimpleChatPreference(allow = it)))
//    }
//    SectionDividerSpaced(true, maxBottomPadding = false)
    val allowVoice = remember(preferences) { mutableStateOf(preferences.voice.allow) }
    FeatureSection(ChatFeature.Voice, allowVoice) {
      applyPrefs(preferences.copy(voice = SimpleChatPreference(allow = it)))
    }
    SectionDividerSpaced(true, maxBottomPadding = false)
    val allowCalls = remember(preferences) { mutableStateOf(preferences.calls.allow) }
    FeatureSection(ChatFeature.Calls, allowCalls) {
      applyPrefs(preferences.copy(calls = SimpleChatPreference(allow = it)))
    }
    SectionDividerSpaced(maxTopPadding = true, maxBottomPadding = false)
    ResetSaveButtons(
      reset = reset,
      save = savePrefs,
      disabled = preferences == currentPreferences
    )
    SectionBottomSpacer()
  }
}

@Composable
private fun FeatureSection(feature: ChatFeature, allowFeature: State<FeatureAllowed>, onSelected: (FeatureAllowed) -> Unit) {
  SectionView {
    ExposedDropDownSettingRow(
      feature.text,
      FeatureAllowed.values().map { it to it.text },
      allowFeature,
      icon = feature.icon,
      onSelected = onSelected,
    )
  }
  SectionTextFooter(feature.allowDescription(allowFeature.value))
}

@Composable
private fun TimedMessagesFeatureSection(allowFeature: State<FeatureAllowed>, onSelected: (Boolean) -> Unit) {
  SectionView {
    PreferenceToggleWithIcon(
      ChatFeature.TimedMessages.text,
      ChatFeature.TimedMessages.icon,
      MaterialTheme.colors.secondary,
      allowFeature.value == FeatureAllowed.ALWAYS || allowFeature.value == FeatureAllowed.YES,
      extraPadding = false,
      onSelected
    )
  }
  SectionTextFooter(ChatFeature.TimedMessages.allowDescription(allowFeature.value))
}

@Composable
private fun ResetSaveButtons(reset: () -> Unit, save: () -> Unit, disabled: Boolean) {
  SectionView {
    SectionItemView(reset, disabled = disabled) {
      Text(stringResource(MR.strings.reset_verb), color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
    }
    SectionItemView(save, disabled = disabled) {
      Text(stringResource(MR.strings.save_and_notify_contacts), color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
    }
  }
}

private fun showUnsavedChangesAlert(save: () -> Unit, revert: () -> Unit) {
  AlertManager.shared.showAlertDialogStacked(
    title = generalGetString(MR.strings.save_preferences_question),
    confirmText = generalGetString(MR.strings.save_and_notify_contacts),
    dismissText = generalGetString(MR.strings.exit_without_saving),
    onConfirm = save,
    onDismiss = revert,
  )
}
