package chat.simplex.app.views.chat.group

import InfoRow
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
fun GroupPreferencesView(m: ChatModel, chatId: String, close: () -> Unit,) {
  val groupInfo = remember { derivedStateOf { (m.getChat(chatId)?.chatInfo as? ChatInfo.Group)?.groupInfo } }
  val gInfo = groupInfo.value ?: return
  var preferences by rememberSaveable(gInfo, stateSaver = serializableSaver()) { mutableStateOf(gInfo.fullGroupPreferences) }
  var currentPreferences by rememberSaveable(gInfo, stateSaver = serializableSaver()) { mutableStateOf(preferences) }

  fun savePrefs(afterSave: () -> Unit = {}) {
    withApi {
      val gp = gInfo.groupProfile.copy(groupPreferences = preferences.toGroupPreferences())
      val gInfo = m.controller.apiUpdateGroup(gInfo.groupId, gp)
      if (gInfo != null) {
        m.updateGroup(gInfo)
        currentPreferences = preferences
      }
      afterSave()
    }
  }
  ModalView(
    close = {
      if (preferences == currentPreferences) close()
      else showUnsavedChangesAlert({ savePrefs(close) }, close)
    },
    background = if (isInDarkTheme()) MaterialTheme.colors.background else SettingsBackgroundLight
  ) {
    GroupPreferencesLayout(
      preferences,
      currentPreferences,
      gInfo,
      applyPrefs = { prefs ->
        preferences = prefs
      },
      reset = {
        preferences = currentPreferences
      },
      savePrefs = ::savePrefs,
    )
  }
}

@Composable
private fun GroupPreferencesLayout(
  preferences: FullGroupPreferences,
  currentPreferences: FullGroupPreferences,
  groupInfo: GroupInfo,
  applyPrefs: (FullGroupPreferences) -> Unit,
  reset: () -> Unit,
  savePrefs: () -> Unit,
) {
  Column(
    Modifier.fillMaxWidth().verticalScroll(rememberScrollState()),
    horizontalAlignment = Alignment.Start,
  ) {
    AppBarTitle(stringResource(R.string.group_preferences))
    val allowDirectMessages = remember(preferences) { mutableStateOf(preferences.directMessages.enable) }
    FeatureSection(GroupFeature.DirectMessages, allowDirectMessages, groupInfo) {
      applyPrefs(preferences.copy(directMessages = GroupPreference(enable = it)))
    }
    SectionSpacer()
    val allowFullDeletion = remember(preferences) { mutableStateOf(preferences.fullDelete.enable) }
    FeatureSection(GroupFeature.FullDelete, allowFullDeletion, groupInfo) {
      applyPrefs(preferences.copy(fullDelete = GroupPreference(enable = it)))
    }
    SectionSpacer()
    val allowVoice = remember(preferences) { mutableStateOf(preferences.voice.enable) }
    FeatureSection(GroupFeature.Voice, allowVoice, groupInfo) {
      applyPrefs(preferences.copy(voice = GroupPreference(enable = it)))
    }
    if (groupInfo.canEdit) {
      SectionSpacer()
      ResetSaveButtons(
        reset = reset,
        save = savePrefs,
        disabled = preferences == currentPreferences
      )
    }
  }
}

@Composable
private fun FeatureSection(feature: GroupFeature, enableFeature: State<GroupFeatureEnabled>, groupInfo: GroupInfo, onSelected: (GroupFeatureEnabled) -> Unit) {
  SectionView {
    val on = enableFeature.value == GroupFeatureEnabled.ON
    val icon = if (on) feature.iconFilled else feature.icon
    val iconTint = if (on) SimplexGreen else HighOrLowlight
    if (groupInfo.canEdit) {
      SectionItemView {
        ExposedDropDownSettingRow(
          feature.text,
          GroupFeatureEnabled.values().map { it to it.text },
          enableFeature,
          icon = icon,
          iconTint = iconTint,
          onSelected = onSelected
        )
      }
    } else {
      InfoRow(
        feature.text,
        enableFeature.value.text,
        icon = icon,
        iconTint = iconTint,
      )
    }
  }
  SectionTextFooter(feature.enableDescription(enableFeature.value, groupInfo.canEdit))
}

@Composable
private fun ResetSaveButtons(reset: () -> Unit, save: () -> Unit, disabled: Boolean) {
  SectionView {
    SectionItemView(reset, disabled = disabled) {
      Text(stringResource(R.string.reset_verb), color = if (disabled) HighOrLowlight else MaterialTheme.colors.primary)
    }
    SectionDivider()
    SectionItemView(save, disabled = disabled) {
      Text(stringResource(R.string.save_and_notify_group_members), color = if (disabled) HighOrLowlight else MaterialTheme.colors.primary)
    }
  }
}

private fun showUnsavedChangesAlert(save: () -> Unit, revert: () -> Unit) {
  AlertManager.shared.showAlertDialogStacked(
    title = generalGetString(R.string.save_preferences_question),
    confirmText = generalGetString(R.string.save_and_notify_group_members),
    dismissText = generalGetString(R.string.exit_without_saving),
    onConfirm = save,
    onDismiss = revert,
  )
}
