package chat.simplex.app.views.usersettings

import SectionItemView
import SectionItemWithValue
import SectionSpacer
import SectionTextFooter
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.views.helpers.*

@Composable
fun GroupPreferencesView(m: ChatModel, groupInfo: GroupInfo) {
  var preferences by remember { mutableStateOf(groupInfo.fullGroupPreferences) }
  var currentPreferences by remember { mutableStateOf(preferences) }
  GroupPreferencesLayout(
    preferences,
    currentPreferences,
    groupInfo,
    applyPrefs = { prefs ->
      preferences = prefs
    },
    reset = {
      preferences = currentPreferences
    },
    savePrefs = {
      withApi {
        val gp = groupInfo.groupProfile.copy(groupPreferences = preferences.toGroupPreferences())
        val gInfo = m.controller.apiUpdateGroup(groupInfo.groupId, gp)
        if (gInfo != null) {
          m.updateGroup(gInfo)
          currentPreferences = preferences
        }
      }
    },
  )
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
    val allowFullDeletion = remember(preferences) { mutableStateOf(preferences.fullDelete.enable) }
    FeatureSection(Feature.FullDelete, allowFullDeletion, groupInfo) {
      applyPrefs(preferences.copy(fullDelete = GroupPreference(enable = it)))
    }

    SectionSpacer()
    val allowVoice = remember(preferences) { mutableStateOf(preferences.voice.enable) }
    FeatureSection(Feature.Voice, allowVoice, groupInfo) {
      applyPrefs(preferences.copy(voice = GroupPreference(enable = it)))
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
private fun FeatureSection(feature: Feature, enableFeature: State<GroupFeatureEnabled>, groupInfo: GroupInfo, onSelected: (GroupFeatureEnabled) -> Unit) {
  SectionView {
    if (groupInfo.canEdit) {
      SectionItemView {
        ExposedDropDownSettingRow(
          feature.text(),
          GroupFeatureEnabled.values().map { it to it.text },
          enableFeature,
          icon = feature.icon(),
          onSelected = onSelected
        )
      }
    } else {
      SectionItemWithValue(
        feature.text(),
        remember { mutableStateOf(enableFeature.value) },
        listOf(ValueTitleDesc(enableFeature.value, enableFeature.value.text, "")),
        icon = null,
        enabled = remember { mutableStateOf(true) },
        onSelected = {}
      )
    }
  }
  SectionTextFooter(feature.enableGroupPrefDescription(enableFeature.value, groupInfo.canEdit))
}

@Composable
private fun ResetSaveButtons(reset: () -> Unit, save: () -> Unit, disabled: Boolean) {
  SectionView {
    SectionItemView(reset) {
      Text(stringResource(R.string.reset_verb), color = if (disabled) HighOrLowlight else MaterialTheme.colors.primary)
    }
    SectionItemView(save) {
      Text(stringResource(R.string.save_and_notify_group_members), color = if (disabled) HighOrLowlight else MaterialTheme.colors.primary)
    }
  }
}
