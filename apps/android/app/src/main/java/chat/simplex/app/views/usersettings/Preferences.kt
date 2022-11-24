package chat.simplex.app.views.usersettings

import SectionCustomFooter
import SectionItemView
import SectionSpacer
import SectionTextFooter
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*

@Composable
fun PreferencesView(m: ChatModel, user: User) {
  var preferences by remember { mutableStateOf(user.fullPreferences) }
  var currentPreferences by remember { mutableStateOf(preferences) }
  PreferencesLayout(
    preferences,
    currentPreferences,
    applyPrefs = { prefs ->
      preferences = prefs
    },
    reset = {
      preferences = currentPreferences
    },
    savePrefs = {
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
      }
    },
  )
}

@Composable
private fun PreferencesLayout(
  preferences: FullChatPreferences,
  currentPreferences: FullChatPreferences,
  applyPrefs: (FullChatPreferences) -> Unit,
  reset: () -> Unit,
  savePrefs: () -> Unit,
) {
  Column {
    Column(
      Modifier.weight(1f).fillMaxWidth().verticalScroll(rememberScrollState()),
      horizontalAlignment = Alignment.Start,
    ) {
      AppBarTitle(stringResource(R.string.your_preferences))
      val allowFullDeletion = remember(preferences) { mutableStateOf(preferences.fullDelete.allow) }
      FeatureSection(Feature.FullDelete, allowFullDeletion) {
        applyPrefs(preferences.copy(fullDelete = ChatPreference(allow = it)))
      }

      SectionSpacer()
      val allowVoice = remember(preferences) { mutableStateOf(preferences.voice.allow) }
      FeatureSection(Feature.Voice, allowVoice) {
        applyPrefs(preferences.copy(voice = ChatPreference(allow = it)))
      }
    }
    SectionCustomFooter(PaddingValues(DEFAULT_PADDING)) {
      ButtonsFooter(
        reset = reset,
        save = savePrefs,
        disabled = preferences == currentPreferences
      )
    }
  }
}

@Composable
private fun FeatureSection(feature: Feature, allowFeature: State<FeatureAllowed>, onSelected: (FeatureAllowed) -> Unit) {
  SectionView {
    SectionItemView {
      ExposedDropDownSettingRow(
        feature.text(), //generalGetString(R.string.chat_preferences_you_allow),
        FeatureAllowed.values().map { it to it.text },
        allowFeature,
        icon = feature.icon(),
        onSelected = onSelected
      )
    }
  }
  SectionTextFooter(feature.allowDescription(allowFeature.value))
}

@Composable
private fun ButtonsFooter(reset: () -> Unit, save: () -> Unit, disabled: Boolean) {
  Row(
    Modifier.fillMaxWidth(),
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    FooterButton(Icons.Outlined.Replay, stringResource(R.string.reset_verb), reset, disabled)
    FooterButton(Icons.Outlined.Check, stringResource(R.string.save_and_notify_contacts), save, disabled)
  }
}
