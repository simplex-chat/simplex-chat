package chat.simplex.app.views.usersettings

import SectionCustomFooter
import SectionDivider
import SectionItemView
import SectionItemWithValue
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
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.stringResource
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*

@Composable
fun ContactPreferencesView(
  m: ChatModel,
  user: User,
  contact: Contact,
) {
  var featuresAllowed by remember { mutableStateOf(contactUserPrefsToFeaturesAllowed(contact.mergedPreferences)) }
  var currentFeaturesAllowed by remember { mutableStateOf(featuresAllowed) }
  ContactPreferencesLayout(
    featuresAllowed,
    currentFeaturesAllowed,
    user,
    contact,
    applyPrefs = { prefs ->
      featuresAllowed = prefs
    },
    reset = {
      featuresAllowed = currentFeaturesAllowed
    },
    savePrefs = {
      withApi {
        val prefs = contactFeaturesAllowedToPrefs(featuresAllowed)
        val toContact = m.controller.apiSetContactPrefs(contact.contactId, prefs)
        if (toContact != null) {
          m.updateContact(toContact)
          currentFeaturesAllowed = featuresAllowed
        }
      }
    },
  )
}

@Composable
private fun ContactPreferencesLayout(
  featuresAllowed: ContactFeaturesAllowed,
  currentFeaturesAllowed: ContactFeaturesAllowed,
  user: User,
  contact: Contact,
  applyPrefs: (ContactFeaturesAllowed) -> Unit,
  reset: () -> Unit,
  savePrefs: () -> Unit,
) {
  Column {
    Column(
      Modifier
        .weight(1f)
        .fillMaxWidth()
        .verticalScroll(rememberScrollState())
        .padding(bottom = DEFAULT_PADDING),
      horizontalAlignment = Alignment.Start,
    ) {
      AppBarTitle(stringResource(R.string.contact_preferences))
      val allowFullDeletion: MutableState<ContactFeatureAllowed> = remember(featuresAllowed) { mutableStateOf(featuresAllowed.fullDelete) }
      FeatureSection(Feature.FullDelete, user.fullPreferences.fullDelete.allow, contact.mergedPreferences.fullDelete, allowFullDeletion) {
        applyPrefs(featuresAllowed.copy(fullDelete = it))
      }

      SectionSpacer()
      val allowVoice: MutableState<ContactFeatureAllowed> = remember(featuresAllowed) { mutableStateOf(featuresAllowed.voice) }
      FeatureSection(Feature.Voice, user.fullPreferences.voice.allow, contact.mergedPreferences.voice, allowVoice) {
        applyPrefs(featuresAllowed.copy(voice = it))
      }
    }

    SectionCustomFooter(PaddingValues(DEFAULT_PADDING)) {
      ButtonsFooter(
        reset = reset,
        save = savePrefs,
        disabled = featuresAllowed == currentFeaturesAllowed
      )
    }
  }
}

@Composable
private fun FeatureSection(
  feature: Feature,
  userDefault: FeatureAllowed,
  pref: ContactUserPreference,
  allowFeature: State<ContactFeatureAllowed>,
  onSelected: (ContactFeatureAllowed) -> Unit
) {
  val enabled = FeatureEnabled.enabled(
    user = ChatPreference(allow = allowFeature.value.allowed),
    contact = pref.contactPreference
  )

  SectionView(
    feature.text().uppercase(),
    icon = feature.icon(),
    iconTint = if (enabled.forUser) Color.Green else if (enabled.forContact) Color.Yellow else Color.Red,
    leadingIcon = true,
  ) {
    SectionItemView {
      ExposedDropDownSettingRow(
        generalGetString(R.string.chat_preferences_you_allow),
        ContactFeatureAllowed.values(userDefault).map { it to it.text },
        allowFeature,
        icon = null,
        onSelected = onSelected
      )
    }
    SectionDivider()
    SectionItemWithValue(
      generalGetString(R.string.chat_preferences_contact_allows),
      remember { mutableStateOf(pref.contactPreference.allow) },
      listOf(ValueTitleDesc(pref.contactPreference.allow, pref.contactPreference.allow.text, "")),
      icon = null,
      enabled = remember { mutableStateOf(true) },
      onSelected = {}
    )
  }
  SectionTextFooter(feature.enabledDescription(enabled))
}

@Composable
private fun ButtonsFooter(reset: () -> Unit, save: () -> Unit, disabled: Boolean) {
  Row(
    Modifier.fillMaxWidth(),
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    FooterButton(Icons.Outlined.Replay, stringResource(R.string.reset_verb), reset, disabled)
    FooterButton(Icons.Outlined.Check, stringResource(R.string.save_and_notify_contact), save, disabled)
  }
}
