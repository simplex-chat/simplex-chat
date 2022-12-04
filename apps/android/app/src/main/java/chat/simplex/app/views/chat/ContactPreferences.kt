package chat.simplex.app.views.chat

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
  contactId: Long,
) {
  val contact = remember { derivedStateOf { (m.getContactChat(contactId)?.chatInfo as? ChatInfo.Direct)?.contact } }
  val ct = contact.value ?: return
  var featuresAllowed by remember(ct) { mutableStateOf(contactUserPrefsToFeaturesAllowed(ct.mergedPreferences)) }
  var currentFeaturesAllowed by remember(ct) { mutableStateOf(featuresAllowed) }
  ContactPreferencesLayout(
    featuresAllowed,
    currentFeaturesAllowed,
    user,
    ct,
    applyPrefs = { prefs ->
      featuresAllowed = prefs
    },
    reset = {
      featuresAllowed = currentFeaturesAllowed
    },
    savePrefs = {
      withApi {
        val prefs = contactFeaturesAllowedToPrefs(featuresAllowed)
        val toContact = m.controller.apiSetContactPrefs(ct.contactId, prefs)
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
  Column(
    Modifier
      .fillMaxWidth()
      .verticalScroll(rememberScrollState())
      .padding(bottom = DEFAULT_PADDING),
    horizontalAlignment = Alignment.Start,
  ) {
    AppBarTitle(stringResource(R.string.contact_preferences))
    val allowFullDeletion: MutableState<ContactFeatureAllowed> = remember(featuresAllowed) { mutableStateOf(featuresAllowed.fullDelete) }
    FeatureSection(ChatFeature.FullDelete, user.fullPreferences.fullDelete.allow, contact.mergedPreferences.fullDelete, allowFullDeletion) {
      applyPrefs(featuresAllowed.copy(fullDelete = it))
    }
    SectionSpacer()
    val allowVoice: MutableState<ContactFeatureAllowed> = remember(featuresAllowed) { mutableStateOf(featuresAllowed.voice) }
    FeatureSection(ChatFeature.Voice, user.fullPreferences.voice.allow, contact.mergedPreferences.voice, allowVoice) {
      applyPrefs(featuresAllowed.copy(voice = it))
    }
    SectionSpacer()
    ResetSaveButtons(
      reset = reset,
      save = savePrefs,
      disabled = featuresAllowed == currentFeaturesAllowed
    )
  }
}

@Composable
private fun FeatureSection(
  feature: ChatFeature,
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
    feature.text.uppercase(),
    icon = feature.iconFilled,
    iconTint = if (enabled.forUser) SimplexGreen else if (enabled.forContact) WarningYellow else Color.Red,
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
    InfoRow(
      generalGetString(R.string.chat_preferences_contact_allows),
      pref.contactPreference.allow.text
    )
  }
  SectionTextFooter(feature.enabledDescription(enabled))
}

@Composable
private fun ResetSaveButtons(reset: () -> Unit, save: () -> Unit, disabled: Boolean) {
  SectionView {
    SectionItemView(reset, disabled = disabled) {
      Text(stringResource(R.string.reset_verb), color = if (disabled) HighOrLowlight else MaterialTheme.colors.primary)
    }
    SectionDivider()
    SectionItemView(save, disabled = disabled) {
      Text(stringResource(R.string.save_and_notify_contact), color = if (disabled) HighOrLowlight else MaterialTheme.colors.primary)
    }
  }
}
