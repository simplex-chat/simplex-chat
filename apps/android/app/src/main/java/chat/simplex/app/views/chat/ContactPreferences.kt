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
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.stringResource
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.usersettings.PreferenceToggle

@Composable
fun ContactPreferencesView(
  m: ChatModel,
  user: User,
  contactId: Long,
  close: () -> Unit,
) {
  val contact = remember { derivedStateOf { (m.getContactChat(contactId)?.chatInfo as? ChatInfo.Direct)?.contact } }
  val ct = contact.value ?: return
  var featuresAllowed by rememberSaveable(ct, stateSaver = serializableSaver()) { mutableStateOf(contactUserPrefsToFeaturesAllowed(ct.mergedPreferences)) }
  var currentFeaturesAllowed by rememberSaveable(ct, stateSaver = serializableSaver()) { mutableStateOf(featuresAllowed) }

  fun savePrefs(afterSave: () -> Unit = {}) {
    withApi {
      val prefs = contactFeaturesAllowedToPrefs(featuresAllowed)
      val toContact = m.controller.apiSetContactPrefs(ct.contactId, prefs)
      if (toContact != null) {
        m.updateContact(toContact)
        currentFeaturesAllowed = featuresAllowed
      }
      afterSave()
    }
  }
  ModalView(
    close = {
      if (featuresAllowed == currentFeaturesAllowed) close()
      else showUnsavedChangesAlert({ savePrefs(close) }, close)
    },
    background = if (isInDarkTheme()) MaterialTheme.colors.background else SettingsBackgroundLight
  ) {
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
      savePrefs = ::savePrefs,
    )
  }
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
    val timedMessages: MutableState<Boolean> = remember(featuresAllowed) { mutableStateOf(featuresAllowed.timedMessagesAllowed) }
    val onTTLUpdated = { ttl: Int? ->
      applyPrefs(featuresAllowed.copy(timedMessagesTTL = ttl ?: 86400))
    }
    TimedMessagesFeatureSection(featuresAllowed, contact.mergedPreferences.timedMessages, timedMessages, onTTLUpdated) { allowed, ttl ->
      applyPrefs(featuresAllowed.copy(timedMessagesAllowed = allowed, timedMessagesTTL = ttl ?: currentFeaturesAllowed.timedMessagesTTL))
    }
    SectionSpacer()
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
    feature.asymmetric,
    user = SimpleChatPreference(allow = allowFeature.value.allowed),
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
private fun TimedMessagesFeatureSection(
  featuresAllowed: ContactFeaturesAllowed,
  pref: ContactUserPreferenceTimed,
  allowFeature: State<Boolean>,
  onTTLUpdated: (Int?) -> Unit,
  onSelected: (Boolean, Int?) -> Unit
) {
  val enabled = FeatureEnabled.enabled(
    ChatFeature.TimedMessages.asymmetric,
    user = TimedMessagesPreference(allow = if (allowFeature.value) FeatureAllowed.YES else FeatureAllowed.NO),
    contact = pref.contactPreference
  )

  SectionView(
    ChatFeature.TimedMessages.text.uppercase(),
    icon = ChatFeature.TimedMessages.iconFilled,
    iconTint = if (enabled.forUser) SimplexGreen else if (enabled.forContact) WarningYellow else Color.Red,
    leadingIcon = true,
  ) {
    SectionItemView {
      PreferenceToggle(
        generalGetString(R.string.chat_preferences_you_allow),
        checked = allowFeature.value,
      ) { allow ->
        onSelected(allow, if (allow) featuresAllowed.timedMessagesTTL ?: 86400 else null)
      }
    }
    SectionDivider()
    InfoRow(
      generalGetString(R.string.chat_preferences_contact_allows),
      pref.contactPreference.allow.text
    )
    SectionDivider()
    if (featuresAllowed.timedMessagesAllowed) {
      val ttl = rememberSaveable(featuresAllowed.timedMessagesTTL) { mutableStateOf(featuresAllowed.timedMessagesTTL) }
      TimedMessagesTTLPicker(ttl, onTTLUpdated)
    } else if (pref.contactPreference.allow == FeatureAllowed.YES || pref.contactPreference.allow == FeatureAllowed.ALWAYS) {
      InfoRow(generalGetString(R.string.delete_after), TimedMessagesPreference.ttlText(pref.contactPreference.ttl))
    }
  }
  SectionTextFooter(ChatFeature.TimedMessages.enabledDescription(enabled))
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

@Composable
fun TimedMessagesTTLPicker(selection: MutableState<Int?>, onSelected: (Int?) -> Unit) {
  val ttlValues = TimedMessagesPreference.ttlValues
  val values = ttlValues + if (ttlValues.contains(selection.value)) listOf() else listOf(selection.value)
  SectionItemView {
    ExposedDropDownSettingRow(
      generalGetString(R.string.delete_after),
      values.map { it to TimedMessagesPreference.ttlText(it) },
      selection,
      onSelected = onSelected
    )
  }
}

private fun showUnsavedChangesAlert(save: () -> Unit, revert: () -> Unit) {
  AlertManager.shared.showAlertDialogStacked(
    title = generalGetString(R.string.save_preferences_question),
    confirmText = generalGetString(R.string.save_and_notify_contact),
    dismissText = generalGetString(R.string.exit_without_saving),
    onConfirm = save,
    onDismiss = revert,
  )
}
