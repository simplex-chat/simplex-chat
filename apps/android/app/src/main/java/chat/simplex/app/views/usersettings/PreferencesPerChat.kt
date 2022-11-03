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
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*

@Composable
fun PreferencesPerChatView(m: ChatModel, user: User, contact: Contact, onContactUpdated: (Contact) -> Unit) {
  val userPrefs = remember { user.profile.preferences ?: ChatPreferences.default }
  val contactPrefs = remember { contact.profile.preferences ?: ChatPreferences.default }
  var currentPrefs by remember { mutableStateOf(contact.userPreferences) }
  var savedPrefs by remember { mutableStateOf(currentPrefs) }
  PreferencesPerChatLayout(
    currentPrefs,
    savedPrefs,
    userPrefs,
    contactPrefs,
    contact,
    applyPrefs = { prefs ->
      currentPrefs = prefs
    },
    revert = {
      currentPrefs = savedPrefs
    },
    savePrefs = {
      showConfirmSavingAlert {
        withApi {
          if (m.controller.apiSetContactPrefs(contact.contactId, currentPrefs) != null) {
            savedPrefs = currentPrefs
            val newContact = contact.copy(userPreferences = currentPrefs)
            m.updateContact(newContact)
            onContactUpdated(newContact)
          }
        }
      }
    },
  )
}

@Composable fun PreferencesPerChatLayout(
  prefs: ChatPreferences,
  savedPrefs: ChatPreferences,
  userPrefs: ChatPreferences,
  contactPrefs: ChatPreferences,
  contact: Contact,
  applyPrefs: (ChatPreferences) -> Unit,
  revert: () -> Unit,
  savePrefs: () -> Unit,
) {
  Column(
    Modifier.fillMaxWidth().verticalScroll(rememberScrollState()),
    horizontalAlignment = Alignment.Start,
  ) {
    AppBarTitle(String.format(stringResource(R.string.chat_preferences_for), contact.displayName))
    val voice = remember(prefs) { mutableStateOf(prefs.voice.toLocal()) }
    VoiceSection(
      voice,
      (userPrefs.voice ?: ChatPreference.voiceDefault).toLocal(),
      contactPrefs.voice.toLocal(),
      contact
    ) {
      applyPrefs(prefs.copy(voice = if (it == ChatPreferenceLocal.DEFAULT) null else it.toPref(ChatPreference.voiceDefault)))
    }

    SectionSpacer()
    val messageDelete = remember(prefs) { mutableStateOf(prefs.messageDelete.toLocal()) }
    MessageDeleteSection(
      messageDelete,
      (userPrefs.messageDelete ?: ChatPreference.messageDeleteDefault).toLocal(),
      contactPrefs.messageDelete.toLocal(),
      contact
    ) {
      applyPrefs(prefs.copy(messageDelete = if (it == ChatPreferenceLocal.DEFAULT) null else it.toPref(ChatPreference.messageDeleteDefault)))
    }

    Spacer(Modifier.height(15.dp))

    SectionCustomFooter(PaddingValues(horizontal = DEFAULT_PADDING)) {
      ButtonsFooter(
        cancel = revert,
        save = savePrefs,
        disabled = prefs == savedPrefs
      )
    }
  }
}

@Composable
private fun VoiceSection(
  current: State<ChatPreferenceLocal>,
  default: ChatPreferenceLocal,
  contactCurrent: ChatPreferenceLocal,
  contact: Contact,
  onSelected: (ChatPreferenceLocal) -> Unit
) {
  fun mapValue(it: ChatPreferenceLocal): ValueTitleDesc<ChatPreferenceLocal> =
    when (it) {
      ChatPreferenceLocal.DEFAULT -> ValueTitleDesc(it, String.format(generalGetString(R.string.chat_preferences_default), mapValue(default).title), mapValue(default).description)
      ChatPreferenceLocal.ON -> ValueTitleDesc(it, generalGetString(R.string.chat_preferences_on), generalGetString(R.string.chat_preferences_voice_on_desc))
      ChatPreferenceLocal.OFF -> ValueTitleDesc(it, generalGetString(R.string.chat_preferences_off), generalGetString(R.string.chat_preferences_voice_off_desc))
      ChatPreferenceLocal.PREFER -> ValueTitleDesc(it, generalGetString(R.string.chat_preferences_prefer), generalGetString(R.string.chat_preferences_voice_prefer_desc))
    }

  val values = remember {
    ChatPreferenceLocal.values().map(::mapValue)
  }
  SectionView {
    SectionItemView {
      val mappedValues = remember { values.map { it.value to it.title } }
      ExposedDropDownSettingRow(
        generalGetString(R.string.chat_preferences_voice),
        mappedValues,
        current,
        icon = Icons.Outlined.Audiotrack,
        onSelected = onSelected
      )
    }
    SectionDivider()
    SectionItemWithValue(
      contact.displayName,
      remember { mutableStateOf(contactCurrent) },
      values,
      icon = Icons.Outlined.AccountCircle,
      enabled = remember { mutableStateOf(false) },
      onSelected = {}
    )
  }
  SectionTextFooter(values.firstOrNull { it.value == current.value }!!.description)
}

@Composable
private fun MessageDeleteSection(
  current: State<ChatPreferenceLocal>,
  default: ChatPreferenceLocal,
  contactCurrent: ChatPreferenceLocal,
  contact: Contact,
  onSelected: (ChatPreferenceLocal) -> Unit
) {
  fun mapValue(it: ChatPreferenceLocal): ValueTitleDesc<ChatPreferenceLocal> =
    when (it) {
      ChatPreferenceLocal.DEFAULT -> ValueTitleDesc(it, String.format(generalGetString(R.string.chat_preferences_default), mapValue(default).title), mapValue(default).description)
      ChatPreferenceLocal.ON -> ValueTitleDesc(it, generalGetString(R.string.chat_preferences_on), generalGetString(R.string.chat_preferences_deletion_on_desc))
      ChatPreferenceLocal.OFF -> ValueTitleDesc(it, generalGetString(R.string.chat_preferences_off), generalGetString(R.string.chat_preferences_deletion_off_desc))
      ChatPreferenceLocal.PREFER -> ValueTitleDesc(it, generalGetString(R.string.chat_preferences_prefer), generalGetString(R.string.chat_preferences_deletion_prefer_desc))
    }

  val values = remember {
    ChatPreferenceLocal.values().map(::mapValue)
  }
  SectionView {
    SectionItemView {
      val mappedValues = remember { values.map { it.value to it.title } }
      ExposedDropDownSettingRow(
        generalGetString(R.string.chat_preferences_deletion),
        mappedValues,
        current,
        icon = Icons.Outlined.Delete,
        onSelected = onSelected
      )
    }
    SectionDivider()
    SectionItemWithValue(
      contact.displayName,
      remember { mutableStateOf(contactCurrent) },
      values,
      icon = Icons.Outlined.AccountCircle,
      enabled = remember { mutableStateOf(false) },
      onSelected = {}
    )
  }
  SectionTextFooter(values.firstOrNull { it.value == current.value }!!.description)
}

private fun showConfirmSavingAlert(onConfirm: () -> Unit) {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(R.string.confirm_saving_prefs_question),
    text = generalGetString(R.string.confirm_saving_prefs_per_contact_info),
    confirmText = generalGetString(R.string.save_verb),
    onConfirm = onConfirm
  )
}

@Composable
private fun ButtonsFooter(cancel: () -> Unit, save: () -> Unit, disabled: Boolean) {
  Row(
    Modifier.fillMaxWidth(),
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    FooterButton(Icons.Outlined.Replay, stringResource(R.string.cancel_verb), cancel, disabled)
    FooterButton(Icons.Outlined.Check, stringResource(R.string.save_verb), save, disabled)
  }
}
