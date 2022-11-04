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

@Composable
private fun PreferencesPerChatLayout(
  prefs: ChatPreferences,
  savedPrefs: ChatPreferences,
  userPrefs: ChatPreferences,
  contactPrefs: ChatPreferences,
  contact: Contact,
  applyPrefs: (ChatPreferences) -> Unit,
  revert: () -> Unit,
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
      AppBarTitle(String.format(stringResource(R.string.chat_preferences_for), contact.displayName))
      val voice = remember(prefs) { mutableStateOf(prefs.voice.toLocal()) }
      VoiceSection(
        voice,
        (userPrefs.voice ?: ChatPreference.voiceDefault).toLocal(),
        (contactPrefs.voice ?: ChatPreference.voiceDefault).toLocal(),
      ) {
        applyPrefs(prefs.copy(voice = if (it == ChatPreferenceLocal.DEFAULT) null else it.toPref(ChatPreference.voiceDefault)))
      }

      SectionSpacer()
      val fullDelete = remember(prefs) { mutableStateOf(prefs.fullDelete.toLocal()) }
      FullDeleteSection(
        fullDelete,
        (userPrefs.fullDelete ?: ChatPreference.fullDeleteDefault).toLocal(),
        (contactPrefs.fullDelete ?: ChatPreference.fullDeleteDefault).toLocal(),
      ) {
        applyPrefs(prefs.copy(fullDelete = if (it == ChatPreferenceLocal.DEFAULT) null else it.toPref(ChatPreference.fullDeleteDefault)))
      }

      SectionSpacer()
      val receipts = remember(prefs) { mutableStateOf(prefs.receipts.toLocal()) }
      ReceiptsSection(
        receipts,
        (userPrefs.receipts ?: ChatPreference.receiptsDefault).toLocal(),
        (contactPrefs.receipts ?: ChatPreference.receiptsDefault).toLocal(),
      ) {
        applyPrefs(prefs.copy(receipts = if (it == ChatPreferenceLocal.DEFAULT) null else it.toPref(ChatPreference.receiptsDefault)))
      }
    }

    SectionCustomFooter(PaddingValues(DEFAULT_PADDING)) {
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
  advanced: Boolean = false,
  onSelected: (ChatPreferenceLocal) -> Unit
) {
  fun mapValue(user: ChatPreferenceLocal): ValueTitleDesc<ChatPreferenceLocal> = when (user) {
    ChatPreferenceLocal.DEFAULT -> ValueTitleDesc(user, String.format(generalGetString(R.string.chat_preferences_default), mapValue(default).title), mapValue(default).description)
    ChatPreferenceLocal.YES -> when (contactCurrent) {
      ChatPreferenceLocal.YES, ChatPreferenceLocal.ALWAYS -> ValueTitleDesc(user, generalGetString(R.string.chat_preferences_yes), generalGetString(R.string.chat_preferences_voice_yes_and_yes_desc))
      ChatPreferenceLocal.NO -> ValueTitleDesc(user, generalGetString(R.string.chat_preferences_yes), generalGetString(R.string.chat_preferences_voice_yes_and_no_desc))
      ChatPreferenceLocal.DEFAULT -> error("Shouldn't be here")
    }
    ChatPreferenceLocal.NO -> when (contactCurrent) {
      ChatPreferenceLocal.YES, ChatPreferenceLocal.NO -> ValueTitleDesc(user, generalGetString(R.string.chat_preferences_no), generalGetString(R.string.chat_preferences_voice_no_and_yes_desc))
      ChatPreferenceLocal.ALWAYS -> ValueTitleDesc(user, generalGetString(R.string.chat_preferences_no), generalGetString(R.string.chat_preferences_voice_no_and_always_desc))
      ChatPreferenceLocal.DEFAULT -> error("Shouldn't be here")
    }
    ChatPreferenceLocal.ALWAYS -> when (contactCurrent) {
      ChatPreferenceLocal.YES, ChatPreferenceLocal.ALWAYS -> ValueTitleDesc(user, generalGetString(R.string.chat_preferences_always), generalGetString(R.string.chat_preferences_voice_yes_and_yes_desc))
      ChatPreferenceLocal.NO -> ValueTitleDesc(user, generalGetString(R.string.chat_preferences_always), generalGetString(R.string.chat_preferences_voice_always_and_no_desc))
      ChatPreferenceLocal.DEFAULT -> error("Shouldn't be here")
    }
  }

  val values = remember {
    ChatPreferenceLocal.values().map(::mapValue)
  }
  SectionView(generalGetString(R.string.chat_preferences_voice).uppercase()) {
    SectionItemView {
      SelectableChatPreferenceRow(current, values, advanced, onSelected)
    }
    SectionDivider()
    SectionItemWithValue(
      generalGetString(R.string.chat_preferences_contact_allows),
      remember { mutableStateOf(contactCurrent) },
      values,
      icon = null,
      enabled = remember { mutableStateOf(false) },
      onSelected = {}
    )
  }
  SectionTextFooter(values.firstOrNull { it.value == current.value }!!.description)
}

@Composable
private fun FullDeleteSection(
  current: State<ChatPreferenceLocal>,
  default: ChatPreferenceLocal,
  contactCurrent: ChatPreferenceLocal,
  advanced: Boolean = false,
  onSelected: (ChatPreferenceLocal) -> Unit
) {
  fun mapValue(user: ChatPreferenceLocal): ValueTitleDesc<ChatPreferenceLocal> = when (user) {
    ChatPreferenceLocal.DEFAULT -> ValueTitleDesc(user, String.format(generalGetString(R.string.chat_preferences_default), mapValue(default).title), mapValue(default).description)
    ChatPreferenceLocal.YES -> when (contactCurrent) {
      ChatPreferenceLocal.YES, ChatPreferenceLocal.ALWAYS -> ValueTitleDesc(user, generalGetString(R.string.chat_preferences_yes), generalGetString(R.string.chat_preferences_deletion_yes_and_yes_desc))
      ChatPreferenceLocal.NO -> ValueTitleDesc(user, generalGetString(R.string.chat_preferences_yes), generalGetString(R.string.chat_preferences_deletion_yes_and_no_desc))
      ChatPreferenceLocal.DEFAULT -> error("Shouldn't be here")
    }
    ChatPreferenceLocal.NO -> when (contactCurrent) {
      ChatPreferenceLocal.YES, ChatPreferenceLocal.NO -> ValueTitleDesc(user, generalGetString(R.string.chat_preferences_no), generalGetString(R.string.chat_preferences_deletion_no_and_yes_desc))
      ChatPreferenceLocal.ALWAYS -> ValueTitleDesc(user, generalGetString(R.string.chat_preferences_no), generalGetString(R.string.chat_preferences_deletion_no_and_always_desc))
      ChatPreferenceLocal.DEFAULT -> error("Shouldn't be here")
    }
    ChatPreferenceLocal.ALWAYS -> when (contactCurrent) {
      ChatPreferenceLocal.YES, ChatPreferenceLocal.ALWAYS -> ValueTitleDesc(user, generalGetString(R.string.chat_preferences_always), generalGetString(R.string.chat_preferences_deletion_yes_and_yes_desc))
      ChatPreferenceLocal.NO -> ValueTitleDesc(user, generalGetString(R.string.chat_preferences_always), generalGetString(R.string.chat_preferences_deletion_always_and_no_desc))
      ChatPreferenceLocal.DEFAULT -> error("Shouldn't be here")
    }
  }
  val values = remember {
    ChatPreferenceLocal.values().map(::mapValue)
  }
  SectionView(generalGetString(R.string.chat_preferences_deletion).uppercase()) {
    SectionItemView {
      SelectableChatPreferenceRow(current, values, advanced, onSelected)
    }
    SectionDivider()
    SectionItemWithValue(
      generalGetString(R.string.chat_preferences_contact_allows),
      remember { mutableStateOf(contactCurrent) },
      values,
      icon = null,
      enabled = remember { mutableStateOf(false) },
      onSelected = {}
    )
  }
  SectionTextFooter(values.firstOrNull { it.value == current.value }!!.description)
}

@Composable
private fun ReceiptsSection(
  current: State<ChatPreferenceLocal>,
  default: ChatPreferenceLocal,
  contactCurrent: ChatPreferenceLocal,
  advanced: Boolean = false,
  onSelected: (ChatPreferenceLocal) -> Unit
) {
  fun mapValue(user: ChatPreferenceLocal): ValueTitleDesc<ChatPreferenceLocal> = when (user) {
    ChatPreferenceLocal.DEFAULT -> ValueTitleDesc(user, String.format(generalGetString(R.string.chat_preferences_default), mapValue(default).title), mapValue(default).description)
    ChatPreferenceLocal.YES -> when (contactCurrent) {
      ChatPreferenceLocal.YES, ChatPreferenceLocal.ALWAYS -> ValueTitleDesc(user, generalGetString(R.string.chat_preferences_yes), generalGetString(R.string.chat_preferences_delivery_receipts_yes_and_yes_desc))
      ChatPreferenceLocal.NO -> ValueTitleDesc(user, generalGetString(R.string.chat_preferences_yes), generalGetString(R.string.chat_preferences_delivery_receipts_yes_and_no_desc))
      ChatPreferenceLocal.DEFAULT -> error("Shouldn't be here")
    }
    ChatPreferenceLocal.NO -> when (contactCurrent) {
      ChatPreferenceLocal.YES, ChatPreferenceLocal.NO -> ValueTitleDesc(user, generalGetString(R.string.chat_preferences_no), generalGetString(R.string.chat_preferences_delivery_receipts_no_and_yes_desc))
      ChatPreferenceLocal.ALWAYS -> ValueTitleDesc(user, generalGetString(R.string.chat_preferences_no), generalGetString(R.string.chat_preferences_delivery_receipts_no_and_always_desc))
      ChatPreferenceLocal.DEFAULT -> error("Shouldn't be here")
    }
    ChatPreferenceLocal.ALWAYS -> when (contactCurrent) {
      ChatPreferenceLocal.YES, ChatPreferenceLocal.ALWAYS -> ValueTitleDesc(user, generalGetString(R.string.chat_preferences_always), generalGetString(R.string.chat_preferences_delivery_receipts_yes_and_yes_desc))
      ChatPreferenceLocal.NO -> ValueTitleDesc(user, generalGetString(R.string.chat_preferences_always), generalGetString(R.string.chat_preferences_delivery_receipts_always_and_no_desc))
      ChatPreferenceLocal.DEFAULT -> error("Shouldn't be here")
    }
  }

  val values = remember {
    ChatPreferenceLocal.values().map(::mapValue)
  }
  SectionView(generalGetString(R.string.chat_preferences_delivery_receipts).uppercase()) {
    SectionItemView {
      SelectableChatPreferenceRow(current, values, advanced, onSelected)
    }
    SectionDivider()
    SectionItemWithValue(
      generalGetString(R.string.chat_preferences_contact_allows),
      remember { mutableStateOf(contactCurrent) },
      values,
      icon = null,
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
