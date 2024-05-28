package chat.simplex.common.views.chat.group

import InfoRow
import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.PreferenceToggleWithIcon
import chat.simplex.common.model.*
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource

private val featureRoles: List<Pair<GroupMemberRole?, String>> = listOf(
  null to generalGetString(MR.strings.feature_roles_all_members),
  GroupMemberRole.Admin to generalGetString(MR.strings.feature_roles_admins),
  GroupMemberRole.Owner to generalGetString(MR.strings.feature_roles_owners)
)

@Composable
fun GroupPreferencesView(m: ChatModel, rhId: Long?, chatId: String, close: () -> Unit) {
  val groupInfo = remember { derivedStateOf {
    val ch = m.getChat(chatId)
    val g = (ch?.chatInfo as? ChatInfo.Group)?.groupInfo
    if (g == null || ch.remoteHostId != rhId) null else g
  }}
  val gInfo = groupInfo.value ?: return
  var preferences by rememberSaveable(gInfo, stateSaver = serializableSaver()) { mutableStateOf(gInfo.fullGroupPreferences) }
  var currentPreferences by rememberSaveable(gInfo, stateSaver = serializableSaver()) { mutableStateOf(preferences) }

  fun savePrefs(afterSave: () -> Unit = {}) {
    withBGApi {
      val gp = gInfo.groupProfile.copy(groupPreferences = preferences.toGroupPreferences())
      val g = m.controller.apiUpdateGroup(rhId, gInfo.groupId, gp)
      if (g != null) {
        m.updateGroup(rhId, g)
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
  ColumnWithScrollBar(
    Modifier.fillMaxWidth(),
  ) {
    AppBarTitle(stringResource(MR.strings.group_preferences))
    val timedMessages = remember(preferences) { mutableStateOf(preferences.timedMessages.enable) }
    val onTTLUpdated = { ttl: Int? ->
      applyPrefs(preferences.copy(timedMessages = preferences.timedMessages.copy(ttl = ttl)))
    }
    FeatureSection(GroupFeature.TimedMessages, timedMessages, null, groupInfo, preferences, onTTLUpdated) { enable, _ ->
      if (enable == GroupFeatureEnabled.ON) {
        applyPrefs(preferences.copy(timedMessages = TimedMessagesGroupPreference(enable = enable, ttl = preferences.timedMessages.ttl ?: 86400)))
      } else {
        applyPrefs(preferences.copy(timedMessages = TimedMessagesGroupPreference(enable = enable, ttl = currentPreferences.timedMessages.ttl)))
      }
    }
    SectionDividerSpaced(true, maxBottomPadding = false)
    val allowDirectMessages = remember(preferences) { mutableStateOf(preferences.directMessages.enable) }
    val directMessagesRole = remember(preferences) { mutableStateOf(preferences.directMessages.role) }
    FeatureSection(GroupFeature.DirectMessages, allowDirectMessages, directMessagesRole, groupInfo, preferences, onTTLUpdated) { enable, role ->
      applyPrefs(preferences.copy(directMessages = RoleGroupPreference(enable = enable, role)))
    }
    SectionDividerSpaced(true, maxBottomPadding = false)
    val allowFullDeletion = remember(preferences) { mutableStateOf(preferences.fullDelete.enable) }
    FeatureSection(GroupFeature.FullDelete, allowFullDeletion, null, groupInfo, preferences, onTTLUpdated) { enable, _ ->
      applyPrefs(preferences.copy(fullDelete = GroupPreference(enable = enable)))
    }
    SectionDividerSpaced(true, maxBottomPadding = false)
    val allowReactions = remember(preferences) { mutableStateOf(preferences.reactions.enable) }
    FeatureSection(GroupFeature.Reactions, allowReactions, null, groupInfo, preferences, onTTLUpdated) { enable, _ ->
      applyPrefs(preferences.copy(reactions = GroupPreference(enable = enable)))
    }
    SectionDividerSpaced(true, maxBottomPadding = false)
    val allowVoice = remember(preferences) { mutableStateOf(preferences.voice.enable) }
    val voiceRole = remember(preferences) { mutableStateOf(preferences.voice.role) }
    FeatureSection(GroupFeature.Voice, allowVoice, voiceRole, groupInfo, preferences, onTTLUpdated) { enable, role ->
      applyPrefs(preferences.copy(voice = RoleGroupPreference(enable = enable, role)))
    }
    SectionDividerSpaced(true, maxBottomPadding = false)
    val allowFiles = remember(preferences) { mutableStateOf(preferences.files.enable) }
    val filesRole = remember(preferences) { mutableStateOf(preferences.files.role) }
    FeatureSection(GroupFeature.Files, allowFiles, filesRole, groupInfo, preferences, onTTLUpdated) { enable, role ->
      applyPrefs(preferences.copy(files = RoleGroupPreference(enable = enable, role)))
    }

    SectionDividerSpaced(true, maxBottomPadding = false)
    val allowSimplexLinks = remember(preferences) { mutableStateOf(preferences.simplexLinks.enable) }
    val simplexLinksRole = remember(preferences) { mutableStateOf(preferences.simplexLinks.role) }
    FeatureSection(GroupFeature.SimplexLinks, allowSimplexLinks, simplexLinksRole, groupInfo, preferences, onTTLUpdated) { enable, role ->
      applyPrefs(preferences.copy(simplexLinks = RoleGroupPreference(enable = enable, role)))
    }

    SectionDividerSpaced(true, maxBottomPadding = false)
    val enableHistory = remember(preferences) { mutableStateOf(preferences.history.enable) }
    FeatureSection(GroupFeature.History, enableHistory, null, groupInfo, preferences, onTTLUpdated) { enable, _ ->
      applyPrefs(preferences.copy(history = GroupPreference(enable = enable)))
    }
    if (groupInfo.canEdit) {
      SectionDividerSpaced(maxTopPadding = true, maxBottomPadding = false)
      ResetSaveButtons(
        reset = reset,
        save = savePrefs,
        disabled = preferences == currentPreferences
      )
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun FeatureSection(
  feature: GroupFeature,
  enableFeature: State<GroupFeatureEnabled>,
  enableForRole: State<GroupMemberRole?>? = null,
  groupInfo: GroupInfo,
  preferences: FullGroupPreferences,
  onTTLUpdated: (Int?) -> Unit,
  onSelected: (GroupFeatureEnabled, GroupMemberRole?) -> Unit
) {
  SectionView {
    val on = enableFeature.value == GroupFeatureEnabled.ON
    val icon = if (on) feature.iconFilled() else feature.icon
    val iconTint = if (on) SimplexGreen else MaterialTheme.colors.secondary
    val timedOn = feature == GroupFeature.TimedMessages && enableFeature.value == GroupFeatureEnabled.ON
    if (groupInfo.canEdit) {
      PreferenceToggleWithIcon(
        feature.text,
        icon,
        iconTint,
        enableFeature.value == GroupFeatureEnabled.ON,
      ) { checked ->
        onSelected(if (checked) GroupFeatureEnabled.ON else GroupFeatureEnabled.OFF, enableForRole?.value)
      }
      if (timedOn) {
        val ttl = rememberSaveable(preferences.timedMessages) { mutableStateOf(preferences.timedMessages.ttl) }
        DropdownCustomTimePickerSettingRow(
          selection = ttl,
          propagateExternalSelectionUpdate = true, // for Reset
          label = generalGetString(MR.strings.delete_after),
          dropdownValues = TimedMessagesPreference.ttlValues,
          customPickerTitle = generalGetString(MR.strings.delete_after),
          customPickerConfirmButtonText = generalGetString(MR.strings.custom_time_picker_select),
          onSelected = onTTLUpdated
        )
      }
      if (enableFeature.value == GroupFeatureEnabled.ON && enableForRole != null) {
        ExposedDropDownSettingRow(
          generalGetString(MR.strings.feature_enabled_for),
          featureRoles,
          enableForRole,
          onSelected = { value ->
            onSelected(enableFeature.value, value)
          }
        )
      }
    } else {
      InfoRow(
        feature.text,
        enableFeature.value.text,
        icon = icon,
        iconTint = iconTint,
      )
      if (timedOn) {
        InfoRow(generalGetString(MR.strings.delete_after), timeText(preferences.timedMessages.ttl))
      }
      if (enableFeature.value == GroupFeatureEnabled.ON && enableForRole != null) {
        InfoRow(generalGetString(MR.strings.feature_enabled_for), featureRoles.firstOrNull { it.first == enableForRole.value }?.second ?: generalGetString(MR.strings.feature_roles_all_members), textColor = MaterialTheme.colors.secondary)
      }
    }
  }
  KeyChangeEffect(enableFeature.value) {
    if (enableFeature.value == GroupFeatureEnabled.OFF) {
      onSelected(enableFeature.value, null)
    }
  }
  SectionTextFooter(feature.enableDescription(enableFeature.value, groupInfo.canEdit))
}

@Composable
private fun ResetSaveButtons(reset: () -> Unit, save: () -> Unit, disabled: Boolean) {
  SectionView {
    SectionItemView(reset, disabled = disabled) {
      Text(stringResource(MR.strings.reset_verb), color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
    }
    SectionItemView(save, disabled = disabled) {
      Text(stringResource(MR.strings.save_and_notify_group_members), color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
    }
  }
}

private fun showUnsavedChangesAlert(save: () -> Unit, revert: () -> Unit) {
  AlertManager.shared.showAlertDialogStacked(
    title = generalGetString(MR.strings.save_preferences_question),
    confirmText = generalGetString(MR.strings.save_and_notify_group_members),
    dismissText = generalGetString(MR.strings.exit_without_saving),
    onConfirm = save,
    onDismiss = revert,
  )
}
