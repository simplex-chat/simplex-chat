package chat.simplex.app.views.chat.group

import SectionItemView
import SectionSpacer
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*

@Composable
fun GroupWelcomeView(m: ChatModel, groupInfo: GroupInfo, close: () -> Unit) {
  var groupInfo by remember { mutableStateOf(groupInfo) }
  val welcomeText = remember { mutableStateOf(groupInfo.groupProfile.description ?: "") }

  fun save(afterSave: () -> Unit = {}) {
    withApi {
      var welcome: String? = welcomeText.value.trim('\n', ' ')
      if (welcome?.length == 0) {
        welcome = null
      }
      val groupProfileUpdated = groupInfo.groupProfile.copy(description = welcome)
      val res = m.controller.apiUpdateGroup(groupInfo.groupId, groupProfileUpdated)
      if (res != null) {
        groupInfo = res
        m.updateGroup(res)
        welcomeText.value = welcome ?: ""
      }
      afterSave()
    }
  }

  ModalView(
    close = {
      if (welcomeText.value == groupInfo.groupProfile.description || (welcomeText.value == "" && groupInfo.groupProfile.description == null)) close()
      else showUnsavedChangesAlert({ save(close) }, close)
    },
  ) {
    GroupWelcomeLayout(
      welcomeText,
      groupInfo,
      save = ::save
    )
  }
}

@Composable
private fun GroupWelcomeLayout(
  welcomeText: MutableState<String>,
  groupInfo: GroupInfo,
  save: () -> Unit,
) {
  Column(
    Modifier.fillMaxWidth().verticalScroll(rememberScrollState()),
  ) {
    AppBarTitle(stringResource(R.string.group_welcome_title))
    val welcomeText = remember { welcomeText }
    TextEditor(Modifier.padding(horizontal = DEFAULT_PADDING).height(160.dp), text = welcomeText)
    SectionSpacer()
    SaveButton(
      save = save,
      disabled = welcomeText.value == groupInfo.groupProfile.description || (welcomeText.value == "" && groupInfo.groupProfile.description == null)
    )
  }
}

@Composable
private fun SaveButton(save: () -> Unit, disabled: Boolean) {
  SectionView {
    SectionItemView(save, disabled = disabled) {
      Text(stringResource(R.string.save_and_update_group_profile), color = if (disabled) HighOrLowlight else MaterialTheme.colors.primary)
    }
  }
}

private fun showUnsavedChangesAlert(save: () -> Unit, revert: () -> Unit) {
  AlertManager.shared.showAlertDialogStacked(
    title = generalGetString(R.string.save_welcome_message_question),
    confirmText = generalGetString(R.string.save_and_update_group_profile),
    dismissText = generalGetString(R.string.exit_without_saving),
    onConfirm = save,
    onDismiss = revert,
  )
}
