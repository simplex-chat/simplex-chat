package chat.simplex.app.views.chat.group

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionView
import TextIconSpaced
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.DEFAULT_PADDING
import chat.simplex.app.views.chat.item.MarkdownText
import chat.simplex.app.views.helpers.*
import kotlinx.coroutines.delay

@Composable
fun GroupWelcomeView(m: ChatModel, groupInfo: GroupInfo, close: () -> Unit) {
  var gInfo by remember { mutableStateOf(groupInfo) }
  val welcomeText = remember { mutableStateOf(gInfo.groupProfile.description ?: "") }

  fun save(afterSave: () -> Unit = {}) {
    withApi {
      var welcome: String? = welcomeText.value.trim('\n', ' ')
      if (welcome?.length == 0) {
        welcome = null
      }
      val groupProfileUpdated = gInfo.groupProfile.copy(description = welcome)
      val res = m.controller.apiUpdateGroup(gInfo.groupId, groupProfileUpdated)
      if (res != null) {
        gInfo = res
        m.updateGroup(res)
        welcomeText.value = welcome ?: ""
      }
      afterSave()
    }
  }

  ModalView(
    close = {
      if (welcomeText.value == gInfo.groupProfile.description || (welcomeText.value == "" && gInfo.groupProfile.description == null)) close()
      else showUnsavedChangesAlert({ save(close) }, close)
    },
  ) {
    GroupWelcomeLayout(
      welcomeText,
      gInfo,
      m.controller.appPrefs.simplexLinkMode.get(),
      save = ::save
    )
  }
}

@Composable
private fun GroupWelcomeLayout(
  welcomeText: MutableState<String>,
  groupInfo: GroupInfo,
  linkMode: SimplexLinkMode,
  save: () -> Unit,
) {
  Column(
    Modifier.fillMaxWidth().verticalScroll(rememberScrollState()),
  ) {
    val editMode = remember { mutableStateOf(true) }
    AppBarTitle(stringResource(R.string.group_welcome_title))
    val wt = rememberSaveable { welcomeText }
    if (groupInfo.canEdit) {
      if (editMode.value) {
        val focusRequester = remember { FocusRequester() }
        TextEditor(
          wt,
          Modifier.height(140.dp), stringResource(R.string.enter_welcome_message),
          focusRequester = focusRequester
        )
        LaunchedEffect(Unit) {
          delay(300)
          focusRequester.requestFocus()
        }
      } else {
        TextPreview(wt.value, linkMode)
      }
      ChangeModeButton(
        editMode.value,
        click = {
          editMode.value = !editMode.value
        },
        wt.value.isEmpty()
      )
      CopyTextButton { copyText(SimplexApp.context, wt.value) }
      SectionDividerSpaced(maxBottomPadding = false)
      SaveButton(
        save = save,
        disabled = wt.value == groupInfo.groupProfile.description || (wt.value == "" && groupInfo.groupProfile.description == null)
      )
    } else {
      TextPreview(wt.value, linkMode)
      CopyTextButton { copyText(SimplexApp.context, wt.value) }
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun TextPreview(text: String, linkMode: SimplexLinkMode, markdown: Boolean = true) {
  Column {
    SelectionContainer(Modifier.fillMaxWidth()) {
      MarkdownText(
        text,
        formattedText = if (markdown) remember(text) { parseToMarkdown(text) } else null,
        modifier = Modifier.fillMaxHeight().padding(horizontal = DEFAULT_PADDING),
        linkMode = linkMode,
        style = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground, lineHeight = 22.sp)
      )
    }
  }
}

@Composable
private fun SaveButton(save: () -> Unit, disabled: Boolean) {
  SectionView {
    SectionItemView(save, disabled = disabled) {
      Text(stringResource(R.string.save_and_update_group_profile), color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
    }
  }
}

@Composable
private fun ChangeModeButton(editMode: Boolean, click: () -> Unit, disabled: Boolean) {
  SectionItemView(click, disabled = disabled) {
    Icon(
      painterResource(if (editMode) R.drawable.ic_visibility else R.drawable.ic_edit),
      contentDescription = generalGetString(R.string.edit_verb),
      tint = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary,
    )
    TextIconSpaced()
    Text(
      stringResource(if (editMode) R.string.group_welcome_preview else R.string.edit_verb),
      color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
    )
  }
}

@Composable
private fun CopyTextButton(click: () -> Unit) {
  SectionItemView(click) {
    Icon(
      painterResource(R.drawable.ic_content_copy),
      contentDescription = generalGetString(R.string.copy_verb),
      tint = MaterialTheme.colors.primary,
    )
    TextIconSpaced()
    Text(stringResource(R.string.copy_verb), color = MaterialTheme.colors.primary)
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
