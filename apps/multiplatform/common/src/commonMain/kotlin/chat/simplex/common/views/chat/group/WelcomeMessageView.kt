package chat.simplex.common.views.chat.group

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
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
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.text.AnnotatedString
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.chat.item.MarkdownText
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.GroupInfo
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.platform.chatJsonLength
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.res.MR
import kotlinx.coroutines.delay

private const val maxByteCount = 1200

@Composable
fun GroupWelcomeView(m: ChatModel, rhId: Long?, groupInfo: GroupInfo, close: () -> Unit) {
  var gInfo by remember { mutableStateOf(groupInfo) }
  val welcomeText = remember { mutableStateOf(gInfo.groupProfile.description ?: "") }

  fun save(afterSave: () -> Unit = {}) {
    withBGApi {
      var welcome: String? = welcomeText.value.trim('\n', ' ')
      if (welcome?.length == 0) {
        welcome = null
      }
      val groupProfileUpdated = gInfo.groupProfile.copy(description = welcome)
      val res = m.controller.apiUpdateGroup(rhId, gInfo.groupId, groupProfileUpdated)
      if (res != null) {
        gInfo = res
        m.updateGroup(rhId, res)
        welcomeText.value = welcome ?: ""
      }
      afterSave()
    }
  }

  ModalView(
    close = {
      when {
        welcomeTextUnchanged(welcomeText, gInfo) -> close()
        !welcomeTextFitsLimit(welcomeText) -> showUnsavedChangesTooLongAlert(close)
        else -> showUnsavedChangesAlert({ save(close) }, close)
      }
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

private fun welcomeTextUnchanged(welcomeText: MutableState<String>, groupInfo: GroupInfo): Boolean {
  return welcomeText.value == groupInfo.groupProfile.description || (welcomeText.value == "" && groupInfo.groupProfile.description == null)
}

private fun welcomeTextFitsLimit(welcomeText: MutableState<String>): Boolean {
  return chatJsonLength(welcomeText.value) <= maxByteCount
}

@Composable
private fun GroupWelcomeLayout(
  welcomeText: MutableState<String>,
  groupInfo: GroupInfo,
  linkMode: SimplexLinkMode,
  save: () -> Unit,
) {
  ColumnWithScrollBar(
    Modifier.fillMaxWidth(),
  ) {
    val editMode = remember { mutableStateOf(true) }
    AppBarTitle(stringResource(MR.strings.group_welcome_title))
    val wt = rememberSaveable { welcomeText }
    if (groupInfo.canEdit) {
      if (editMode.value) {
        val focusRequester = remember { FocusRequester() }
        TextEditor(
          wt,
          Modifier.height(140.dp), stringResource(MR.strings.enter_welcome_message),
          focusRequester = focusRequester
        )
        LaunchedEffect(Unit) {
          delay(300)
          focusRequester.requestFocus()
        }
      } else {
        TextPreview(wt.value, linkMode)
      }
      SectionTextFooter(
        if (!welcomeTextFitsLimit(wt)) { generalGetString(MR.strings.message_too_large) } else "",
        color = if (welcomeTextFitsLimit(wt)) MaterialTheme.colors.secondary else Color.Red
      )

      Spacer(Modifier.size(8.dp))

      ChangeModeButton(
        editMode.value,
        click = {
          editMode.value = !editMode.value
        },
        wt.value.isEmpty()
      )
      val clipboard = LocalClipboardManager.current
      CopyTextButton { clipboard.setText(AnnotatedString(wt.value)) }

      Divider(
        Modifier.padding(
          start = DEFAULT_PADDING_HALF,
          top = 8.dp,
          end = DEFAULT_PADDING_HALF,
          bottom = 8.dp)
      )

      SaveButton(
        save = save,
        disabled = welcomeTextUnchanged(wt, groupInfo) || !welcomeTextFitsLimit(wt)
      )
    } else {
      val clipboard = LocalClipboardManager.current
      TextPreview(wt.value, linkMode)
      CopyTextButton { clipboard.setText(AnnotatedString(wt.value)) }
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun TextPreview(text: String, linkMode: SimplexLinkMode, markdown: Boolean = true) {
  val uriHandler = LocalUriHandler.current
  Column {
    SelectionContainer(Modifier.fillMaxWidth()) {
      MarkdownText(
        text,
        formattedText = if (markdown) remember(text) { parseToMarkdown(text) } else null,
        toggleSecrets = false,
        modifier = Modifier.fillMaxHeight().padding(horizontal = DEFAULT_PADDING),
        linkMode = linkMode, uriHandler = uriHandler,
        style = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground, lineHeight = 22.sp)
      )
    }
  }
}

@Composable
private fun SaveButton(save: () -> Unit, disabled: Boolean) {
  SectionView {
    SectionItemView(save, disabled = disabled) {
      Text(stringResource(MR.strings.save_and_update_group_profile), color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
    }
  }
}

@Composable
private fun ChangeModeButton(editMode: Boolean, click: () -> Unit, disabled: Boolean) {
  SectionItemView(click, disabled = disabled) {
    Icon(
      painterResource(if (editMode) MR.images.ic_visibility else MR.images.ic_edit),
      contentDescription = generalGetString(MR.strings.edit_verb),
      tint = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary,
    )
    TextIconSpaced()
    Text(
      stringResource(if (editMode) MR.strings.group_welcome_preview else MR.strings.edit_verb),
      color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
    )
  }
}

@Composable
private fun CopyTextButton(click: () -> Unit) {
  SectionItemView(click) {
    Icon(
      painterResource(MR.images.ic_content_copy),
      contentDescription = generalGetString(MR.strings.copy_verb),
      tint = MaterialTheme.colors.primary,
    )
    TextIconSpaced()
    Text(stringResource(MR.strings.copy_verb), color = MaterialTheme.colors.primary)
  }
}

private fun showUnsavedChangesAlert(save: () -> Unit, revert: () -> Unit) {
  AlertManager.shared.showAlertDialogStacked(
    title = generalGetString(MR.strings.save_welcome_message_question),
    confirmText = generalGetString(MR.strings.save_and_update_group_profile),
    dismissText = generalGetString(MR.strings.exit_without_saving),
    onConfirm = save,
    onDismiss = revert,
  )
}

private fun showUnsavedChangesTooLongAlert(revert: () -> Unit) {
  AlertManager.shared.showAlertDialogStacked(
    title = generalGetString(MR.strings.welcome_message_is_too_long),
    confirmText = generalGetString(MR.strings.exit_without_saving),
    onConfirm = revert,
  )
}
