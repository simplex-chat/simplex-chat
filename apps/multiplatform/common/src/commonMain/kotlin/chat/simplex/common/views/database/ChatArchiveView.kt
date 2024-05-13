package chat.simplex.common.views.database

import SectionBottomSpacer
import SectionTextFooter
import SectionView
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.SimpleXTheme
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.res.MR
import kotlinx.datetime.*
import java.io.File
import java.net.URI
import java.text.SimpleDateFormat
import java.util.*

@Composable
fun ChatArchiveView(m: ChatModel, title: String, archiveName: String, archiveTime: Instant) {
  val archivePath = filesDir.absolutePath + File.separator + archiveName
  val saveArchiveLauncher = rememberFileChooserLauncher(false) {  to: URI? ->
    if (to != null) {
      copyFileToFile(File(archivePath), to) {}
    }
  }
  ChatArchiveLayout(
    title,
    archiveTime,
    saveArchive = { withLongRunningApi { saveArchiveLauncher.launch(archivePath.substringAfterLast(File.separator)) }},
    deleteArchiveAlert = { deleteArchiveAlert(m, archivePath) }
  )
}

@Composable
fun ChatArchiveLayout(
  title: String,
  archiveTime: Instant,
  saveArchive: () -> Unit,
  deleteArchiveAlert: () -> Unit
) {
  ColumnWithScrollBar(
    Modifier.fillMaxWidth(),
  ) {
    AppBarTitle(title)
    SectionView(stringResource(MR.strings.chat_archive_section)) {
      SettingsActionItem(
        painterResource(MR.images.ic_ios_share),
        stringResource(MR.strings.save_archive),
        saveArchive,
        textColor = MaterialTheme.colors.primary,
        iconColor = MaterialTheme.colors.primary,
      )
      SettingsActionItem(
        painterResource(MR.images.ic_delete),
        stringResource(MR.strings.delete_archive),
        deleteArchiveAlert,
        textColor = Color.Red,
        iconColor = Color.Red,
      )
    }
    val archiveTs = SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z", Locale.US).format(Date.from(archiveTime.toJavaInstant()))
    SectionTextFooter(
      String.format(generalGetString(MR.strings.archive_created_on_ts), archiveTs)
    )
    SectionBottomSpacer()
  }
}

private fun deleteArchiveAlert(m: ChatModel, archivePath: String) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.delete_chat_archive_question),
    confirmText = generalGetString(MR.strings.delete_verb),
    onConfirm = {
      val fileDeleted = File(archivePath).delete()
      if (fileDeleted) {
        m.controller.appPrefs.chatArchiveName.set(null)
        m.controller.appPrefs.chatArchiveTime.set(null)
        ModalManager.start.closeModal()
      } else {
        Log.e(TAG, "deleteArchiveAlert delete() error")
      }
    },
    destructive = true,
  )
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
fun PreviewChatArchiveLayout() {
  SimpleXTheme {
    ChatArchiveLayout(
      "New database archive",
      archiveTime = Clock.System.now(),
      saveArchive = {},
      deleteArchiveAlert = {}
    )
  }
}
