package chat.simplex.app.views.database

import android.content.Context
import android.content.res.Configuration
import android.net.Uri
import android.widget.Toast
import androidx.activity.compose.ManagedActivityResultLauncher
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.usersettings.SettingsActionItem
import chat.simplex.app.views.usersettings.SettingsSectionView
import kotlinx.datetime.Clock
import kotlinx.datetime.Instant
import java.io.BufferedOutputStream
import java.io.File

@Composable
fun ChatArchiveView(m: ChatModel, title: String, archiveName: String) {
  val context = LocalContext.current
  val archivePath = "${getFilesDirectory(context)}/$archiveName"
  val chatArchiveName = remember { mutableStateOf(m.controller.appPrefs.chatArchiveName.get()) }
  val chatArchiveTime = remember { mutableStateOf(m.controller.appPrefs.chatArchiveTime.get()) }
  val saveArchiveLauncher = rememberSaveArchiveLauncher(cxt = context, archivePath)
  ChatArchiveLayout(
    title,
    chatArchiveTime,
    saveArchive = { saveArchiveLauncher.launch(archivePath.substringAfterLast("/")) },
    deleteArchiveAlert = { deleteArchiveAlert(m, archivePath, chatArchiveName, chatArchiveTime) }
  )
}

@Composable
fun ChatArchiveLayout(
  title: String,
  chatArchiveTime: MutableState<Instant?>,
  saveArchive: () -> Unit,
  deleteArchiveAlert: () -> Unit
) {
  Column(
    Modifier.fillMaxWidth(),
    horizontalAlignment = Alignment.Start,
  ) {
    @Composable fun divider() = Divider(Modifier.padding(horizontal = 8.dp))
    Text(
      title,
      Modifier.padding(start = 16.dp, bottom = 24.dp),
      style = MaterialTheme.typography.h1
    )

    SettingsSectionView(stringResource(R.string.chat_database_section)) {
      SettingsActionItem(
        Icons.Outlined.IosShare,
        stringResource(R.string.save_archive),
        deleteArchiveAlert,
        textColor = MaterialTheme.colors.primary
      )
      divider()
      SettingsActionItem(
        Icons.Outlined.Delete,
        stringResource(R.string.delete_archive),
        saveArchive,
        textColor = Color.Red
      )
    }
    if (chatArchiveTime.value != null) {
      SettingsSectionFooter(
        String.format(generalGetString(R.string.archive_created_on_ts), chatArchiveTime.value.toString())
      )
    }
  }
}

@Composable
private fun rememberSaveArchiveLauncher(cxt: Context, chatArchivePath: String): ManagedActivityResultLauncher<String, Uri?> =
  rememberLauncherForActivityResult(
    contract = ActivityResultContracts.CreateDocument(),
    onResult = { destination ->
      destination?.let {
        val contentResolver = cxt.contentResolver
        contentResolver.openOutputStream(destination)?.let { stream ->
          val outputStream = BufferedOutputStream(stream)
          val file = File(chatArchivePath)
          outputStream.write(file.readBytes())
          outputStream.close()
          Toast.makeText(cxt, generalGetString(R.string.file_saved), Toast.LENGTH_SHORT).show()
        }
      }
    }
  )

private fun deleteArchiveAlert(m: ChatModel, archivePath: String, chatArchiveName: MutableState<String?>, chatArchiveTime: MutableState<Instant?>) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.delete_chat_archive_question),
    confirmText = generalGetString(R.string.delete_verb),
    onConfirm = {
      File(archivePath).delete()
      chatArchiveName.value = null
      chatArchiveTime.value = null
    }
  )
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewChatArchiveLayout() {
  SimpleXTheme {
    ChatArchiveLayout(
      title = "New database archive",
      chatArchiveTime = remember { mutableStateOf(Clock.System.now()) },
      saveArchive = {},
      deleteArchiveAlert = {}
    )
  }
}
