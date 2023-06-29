package chat.simplex.app.views.database

import SectionBottomSpacer
import SectionTextFooter
import SectionView
import android.content.Context
import android.content.res.Configuration
import android.net.Uri
import android.util.Log
import android.widget.Toast
import androidx.activity.compose.ManagedActivityResultLauncher
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import chat.simplex.app.R
import chat.simplex.app.TAG
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.usersettings.*
import kotlinx.datetime.*
import java.io.BufferedOutputStream
import java.io.File
import java.text.SimpleDateFormat
import java.util.*

@Composable
fun ChatArchiveView(m: ChatModel, title: String, archiveName: String, archiveTime: Instant) {
  val context = LocalContext.current
  val archivePath = "${getFilesDirectory(context)}/$archiveName"
  val saveArchiveLauncher = rememberSaveArchiveLauncher(cxt = context, archivePath)
  ChatArchiveLayout(
    title,
    archiveTime,
    saveArchive = { saveArchiveLauncher.launch(archivePath.substringAfterLast("/")) },
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
  Column(
    Modifier.fillMaxWidth(),
  ) {
    AppBarTitle(title)
    SectionView(stringResource(R.string.chat_archive_section)) {
      SettingsActionItem(
        painterResource(R.drawable.ic_ios_share),
        stringResource(R.string.save_archive),
        saveArchive,
        textColor = MaterialTheme.colors.primary,
        iconColor = MaterialTheme.colors.primary,
      )
      SettingsActionItem(
        painterResource(R.drawable.ic_delete),
        stringResource(R.string.delete_archive),
        deleteArchiveAlert,
        textColor = Color.Red,
        iconColor = Color.Red,
      )
    }
    val archiveTs = SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z", Locale.US).format(Date.from(archiveTime.toJavaInstant()))
    SectionTextFooter(
      String.format(generalGetString(R.string.archive_created_on_ts), archiveTs)
    )
    SectionBottomSpacer()
  }
}

@Composable
private fun rememberSaveArchiveLauncher(cxt: Context, chatArchivePath: String): ManagedActivityResultLauncher<String, Uri?> =
  rememberLauncherForActivityResult(
    contract = ActivityResultContracts.CreateDocument(),
    onResult = { destination ->
      try {
        destination?.let {
          val contentResolver = cxt.contentResolver
          contentResolver.openOutputStream(destination)?.let { stream ->
            val outputStream = BufferedOutputStream(stream)
            File(chatArchivePath).inputStream().use { it.copyTo(outputStream) }
            outputStream.close()
            Toast.makeText(cxt, generalGetString(R.string.file_saved), Toast.LENGTH_SHORT).show()
          }
        }
      } catch (e: Error) {
        Toast.makeText(cxt, generalGetString(R.string.error_saving_file), Toast.LENGTH_SHORT).show()
        Log.e(TAG, "rememberSaveArchiveLauncher error saving archive $e")
      }
    }
  )

private fun deleteArchiveAlert(m: ChatModel, archivePath: String) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.delete_chat_archive_question),
    confirmText = generalGetString(R.string.delete_verb),
    onConfirm = {
      val fileDeleted = File(archivePath).delete()
      if (fileDeleted) {
        m.controller.appPrefs.chatArchiveName.set(null)
        m.controller.appPrefs.chatArchiveTime.set(null)
        ModalManager.shared.closeModal()
      } else {
        Log.e(TAG, "deleteArchiveAlert delete() error")
      }
    },
    destructive = true,
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
      "New database archive",
      archiveTime = Clock.System.now(),
      saveArchive = {},
      deleteArchiveAlert = {}
    )
  }
}
