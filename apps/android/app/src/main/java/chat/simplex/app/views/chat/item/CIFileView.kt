package chat.simplex.app.views.chat.item

import android.widget.Toast
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.*
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import kotlinx.coroutines.delay
import kotlinx.coroutines.isActive
import kotlinx.datetime.Clock
import java.io.File

@Composable
fun CIFileView(
  file: CIFile?,
  edited: Boolean,
  receiveFile: (Long) -> Unit
) {
  val context = LocalContext.current
  val saveFileLauncher = rememberSaveFileLauncher(cxt = context, ciFile = file)
  val voiceInfo = remember { mutableStateOf(0 to 0) }
  val voicePlaying = rememberSaveable { mutableStateOf(false) }
  LaunchedEffect(file?.fileName, file?.fileStatus) {
    if (file != null && file.loaded && file.isVoiceMessage() && voiceInfo.value.second == 0) {
      val filePath = getLoadedFilePath(context, file)
      if (filePath != null && File(filePath).exists()) {
        voiceInfo.value = voiceInfo.value.first to AudioPlayer.duration(filePath)
      }
    }
  }

  LaunchedEffect(voicePlaying.value) {
    if (voicePlaying.value) {
      while (isActive && voicePlaying.value) {
        voiceInfo.value = AudioPlayer.progressAndDuration()
        if (voiceInfo.value.first == voiceInfo.value.second) {
          voiceInfo.value = 0 to voiceInfo.value.second
          voicePlaying.value = false
        }
        delay(100)
      }
    }
  }

  LaunchedEffect(AudioPlayer.currentlyPlaying.value) {
    val current = AudioPlayer.currentlyPlaying.value ?: return@LaunchedEffect
    file?.filePath ?: return@LaunchedEffect
    if (!current.endsWith(File.separator + file.filePath) && voicePlaying.value) {
      voicePlaying.value = false
    }
  }

  @Composable
  fun fileIcon(
    innerIcon: ImageVector? = null,
    color: Color = if (isInDarkTheme()) FileDark else FileLight
  ) {
    Box(
      contentAlignment = Alignment.Center
    ) {
      Icon(
        Icons.Filled.InsertDriveFile,
        stringResource(R.string.icon_descr_file),
        Modifier.fillMaxSize(),
        tint = color
      )
      if (innerIcon != null) {
        Icon(
          innerIcon,
          stringResource(R.string.icon_descr_file),
          Modifier
            .size(32.dp)
            .padding(top = 12.dp),
          tint = Color.White
        )
      }
    }
  }

  fun fileSizeValid(): Boolean {
    if (file != null) {
      return file.fileSize <= MAX_FILE_SIZE
    }
    return false
  }

  fun fileAction() {
    if (file != null) {
      if (file.isVoiceMessage() && file.loaded) {
        if (!voicePlaying.value) {
          AudioPlayer.start(getAppFilePath(context, file.filePath ?: return), voiceInfo.value.first)
        } else {
          voiceInfo.value = AudioPlayer.pause() to voiceInfo.value.second
        }
        voicePlaying.value = !voicePlaying.value
        return
      }
      when (file.fileStatus) {
        CIFileStatus.RcvInvitation -> {
          if (fileSizeValid()) {
            receiveFile(file.fileId)
          } else {
            AlertManager.shared.showAlertMsg(
              generalGetString(R.string.large_file),
              String.format(generalGetString(R.string.contact_sent_large_file), formatBytes(MAX_FILE_SIZE))
            )
          }
        }
        CIFileStatus.RcvAccepted ->
          AlertManager.shared.showAlertMsg(
            generalGetString(R.string.waiting_for_file),
            String.format(generalGetString(R.string.file_will_be_received_when_contact_is_online), MAX_FILE_SIZE)
          )
        CIFileStatus.RcvComplete -> {
          val filePath = getLoadedFilePath(context, file)
          if (filePath != null) {
            saveFileLauncher.launch(file.fileName)
          } else {
            Toast.makeText(context, generalGetString(R.string.file_not_found), Toast.LENGTH_SHORT).show()
          }
        }
        else -> {}
      }
    }
  }

  @Composable
  fun progressIndicator() {
    CircularProgressIndicator(
      Modifier.size(32.dp),
      color = if (isInDarkTheme()) FileDark else FileLight,
      strokeWidth = 4.dp
    )
  }

  @Composable
  fun fileIndicator() {
    Box(
      Modifier
        .size(42.dp)
        .clip(RoundedCornerShape(4.dp))
        .clickable(onClick = { fileAction() }),
      contentAlignment = Alignment.Center
    ) {
      if (file != null) {
        if (file.isVoiceMessage() && file.loaded) {
          Box(
            contentAlignment = Alignment.Center
          ) {
            Icon(
              if (voicePlaying.value) Icons.Filled.Pause else Icons.Filled.PlayArrow,
              stringResource(R.string.icon_descr_file),
              Modifier.size(36.dp),
              tint = HighOrLowlight,
            )
          }
        } else {
          when (file.fileStatus) {
            CIFileStatus.SndStored -> fileIcon()
            CIFileStatus.SndTransfer -> progressIndicator()
            CIFileStatus.SndComplete -> fileIcon(innerIcon = Icons.Filled.Check)
            CIFileStatus.SndCancelled -> fileIcon(innerIcon = Icons.Outlined.Close)
            CIFileStatus.RcvInvitation ->
              if (fileSizeValid())
                fileIcon(innerIcon = Icons.Outlined.ArrowDownward, color = MaterialTheme.colors.primary)
              else
                fileIcon(innerIcon = Icons.Outlined.PriorityHigh, color = WarningOrange)
            CIFileStatus.RcvAccepted -> fileIcon(innerIcon = Icons.Outlined.MoreHoriz)
            CIFileStatus.RcvTransfer -> progressIndicator()
            CIFileStatus.RcvComplete -> fileIcon()
            CIFileStatus.RcvCancelled -> fileIcon(innerIcon = Icons.Outlined.Close)
          }
        }
      } else {
        fileIcon()
      }
    }
  }

  Row(
    Modifier.padding(top = 4.dp, bottom = 6.dp, start = 6.dp, end = 12.dp),
    verticalAlignment = Alignment.Bottom,
    horizontalArrangement = Arrangement.spacedBy(2.dp)
  ) {
    fileIndicator()
    val metaReserve = if (edited)
      "                     "
    else
      "                 "
    if (file != null) {
      Column(
        horizontalAlignment = Alignment.Start
      ) {
        Text(
          if (file.isVoiceMessage()) stringResource(R.string.voice_message) else file.fileName,
          maxLines = 1
        )
        val text = if (file.isVoiceMessage()) {
          val time = if (voicePlaying.value) voiceInfo.value.first else voiceInfo.value.second
          val mins = time / 1000 / 60
          val secs = time / 1000 % 60
          String.format("%02d:%02d", mins, secs)
        } else formatBytes(file.fileSize) + metaReserve
        Text(
          text,
          color = HighOrLowlight,
          fontSize = 14.sp,
          maxLines = 1
        )
      }
    } else {
      Text(metaReserve)
    }
  }
}

class ChatItemProvider: PreviewParameterProvider<ChatItem> {
  private val sentFile = ChatItem(
    chatDir = CIDirection.DirectSnd(),
    meta = CIMeta.getSample(1, Clock.System.now(), "", CIStatus.SndSent(), itemDeleted = false, itemEdited = true, editable = false),
    content = CIContent.SndMsgContent(msgContent = MsgContent.MCFile("")),
    quotedItem = null,
    file = CIFile.getSample(fileStatus = CIFileStatus.SndComplete)
  )
  private val fileChatItemWtFile = ChatItem(
    chatDir = CIDirection.DirectRcv(),
    meta = CIMeta.getSample(1, Clock.System.now(), "", CIStatus.RcvRead(), itemDeleted = false, itemEdited = false, editable = false),
    content = CIContent.RcvMsgContent(msgContent = MsgContent.MCFile("")),
    quotedItem = null,
    file = null
  )
  override val values = listOf(
    sentFile,
    ChatItem.getFileMsgContentSample(),
    ChatItem.getFileMsgContentSample(fileName = "some_long_file_name_here", fileStatus = CIFileStatus.RcvInvitation),
    ChatItem.getFileMsgContentSample(fileStatus = CIFileStatus.RcvAccepted),
    ChatItem.getFileMsgContentSample(fileStatus = CIFileStatus.RcvTransfer),
    ChatItem.getFileMsgContentSample(fileStatus = CIFileStatus.RcvCancelled),
    ChatItem.getFileMsgContentSample(fileSize = 1_000_000_000, fileStatus = CIFileStatus.RcvInvitation),
    ChatItem.getFileMsgContentSample(text = "Hello there", fileStatus = CIFileStatus.RcvInvitation),
    ChatItem.getFileMsgContentSample(text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.", fileStatus = CIFileStatus.RcvInvitation),
    fileChatItemWtFile
  ).asSequence()
}

@Preview
@Composable
fun PreviewCIFileFramedItemView(@PreviewParameter(ChatItemProvider::class) chatItem: ChatItem) {
  val showMenu = remember { mutableStateOf(false) }
  SimpleXTheme {
    FramedItemView(ChatInfo.Direct.sampleData, chatItem, showMenu = showMenu, receiveFile = {})
  }
}
