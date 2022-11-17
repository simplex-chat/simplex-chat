package chat.simplex.app.views.chat.item

import android.net.Uri
import android.widget.Toast
import androidx.activity.compose.ManagedActivityResultLauncher
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
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.SimplexApp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import kotlinx.coroutines.delay
import kotlinx.coroutines.isActive

@Composable
fun CIVoiceView(
  durationSec: Int,
  file: CIFile?,
  edited: Boolean,
  sent: Boolean,
  hasText: Boolean,
  ci: ChatItem,
  metaColor: Color,
  receiveFile: (Long) -> Unit
) {
  Row(
    Modifier.padding(top = 4.dp, bottom = 6.dp, start = 6.dp, end = 12.dp),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.spacedBy(2.dp)
  ) {
    if (file != null) {
      val context = LocalContext.current
      val audioPlaying = rememberSaveable { mutableStateOf(false) }
      val audioInfo = remember(file.filePath) {
        file.audioInfo.value = file.audioInfo.value.copy(durationMs = durationSec * 1000)
        file.audioInfo
      }
      val play = play@{
        audioPlaying.value = AudioPlayer.start(getAppFilePath(SimplexApp.context, file.filePath ?: return@play), audioInfo.value.progressMs) {
          audioPlaying.value = false
        }
      }
      val pause = {
        audioInfo.value = ProgressAndDuration(AudioPlayer.pause(), audioInfo.value.durationMs)
        audioPlaying.value = false
      }
      val filePath = remember(file.filePath, file.fileStatus) { getLoadedFilePath(context, file) }
      MiniAudioPlayer(filePath, audioPlaying, audioInfo)

      val time = if (audioPlaying.value) audioInfo.value.progressMs else audioInfo.value.durationMs
      val minWidth = with(LocalDensity.current) { if (hasText) 50.sp.toDp() else 40.sp.toDp() }
      val text = String.format("%02d:%02d", time / 1000 / 60, time / 1000 % 60)
      if (hasText) {
        fileIndicator(file, audioPlaying.value, sent, hasText, receiveFile, play, pause)
        Text(
          text,
          Modifier.widthIn(min = minWidth),
          color = HighOrLowlight,
          fontSize = 16.sp,
          textAlign = TextAlign.End,
          maxLines = 1
        )
      } else {
        if (sent) {
          Row {
            Row(verticalAlignment = Alignment.CenterVertically) {
              Spacer(Modifier.height(56.dp))
              Text(
                text,
                Modifier
                  .widthIn(min = minWidth)
                  .padding(start = 0.dp, end = 10.dp),
                color = HighOrLowlight,
                fontSize = 16.sp,
                textAlign = TextAlign.End,
                maxLines = 1
              )
            }
            Column {
              fileIndicator(file, audioPlaying.value, sent, hasText, receiveFile, play, pause)
              Box(Modifier.align(Alignment.CenterHorizontally)) {
                CIMetaView(ci, metaColor)
              }
            }
          }
        } else {
          Row {
            Column {
              fileIndicator(file, audioPlaying.value, sent, hasText, receiveFile, play, pause)
              Box(Modifier.align(Alignment.CenterHorizontally)) {
                CIMetaView(ci, metaColor)
              }
            }
            Row(verticalAlignment = Alignment.CenterVertically) {
              Text(
                text,
                Modifier
                  .widthIn(min = minWidth)
                  .padding(start = 10.dp, end = 0.dp),
                color = HighOrLowlight,
                fontSize = 16.sp,
                textAlign = TextAlign.End,
                maxLines = 1
              )
              Spacer(Modifier.height(56.dp))
            }
          }
        }
      }
    } else {
      fileIndicator(null, false, sent, hasText, receiveFile, {}, {})
      val metaReserve = if (edited)
        "                     "
      else
        "                 "
      Text(metaReserve)
    }
  }
}

@Composable
private fun fileIcon(
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

private fun fileAction(
  file: CIFile?,
  receiveFile: (Long) -> Unit,
  saveFileLauncher: ManagedActivityResultLauncher<String, Uri?>
) {
  if (file != null) {
    when (file.fileStatus) {
      CIFileStatus.RcvInvitation -> {
        if (fileSizeValid(file)) {
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
        val filePath = getLoadedFilePath(SimplexApp.context, file)
        if (filePath != null) {
          saveFileLauncher.launch(file.fileName)
        } else {
          Toast.makeText(SimplexApp.context, generalGetString(R.string.file_not_found), Toast.LENGTH_SHORT).show()
        }
      }
      else -> {}
    }
  }
}

@Composable
private fun fileIndicator(
  file: CIFile?,
  audioPlaying: Boolean,
  sent: Boolean,
  hasText: Boolean,
  receiveFile: (Long) -> Unit,
  play: () -> Unit,
  pause: () -> Unit
) {
  val saveFileLauncher = rememberSaveFileLauncher(cxt = LocalContext.current, ciFile = file)
  if (file != null && file.loaded) {
    if (hasText) {
      IconButton({ if (!audioPlaying) play() else pause() },) {
        Icon(
          imageVector = if (audioPlaying) Icons.Filled.Pause else Icons.Filled.PlayArrow,
          contentDescription = null,
          Modifier.size(36.dp),
          tint = MaterialTheme.colors.primary
        )
      }
    } else {
      FloatingActionButton(
        onClick = { if (!audioPlaying) play() else pause() },
        elevation = FloatingActionButtonDefaults.elevation(0.dp, 0.dp, 0.dp, 0.dp),
        backgroundColor = if (sent) SentColorLight else ReceivedColorLight
      ) {
        Icon(
          imageVector = if (audioPlaying) Icons.Filled.Pause else Icons.Filled.PlayArrow,
          contentDescription = null,
          Modifier.size(36.dp),
          tint = MaterialTheme.colors.primary
        )
      }
    }
  } else {
    Box(
      Modifier
        .size(42.dp)
        .clip(RoundedCornerShape(4.dp))
        .clickable(onClick = {
          fileAction(file, receiveFile, saveFileLauncher)
        }),
      contentAlignment = Alignment.Center
    ) {
      if (file != null) {
        when (file.fileStatus) {
          CIFileStatus.SndStored -> fileIcon()
          CIFileStatus.SndTransfer -> progressIndicator()
          CIFileStatus.SndComplete -> fileIcon(innerIcon = Icons.Filled.Check)
          CIFileStatus.SndCancelled -> fileIcon(innerIcon = Icons.Outlined.Close)
          CIFileStatus.RcvInvitation ->
            if (fileSizeValid(file))
              fileIcon(innerIcon = Icons.Outlined.ArrowDownward, color = MaterialTheme.colors.primary)
            else
              fileIcon(innerIcon = Icons.Outlined.PriorityHigh, color = WarningOrange)

          CIFileStatus.RcvAccepted -> fileIcon(innerIcon = Icons.Outlined.MoreHoriz)
          CIFileStatus.RcvTransfer -> progressIndicator()
          CIFileStatus.RcvComplete -> fileIcon()
          CIFileStatus.RcvCancelled -> fileIcon(innerIcon = Icons.Outlined.Close)
        }
      } else {
        fileIcon()
      }
    }
  }
}

@Composable
private fun progressIndicator() {
  CircularProgressIndicator(
    Modifier.size(32.dp),
    color = if (isInDarkTheme()) FileDark else FileLight,
    strokeWidth = 4.dp
  )
}

private fun fileSizeValid(file: CIFile?): Boolean {
  if (file != null) {
    return file.fileSize <= MAX_FILE_SIZE
  }
  return false
}

@Composable
fun MiniAudioPlayer(
  filePath: String?,
  playing: MutableState<Boolean>,
  info: MutableState<ProgressAndDuration>
) {
  val audioInfo = remember { info }
  val audioPlaying = rememberSaveable { playing }
  LaunchedEffect(filePath) {
    if (filePath != null && audioInfo.value.durationMs == 0) {
      audioInfo.value = ProgressAndDuration(audioInfo.value.progressMs, AudioPlayer.duration(filePath))
    }
  }
  LaunchedEffect(audioPlaying.value) {
    while (isActive && audioPlaying.value) {
      audioInfo.value = AudioPlayer.progressAndDurationOrEnded()
      if (audioInfo.value.progressMs == audioInfo.value.durationMs) {
        audioInfo.value = ProgressAndDuration(0, audioInfo.value.durationMs)
        audioPlaying.value = false
      }
      delay(100)
    }
  }
  LaunchedEffect(AudioPlayer.currentlyPlaying.value) {
    val currentFileName = AudioPlayer.currentlyPlaying.value?.first ?: return@LaunchedEffect
    filePath ?: return@LaunchedEffect
    if (currentFileName != filePath && audioPlaying.value) {
      audioPlaying.value = false
    }
  }
}
