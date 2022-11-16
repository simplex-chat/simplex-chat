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
fun CIAudioView(
  duration: Int,
  file: CIFile?,
  edited: Boolean,
  receiveFile: (Long) -> Unit
) {
  val context = LocalContext.current
  val saveFileLauncher = rememberSaveFileLauncher(cxt = context, ciFile = file)

  val audioPlaying = rememberSaveable { mutableStateOf(false) }
  val audioInfo = remember(file) {
    if (file != null) {
      if (file.audioInfo == null) file.audioInfo = mutableStateOf(ProgressAndDuration(duration = duration))
      file.audioInfo!!
    } else mutableStateOf(ProgressAndDuration(duration = duration))
  }
  val play = play@ {
    audioPlaying.value = AudioPlayer.start(getAppFilePath(SimplexApp.context, file?.filePath ?: return@play), audioInfo.value.progress) {
      audioPlaying.value = false
    }
  }
  val pause = {
    audioInfo.value = ProgressAndDuration(AudioPlayer.pause(), audioInfo.value.duration)
    audioPlaying.value = false
  }
  val filePath = remember(file?.fileId, file?.fileStatus) { getLoadedFilePath(context, file) }
  MiniAudioPlayer(filePath, audioPlaying, audioInfo)

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
      if (file.loaded) {
        if (!audioPlaying.value) play() else pause()
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
        if (file.loaded) {
          Box(
            contentAlignment = Alignment.Center
          ) {
            Icon(
              if (audioPlaying.value) Icons.Filled.Pause else Icons.Filled.PlayArrow,
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
          stringResource(R.string.voice_message),
          maxLines = 1
        )
        val time = if (audioPlaying.value) audioInfo.value.progress else audioInfo.value.duration
        val mins = time / 1000 / 60
        val secs = time / 1000 % 60
        Text(
          String.format("%02d:%02d", mins, secs),
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

@Composable
fun MiniAudioPlayer(filePath: String?, playing: MutableState<Boolean>, info: MutableState<ProgressAndDuration>): Pair<MutableState<ProgressAndDuration>, MutableState<Boolean>> {
  val audioInfo = remember { info }
  val audioPlaying = rememberSaveable { playing }
  LaunchedEffect(filePath) {
    if (filePath != null && audioInfo.value.duration == 0) {
        audioInfo.value = ProgressAndDuration(audioInfo.value.progress, AudioPlayer.duration(filePath))
    }
  }
  LaunchedEffect(audioPlaying.value) {
    while (isActive && audioPlaying.value) {
      audioInfo.value = AudioPlayer.progressAndDurationOrEnded()
      if (audioInfo.value.progress == audioInfo.value.duration) {
        audioInfo.value = ProgressAndDuration(0, audioInfo.value.duration)
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
  return audioInfo to audioPlaying
}
