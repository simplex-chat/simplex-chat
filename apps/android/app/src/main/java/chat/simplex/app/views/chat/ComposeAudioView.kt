import androidx.compose.animation.core.Animatable
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.*
import androidx.compose.material.icons.outlined.Close
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.item.MiniAudioPlayer
import chat.simplex.app.views.chat.item.SentColorLight
import chat.simplex.app.views.helpers.*
import kotlinx.coroutines.flow.distinctUntilChanged

@Composable
fun ComposeAudioView(filePath: String, durationMs: Int, finished: Boolean, cancelFile: () -> Unit, cancelEnabled: Boolean) {
  BoxWithConstraints(Modifier
    .fillMaxWidth()
  ) {
    val audioPlaying = rememberSaveable { mutableStateOf(false) }
    val audioInfo = rememberSaveable(saver = ProgressAndDuration.Saver) {
      mutableStateOf(ProgressAndDuration(durationMs = durationMs))
    }
    LaunchedEffect(durationMs) {
      audioInfo.value = audioInfo.value.copy(durationMs = durationMs)
    }
    val progressBarWidth = remember { Animatable(0f) }
    LaunchedEffect(durationMs, finished) {
      snapshotFlow { audioInfo.value }
        .distinctUntilChanged()
        .collect {
          val number = if (audioPlaying.value) audioInfo.value.progressMs else if (!finished) durationMs else 0
          val new = if (audioPlaying.value || finished)
            ((number.toDouble() / durationMs) * maxWidth.value).dp
          else
            (((number.toDouble()) / MAX_VOICE_MILLIS_FOR_SENDING) * maxWidth.value).dp
          progressBarWidth.animateTo(new.value, audioProgressBarAnimationSpec())
        }
    }
    Spacer(
      Modifier
        .requiredWidth(progressBarWidth.value.dp)
        .padding(top = 58.dp)
        .height(2.dp)
        .background(MaterialTheme.colors.primary)
    )
    Row(
      Modifier
        .height(60.dp)
        .fillMaxWidth()
        .padding(top = 8.dp)
        .background(SentColorLight),
      verticalAlignment = Alignment.CenterVertically
    ) {
      val play = play@{
        audioPlaying.value = AudioPlayer.start(filePath, audioInfo.value.progressMs) {
          audioPlaying.value = false
        }
      }
      val pause = {
        audioInfo.value = ProgressAndDuration(AudioPlayer.pause(), audioInfo.value.durationMs)
        audioPlaying.value = false
      }
      MiniAudioPlayer(filePath, audioPlaying, audioInfo)

      IconButton({ if (!audioPlaying.value) play() else pause() }) {
        Icon(
          if (audioPlaying.value) Icons.Filled.Pause else Icons.Filled.PlayArrow,
          stringResource(R.string.icon_descr_file),
          Modifier
            .padding(start = 4.dp, end = 2.dp)
            .size(36.dp),
          tint = MaterialTheme.colors.primary
        )
      }
      val numberInText = remember(durationMs, audioInfo.value) {
        derivedStateOf { if (audioPlaying.value) audioInfo.value.progressMs / 1000 else durationMs / 1000 }
      }
      val text = "%02d:%02d".format(numberInText.value / 60, numberInText.value % 60)
      Text(
        text,
        style = TextStyle.Default.copy(fontSize = 20.sp, fontWeight = FontWeight.Bold),
        color = HighOrLowlight,
      )
      Spacer(Modifier.weight(1f))
      if (cancelEnabled) {
        IconButton(onClick = cancelFile, modifier = Modifier.padding(0.dp)) {
          Icon(
            Icons.Outlined.Close,
            contentDescription = stringResource(R.string.icon_descr_cancel_file_preview),
            tint = MaterialTheme.colors.primary,
            modifier = Modifier.padding(10.dp)
          )
        }
      }
    }
  }
}

@Preview
@Composable
fun PreviewComposeAudioView() {
  SimpleXTheme {
    ComposeFileView(
      "test.txt",
      cancelFile = {},
      cancelEnabled = true
    )
  }
}
