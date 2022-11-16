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
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.item.MiniAudioPlayer
import chat.simplex.app.views.chat.item.SentColorLight
import chat.simplex.app.views.helpers.*

@Composable
fun ComposeAudioView(filePath: String, duration: Int, cancelFile: () -> Unit, cancelEnabled: Boolean) {
  Row(
    Modifier
      .height(60.dp)
      .fillMaxWidth()
      .padding(top = 8.dp)
      .background(SentColorLight),
    verticalAlignment = Alignment.CenterVertically
  ) {

    val audioPlaying = rememberSaveable { mutableStateOf(false) }
    val audioInfo = remember { mutableStateOf(ProgressAndDuration(duration = duration)) }
    val play = play@ {
      audioPlaying.value = AudioPlayer.start(filePath, audioInfo.value.progress) {
        audioPlaying.value = false
      }
    }
    val pause = {
      audioInfo.value = ProgressAndDuration(AudioPlayer.pause(), audioInfo.value.duration)
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
        tint = if (isInDarkTheme()) FileDark else FileLight
      )
    }
    Text(filePath)
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
