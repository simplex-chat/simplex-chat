package chat.simplex.common.views.chat

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.common.ui.theme.*
import chat.simplex.res.*

@Composable
fun ComposeFileView(fileName: String, cancelFile: () -> Unit, cancelEnabled: Boolean) {
  Row(
    Modifier
      .height(52.dp)
      .fillMaxWidth()
      .padding(start = 12.dp, end = 4.dp),
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(
      painterResource(MR.images.ic_draft_filled),
      stringResource(MR.strings.icon_descr_file),
      Modifier
        .padding(end = 10.dp)
        .size(32.dp),
      tint = if (isInDarkTheme()) FileDark else FileLight
    )
    Text(
      fileName,
      maxLines = 1,
      overflow = TextOverflow.Ellipsis,
      fontWeight = FontWeight.Medium,
      color = MaterialTheme.colors.onSurface,
      modifier = Modifier.weight(1f)
    )
    if (cancelEnabled) {
      IconButton(onClick = cancelFile, modifier = Modifier.padding(0.dp)) {
        Icon(
          painterResource(MR.images.ic_close),
          contentDescription = stringResource(MR.strings.icon_descr_cancel_file_preview),
          tint = MaterialTheme.colors.primary,
          modifier = Modifier.padding(10.dp)
        )
      }
    }
  }
}

@Preview
@Composable
fun PreviewComposeFileView() {
  SimpleXTheme {
    ComposeFileView(
      "test.txt",
      cancelFile = {},
      cancelEnabled = true
    )
  }
}
