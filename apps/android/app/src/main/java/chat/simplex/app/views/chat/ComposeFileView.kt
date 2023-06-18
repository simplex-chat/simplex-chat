import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.collectAsState
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.ui.theme.*

@Composable
fun ComposeFileView(fileName: String, cancelFile: () -> Unit, cancelEnabled: Boolean) {
  val sentColor = CurrentColors.collectAsState().value.appColors.sentMessage
  Row(
    Modifier
      .height(60.dp)
      .fillMaxWidth()
      .padding(top = 8.dp)
      .background(sentColor),
    verticalAlignment = Alignment.CenterVertically
  ) {
    Icon(
      painterResource(R.drawable.ic_draft_filled),
      stringResource(R.string.icon_descr_file),
      Modifier
        .padding(start = 4.dp, end = 2.dp)
        .size(36.dp),
      tint = if (isInDarkTheme()) FileDark else FileLight
    )
    Text(fileName)
    Spacer(Modifier.weight(1f))
    if (cancelEnabled) {
      IconButton(onClick = cancelFile, modifier = Modifier.padding(0.dp)) {
        Icon(
          painterResource(R.drawable.ic_close),
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
fun PreviewComposeFileView() {
  SimpleXTheme {
    ComposeFileView(
      "test.txt",
      cancelFile = {},
      cancelEnabled = true
    )
  }
}
