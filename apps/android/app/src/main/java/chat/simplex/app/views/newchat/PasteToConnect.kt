package chat.simplex.app.views.newchat

import android.content.res.Configuration
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.generalGetString

@Composable
fun PasteToConnect(connectViaLink: (String) -> Unit) {
  fun pasteFromClipboard() : String {
    return ""
  }

  var address = remember { mutableStateOf<String?>(null) }
  Row {
    BasicTextField(
      value = address.value ?: "",
      onValueChange = {s-> address.value = s},
      decorationBox = { innerTextField ->
        Surface(
          shape = RoundedCornerShape(18.dp),
          border = BorderStroke(1.dp, MaterialTheme.colors.secondary)
        ) {
          Row(
            Modifier.background(MaterialTheme.colors.background),
            verticalAlignment = Alignment.Bottom
          ) {
            Box(
              Modifier
                .weight(1f)
                .padding(horizontal = 12.dp)
                .padding(top = 5.dp)
                .padding(bottom = 7.dp)
            ) {
              innerTextField()
            }
            val color = MaterialTheme.colors.primary
            Icon(
              if (address.value == null) Icons.Outlined.ContentPaste else Icons.Outlined.Check,
              generalGetString(if (address.value == null) R.string.icon_paste_from_clipboard else R.string.icon_checkmark_action),
              tint = Color.White,
              modifier = Modifier
                .size(36.dp)
                .padding(4.dp)
                .clip(CircleShape)
                .background(color)
                .clickable {
                  if (address.value == null) {
                    address.value = pasteFromClipboard()
                  } else {
                    address.value?.let(connectViaLink)
                  }
                }
            )
          }
        }
      }
    )
  }
}


@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  name = "Dark Mode"
)
@Composable
fun PreviewDeletedItemView() {
  SimpleXTheme {
    PasteToConnect { _ -> }
  }
}