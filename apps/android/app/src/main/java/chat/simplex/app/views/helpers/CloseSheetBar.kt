package chat.simplex.app.views.helpers

import android.content.res.Configuration
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Close
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.ui.theme.*

@Composable
fun CloseSheetBar(title: String? = null, close: () -> Unit) {
  Row (
    Modifier
      .fillMaxWidth()
      .height(60.dp),
    horizontalArrangement = if (title != null) Arrangement.SpaceBetween else Arrangement.End,
    verticalAlignment = Alignment.CenterVertically
  ) {
    if (title != null) {
      Text(
        title,
        style = MaterialTheme.typography.h6.copy(fontWeight = FontWeight.Normal),
        modifier = Modifier.padding(horizontal = DEFAULT_PADDING).weight(1f),
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
      )
    }
    IconButton(onClick = close) {
      Icon(
        Icons.Outlined.Close,
        stringResource(R.string.icon_descr_close_button),
        tint = MaterialTheme.colors.primary,
        modifier = Modifier.padding(10.dp)
      )
    }
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewCloseSheetBar() {
  SimpleXTheme {
    CloseSheetBar("Test", close = {})
  }
}
