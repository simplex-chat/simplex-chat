package chat.simplex.app.ui.theme

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Share
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp

@Composable
fun SimpleButton(text: String, icon: ImageVector,
                 color: Color = MaterialTheme.colors.primary,
                 click: () -> Unit) {
  SimpleButtonFrame(click) {
    Icon(
      icon, text, tint = color,
      modifier = Modifier.padding(end = 8.dp)
    )
    Text(text, style = MaterialTheme.typography.caption, color = color)
  }
}

@Composable
fun SimpleButtonFrame(click: () -> Unit, disabled: Boolean = false, content: @Composable () -> Unit) {
  Surface(shape = RoundedCornerShape(20.dp)) {
    val modifier = if (disabled) Modifier else Modifier.clickable { click() }
    Row(
      verticalAlignment = Alignment.CenterVertically,
      modifier = modifier.padding(8.dp)
    ) { content() }
  }
}

@Preview
@Composable
fun PreviewCloseSheetBar() {
  SimpleXTheme {
    SimpleButton(text = "Share", icon = Icons.Outlined.Share, click = {})
  }
}
