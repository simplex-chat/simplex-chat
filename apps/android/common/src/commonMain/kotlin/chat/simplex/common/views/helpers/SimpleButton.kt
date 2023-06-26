package chat.simplex.common.views.helpers

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import dev.icerock.moko.resources.compose.painterResource
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextDecoration
import androidx.compose.ui.unit.dp
import chat.simplex.common.ui.theme.SimpleXTheme
import com.icerockdev.library.MR

@Composable
fun SimpleButton(text: String, icon: Painter,
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
fun SimpleButtonDecorated(text: String, icon: Painter,
  color: Color = MaterialTheme.colors.primary,
  textDecoration: TextDecoration = TextDecoration.Underline,
  fontWeight: FontWeight = FontWeight.Normal,
  click: () -> Unit) {
  SimpleButtonFrame(click) {
    Icon(
      icon, text, tint = color,
      modifier = Modifier.padding(end = 8.dp)
    )
    Text(text, style = MaterialTheme.typography.caption, fontWeight = fontWeight, color = color, textDecoration = textDecoration)
  }
}

@Composable
fun SimpleButton(
  text: String, icon: Painter,
  color: Color = MaterialTheme.colors.primary,
  disabled: Boolean,
  click: () -> Unit
) {
  SimpleButtonFrame(click, disabled = disabled) {
    Icon(
      icon, text, tint = if (disabled) MaterialTheme.colors.secondary else color,
      modifier = Modifier.padding(end = 8.dp)
    )
    Text(text, style = MaterialTheme.typography.caption, color = if (disabled) MaterialTheme.colors.secondary else color)
  }
}

@Composable
fun SimpleButtonIconEnded(
  text: String,
  icon: Painter,
  color: Color = MaterialTheme.colors.primary,
  click: () -> Unit
) {
  SimpleButtonFrame(click) {
    Text(text, style = MaterialTheme.typography.caption, color = color)
    Icon(
      icon, text, tint = color,
      modifier = Modifier.padding(start = 8.dp)
    )
  }
}

@Composable
fun SimpleButtonFrame(click: () -> Unit, modifier: Modifier = Modifier, disabled: Boolean = false, content: @Composable RowScope.() -> Unit) {
  Box(Modifier.clip(RoundedCornerShape(20.dp))) {
    val modifier = if (disabled) modifier else modifier.clickable { click() }
    Row(
      verticalAlignment = Alignment.CenterVertically,
      modifier = modifier.padding(8.dp)
    ) { content() }
  }
}

@Preview
@Composable
fun PreviewShareButton() {
  SimpleXTheme {
    SimpleButton(text = "Share", icon = painterResource(MR.images.ic_share), click = {})
  }
}
