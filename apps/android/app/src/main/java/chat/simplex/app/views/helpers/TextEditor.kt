package chat.simplex.app.views.helpers

import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Surface
import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.SolidColor
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.input.KeyboardCapitalization
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.ui.theme.HighOrLowlight

@Composable
fun TextEditor(modifier: Modifier, text: MutableState<String>) {
  BasicTextField(
    value = text.value,
    onValueChange = { text.value = it },
    textStyle = TextStyle(
      fontFamily = FontFamily.Monospace, fontSize = 14.sp,
      color = MaterialTheme.colors.onBackground
    ),
    keyboardOptions = KeyboardOptions.Default.copy(
      capitalization = KeyboardCapitalization.None,
      autoCorrect = false
    ),
    modifier = modifier,
    cursorBrush = SolidColor(HighOrLowlight),
    decorationBox = { innerTextField ->
      Surface(
        shape = RoundedCornerShape(10.dp),
        border = BorderStroke(1.dp, MaterialTheme.colors.secondary)
      ) {
        Row(
          Modifier.background(MaterialTheme.colors.background),
          verticalAlignment = Alignment.Top
        ) {
          Box(
            Modifier
              .weight(1f)
              .padding(vertical = 5.dp, horizontal = 7.dp)
          ) {
            innerTextField()
          }
        }
      }
    }
  )
}
