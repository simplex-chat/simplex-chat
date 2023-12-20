package chat.simplex.common.views.chat.item

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.unit.dp
import chat.simplex.res.MR

@Composable
fun CIBrokenComposableView(alignment: Alignment) {
  Box(Modifier.fillMaxWidth().padding(horizontal = 10.dp, vertical = 6.dp), contentAlignment = alignment) {
    Text(stringResource(MR.strings.error_showing_message), color = MaterialTheme.colors.error, fontStyle = FontStyle.Italic)
  }
}
