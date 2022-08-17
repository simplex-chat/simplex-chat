package chat.simplex.app.views.reusable

import androidx.annotation.StringRes
import androidx.compose.foundation.layout.padding
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.capitalize
import androidx.compose.ui.text.intl.Locale
import androidx.compose.ui.unit.dp

val AppBarHeight = 56.dp

@Composable
fun DefaultDropdownMenuItem(icon: ImageVector, @StringRes textId: Int, modifier: Modifier = Modifier, onClick: () -> Unit) {
  DropdownMenuItem(
    onClick = onClick
  ) {
    Icon(icon, stringResource(textId), modifier.padding(end = 15.dp), tint = MaterialTheme.colors.primary)
    Text(
      stringResource(textId).capitalize(Locale.current),
      maxLines = 1,
    )
  }
}