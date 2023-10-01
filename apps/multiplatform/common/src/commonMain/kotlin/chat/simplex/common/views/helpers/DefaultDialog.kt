package chat.simplex.common.views.helpers

import androidx.compose.runtime.Composable

@Composable
expect fun DefaultDialog(
  onDismissRequest: () -> Unit,
  content: @Composable () -> Unit
)
