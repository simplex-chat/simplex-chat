package chat.simplex.common.views.usersettings.networkAndServers

import androidx.compose.foundation.ScrollState
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.verticalScroll
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp

@Composable
actual fun ConditionsBox(modifier: Modifier, scrollState: ScrollState, content: @Composable() (BoxScope.() -> Unit)){
  Box(
    modifier = modifier
      .verticalScroll(scrollState)
      .padding(8.dp)
  ) {
    content()
  }
}
