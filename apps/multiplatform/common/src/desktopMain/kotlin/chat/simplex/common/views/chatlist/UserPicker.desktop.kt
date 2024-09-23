package chat.simplex.common.views.chatlist

import androidx.compose.animation.*
import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.User
import chat.simplex.common.model.UserInfo
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import kotlinx.coroutines.flow.MutableStateFlow

@Composable
actual fun UserPickerUsersSection(
  users: List<UserInfo>,
  stopped: Boolean,
  onUserClicked: (user: User) -> Unit,
) {
  if (users.isNotEmpty()) {
    val userRows = users.chunked(5)
    val rowsToDisplay = if (userRows.size > 2) 2 else userRows.size
    val horizontalPadding = DEFAULT_PADDING_HALF + 8.dp

    Column(Modifier
      .padding(horizontal = horizontalPadding, vertical = DEFAULT_PADDING_HALF)
      .height(55.dp * rowsToDisplay + (if (rowsToDisplay > 1) DEFAULT_PADDING else 0.dp))
    ) {
      ColumnWithScrollBar(
        verticalArrangement = Arrangement.spacedBy(DEFAULT_PADDING)
      ) {
        val spaceBetween = (((DEFAULT_START_MODAL_WIDTH * fontSizeSqrtMultiplier) - (horizontalPadding)) - (55.dp * 5)) / 5

        userRows.forEach { row ->
          Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.spacedBy(spaceBetween),
          ) {
            row.forEach { u ->
              UserPickerUserBox(u, stopped, size = 55.dp) {
                onUserClicked(u.user)
              }
            }
          }
        }
      }
    }
  }
}

@Composable
actual fun PlatformUserPicker(modifier: Modifier, pickerState: MutableStateFlow<AnimatedViewState>, content: @Composable () -> Unit) {
  AnimatedVisibility(
    visible = pickerState.value.isVisible(),
    enter = fadeIn(),
    exit = fadeOut()
  ) {
    Box(
      Modifier
        .fillMaxSize()
        .clickable(interactionSource = remember { MutableInteractionSource() }, indication = null, onClick = { pickerState.value = AnimatedViewState.HIDING }),
      contentAlignment = Alignment.TopStart
    ) {
      ColumnWithScrollBar(modifier) {
        content()
      }
    }
  }
}