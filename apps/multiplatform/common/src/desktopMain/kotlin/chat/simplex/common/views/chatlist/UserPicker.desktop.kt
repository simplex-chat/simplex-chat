package chat.simplex.common.views.chatlist

import androidx.compose.animation.*
import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.User
import chat.simplex.common.model.UserInfo
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import kotlinx.coroutines.flow.MutableStateFlow

@Composable
actual fun UserPickerUsersSection(
  users: List<UserInfo>,
  iconColor: Color,
  stopped: Boolean,
  onUserClicked: (user: User) -> Unit,
) {
  if (users.isNotEmpty()) {
    val userRows = users.chunked(5)
    val rowsToDisplay = if (userRows.size > 2) 2 else userRows.size
    val horizontalPadding = DEFAULT_PADDING_HALF + 8.dp

    Column(Modifier
      .padding(horizontal = horizontalPadding)
      .height((55.dp + 16.sp.toDp()) * rowsToDisplay + (if (rowsToDisplay > 1) DEFAULT_PADDING else 0.dp))
    ) {
      ColumnWithScrollBarNoAppBar(
        verticalArrangement = Arrangement.spacedBy(DEFAULT_PADDING)
      ) {
        val spaceBetween = (((DEFAULT_START_MODAL_WIDTH * fontSizeSqrtMultiplier) - (horizontalPadding)) - (65.dp * 5)) / 5

        userRows.forEach { row ->
          Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.spacedBy(spaceBetween),
          ) {
            row.forEach { u ->
              Column(modifier = Modifier
                .clickable (
                  onClick = { onUserClicked(u.user) },
                  enabled = !stopped
                ),
                horizontalAlignment = Alignment.CenterHorizontally,
              ) {
                val user = u.user
                Box {
                  ProfileImage(size = 55.dp, image = user.profile.image, color = iconColor)

                  if (u.unreadCount > 0 && !user.activeUser) {
                    unreadBadge(u.unreadCount, user.showNtfs, true)
                  }
                }

                Text(
                  user.displayName,
                  fontSize = 12.sp,
                  fontWeight = if (user.activeUser) FontWeight.Bold else FontWeight.Normal,
                  maxLines = 1,
                  overflow = TextOverflow.Ellipsis,
                  modifier = Modifier.width(65.dp),
                  textAlign = TextAlign.Center
                )
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
      val oneHandUI = remember { appPrefs.oneHandUI.state }
      ColumnWithScrollBarNoAppBar(modifier.align(if (oneHandUI.value) Alignment.BottomCenter else Alignment.TopCenter)) {
        content()
      }
    }
  }
}
