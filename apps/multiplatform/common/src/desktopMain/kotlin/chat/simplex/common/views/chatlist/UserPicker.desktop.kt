package chat.simplex.common.views.chatlist

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.User
import chat.simplex.common.model.UserInfo
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.fontSizeSqrtMultiplier
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
actual fun UserPickerInactiveUsersSection(
  users: List<UserInfo>,
  stopped: Boolean,
  onShowAllProfilesClicked: () -> Unit,
  onUserClicked: (user: User) -> Unit,
) {
  if (users.isNotEmpty()) {
    val userRows = users.chunked(5)
    val rowsToDisplay = if (userRows.count() > 2) 2 else userRows.count()
    val horizontalPadding = DEFAULT_PADDING_HALF + 8.dp

    Column(Modifier.padding(horizontal = horizontalPadding).height((55.dp + DEFAULT_PADDING) * rowsToDisplay)) {
      ColumnWithScrollBar(
        verticalArrangement = Arrangement.spacedBy(DEFAULT_PADDING)
      ) {
        val spaceBetween = (((DEFAULT_START_MODAL_WIDTH * fontSizeSqrtMultiplier) - (horizontalPadding)) - (55.dp * 5f)) / 5f

        userRows.forEach { row ->
          Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.spacedBy(spaceBetween),
          ) {
            row.forEach { u ->
              UserPickerInactiveUserBadge(u, stopped, size = 55) {
                onUserClicked(u.user)
              }
            }
          }
        }
      }
    }
  }

  UserPickerOptionRow(
    painterResource(MR.images.ic_manage_accounts),
    stringResource(MR.strings.your_chat_profiles),
    onShowAllProfilesClicked
  )
}
