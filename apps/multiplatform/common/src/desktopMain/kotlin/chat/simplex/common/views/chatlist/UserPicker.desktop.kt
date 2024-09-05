package chat.simplex.common.views.chatlist

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.grid.*
import androidx.compose.foundation.lazy.itemsIndexed
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.User
import chat.simplex.common.model.UserInfo
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.platform.LazyColumnWithScrollBar
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
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

    Column(Modifier.padding(horizontal = DEFAULT_PADDING).height((55.dp + DEFAULT_PADDING) * rowsToDisplay)) {
      ColumnWithScrollBar(verticalArrangement = Arrangement.spacedBy(DEFAULT_PADDING)) {
        userRows.forEach { row ->
          Row(
            horizontalArrangement = Arrangement.spacedBy(18.dp),
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
