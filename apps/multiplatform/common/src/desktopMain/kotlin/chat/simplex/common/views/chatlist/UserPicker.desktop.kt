package chat.simplex.common.views.chatlist

import androidx.compose.runtime.Composable
import chat.simplex.common.model.User
import chat.simplex.common.model.UserInfo


@Composable
actual fun UserPickerInactiveUsersSection(
  users: List<UserInfo>,
  stopped: Boolean,
  onShowAllProfilesClicked: () -> Unit,
  onUserClicked: (user: User) -> Unit,
) {
}
