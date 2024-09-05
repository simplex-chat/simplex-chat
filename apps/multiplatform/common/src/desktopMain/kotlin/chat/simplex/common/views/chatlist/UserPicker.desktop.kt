package chat.simplex.common.views.chatlist

import androidx.compose.runtime.Composable
import chat.simplex.common.model.User
import chat.simplex.common.model.UserInfo
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
  UserPickerOptionRow(
    painterResource(MR.images.ic_manage_accounts),
    stringResource(MR.strings.your_chat_profiles),
    onShowAllProfilesClicked
  )
}
