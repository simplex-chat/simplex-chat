package chat.simplex.app.views.usersettings

import SectionDivider
import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.item.ItemAction
import chat.simplex.app.views.chatlist.UserProfilePickerItem
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.onboarding.CreateProfile

@Composable
fun UserProfilesView(m: ChatModel) {
  val users by remember { derivedStateOf { m.users.map { it.user } } }
  UserProfilesView(
    users = users,
    addUser = {
      ModalManager.shared.showModalCloseable { close ->
        CreateProfile(m, close)
      }
    },
    activateUser = { user ->
      withBGApi {
        m.controller.changeActiveUser(user.userId)
      }
    },
    removeUser = { user ->
      val text = buildAnnotatedString {
        append(generalGetString(R.string.users_delete_all_chats_deleted) + "\n\n" + generalGetString(R.string.users_delete_profile_for) + " ")
        withStyle(SpanStyle(fontWeight = FontWeight.Bold)) {
          append(user.displayName)
        }
        append(":")
      }
      AlertManager.shared.showAlertDialogButtonsColumn(
        title = generalGetString(R.string.users_delete_question),
        text = text,
        buttons = {
          Column {
            SectionItemView({
              AlertManager.shared.hideAlert()
              removeUser(m, user, users, true)
            }) {
              Text(stringResource(R.string.users_delete_with_connections), color = Color.Red)
            }
            SectionItemView({
              AlertManager.shared.hideAlert()
              removeUser(m, user, users, false)
            }
            ) {
              Text(stringResource(R.string.users_delete_data_only), color = Color.Red)
            }
          }
        }
      )
    }
  )
}

@Composable
private fun UserProfilesView(
  users: List<User>,
  addUser: () -> Unit,
  activateUser: (User) -> Unit,
  removeUser: (User) -> Unit,
) {
  Column(
    Modifier
      .fillMaxWidth()
      .verticalScroll(rememberScrollState())
      .padding(bottom = DEFAULT_PADDING),
  ) {
    AppBarTitle(stringResource(R.string.your_chat_profiles))

    SectionView {
      for (user in users) {
        UserView(user, users, activateUser, removeUser)
        SectionDivider()
      }
      SectionItemView(addUser, minHeight = 68.dp) {
        Icon(Icons.Outlined.Add, stringResource(R.string.users_add), tint = MaterialTheme.colors.primary)
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text(stringResource(R.string.users_add), color = MaterialTheme.colors.primary)
      }
    }
    SectionTextFooter(stringResource(R.string.your_chat_profiles_stored_locally))
  }
}

@Composable
private fun UserView(user: User, users: List<User>, activateUser: (User) -> Unit, removeUser: (User) -> Unit) {
  var showDropdownMenu by remember { mutableStateOf(false) }
  UserProfilePickerItem(user, onLongClick = { if (users.size > 1) showDropdownMenu = true }) {
    activateUser(user)
  }
  Box(Modifier.padding(horizontal = 16.dp)) {
    DropdownMenu(
      expanded = showDropdownMenu,
      onDismissRequest = { showDropdownMenu = false },
      Modifier.width(220.dp)
    ) {
      ItemAction(stringResource(R.string.delete_verb), Icons.Outlined.Delete, color = Color.Red, onClick = {
        removeUser(user)
        showDropdownMenu = false
      }
      )
    }
  }
}

private fun removeUser(m: ChatModel, user: User, users: List<User>, delSMPQueues: Boolean) {
  if (users.size < 2) return

  withBGApi {
    try {
      if (user.activeUser) {
        val newActive = users.first { !it.activeUser }
        m.controller.changeActiveUser_(newActive.userId)
      }
      m.controller.apiDeleteUser(user.userId, delSMPQueues)
      m.users.removeAll { it.user.userId == user.userId }
    } catch (e: Exception) {
      AlertManager.shared.showAlertMsg(generalGetString(R.string.error_deleting_user), e.stackTraceToString())
    }
  }
}
