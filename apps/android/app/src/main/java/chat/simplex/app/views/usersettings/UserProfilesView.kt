package chat.simplex.app.views.usersettings

import SectionDivider
import SectionItemView
import SectionSpacer
import SectionTextFooter
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.chatPasswordHash
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.item.ItemAction
import chat.simplex.app.views.chatlist.UserProfilePickerItem
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.onboarding.CreateProfile
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch

@Composable
fun UserProfilesView(m: ChatModel, search: MutableState<String>, profileHidden: MutableState<Boolean>) {
  val searchTextOrPassword = rememberSaveable { search }
  val users by remember { derivedStateOf { m.users.map { it.user } } }
  val filteredUsers by remember { derivedStateOf { filteredUsers(m, searchTextOrPassword.value) } }
  UserProfilesView(
    users = users,
    filteredUsers = filteredUsers,
    profileHidden = profileHidden,
    searchTextOrPassword = searchTextOrPassword,
    showHiddenProfilesNotice = m.controller.appPrefs.showHiddenProfilesNotice,
    visibleUsersCount = visibleUsersCount(m),
    prefPerformLA = m.controller.appPrefs.performLA.state.value,
    addUser = {
      ModalManager.shared.showModalCloseable { close ->
        CreateProfile(m, close)
      }
    },
    activateUser = { user ->
      withBGApi {
        m.controller.changeActiveUser(user.userId, userViewPassword(user, searchTextOrPassword.value))
      }
    },
    removeUser = { user ->
      if (m.users.size > 1 && (user.hidden || visibleUsersCount(m) > 1)) {
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
                removeUser(m, user, users, true, searchTextOrPassword.value)
              }) {
                Text(stringResource(R.string.users_delete_with_connections), color = Color.Red)
              }
              SectionItemView({
                AlertManager.shared.hideAlert()
                removeUser(m, user, users, false, searchTextOrPassword.value)
              }
              ) {
                Text(stringResource(R.string.users_delete_data_only), color = Color.Red)
              }
            }
          }
        )
      } else {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(R.string.cant_delete_user_profile),
          text = if (m.users.size > 1) {
            generalGetString(R.string.should_be_at_least_one_visible_profile)
          } else {
            generalGetString(R.string.should_be_at_least_one_profile)
          }
        )
      }
    },
    unhideUser = { user ->
      setUserPrivacy(m, user) { m.controller.apiUnhideUser(user.userId, userViewPassword(user, searchTextOrPassword.value)) }
    },
    muteUser = { user ->
      setUserPrivacy(m, user, onSuccess = { if (m.controller.appPrefs.showMuteProfileAlert.get()) showMuteProfileAlert(m.controller.appPrefs.showMuteProfileAlert) }) {
        m.controller.apiMuteUser(user.userId, userViewPassword(user, searchTextOrPassword.value))
      }
    },
    unmuteUser = { user ->
      setUserPrivacy(m, user) { m.controller.apiUnmuteUser(user.userId, userViewPassword(user, searchTextOrPassword.value)) }
    },
    showHiddenProfile = { user ->
      ModalManager.shared.showModalCloseable { close ->
        HiddenProfileView(m, user) {
          profileHidden.value = true
          withBGApi {
            delay(15_000)
            profileHidden.value = false
          }
          close()
        }
      }
    }
  )
}

@Composable
private fun UserProfilesView(
  users: List<User>,
  filteredUsers: List<User>,
  searchTextOrPassword: MutableState<String>,
  profileHidden: MutableState<Boolean>,
  visibleUsersCount: Int,
  showHiddenProfilesNotice: SharedPreference<Boolean>,
  prefPerformLA: Boolean,
  addUser: () -> Unit,
  activateUser: (User) -> Unit,
  removeUser: (User) -> Unit,
  unhideUser: (User) -> Unit,
  muteUser: (User) -> Unit,
  unmuteUser: (User) -> Unit,
  showHiddenProfile: (User) -> Unit,
) {
  Column(
    Modifier
      .fillMaxWidth()
      .verticalScroll(rememberScrollState())
      .padding(bottom = DEFAULT_PADDING),
  ) {
    AppBarTitle(stringResource(R.string.your_chat_profiles))
    if (profileHidden.value) {
      SectionView {
        SettingsActionItem(Icons.Outlined.LockOpen, stringResource(R.string.enter_password_to_show), click = {
          profileHidden.value = false
        }
        )
      }
      SectionSpacer()
    }

    SectionView {
      for (user in filteredUsers) {
        UserView(user, users, visibleUsersCount, prefPerformLA, activateUser, removeUser, unhideUser, muteUser, unmuteUser, showHiddenProfile)
        SectionDivider()
      }
      if (searchTextOrPassword.value.isEmpty()) {
        SectionItemView(addUser, minHeight = 68.dp) {
          Icon(Icons.Outlined.Add, stringResource(R.string.users_add), tint = MaterialTheme.colors.primary)
          Spacer(Modifier.padding(horizontal = 4.dp))
          Text(stringResource(R.string.users_add), color = MaterialTheme.colors.primary)
        }
      }
    }
    SectionTextFooter(stringResource(R.string.tap_to_activate_profile))
    LaunchedEffect(Unit) {
      if (showHiddenProfilesNotice.state.value && users.size > 1) {
        AlertManager.shared.showAlertDialog(
          title = generalGetString(R.string.make_profile_private),
          text = generalGetString(R.string.you_can_hide_or_mute_user_profile),
          confirmText = generalGetString(R.string.dont_show_again),
          onConfirm = {
            showHiddenProfilesNotice.set(false)
          },
          dismissText = generalGetString(R.string.ok)
        )
      }
    }
  }
}

@Composable
private fun UserView(
  user: User,
  users: List<User>,
  visibleUsersCount: Int,
  prefPerformLA: Boolean,
  activateUser: (User) -> Unit,
  removeUser: (User) -> Unit,
  unhideUser: (User) -> Unit,
  muteUser: (User) -> Unit,
  unmuteUser: (User) -> Unit,
  showHiddenProfile: (User) -> Unit,
) {
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

      if (user.hidden) {
        ItemAction(stringResource(R.string.user_unhide), Icons.Outlined.Delete, color = SimplexGreen, onClick = {
          showDropdownMenu = false
          unhideUser(user)
        }
        )
      } else {
        if (visibleUsersCount > 1 && prefPerformLA) {
          ItemAction(stringResource(R.string.user_hide), Icons.Outlined.VisibilityOff, color = HighOrLowlight, onClick = {
            showDropdownMenu = false
            showHiddenProfile(user)
          }
          )
        }
        if (user.showNtfs) {
          ItemAction(stringResource(R.string.user_mute), Icons.Outlined.NotificationsOff, color = MaterialTheme.colors.primary, onClick = {
            showDropdownMenu = false
            muteUser(user)
          }
          )
        } else {
          ItemAction(stringResource(R.string.user_unmute), Icons.Outlined.Visibility, color = MaterialTheme.colors.primary, onClick = {
            showDropdownMenu = false
            unmuteUser(user)
          }
          )
        }
      }
    }
  }
}

private fun filteredUsers(m: ChatModel, searchTextOrPassword: String): List<User> {
  val s = searchTextOrPassword.trim()
  val lower = s.lowercase()
  return m.users.filter { u ->
    if ((u.user.activeUser || u.user.viewPwdHash == null) && (s == "" || u.user.chatViewName.lowercase().contains(lower))) {
      true
    } else if (u.user.viewPwdHash != null) {
      s != "" && chatPasswordHash(s, u.user.viewPwdHash.salt) == u.user.viewPwdHash.hash
    } else {
      false
    }
  }.map { it.user }
}

private fun visibleUsersCount(m: ChatModel): Int = m.users.filter { u -> !u.user.hidden }.size

private fun userViewPassword(user: User, searchTextOrPassword: String): String? =
  if (user.activeUser || !user.hidden) null else searchTextOrPassword

private fun removeUser(m: ChatModel, user: User, users: List<User>, delSMPQueues: Boolean, searchTextOrPassword: String) {
  if (users.size < 2) return

  withBGApi {
    suspend fun deleteUser(user: User) {
      m.controller.apiDeleteUser(user.userId, delSMPQueues, userViewPassword(user, searchTextOrPassword))
      m.removeUser(user)
    }
    try {
      if (user.activeUser) {
        val newActive = users.firstOrNull { u -> !u.activeUser && !u.hidden }
        if (newActive != null) {
          m.controller.changeActiveUser_(newActive.userId, null)
          deleteUser(user)
        }
      } else {
        deleteUser(user)
      }
    } catch (e: Exception) {
      AlertManager.shared.showAlertMsg(generalGetString(R.string.error_deleting_user), e.stackTraceToString())
    }
  }
}

private fun setUserPrivacy(m: ChatModel, user: User, onSuccess: (() -> Unit)? = null, api: suspend () -> User) {
  withBGApi {
    try {
      m.updateUser(api())
      onSuccess?.invoke()
    } catch (e: Exception) {
      AlertManager.shared.showAlertMsg(
        title = generalGetString(R.string.error_updating_user_privacy),
        text = e.stackTraceToString()
      )
    }
  }
}

private fun showMuteProfileAlert(showMuteProfileAlert: SharedPreference<Boolean>) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.muted_when_inactive),
    text = generalGetString(R.string.you_will_still_receive_calls_and_ntfs),
    confirmText = generalGetString(R.string.dont_show_again),
    onConfirm = {
      showMuteProfileAlert.set(false)
    },
    dismissText = generalGetString(R.string.ok)
  )
}