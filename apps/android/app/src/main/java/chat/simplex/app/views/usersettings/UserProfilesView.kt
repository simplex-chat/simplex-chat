package chat.simplex.app.views.usersettings

import SectionBottomSpacer
import SectionDivider
import SectionItemView
import SectionItemViewSpaceBetween
import SectionSpacer
import SectionTextFooter
import SectionView
import androidx.annotation.StringRes
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.chatPasswordHash
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.item.ItemAction
import chat.simplex.app.views.chatlist.UserProfilePickerItem
import chat.simplex.app.views.chatlist.UserProfileRow
import chat.simplex.app.views.database.PassphraseField
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.onboarding.CreateProfile
import kotlinx.coroutines.delay

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
    prefPerformLA = m.controller.appPrefs.performLA.get(),
    addUser = {
      ModalManager.shared.showModalCloseable { close ->
        CreateProfile(m, close)
      }
    },
    activateUser = { user ->
      withBGApi {
        m.controller.changeActiveUser(user.userId, userViewPassword(user, searchTextOrPassword.value.trim()))
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
                removeUser(m, user, users, true, searchTextOrPassword.value.trim())
              }) {
                Text(stringResource(R.string.users_delete_with_connections), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = Color.Red)
              }
              SectionItemView({
                AlertManager.shared.hideAlert()
                removeUser(m, user, users, false, searchTextOrPassword.value.trim())
              }
              ) {
                Text(stringResource(R.string.users_delete_data_only), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = Color.Red)
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
      if (passwordEntryRequired(user, searchTextOrPassword.value)) {
        ModalManager.shared.showModalCloseable(true) { close ->
          ProfileActionView(UserProfileAction.UNHIDE, user) { pwd ->
            withBGApi {
              setUserPrivacy(m) { m.controller.apiUnhideUser(user.userId, pwd) }
              close()
            }
          }
        }
      } else {
        withBGApi { setUserPrivacy(m) { m.controller.apiUnhideUser(user.userId, searchTextOrPassword.value.trim()) } }
      }
    },
    muteUser = { user ->
      withBGApi {
        setUserPrivacy(m, onSuccess = {
          if (m.controller.appPrefs.showMuteProfileAlert.get()) showMuteProfileAlert(m.controller.appPrefs.showMuteProfileAlert)
        }) { m.controller.apiMuteUser(user.userId) }
      }
    },
    unmuteUser = { user ->
      withBGApi { setUserPrivacy(m) { m.controller.apiUnmuteUser(user.userId) } }
    },
    showHiddenProfile = { user ->
      ModalManager.shared.showModalCloseable(true) { close ->
        HiddenProfileView(m, user) {
          profileHidden.value = true
          withBGApi {
            delay(10_000)
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
  prefPerformLA: Boolean,
  showHiddenProfilesNotice: SharedPreference<Boolean>,
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
  ) {
    if (profileHidden.value) {
      SectionView {
        SettingsActionItem(painterResource(R.drawable.ic_lock_open), stringResource(R.string.enter_password_to_show), click = {
          profileHidden.value = false
        }
        )
      }
      SectionSpacer()
    }
    AppBarTitle(stringResource(R.string.your_chat_profiles))

    SectionView {
      for (user in filteredUsers) {
        UserView(user, users, visibleUsersCount, prefPerformLA, activateUser, removeUser, unhideUser, muteUser, unmuteUser, showHiddenProfile)
        SectionDivider()
      }
      if (searchTextOrPassword.value.trim().isEmpty()) {
        SectionItemView(addUser, minHeight = 68.dp) {
          Icon(painterResource(R.drawable.ic_add), stringResource(R.string.users_add), tint = MaterialTheme.colors.primary)
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
          confirmText = generalGetString(R.string.ok),
          dismissText = generalGetString(R.string.dont_show_again),
          onDismiss = {
            showHiddenProfilesNotice.set(false)
          },
        )
      }
    }
    SectionBottomSpacer()
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
  val showMenu = remember { mutableStateOf(false) }
  UserProfilePickerItem(user, onLongClick = { if (users.size > 1) showMenu.value = true }) {
    activateUser(user)
  }
  Box(Modifier.padding(horizontal = DEFAULT_PADDING)) {
    DefaultDropdownMenu(showMenu) {
      if (user.hidden) {
        ItemAction(stringResource(R.string.user_unhide), painterResource(R.drawable.ic_lock_open), onClick = {
          showMenu.value = false
          unhideUser(user)
        })
      } else {
        if (visibleUsersCount > 1 && prefPerformLA) {
          ItemAction(stringResource(R.string.user_hide), painterResource(R.drawable.ic_lock), onClick = {
            showMenu.value = false
            showHiddenProfile(user)
          })
        }
        if (user.showNtfs) {
          ItemAction(stringResource(R.string.user_mute), painterResource(R.drawable.ic_notifications_off), onClick = {
            showMenu.value = false
            muteUser(user)
          })
        } else {
          ItemAction(stringResource(R.string.user_unmute), painterResource(R.drawable.ic_notifications), onClick = {
            showMenu.value = false
            unmuteUser(user)
          })
        }
      }
      ItemAction(stringResource(R.string.delete_verb), painterResource(R.drawable.ic_delete), color = Color.Red, onClick = {
        removeUser(user)
        showMenu.value = false
      })
    }
  }
}

enum class UserProfileAction {
  DELETE,
  UNHIDE
}

@Composable
private fun ProfileActionView(action: UserProfileAction, user: User, doAction: (String) -> Unit) {
  Column(
    Modifier
      .fillMaxWidth()
      .verticalScroll(rememberScrollState())
  ) {
    val actionPassword = rememberSaveable { mutableStateOf("") }
    val passwordValid by remember { derivedStateOf { actionPassword.value == actionPassword.value.trim() } }
    val actionEnabled by remember { derivedStateOf { actionPassword.value != "" && passwordValid && correctPassword(user, actionPassword.value) } }

    @Composable fun ActionHeader(@StringRes title: Int) {
      AppBarTitle(stringResource(title))
      SectionView(padding = PaddingValues(start = 8.dp, end = DEFAULT_PADDING)) {
        UserProfileRow(user)
      }
      SectionSpacer()
    }

    @Composable fun PasswordAndAction(@StringRes label: Int, color: Color = MaterialTheme.colors.primary) {
      SectionView() {
        SectionItemView {
          PassphraseField(actionPassword, generalGetString(R.string.profile_password), isValid = { passwordValid }, showStrength = true)
        }
        SectionItemViewSpaceBetween({ doAction(actionPassword.value) }, disabled = !actionEnabled, minHeight = TextFieldDefaults.MinHeight) {
          Text(generalGetString(label), color = if (actionEnabled) color else MaterialTheme.colors.secondary)
        }
      }
    }

    when (action) {
      UserProfileAction.DELETE -> {
        ActionHeader(R.string.delete_profile)
        PasswordAndAction(R.string.delete_chat_profile, color = Color.Red)
        if (actionEnabled) {
          SectionTextFooter(stringResource(R.string.users_delete_all_chats_deleted))
        }
      }
      UserProfileAction.UNHIDE -> {
        ActionHeader(R.string.unhide_profile)
        PasswordAndAction(R.string.unhide_chat_profile)
      }
    }
    SectionBottomSpacer()
  }
}

private fun filteredUsers(m: ChatModel, searchTextOrPassword: String): List<User> {
  val s = searchTextOrPassword.trim()
  val lower = s.lowercase()
  return m.users.filter { u ->
    if ((u.user.activeUser || !u.user.hidden) && (s == "" || u.user.chatViewName.lowercase().contains(lower))) {
      true
    } else {
      correctPassword(u.user, s)
    }
  }.map { it.user }
}

private fun visibleUsersCount(m: ChatModel): Int = m.users.filter { u -> !u.user.hidden }.size

private fun correctPassword(user: User, pwd: String): Boolean {
  val ph = user.viewPwdHash
  return ph != null && pwd != "" && chatPasswordHash(pwd, ph.salt) == ph.hash
}

private fun userViewPassword(user: User, searchTextOrPassword: String): String? =
  if (user.hidden) searchTextOrPassword.trim() else null

private fun passwordEntryRequired(user: User, searchTextOrPassword: String): Boolean =
  user.hidden && user.activeUser && !correctPassword(user, searchTextOrPassword.trim())

private fun removeUser(m: ChatModel, user: User, users: List<User>, delSMPQueues: Boolean, searchTextOrPassword: String) {
  if (passwordEntryRequired(user, searchTextOrPassword)) {
    ModalManager.shared.showModalCloseable(true) { close ->
      ProfileActionView(UserProfileAction.DELETE, user) { pwd ->
        withBGApi {
          doRemoveUser(m, user, users, delSMPQueues, pwd)
          close()
        }
      }
    }
  } else {
    withBGApi { doRemoveUser(m, user, users, delSMPQueues, userViewPassword(user, searchTextOrPassword.trim())) }
  }
}

private suspend fun doRemoveUser(m: ChatModel, user: User, users: List<User>, delSMPQueues: Boolean, viewPwd: String?) {
  if (users.size < 2) return

  suspend fun deleteUser(user: User) {
    m.controller.apiDeleteUser(user.userId, delSMPQueues, viewPwd)
    m.removeUser(user)
  }
  try {
    if (user.activeUser) {
      val newActive = users.firstOrNull { u -> !u.activeUser && !u.hidden }
      if (newActive != null) {
        m.controller.changeActiveUser_(newActive.userId, null)
        deleteUser(user.copy(activeUser = false))
      }
    } else {
      deleteUser(user)
    }
  } catch (e: Exception) {
    AlertManager.shared.showAlertMsg(generalGetString(R.string.error_deleting_user), e.stackTraceToString())
  }
}

private suspend fun setUserPrivacy(m: ChatModel, onSuccess: (() -> Unit)? = null, api: suspend () -> User) {
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

private fun showMuteProfileAlert(showMuteProfileAlert: SharedPreference<Boolean>) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.muted_when_inactive),
    text = generalGetString(R.string.you_will_still_receive_calls_and_ntfs),
    confirmText = generalGetString(R.string.ok),
    dismissText = generalGetString(R.string.dont_show_again),
    onDismiss = {
      showMuteProfileAlert.set(false)
    },
  )
}
