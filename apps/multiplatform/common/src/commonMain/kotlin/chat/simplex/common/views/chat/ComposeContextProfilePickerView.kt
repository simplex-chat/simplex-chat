package chat.simplex.common.views.chat

import TextIconSpaced
import androidx.compose.animation.core.*
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.IncognitoOptionImage
import chat.simplex.common.views.usersettings.IncognitoView
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

private val USER_ROW_AVATAR_SIZE = 42.dp
private val USER_ROW_VERTICAL_PADDING = 8.dp
private val USER_PICKER_ROW_SIZE = USER_ROW_AVATAR_SIZE + (USER_ROW_VERTICAL_PADDING * 2f)
private val MAX_USER_PICKER_HEIGHT = (USER_PICKER_ROW_SIZE * 4) + (USER_ROW_AVATAR_SIZE + USER_ROW_VERTICAL_PADDING - 4.dp)

@Composable
fun ComposeContextProfilePickerView(
  rhId: Long?,
  chat: Chat,
  currentUser: User
) {
  val selectedUser = remember { mutableStateOf(currentUser) }
  val incognitoDefault = chatModel.controller.appPrefs.incognito.get()
  val users = chatModel.users.map { it.user }.filter { u -> u.activeUser || !u.hidden }
  val listExpanded = remember { mutableStateOf(false) }

  val maxHeightInPx = with(LocalDensity.current) { windowHeight().toPx() }
  val isVisible = remember { mutableStateOf(false) }
  val offsetY = remember { Animatable(maxHeightInPx) }

  LaunchedEffect(isVisible.value) {
    if (isVisible.value) {
      offsetY.animateTo(
        targetValue = 0f,
        animationSpec = contextUserPickerAnimSpec()
      )
    }
  }

  @Composable
  fun ExpandCollapseChevron() {
    if (listExpanded.value) {
      Icon(
        painterResource(
          MR.images.ic_chevron_down
        ),
        contentDescription = null,
        Modifier.size(20.dp),
        tint = MaterialTheme.colors.secondary,
      )
    } else if (!chat.chatInfo.profileChangeProhibited) {
      Icon(
        painterResource(
          MR.images.ic_chevron_up
        ),
        contentDescription = null,
        Modifier.size(20.dp),
        tint = MaterialTheme.colors.secondary,
      )
    }
  }

  fun changeProfile(newUser: User) {
    withApi {
      if (chat.chatInfo is ChatInfo.Direct) {
        val updatedContact = chatModel.controller.apiChangePreparedContactUser(rhId, chat.chatInfo.contact.contactId, newUser.userId)
        if (updatedContact != null) {
          selectedUser.value = newUser
          chatModel.controller.appPrefs.incognito.set(false)
          listExpanded.value = false
          chatModel.chatsContext.updateContact(rhId, updatedContact)
        }
      } else if (chat.chatInfo is ChatInfo.Group) {
        val updatedGroup = chatModel.controller.apiChangePreparedGroupUser(rhId, chat.chatInfo.groupInfo.groupId, newUser.userId)
        if (updatedGroup != null) {
          selectedUser.value = newUser
          chatModel.controller.appPrefs.incognito.set(false)
          listExpanded.value = false
          chatModel.chatsContext.updateGroup(rhId, updatedGroup)
        }
      }
      chatModel.controller.changeActiveUser_(
        rhId = newUser.remoteHostId,
        toUserId = newUser.userId,
        viewPwd = null,
        keepingChatId = chat.id
      )
      if (chatModel.currentUser.value?.userId != newUser.userId) {
        AlertManager.shared.showAlertMsg(
          generalGetString(MR.strings.switching_profile_error_title),
          String.format(generalGetString(MR.strings.switching_profile_error_message), newUser.chatViewName)
        )
      }
    }
  }

  fun showCantChangeProfileAlert() {
    AlertManager.shared.showAlertMsg(
      generalGetString(MR.strings.context_user_picker_cant_change_profile_alert_title),
      generalGetString(MR.strings.context_user_picker_cant_change_profile_alert_message)
    )
  }

  @Composable
  fun ProfilePickerUserOption(user: User) {
    Row(
      Modifier
        .fillMaxWidth()
        .sizeIn(minHeight = DEFAULT_MIN_SECTION_ITEM_HEIGHT + 8.dp)
        .clickable(onClick = {
          if (!chat.chatInfo.profileChangeProhibited) {
            if (selectedUser.value.userId == user.userId) {
              if (!incognitoDefault) {
                listExpanded.value = !listExpanded.value
              } else {
                chatModel.controller.appPrefs.incognito.set(false)
                listExpanded.value = false
              }
            } else {
              changeProfile(user)
            }
          } else {
            showCantChangeProfileAlert()
          }
        })
        .padding(horizontal = DEFAULT_PADDING_HALF, vertical = 4.dp),
      horizontalArrangement = Arrangement.SpaceBetween,
      verticalAlignment = Alignment.CenterVertically
    ) {
      ProfileImage(size = USER_ROW_AVATAR_SIZE, image = user.image)
      TextIconSpaced(false)
      Text(
        user.chatViewName,
        modifier = Modifier.align(Alignment.CenterVertically),
        fontWeight = if (selectedUser.value.userId == user.userId && !incognitoDefault) FontWeight.Medium else FontWeight.Normal
      )

      Spacer(Modifier.weight(1f))

      if (selectedUser.value.userId == user.userId && !incognitoDefault) {
        ExpandCollapseChevron()
      }
    }
  }

  @Composable
  fun IncognitoOption() {
    Row(
      Modifier
        .fillMaxWidth()
        .sizeIn(minHeight = DEFAULT_MIN_SECTION_ITEM_HEIGHT + 8.dp)
        .clickable(onClick = {
          if (!chat.chatInfo.profileChangeProhibited) {
            if (incognitoDefault) {
              listExpanded.value = !listExpanded.value
            } else {
              chatModel.controller.appPrefs.incognito.set(true)
              listExpanded.value = false
            }
          } else {
            showCantChangeProfileAlert()
          }
        })
        .padding(horizontal = DEFAULT_PADDING_HALF, vertical = 4.dp),
      horizontalArrangement = Arrangement.SpaceBetween,
      verticalAlignment = Alignment.CenterVertically
    ) {
      IncognitoOptionImage()
      TextIconSpaced(false)
      Text(
        stringResource(MR.strings.incognito),
        modifier = Modifier.align(Alignment.CenterVertically),
        fontWeight = if (incognitoDefault) FontWeight.Medium else FontWeight.Normal
      )
      Spacer(Modifier.padding(6.dp))
      Column(Modifier
        .size(48.dp)
        .clip(CircleShape)
        .clickable(
          onClick = {
            if (ModalManager.end.isLastModalOpen(ModalViewId.CONTEXT_USER_PICKER_INCOGNITO)) {
              ModalManager.end.closeModal()
            } else {
              ModalManager.end.showModal(id = ModalViewId.CONTEXT_USER_PICKER_INCOGNITO) { IncognitoView() }
            }
          }
        ),
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.Center
      ) {
        Icon(
          painterResource(MR.images.ic_info),
          stringResource(MR.strings.incognito),
          tint = MaterialTheme.colors.primary
        )
      }

      Spacer(Modifier.weight(1f))

      if (incognitoDefault) {
        ExpandCollapseChevron()
      }
    }
  }

  @Composable
  fun ProfilePicker() {
    LazyColumnWithScrollBarNoAppBar(
      Modifier
        .heightIn(max = MAX_USER_PICKER_HEIGHT)
        .background(MaterialTheme.colors.surface),
      reverseLayout = true,
      maxHeight = remember { mutableStateOf(MAX_USER_PICKER_HEIGHT) },
      containerAlignment = Alignment.BottomEnd
    ) {
      val otherUsers = users.filter { u -> u.userId != selectedUser.value.userId }.sortedByDescending { it.activeOrder }

      if (incognitoDefault) {
        item {
          IncognitoOption()
          Divider(
            Modifier.padding(
              start = DEFAULT_PADDING_HALF,
              end = DEFAULT_PADDING_HALF,
            )
          )
          ProfilePickerUserOption(selectedUser.value)
        }
      } else {
        item {
          ProfilePickerUserOption(selectedUser.value)
          Divider(
            Modifier.padding(
              start = DEFAULT_PADDING_HALF,
              end = DEFAULT_PADDING_HALF,
            )
          )
          IncognitoOption()
        }
      }

      items(otherUsers, key = { it.userId }) { user ->
        Divider(
          Modifier.padding(
            start = DEFAULT_PADDING_HALF,
            end = DEFAULT_PADDING_HALF,
          )
        )
        ProfilePickerUserOption(user)
      }
    }
  }

  @Composable
  fun CurrentSelection() {
    Column(
      Modifier
        .background(MaterialTheme.colors.surface),
    ) {
      Text(
        generalGetString(MR.strings.context_user_picker_your_profile),
        Modifier.padding(horizontal = 14.dp).padding(top = 8.dp),
        color = MaterialTheme.colors.secondary
      )

      if (chat.chatInfo.profileChangeProhibited) {
        if (chat.chatInfo.incognito) {
          IncognitoOption()
        } else {
          ProfilePickerUserOption(selectedUser.value)
        }
      } else if (incognitoDefault) {
        IncognitoOption()
      } else {
        ProfilePickerUserOption(selectedUser.value)
      }
    }
  }

  if (!listExpanded.value || chat.chatInfo.profileChangeProhibited) {
    CurrentSelection()
  } else {
    ProfilePicker()
  }
}
