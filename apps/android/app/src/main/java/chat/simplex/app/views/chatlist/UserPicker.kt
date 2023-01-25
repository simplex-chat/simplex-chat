package chat.simplex.app.views.chatlist

import SectionItemViewSpaceBetween
import android.util.Log
import androidx.compose.animation.core.*
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Done
import androidx.compose.material.icons.outlined.Settings
import androidx.compose.runtime.*
import androidx.compose.ui.*
import androidx.compose.ui.draw.shadow
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.capitalize
import androidx.compose.ui.text.intl.Locale
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.*
import chat.simplex.app.R
import chat.simplex.app.TAG
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.UserInfo
import chat.simplex.app.ui.theme.DEFAULT_PADDING
import chat.simplex.app.views.helpers.*
import kotlinx.coroutines.flow.*
import kotlinx.coroutines.launch
import kotlin.math.roundToInt

@Composable
fun UserPicker(chatModel: ChatModel, userPickerState: MutableStateFlow<AnimatedViewState>, openSettings: () -> Unit) {
  val scope = rememberCoroutineScope()
  var newChat by remember { mutableStateOf(userPickerState.value) }
  val users by remember { derivedStateOf { chatModel.users.sortedByDescending { it.user.activeUser } } }
  val animatedFloat = remember { Animatable(if (newChat.isVisible()) 0f else 1f) }
  LaunchedEffect(Unit) {
    launch {
      userPickerState.collect {
        newChat = it
        launch {
          animatedFloat.animateTo(if (newChat.isVisible()) 1f else 0f, newChatSheetAnimSpec())
          if (newChat.isHiding()) userPickerState.value = AnimatedViewState.GONE
        }
      }
    }
  }
  LaunchedEffect(Unit) {
    snapshotFlow { newChat.isVisible() }
      .distinctUntilChanged()
      .filter { it }
      .collect {
        try {
          val updatedUsers = chatModel.controller.listUsers().sortedByDescending { it.user.activeUser }
          var same = users.size == updatedUsers.size
          if (same) {
            for (i in 0 until minOf(users.size, updatedUsers.size)) {
              val prev = updatedUsers[i].user
              val next = users[i].user
              if (prev.userId != next.userId || prev.activeUser != next.activeUser || prev.chatViewName != next.chatViewName || prev.image != next.image) {
                same = false
                break
              }
            }
          }
          if (!same) {
            chatModel.users.clear()
            chatModel.users.addAll(updatedUsers)
          }
        } catch (e: Exception) {
          Log.e(TAG, "Error updating users ${e.stackTraceToString()}")
        }
      }
  }
  val xOffset = with(LocalDensity.current) { 10.dp.roundToPx() }
  val maxWidth = with(LocalDensity.current) { LocalConfiguration.current.screenWidthDp * density }
  Box(Modifier
    .fillMaxSize()
    .offset { IntOffset(if (newChat.isGone()) -maxWidth.roundToInt() else xOffset, 0) }
    .clickable(interactionSource = remember { MutableInteractionSource() }, indication = null, onClick = { userPickerState.value = AnimatedViewState.HIDING })
    .padding(bottom = 10.dp, top = 10.dp)
    .graphicsLayer {
      alpha = animatedFloat.value
      translationY = (animatedFloat.value - 1) * xOffset
    }
  ) {
    Column(
      Modifier
        .widthIn(min = 220.dp)
        .width(IntrinsicSize.Min)
        .height(IntrinsicSize.Min)
        .shadow(8.dp, MaterialTheme.shapes.small, clip = false)
        .background(MaterialTheme.colors.background, MaterialTheme.shapes.small)
    ) {
      Column(Modifier.weight(1f).verticalScroll(rememberScrollState())) {
        users.forEachIndexed { i, u ->
          UserProfilePickerItem(u) {
            userPickerState.value = AnimatedViewState.HIDING
            scope.launch {
              if (!u.user.activeUser) {
                chatModel.controller.changeActiveUser(u.user.userId)
              }
            }
          }
          if (i != users.lastIndex) {
            Divider(Modifier.requiredHeight(1.dp))
          }
        }
      }
      Divider()
      SettingsPickerItem {
        openSettings()
        userPickerState.value = AnimatedViewState.GONE
      }
    }
  }
}

@Composable
private fun UserProfilePickerItem(u: UserInfo, onClick: () -> Unit) {
  SectionItemViewSpaceBetween(onClick, padding = PaddingValues(start = DEFAULT_PADDING - 2.dp, end = DEFAULT_PADDING)) {
    Row(Modifier.widthIn(max = LocalConfiguration.current.screenWidthDp.dp * 0.7f), verticalAlignment = Alignment.CenterVertically) {
      ProfileImage(
        image = u.user.image,
        size = 40.dp
      )
      Text(
        u.user.chatViewName,
        modifier = Modifier
          .padding(start = 10.dp, end = 10.dp)
      )
    }
    if (u.user.activeUser) {
      Icon(Icons.Filled.Done, null, Modifier.size(20.dp), tint = MaterialTheme.colors.primary)
    } else if (u.unreadCount > 0) {
      Text(
        unreadCountStr(u.unreadCount),
        color = MaterialTheme.colors.onPrimary,
        fontSize = 11.sp,
        modifier = Modifier
          .background(MaterialTheme.colors.primary, shape = CircleShape)
          .sizeIn(minWidth = 20.dp, minHeight = 20.dp)
          .padding(horizontal = 3.dp)
          .padding(vertical = 1.dp),
        textAlign = TextAlign.Center,
        maxLines = 1
      )
    }
  }
}

@Composable
private fun SettingsPickerItem(onClick: () -> Unit) {
  SectionItemViewSpaceBetween(onClick) {
    val text = generalGetString(R.string.settings_section_title_settings).lowercase().capitalize(Locale.current)
    Text(
      text,
      color = MaterialTheme.colors.onBackground,
    )
    Icon(Icons.Outlined.Settings, text, Modifier.size(20.dp), tint = MaterialTheme.colors.onBackground)
  }
}