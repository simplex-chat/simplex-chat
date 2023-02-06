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
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.intl.Locale
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.*
import chat.simplex.app.R
import chat.simplex.app.TAG
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.*
import kotlinx.coroutines.launch
import kotlin.math.roundToInt

@Composable
fun UserPicker(chatModel: ChatModel, userPickerState: MutableStateFlow<AnimatedViewState>, switchingUsers: MutableState<Boolean>, openSettings: () -> Unit) {
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
        .shadow(8.dp, MaterialTheme.shapes.medium, clip = false)
        .background(MaterialTheme.colors.background, MaterialTheme.shapes.medium)
    ) {
      Column(Modifier.weight(1f).verticalScroll(rememberScrollState())) {
        users.forEach { u ->
          UserProfilePickerItem(u.user, u.unreadCount, openSettings = {
            openSettings()
            userPickerState.value = AnimatedViewState.GONE
          }) {
            userPickerState.value = AnimatedViewState.HIDING
            if (!u.user.activeUser) {
              chatModel.chats.clear()
              scope.launch {
                val job = launch {
                  delay(500)
                  switchingUsers.value = true
                }
                chatModel.controller.changeActiveUser(u.user.userId)
                job.cancel()
                switchingUsers.value = false
              }
            }
          }
          Divider(Modifier.requiredHeight(1.dp))
          if (u.user.activeUser) Divider(Modifier.requiredHeight(0.5.dp))
        }
      }
      SettingsPickerItem {
        openSettings()
        userPickerState.value = AnimatedViewState.GONE
      }
    }
  }
}

@Composable
fun UserProfilePickerItem(u: User, unreadCount: Int = 0, onLongClick: () -> Unit = {}, openSettings: () -> Unit = {}, onClick: () -> Unit) {
  Row(
    Modifier
      .fillMaxWidth()
      .sizeIn(minHeight = 46.dp)
      .combinedClickable(
        onClick = if (u.activeUser) openSettings else onClick,
        onLongClick = onLongClick,
        interactionSource = remember { MutableInteractionSource() },
        indication = if (!u.activeUser) LocalIndication.current else null
      )
      .padding(PaddingValues(start = 8.dp, end = DEFAULT_PADDING)),
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    Row(
      Modifier
        .widthIn(max = LocalConfiguration.current.screenWidthDp.dp * 0.7f)
        .padding(vertical = 8.dp),
      verticalAlignment = Alignment.CenterVertically
    ) {
      ProfileImage(
        image = u.image,
        size = 54.dp
      )
      Text(
        u.displayName,
        modifier = Modifier
          .padding(start = 8.dp, end = 8.dp),
        fontWeight = if (u.activeUser) FontWeight.Medium else FontWeight.Normal
      )
    }
    if (u.activeUser) {
      Icon(Icons.Filled.Done, null, Modifier.size(20.dp), tint = MaterialTheme.colors.onBackground)
    } else if (unreadCount > 0) {
      Row {
        Text(
          unreadCountStr(unreadCount),
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
        Spacer(Modifier.width(2.dp))
      }
    } else {
      Box(Modifier.size(20.dp))
    }
  }
}

@Composable
private fun SettingsPickerItem(onClick: () -> Unit) {
  SectionItemViewSpaceBetween(onClick, minHeight = 68.dp) {
    val text = generalGetString(R.string.settings_section_title_settings).lowercase().capitalize(Locale.current)
    Text(
      text,
      color = MaterialTheme.colors.onBackground,
    )
    Icon(Icons.Outlined.Settings, text, Modifier.size(20.dp), tint = MaterialTheme.colors.onBackground)
  }
}
