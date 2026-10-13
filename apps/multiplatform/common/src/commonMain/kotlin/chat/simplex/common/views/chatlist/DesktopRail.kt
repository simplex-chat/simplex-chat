package chat.simplex.common.views.chatlist

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.item.ReactionIcon
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.localauth.VerticalDivider
import chat.simplex.common.views.usersettings.SettingsView
import chat.simplex.res.*
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.flow.MutableStateFlow

val DESKTOP_RAIL_WIDTH = 64.dp
private val RAIL_ITEM_SIZE = 40.dp
private val RAIL_ICON_SIZE = 22.dp
private val RAIL_ACTIVE_MARK_WIDTH = 4.dp
private val RAIL_ACTIVE_BACKGROUND_WIDTH = 40.dp
private val RAIL_ACTIVE_BACKGROUND_HEIGHT = 32.dp
private val RAIL_UNREAD_DOT_SIZE = 6.dp
private const val RAIL_ACTIVE_BACKGROUND_ALPHA = 0.12f
private const val RAIL_BACKGROUND_ALPHA = 0.04f

@Composable
// Spec: spec/client/chat-list.md#DesktopRail
fun DesktopRail(userPickerState: MutableStateFlow<AnimatedViewState>, setPerformLA: (Boolean) -> Unit) {
  Row(
    Modifier
      .width(DESKTOP_RAIL_WIDTH * fontSizeSqrtMultiplier)
      .fillMaxHeight()
      .background(MaterialTheme.colors.onBackground.copy(alpha = RAIL_BACKGROUND_ALPHA))
  ) {
    RailContent(userPickerState, setPerformLA)
    VerticalDivider()
  }
}

@Composable
private fun RowScope.RailContent(userPickerState: MutableStateFlow<AnimatedViewState>, setPerformLA: (Boolean) -> Unit) {
  Column(
    Modifier.weight(1f).fillMaxHeight().padding(vertical = DEFAULT_PADDING_HALF),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    Column(
      Modifier.weight(1f).verticalScroll(rememberScrollState()),
      horizontalAlignment = Alignment.CenterHorizontally,
      verticalArrangement = Arrangement.spacedBy(DEFAULT_PADDING_HALF)
    ) {
      if (chatModel.users.isNotEmpty()) {
        RailUsers(userPickerState)
        Divider(Modifier.padding(horizontal = DEFAULT_PADDING_HALF))
        RailTags()
      }
    }
    Divider(Modifier.padding(horizontal = DEFAULT_PADDING_HALF, vertical = DEFAULT_PADDING_HALF / 2))
    RailItem(MR.images.ic_settings, stringResource(MR.strings.settings_section_title_settings), active = false) {
      ModalManager.start.closeModals()
      ModalManager.start.showModalCloseable(cardScreen = true) { close ->
        SettingsView(chatModel, setPerformLA, close)
      }
    }
  }
}

@Composable
private fun RailUsers(userPickerState: MutableStateFlow<AnimatedViewState>) {
  val users by remember {
    derivedStateOf {
      chatModel.users
        .filter { u -> u.user.activeUser || !u.user.hidden }
        .sortedByDescending { it.user.activeOrder }
    }
  }
  val switchAllowed = profileSwitchAllowed(remember { chatModel.sharedContent }.value) && !remember { chatModel.switchingUsersAndHosts }.value
  val switchEnabled = switchAllowed && (remember { chatModel.chatRunning }.value == true || chatModel.connectedToRemote)
  users.forEach { u ->
    val user = u.user
    Box(Modifier.fillMaxWidth(), contentAlignment = Alignment.Center) {
      if (user.activeUser) {
        Box(
          Modifier
            .align(Alignment.CenterStart)
            .size(RAIL_ACTIVE_MARK_WIDTH, RAIL_ITEM_SIZE * fontSizeSqrtMultiplier / 2)
            .background(MaterialTheme.colors.primary, RoundedCornerShape(topEnd = RAIL_ACTIVE_MARK_WIDTH, bottomEnd = RAIL_ACTIVE_MARK_WIDTH))
        )
      }
      Box {
        Box(
          Modifier
            .clip(CircleShape)
            .clickable(enabled = if (user.activeUser) switchAllowed else switchEnabled) {
              if (user.activeUser) {
                ModalManager.start.closeModals()
                userPickerState.value = AnimatedViewState.VISIBLE
              } else {
                if (userPickerState.value.isVisible()) userPickerState.value = AnimatedViewState.HIDING
                switchToUser(user)
              }
            }
        ) {
          ProfileImage(RAIL_ITEM_SIZE * fontSizeSqrtMultiplier, user.profile.image)
        }
        if (!user.activeUser && u.unreadCount > 0) {
          userUnreadBadge(u.unreadCount, user.showNtfs, hasPadding = false)
        }
      }
    }
  }
}

@Composable
private fun RailTags() {
  val presetTags = remember { chatModel.presetTags }
  val userTags = remember { chatModel.userTags }
  val unreadTags = remember { chatModel.unreadTags }
  val activeFilter = remember { chatModel.activeChatTagFilter }
  val rhId = chatModel.remoteHostId()

  RailItem(MR.images.ic_menu, stringResource(MR.strings.chat_list_all), active = activeFilter.value == null) {
    ModalManager.start.closeModals()
    activeFilter.value = null
  }
  PresetTagKind.entries.filter { (presetTags[it] ?: 0) > 0 }.forEach { tag ->
    val filter = ActiveFilter.PresetTag(tag)
    val active = activeFilter.value == filter
    val (icon, _, text) = presetTagLabel(tag, active)
    RailItem(icon, stringResource(text), active) { toggleFilter(filter) }
  }
  userTags.value.forEach { tag ->
    val filter = ActiveFilter.UserTag(tag)
    val active = activeFilter.value == filter
    val showMenu = rememberSaveable { mutableStateOf(false) }
    val saving = remember { mutableStateOf(false) }
    Box {
      RailItem(
        if (active) MR.images.ic_label_filled else MR.images.ic_label,
        tag.chatTagText,
        active,
        emoji = tag.chatTagEmoji,
        unread = (unreadTags[tag.chatTagId] ?: 0) > 0,
        onLongClick = { showMenu.value = true },
        enabled = !saving.value
      ) { toggleFilter(filter) }
      TagsDropdownMenu(rhId, tag, showMenu, saving)
    }
  }
  RailItem(MR.images.ic_add, stringResource(MR.strings.chat_list_add_list), active = false) {
    ModalManager.start.closeModals()
    ModalManager.start.showModalCloseable { close ->
      TagListEditor(rhId = rhId, close = close)
    }
  }
}

private fun toggleFilter(filter: ActiveFilter) {
  ModalManager.start.closeModals()
  chatModel.activeChatTagFilter.value = if (chatModel.activeChatTagFilter.value == filter) null else filter
}

@Composable
private fun RailItem(
  icon: ImageResource,
  label: String,
  active: Boolean,
  emoji: String? = null,
  unread: Boolean = false,
  onLongClick: () -> Unit = {},
  enabled: Boolean = true,
  onClick: () -> Unit
) {
  val color = if (active) MaterialTheme.colors.primary else MaterialTheme.colors.secondary
  Column(
    Modifier
      .fillMaxWidth()
      .combinedClickable(enabled = enabled, onClick = onClick, onLongClick = onLongClick)
      .onRightClick(onLongClick)
      .padding(vertical = 4.dp),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    Box(
      Modifier
        .size(RAIL_ACTIVE_BACKGROUND_WIDTH * fontSizeSqrtMultiplier, RAIL_ACTIVE_BACKGROUND_HEIGHT * fontSizeSqrtMultiplier)
        .background(if (active) MaterialTheme.colors.primary.copy(alpha = RAIL_ACTIVE_BACKGROUND_ALPHA) else Color.Transparent, RoundedCornerShape(DEFAULT_PADDING_HALF)),
      contentAlignment = Alignment.Center
    ) {
      if (emoji != null) {
        ReactionIcon(emoji, fontSize = 18.sp)
      } else {
        Icon(painterResource(icon), label, Modifier.size(RAIL_ICON_SIZE * fontSizeSqrtMultiplier), tint = color)
      }
      if (unread) {
        Box(Modifier.align(Alignment.TopEnd).padding(DEFAULT_PADDING_HALF / 2).size(RAIL_UNREAD_DOT_SIZE).background(MaterialTheme.colors.primary, CircleShape))
      }
    }
    Text(
      label,
      Modifier.padding(horizontal = 2.dp),
      color = color,
      fontSize = 11.sp,
      fontWeight = if (active) FontWeight.Medium else FontWeight.Normal,
      textAlign = TextAlign.Center,
      maxLines = 1,
      overflow = TextOverflow.Ellipsis
    )
  }
}
