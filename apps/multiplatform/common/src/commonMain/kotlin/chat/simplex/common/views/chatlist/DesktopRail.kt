package chat.simplex.common.views.chatlist

import androidx.compose.animation.core.animateDpAsState
import androidx.compose.animation.core.animateFloatAsState
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.interaction.collectIsHoveredAsState
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.item.ReactionIcon
import chat.simplex.common.views.chat.item.flatContrastSecondaryVariant
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.SettingsView
import chat.simplex.res.*
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.flow.MutableStateFlow
import java.text.BreakIterator
import kotlin.math.roundToInt

val DESKTOP_RAIL_WIDTH = 64.dp
private val RAIL_ITEM_SIZE = 40.dp
private val RAIL_ICON_SIZE = 22.dp
private val RAIL_ACTIVE_MARK_WIDTH = 4.dp
private val RAIL_ACTIVE_BACKGROUND_WIDTH = 40.dp
private val RAIL_ACTIVE_BACKGROUND_HEIGHT = 32.dp
private val RAIL_UNREAD_DOT_SIZE = 6.dp
private const val RAIL_ACTIVE_BACKGROUND_ALPHA = 0.1f
private val RAIL_ITEM_INSET = 6.dp
private const val RAIL_TILE_CIRCLE_PERCENT = 50f
// hovered and active tiles scale their corner radius by this ratio, so circles turn into rounded squares
private const val RAIL_TILE_HOVER_CORNER_RATIO = 0.6f
private const val RAIL_TILE_ALPHA = 0.15f
private val RAIL_INITIAL_FONT_SIZE = 18.sp
private val RAIL_FADE_HEIGHT = 12.dp
private const val RAIL_MARK_ACTIVE_FRACTION = 0.8f
private const val RAIL_MARK_HOVER_FRACTION = 0.4f
// the rail and the chat list are shaded apart from the chat, which keeps the theme background and wallpaper:
// light themes darken both, dark themes darken the rail and lift the list, since darkening near-black is invisible
private const val FLAT_RAIL_SHADE_LIGHT = 0.07f
private const val FLAT_LIST_SHADE_LIGHT = 0.035f
private const val FLAT_RAIL_SHADE_DARK = 0.5f
private const val FLAT_LIST_LIFT_DARK = 0.045f

@Composable
fun flatRailColor(): Color =
  MaterialTheme.colors.background.mixWith(Color.Black, 1 - if (MaterialTheme.colors.isLight) FLAT_RAIL_SHADE_LIGHT else FLAT_RAIL_SHADE_DARK)

@Composable
fun flatListColor(): Color =
  if (MaterialTheme.colors.isLight) MaterialTheme.colors.background.mixWith(Color.Black, 1 - FLAT_LIST_SHADE_LIGHT)
  else MaterialTheme.colors.background.mixWith(Color.White, 1 - FLAT_LIST_LIFT_DARK)

@Composable
fun FlatListSurface(content: @Composable () -> Unit) {
  val colors = MaterialTheme.colors
  val background = flatListColor()
  Box(Modifier.fillMaxSize().background(background)) {
    MaterialTheme(
      colors = colors.copy(background = background, secondaryVariant = flatContrastSecondaryVariant(colors)),
      typography = MaterialTheme.typography,
      shapes = MaterialTheme.shapes,
      content = content
    )
  }
}

// Spec: spec/client/chat-list.md#DesktopRail
@Composable
fun DesktopRail(userPickerState: MutableStateFlow<AnimatedViewState>, setPerformLA: (Boolean) -> Unit) {
  Column(
    Modifier
      .width(DESKTOP_RAIL_WIDTH * fontSizeSqrtMultiplier)
      .fillMaxHeight()
      .background(flatRailColor())
      .padding(vertical = DEFAULT_PADDING_HALF),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    val railColor = flatRailColor()
    Box(Modifier.weight(1f)) {
      Column(
        Modifier.fillMaxSize().verticalScroll(rememberScrollState()),
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.spacedBy(DEFAULT_PADDING_HALF)
      ) {
        if (chatModel.users.isNotEmpty()) {
          RailUsers(userPickerState)
          Divider(Modifier.padding(horizontal = DEFAULT_PADDING_HALF))
          RailTags()
        }
      }
      Box(
        Modifier
          .align(Alignment.BottomCenter)
          .fillMaxWidth()
          .height(RAIL_FADE_HEIGHT)
          .background(Brush.verticalGradient(listOf(Color.Transparent, railColor)))
      )
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
    key(u.user.userId) {
      val user = u.user
      val interactionSource = remember { MutableInteractionSource() }
      val hovered = interactionSource.collectIsHoveredAsState().value
      val tileSize = RAIL_ITEM_SIZE * fontSizeSqrtMultiplier
      val markHeight by animateDpAsState(
        when {
          user.activeUser -> tileSize * RAIL_MARK_ACTIVE_FRACTION
          hovered -> tileSize * RAIL_MARK_HOVER_FRACTION
          else -> 0.dp
        }
      )
      val baseCorner = remember { appPreferences.profileImageCornerRadius.state }.value.coerceIn(0f, RAIL_TILE_CIRCLE_PERCENT)
      val corner by animateFloatAsState(if (user.activeUser || hovered) baseCorner * RAIL_TILE_HOVER_CORNER_RATIO else baseCorner)
      Box(Modifier.fillMaxWidth(), contentAlignment = Alignment.Center) {
        Box(
          Modifier
            .align(Alignment.CenterStart)
            .size(RAIL_ACTIVE_MARK_WIDTH, markHeight)
            .background(MaterialTheme.colors.primary, RoundedCornerShape(topEnd = RAIL_ACTIVE_MARK_WIDTH, bottomEnd = RAIL_ACTIVE_MARK_WIDTH))
        )
        Box {
          Box(
            Modifier
              .size(tileSize)
              .clip(RoundedCornerShape(percent = corner.roundToInt()))
              .clickable(interactionSource = interactionSource, indication = LocalIndication.current, enabled = if (user.activeUser) switchAllowed else switchEnabled) {
                if (user.activeUser) {
                  ModalManager.start.closeModals()
                  userPickerState.value = AnimatedViewState.VISIBLE
                } else {
                  if (userPickerState.value.isVisible()) userPickerState.value = AnimatedViewState.HIDING
                  switchToUser(user)
                }
              }
          ) {
            RailProfileTile(user)
          }
          if (!user.activeUser && u.unreadCount > 0) {
            userUnreadBadge(u.unreadCount, user.showNtfs, hasPadding = false)
          }
        }
      }
    }
  }
}

@Composable
private fun RailProfileTile(user: User) {
  val image = user.profile.image
  if (image != null) {
    val bitmap = remember(image) { base64ToBitmap(image) }
    Image(bitmap, user.displayName, Modifier.fillMaxSize(), contentScale = ContentScale.Crop)
  } else {
    Box(Modifier.fillMaxSize().background(MaterialTheme.colors.primary.copy(alpha = RAIL_TILE_ALPHA)), contentAlignment = Alignment.Center) {
      Text(firstGrapheme(user.displayName).uppercase(), color = MaterialTheme.colors.primary, fontSize = RAIL_INITIAL_FONT_SIZE, fontWeight = FontWeight.SemiBold)
    }
  }
}

private val REGIONAL_INDICATORS = 0x1F1E6..0x1F1FF

fun firstGrapheme(text: String): String {
  val iterator = BreakIterator.getCharacterInstance()
  iterator.setText(text)
  var end = iterator.next()
  if (end == BreakIterator.DONE) return ""
  // JDK before 20 splits a flag emoji into its two regional indicators; newer JDKs return the whole flag
  val first = text.codePointAt(0)
  if (first in REGIONAL_INDICATORS && end == Character.charCount(first) && end < text.length && text.codePointAt(end) in REGIONAL_INDICATORS) {
    end = iterator.next()
  }
  return text.substring(0, end)
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
  val color = if (active) MaterialTheme.colors.onBackground else MaterialTheme.colors.secondary
  Column(
    Modifier
      .fillMaxWidth()
      .padding(horizontal = RAIL_ITEM_INSET)
      .clip(RoundedCornerShape(DEFAULT_PADDING_HALF))
      .combinedClickable(enabled = enabled, onClick = onClick, onLongClick = onLongClick)
      .onRightClick(onLongClick)
      .padding(vertical = 4.dp),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    Box(
      Modifier
        .size(RAIL_ACTIVE_BACKGROUND_WIDTH * fontSizeSqrtMultiplier, RAIL_ACTIVE_BACKGROUND_HEIGHT * fontSizeSqrtMultiplier)
        .background(if (active) MaterialTheme.colors.onBackground.copy(alpha = RAIL_ACTIVE_BACKGROUND_ALPHA) else Color.Transparent, RoundedCornerShape(DEFAULT_PADDING_HALF)),
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
