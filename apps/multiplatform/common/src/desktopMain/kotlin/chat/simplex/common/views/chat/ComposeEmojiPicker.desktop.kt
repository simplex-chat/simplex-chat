package chat.simplex.common.views.chat

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.grid.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.focus.focusProperties
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.input.key.*
import androidx.compose.ui.layout.onGloballyPositioned
import androidx.compose.ui.layout.positionInWindow
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.*
import androidx.compose.ui.window.*
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.common.ui.theme.EmojiFont
import chat.simplex.common.views.helpers.KeyChangeEffect
import chat.simplex.common.views.helpers.SearchTextField
import chat.simplex.res.*
import dev.icerock.moko.resources.StringResource
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.launch

private const val EMOJI_DATA_RESOURCE = "/emoji/emoji.txt"
private const val EMOJI_GROUP_PREFIX = "@"
private val EMOJI_POPUP_WIDTH = 360.dp
private val EMOJI_POPUP_HEIGHT = 420.dp
private val EMOJI_CELL_SIZE = 38.dp
private val EMOJI_SIZE = 26.dp
private val EMOJI_TAB_SIZE = 32.dp
private val EMOJI_TAB_ICON_SIZE = 20.dp
// the buttons left of the message field overlap by 8 dp, this keeps the emoji button a little apart
private val EMOJI_BUTTON_START_PADDING = 4.dp

enum class EmojiCategory(val unicodeGroup: String, val icon: String, val title: StringResource) {
  Smileys("Smileys & Emotion", "😀", MR.strings.emoji_category_smileys),
  People("People & Body", "👋", MR.strings.emoji_category_people),
  Animals("Animals & Nature", "🐻", MR.strings.emoji_category_animals),
  Food("Food & Drink", "🍔", MR.strings.emoji_category_food),
  Travel("Travel & Places", "🚗", MR.strings.emoji_category_travel),
  Activities("Activities", "⚽", MR.strings.emoji_category_activities),
  Objects("Objects", "💡", MR.strings.emoji_category_objects),
  Symbols("Symbols", "🔣", MR.strings.emoji_category_symbols),
  Flags("Flags", "🏁", MR.strings.emoji_category_flags),
}

data class EmojiData(val emoji: String, val name: String)

data class EmojiSection(val category: EmojiCategory, val emojis: List<EmojiData>)

fun parseEmojiData(lines: Sequence<String>): List<EmojiSection> {
  val sections = mutableListOf<EmojiSection>()
  var emojis = mutableListOf<EmojiData>()
  for (line in lines) {
    if (line.isEmpty()) continue
    if (line.startsWith(EMOJI_GROUP_PREFIX)) {
      val group = line.removePrefix(EMOJI_GROUP_PREFIX)
      val category = EmojiCategory.entries.firstOrNull { it.unicodeGroup == group }
        ?: throw IllegalArgumentException("unknown emoji group: $group")
      emojis = mutableListOf()
      sections.add(EmojiSection(category, emojis))
    } else {
      val (emoji, name) = line.split("\t", limit = 2)
      emojis.add(EmojiData(emoji, name))
    }
  }
  return sections
}

val emojiSections: List<EmojiSection> by lazy {
  val stream = EmojiCategory::class.java.getResourceAsStream(EMOJI_DATA_RESOURCE)
    ?: throw IllegalStateException("missing resource $EMOJI_DATA_RESOURCE")
  stream.bufferedReader().useLines(::parseEmojiData)
}

fun searchEmoji(sections: List<EmojiSection>, query: String): List<EmojiData> {
  val q = query.trim()
  return sections.flatMap { s -> s.emojis.filter { it.name.contains(q, ignoreCase = true) } }
}

@Composable
actual fun ComposeEmojiButton(pickerShown: MutableState<Boolean>, enabled: Boolean, onEmoji: (String) -> Unit) {
  // the popup opens above the button, so in a short window it must not be taller than the space there
  var spaceAbove by remember { mutableStateOf(EMOJI_POPUP_HEIGHT) }
  val density = LocalDensity.current
  Box(Modifier.padding(start = EMOJI_BUTTON_START_PADDING).onGloballyPositioned { spaceAbove = with(density) { it.positionInWindow().y.toDp() } }) {
    EmojiIconButton(painterResource(MR.images.ic_add_reaction), stringResource(MR.strings.icon_descr_emoji), enabled) {
      pickerShown.value = !pickerShown.value
    }
    if (pickerShown.value) {
      Popup(
        popupPositionProvider = AboveAnchorPositionProvider,
        onDismissRequest = { pickerShown.value = false },
        properties = PopupProperties(focusable = true),
        // closing on KeyUp, as the window does, so that the same key release does not also close the top modal
        onKeyEvent = {
          if (it.key == Key.Escape) {
            if (it.type == KeyEventType.KeyUp) pickerShown.value = false
            true
          } else false
        }
      ) {
        EmojiPicker(height = minOf(EMOJI_POPUP_HEIGHT, spaceAbove), onEmoji = onEmoji)
      }
    }
  }
}

private object AboveAnchorPositionProvider : PopupPositionProvider {
  override fun calculatePosition(anchorBounds: IntRect, windowSize: IntSize, layoutDirection: LayoutDirection, popupContentSize: IntSize): IntOffset {
    val x = if (layoutDirection == LayoutDirection.Ltr) anchorBounds.left else anchorBounds.right - popupContentSize.width
    return IntOffset(
      x.coerceIn(0, maxOf(0, windowSize.width - popupContentSize.width)),
      (anchorBounds.top - popupContentSize.height).coerceAtLeast(0)
    )
  }
}

@Composable
private fun EmojiPicker(height: Dp, onEmoji: (String) -> Unit) {
  val sections = remember { emojiSections }
  val query = remember { mutableStateOf("") }
  val found = remember { derivedStateOf { if (query.value.isBlank()) null else searchEmoji(sections, query.value) } }
  val gridState = rememberLazyGridState()
  KeyChangeEffect(found.value) { gridState.scrollToItem(0) }
  val headerIndexes = remember(sections) {
    var index = 0
    sections.map { s -> index.also { index += s.emojis.size + 1 } }
  }
  Surface(
    Modifier.size(EMOJI_POPUP_WIDTH, height),
    shape = RoundedCornerShape(12.dp),
    elevation = 8.dp,
    border = BorderStroke(1.dp, MaterialTheme.colors.secondary.copy(alpha = 0.2f))
  ) {
    Column {
      Row(verticalAlignment = Alignment.CenterVertically) {
        Icon(
          painterResource(MR.images.ic_search),
          contentDescription = null,
          Modifier.padding(start = DEFAULT_PADDING_HALF, end = DEFAULT_PADDING_HALF).size(22.dp),
          tint = MaterialTheme.colors.secondary
        )
        SearchTextField(Modifier.weight(1f), alwaysVisible = false) { query.value = it }
      }
      if (found.value == null) {
        EmojiCategoryTabs(sections, headerIndexes, gridState)
      }
      Divider()
      LazyVerticalGrid(
        columns = GridCells.Adaptive(EMOJI_CELL_SIZE),
        state = gridState,
        modifier = Modifier.fillMaxSize(),
        contentPadding = PaddingValues(6.dp)
      ) {
        val foundEmojis = found.value
        if (foundEmojis == null) {
          sections.forEach { s ->
            sectionHeader(s.category.title)
            items(s.emojis) { EmojiCell(it.emoji, onEmoji) }
          }
        } else if (foundEmojis.isEmpty()) {
          sectionHeader(MR.strings.no_emoji_found)
        } else {
          items(foundEmojis) { EmojiCell(it.emoji, onEmoji) }
        }
      }
    }
  }
}

@Composable
private fun EmojiCategoryTabs(sections: List<EmojiSection>, headerIndexes: List<Int>, gridState: LazyGridState) {
  val scope = rememberCoroutineScope()
  val selected = remember {
    derivedStateOf { headerIndexes.indexOfLast { it <= gridState.firstVisibleItemIndex } }
  }
  Row(Modifier.fillMaxWidth().padding(horizontal = 6.dp), horizontalArrangement = Arrangement.SpaceBetween) {
    sections.forEachIndexed { i, s ->
      EmojiTab(selected = selected.value == i, onClick = { scope.launch { gridState.scrollToItem(headerIndexes[i]) } }) {
        Text(s.category.icon, fontFamily = EmojiFont, fontSize = with(LocalDensity.current) { EMOJI_TAB_ICON_SIZE.toSp() })
      }
    }
  }
}

@Composable
private fun EmojiTab(selected: Boolean, onClick: () -> Unit, content: @Composable () -> Unit) {
  Box(
    Modifier
      .size(EMOJI_TAB_SIZE)
      .clip(RoundedCornerShape(8.dp))
      .background(if (selected) MaterialTheme.colors.secondary.copy(alpha = 0.15f) else Color.Transparent)
      .focusProperties { canFocus = false }
      .clickable(onClick = onClick),
    contentAlignment = Alignment.Center
  ) {
    content()
  }
}

private fun LazyGridScope.sectionHeader(title: StringResource) {
  item(span = { GridItemSpan(maxLineSpan) }) {
    Text(
      stringResource(title),
      Modifier.padding(start = 4.dp, top = 8.dp, bottom = 4.dp),
      style = MaterialTheme.typography.body2,
      color = MaterialTheme.colors.secondary
    )
  }
}

@Composable
private fun EmojiCell(emoji: String, onEmoji: (String) -> Unit) {
  Box(
    Modifier
      .size(EMOJI_CELL_SIZE)
      .clip(RoundedCornerShape(8.dp))
      // not focusable, so that the search field keeps focus and typing after a click goes to search
      .focusProperties { canFocus = false }
      .clickable { onEmoji(emoji) },
    contentAlignment = Alignment.Center
  ) {
    Text(emoji, fontFamily = EmojiFont, fontSize = with(LocalDensity.current) { EMOJI_SIZE.toSp() }, maxLines = 1)
  }
}

@Composable
private fun EmojiIconButton(icon: Painter, contentDescription: String, enabled: Boolean, onClick: () -> Unit) {
  IconButton(onClick, enabled = enabled) {
    Icon(
      icon,
      contentDescription = contentDescription,
      tint = if (enabled) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
      modifier = Modifier
        .size(28.dp)
        .clip(CircleShape)
    )
  }
}
