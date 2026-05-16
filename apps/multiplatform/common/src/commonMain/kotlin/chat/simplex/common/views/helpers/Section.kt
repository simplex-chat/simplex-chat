import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.draw.drawWithContent
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.platform.LocalDensity
import dev.icerock.moko.resources.compose.painterResource
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import chat.simplex.common.platform.onRightClick
import chat.simplex.common.platform.windowWidth
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.SelectableCard
import chat.simplex.common.views.usersettings.SettingsActionItemWithContent
import chat.simplex.res.MR

private val SectionCardShape = RoundedCornerShape(16.dp)
val CARD_PADDING = 16.dp
val CARD_ITEM_PADDING = CARD_PADDING - 1.dp

// Set to true by SectionView around its inner Column. SectionItemView reads it
// to decide whether to draw the 2dp bottom divider. False default keeps
// standalone usage (alerts, pickers, custom contexts) unaffected.
internal val LocalInSectionCard = staticCompositionLocalOf { false }


@Composable
private fun Modifier.sectionItemDivider(): Modifier {
  if (!LocalInSectionCard.current) return this
  return this.drawWithContent {
    drawContent()
    drawLine(canvasColorForCurrentTheme(), Offset(0f, size.height), Offset(size.width, size.height), strokeWidth = 2.dp.toPx())
  }
}

@Composable
fun SectionView(title: String? = null, contentPadding: PaddingValues = PaddingValues(), headerBottomPadding: Dp = 8.dp, content: (@Composable ColumnScope.() -> Unit)) {
  val cardColor = if (CurrentColors.value.base == DefaultTheme.LIGHT) Color.White
    else MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.95f)
  Column {
    if (title != null) {
      Text(
        title, color = MaterialTheme.colors.secondary, style = MaterialTheme.typography.body2,
        modifier = Modifier.padding(start = DEFAULT_PADDING + DEFAULT_PADDING_HALF, bottom = headerBottomPadding), fontSize = 12.sp
      )
    }
    CompositionLocalProvider(LocalInSectionCard provides true) {
      Column(
        Modifier
          .padding(horizontal = CARD_PADDING)
          .fillMaxWidth()
          .clip(SectionCardShape)
          .background(cardColor)
          .padding(contentPadding)
      ) { content() }
    }
  }
}

@Composable
fun SectionView(
  title: String,
  icon: Painter,
  iconTint: Color = MaterialTheme.colors.secondary,
  leadingIcon: Boolean = false,
  padding: PaddingValues = PaddingValues(),
  content: (@Composable ColumnScope.() -> Unit)
) {
  val cardColor = if (CurrentColors.value.base == DefaultTheme.LIGHT) Color.White
    else MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.95f)
  Column {
    val iconSize = with(LocalDensity.current) { 21.sp.toDp() }
    Row(Modifier.padding(start = DEFAULT_PADDING + DEFAULT_PADDING_HALF, bottom = 5.dp), verticalAlignment = Alignment.CenterVertically) {
      if (leadingIcon) Icon(icon, null, Modifier.padding(end = DEFAULT_PADDING_HALF).size(iconSize), tint = iconTint)
      Text(title, color = MaterialTheme.colors.secondary, style = MaterialTheme.typography.body2, fontSize = 12.sp)
      if (!leadingIcon) Icon(icon, null, Modifier.padding(start = DEFAULT_PADDING_HALF).size(iconSize), tint = iconTint)
    }
    CompositionLocalProvider(LocalInSectionCard provides true) {
      Column(
        Modifier
          .padding(horizontal = CARD_PADDING)
          .fillMaxWidth()
          .clip(SectionCardShape)
          .background(cardColor)
          .padding(padding)
      ) { content() }
    }
  }
}

@Composable
fun SectionViewWithButton(title: String? = null, titleButton: (@Composable () -> Unit)?, contentPadding: PaddingValues = PaddingValues(), headerBottomPadding: Dp = 8.dp, content: (@Composable ColumnScope.() -> Unit)) {
  val cardColor = if (CurrentColors.value.base == DefaultTheme.LIGHT) Color.White
    else MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.95f)
  Column {
    if (title != null || titleButton != null) {
      Row(modifier = Modifier.padding(start = DEFAULT_PADDING + DEFAULT_PADDING_HALF, end = DEFAULT_PADDING + DEFAULT_PADDING_HALF, bottom = headerBottomPadding).fillMaxWidth()) {
        if (title != null) {
          Text(title, color = MaterialTheme.colors.secondary, style = MaterialTheme.typography.body2, fontSize = 12.sp)
        }
        if (titleButton != null) {
          Spacer(modifier = Modifier.weight(1f))
          titleButton()
        }
      }
    }
    CompositionLocalProvider(LocalInSectionCard provides true) {
      Column(
        Modifier
          .padding(horizontal = CARD_PADDING)
          .fillMaxWidth()
          .clip(SectionCardShape)
          .background(cardColor)
          .padding(contentPadding)
      ) { content() }
    }
  }
}

@Composable
fun <T> SectionViewSelectable(
  title: String?,
  currentValue: State<T>,
  values: List<ValueTitleDesc<T>>,
  onSelected: (T) -> Unit,
) {
  SectionView(title) {
    Column {
      values.forEach { item ->
        SectionItemViewSpaceBetween({ onSelected(item.value) }) {
          Text(item.title)
          if (currentValue.value == item.value) {
            Icon(painterResource(MR.images.ic_check), item.title, tint = MaterialTheme.colors.primary)
          }
        }
        Spacer(Modifier.padding(horizontal = 4.dp))
      }
    }
  }
  SectionTextFooter(values.firstOrNull { it.value == currentValue.value }?.description ?: AnnotatedString(""))
}

@Composable
fun <T> SectionViewSelectableCards(
  title: String?,
  currentValue: State<T>,
  values: List<ValueTitleDesc<T>>,
  onSelected: (T) -> Unit,
) {
  SectionView(title) {
    Column(Modifier.padding(horizontal = DEFAULT_PADDING)) {
      if (title != null) {
        Text(title, Modifier.fillMaxWidth(), textAlign = TextAlign.Center)
        Spacer(Modifier.height(DEFAULT_PADDING * 2f))
      }
      values.forEach { item ->
        SelectableCard(currentValue, item.value, item.title, item.description, onSelected)
      }
    }
  }
}

@Composable
fun SectionItemView(
  click: (() -> Unit)? = null,
  minHeight: Dp = DEFAULT_MIN_SECTION_ITEM_HEIGHT + 8.dp,
  disabled: Boolean = false,
  extraPadding: Boolean = false,
  padding: PaddingValues = if (extraPadding)
    PaddingValues(start = DEFAULT_PADDING * 1.7f, end = CARD_ITEM_PADDING, top = DEFAULT_MIN_SECTION_ITEM_PADDING_VERTICAL, bottom = DEFAULT_MIN_SECTION_ITEM_PADDING_VERTICAL)
  else
    PaddingValues(horizontal = CARD_ITEM_PADDING, vertical = DEFAULT_MIN_SECTION_ITEM_PADDING_VERTICAL),
  content: (@Composable RowScope.() -> Unit)
) {
  val modifier = Modifier
    .fillMaxWidth()
    .sizeIn(minHeight = minHeight)
    .sectionItemDivider()
  Row(
    if (click == null || disabled) modifier.padding(padding) else modifier.clickable(onClick = click).padding(padding),
    verticalAlignment = Alignment.CenterVertically
  ) {
    content()
  }
}

@Composable
fun SectionItemViewWithoutMinPadding(
  click: (() -> Unit)? = null,
  minHeight: Dp = DEFAULT_MIN_SECTION_ITEM_HEIGHT + 8.dp,
  disabled: Boolean = false,
  extraPadding: Boolean = false,
  padding: PaddingValues = if (extraPadding)
    PaddingValues(start = DEFAULT_PADDING * 1.7f, end = CARD_ITEM_PADDING)
  else
    PaddingValues(horizontal = CARD_ITEM_PADDING),
  content: (@Composable RowScope.() -> Unit)
) {
  SectionItemView(click, minHeight, disabled, extraPadding, padding, content)
}

@Composable
fun SectionItemViewLongClickable(
  click: () -> Unit,
  longClick: () -> Unit,
  minHeight: Dp = DEFAULT_MIN_SECTION_ITEM_HEIGHT + 8.dp,
  disabled: Boolean = false,
  extraPadding: Boolean = false,
  padding: PaddingValues = if (extraPadding)
    PaddingValues(start = DEFAULT_PADDING * 1.7f, end = CARD_ITEM_PADDING, top = DEFAULT_MIN_SECTION_ITEM_PADDING_VERTICAL, bottom = DEFAULT_MIN_SECTION_ITEM_PADDING_VERTICAL)
  else
    PaddingValues(horizontal = CARD_ITEM_PADDING, vertical = DEFAULT_MIN_SECTION_ITEM_PADDING_VERTICAL),
  content: (@Composable RowScope.() -> Unit)
) {
  val modifier = Modifier
    .fillMaxWidth()
    .sizeIn(minHeight = minHeight)
    .sectionItemDivider()
  Row(
    if (disabled) {
      modifier.padding(padding)
    } else {
      modifier.combinedClickable(onClick = click, onLongClick = longClick).onRightClick(longClick).padding(padding)
    },
    verticalAlignment = Alignment.CenterVertically
  ) {
    content()
  }
}

@Composable
fun SectionItemViewSpaceBetween(
  click: (() -> Unit)? = null,
  onLongClick: (() -> Unit)? = null,
  minHeight: Dp = DEFAULT_MIN_SECTION_ITEM_HEIGHT + 8.dp,
  padding: PaddingValues = PaddingValues(horizontal = CARD_ITEM_PADDING),
  disabled: Boolean = false,
  content: (@Composable RowScope.() -> Unit)
) {
  val modifier = Modifier
    .fillMaxWidth()
    .sizeIn(minHeight = minHeight)
    .sectionItemDivider()
  Row(
    if (click == null || disabled) modifier.padding(padding).padding(vertical = DEFAULT_MIN_SECTION_ITEM_PADDING_VERTICAL) else modifier
      .combinedClickable(onClick = click, onLongClick = onLongClick).padding(padding)
      .onRightClick { onLongClick?.invoke() },
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    content()
  }
}

@Composable
fun <T> SectionItemWithValue(
  title: String,
  currentValue: State<T>,
  values: List<ValueTitle<T>>,
  label: String? = null,
  icon: Painter? = null,
  iconTint: Color = MaterialTheme.colors.secondary,
  enabled: State<Boolean> = mutableStateOf(true),
  onSelected: () -> Unit
) {
  SettingsActionItemWithContent(icon = icon, text = title, iconColor = iconTint, click = if (enabled.value) onSelected else null, disabled = !enabled.value) {
    Row(
      Modifier.padding(start = 10.dp),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.End
    ) {
      Text(
        (values.firstOrNull { it.value == currentValue.value }?.title ?: "") + (if (label != null) " $label" else ""),
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
        color = MaterialTheme.colors.secondary
      )
    }
  }
}

@Composable
fun SectionTextFooter(text: String, color: Color = MaterialTheme.colors.secondary) {
  SectionTextFooter(AnnotatedString(text), color = color)
}

@Composable
fun SectionTextFooter(text: AnnotatedString, textAlign: TextAlign = TextAlign.Start, color: Color = MaterialTheme.colors.secondary) {
  Text(
    text,
    Modifier.padding(start = DEFAULT_PADDING + DEFAULT_PADDING_HALF, end = DEFAULT_PADDING + DEFAULT_PADDING_HALF, top = DEFAULT_PADDING_HALF).fillMaxWidth(0.9F),
    color = color,
    lineHeight = 18.sp,
    fontSize = 14.sp,
    textAlign = textAlign
  )
}

@Composable
fun SectionCustomFooter(padding: PaddingValues = PaddingValues(start = DEFAULT_PADDING + DEFAULT_PADDING_HALF, end = DEFAULT_PADDING + DEFAULT_PADDING_HALF, top = 5.dp), content: (@Composable () -> Unit)) {
  Row(
    Modifier.padding(padding)
  ) {
    content()
  }
}

// Explicit 2dp canvas-color divider for inserting between non-SectionItemView
// composables inside a SectionView card (e.g., between a custom block and a
// SectionItemView). Auto-divider on SectionItemView handles the row-to-row
// case; this one covers manual placement around mixed content. No-op outside
// a SectionView card.
@Composable
fun SectionDivider() {
  if (!LocalInSectionCard.current) return
  Box(Modifier.fillMaxWidth().height(2.dp).background(canvasColorForCurrentTheme()))
}

@Composable
fun SectionDividerSpaced(maxTopPadding: Boolean = false, maxBottomPadding: Boolean = true) {
  Spacer(Modifier.height(if (maxTopPadding || maxBottomPadding) DEFAULT_PADDING else DEFAULT_PADDING_HALF))
}

@Composable
fun SectionSpacer() {
  Spacer(Modifier.height(30.dp))
}

@Composable
fun SectionBottomSpacer() {
  Spacer(Modifier.height(DEFAULT_BOTTOM_PADDING))
}

@Composable
fun TextIconSpaced(extraPadding: Boolean = false) {
  Spacer(Modifier.padding(horizontal = if (extraPadding) 17.dp else DEFAULT_PADDING_HALF))
}

@Composable
fun InfoRow(title: String, value: String, icon: Painter? = null, iconTint: Color? = null, textColor: Color = MaterialTheme.colors.onBackground, padding: PaddingValues = PaddingValues(horizontal = DEFAULT_PADDING)) {
  SectionItemViewSpaceBetween(padding = padding) {
    Row {
      val iconSize = with(LocalDensity.current) { 21.sp.toDp() }
      if (icon != null) Icon(icon, title, Modifier.padding(end = 8.dp).size(iconSize), tint = iconTint ?: MaterialTheme.colors.secondary)
      Text(title, color = textColor)
    }
    Text(value, color = MaterialTheme.colors.secondary)
  }
}

fun numOrDash(n: Number): String = if (n.toLong() == 0L) "-" else n.toString()

@Composable
fun InfoRowTwoValues(
  title: String,
  title2: String,
  value: Int,
  value2: Int,
  textColor: Color = MaterialTheme.colors.onBackground
) {
  SectionItemViewSpaceBetween {
    Row(
      verticalAlignment = Alignment.Bottom
    ) {
      Text(
        text = title,
        color = textColor,
      )
      Text(
        text = " / ",
        fontSize = 12.sp,
      )
      Text(
        text = title2,
        color = textColor,
        fontSize = 12.sp,
      )
    }
    Row(verticalAlignment = Alignment.Bottom) {
      if (value == 0 && value2 == 0) {
        Text(
          text = "-",
          color = MaterialTheme.colors.secondary
        )
      } else {
        Text(
          text = numOrDash(value),
          color = MaterialTheme.colors.secondary,
        )
        Text(
          text = " / ",
          color = MaterialTheme.colors.secondary,
          fontSize = 12.sp,
        )
        Text(
          text = numOrDash(value2),
          color = MaterialTheme.colors.secondary,
          fontSize = 12.sp,
        )
      }
    }
  }
}


@Composable
fun InfoRowEllipsis(title: String, value: String, onClick: () -> Unit) {
  SectionItemViewSpaceBetween(onClick) {
    val screenWidthDp = windowWidth()
    Text(title)
    Text(
      value,
      Modifier
        .padding(start = 10.dp)
        .widthIn(max = (screenWidthDp / 2)),
      maxLines = 1,
      overflow = TextOverflow.Ellipsis,
      color = MaterialTheme.colors.secondary
    )
  }
}
