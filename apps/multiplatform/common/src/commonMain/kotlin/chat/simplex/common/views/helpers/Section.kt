import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
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

@Composable
fun SectionView(title: String? = null, contentPadding: PaddingValues = PaddingValues(), headerBottomPadding: Dp = DEFAULT_PADDING, content: (@Composable ColumnScope.() -> Unit)) {
  Column {
    if (title != null) {
      Text(
        title, color = MaterialTheme.colors.secondary, style = MaterialTheme.typography.body2,
        modifier = Modifier.padding(start = DEFAULT_PADDING, bottom = headerBottomPadding), fontSize = 12.sp
      )
    }
    Column(Modifier.padding(contentPadding).fillMaxWidth()) { content() }
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
  Column {
    val iconSize = with(LocalDensity.current) { 21.sp.toDp() }
    Row(Modifier.padding(start = DEFAULT_PADDING, bottom = 5.dp), verticalAlignment = Alignment.CenterVertically) {
      if (leadingIcon) Icon(icon, null, Modifier.padding(end = DEFAULT_PADDING_HALF).size(iconSize), tint = iconTint)
      Text(title, color = MaterialTheme.colors.secondary, style = MaterialTheme.typography.body2, fontSize = 12.sp)
      if (!leadingIcon) Icon(icon, null, Modifier.padding(start = DEFAULT_PADDING_HALF).size(iconSize), tint = iconTint)
    }
    Column(Modifier.padding(padding).fillMaxWidth()) { content() }
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
  SectionTextFooter(values.first { it.value == currentValue.value }.description)
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
  minHeight: Dp = DEFAULT_MIN_SECTION_ITEM_HEIGHT,
  disabled: Boolean = false,
  extraPadding: Boolean = false,
  padding: PaddingValues = if (extraPadding)
    PaddingValues(start = DEFAULT_PADDING * 1.7f, end = DEFAULT_PADDING, top = DEFAULT_MIN_SECTION_ITEM_PADDING_VERTICAL, bottom = DEFAULT_MIN_SECTION_ITEM_PADDING_VERTICAL)
  else
    PaddingValues(horizontal = DEFAULT_PADDING, vertical = DEFAULT_MIN_SECTION_ITEM_PADDING_VERTICAL),
  content: (@Composable RowScope.() -> Unit)
) {
  val modifier = Modifier
    .fillMaxWidth()
    .sizeIn(minHeight = minHeight)
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
  minHeight: Dp = DEFAULT_MIN_SECTION_ITEM_HEIGHT,
  disabled: Boolean = false,
  extraPadding: Boolean = false,
  padding: PaddingValues = if (extraPadding)
    PaddingValues(start = DEFAULT_PADDING * 1.7f, end = DEFAULT_PADDING)
  else
    PaddingValues(horizontal = DEFAULT_PADDING),
  content: (@Composable RowScope.() -> Unit)
) {
  SectionItemView(click, minHeight, disabled, extraPadding, padding, content)
}

@Composable
fun SectionItemViewLongClickable(
  click: () -> Unit,
  longClick: () -> Unit,
  minHeight: Dp = DEFAULT_MIN_SECTION_ITEM_HEIGHT,
  disabled: Boolean = false,
  extraPadding: Boolean = false,
  padding: PaddingValues = if (extraPadding)
    PaddingValues(start = DEFAULT_PADDING * 1.7f, end = DEFAULT_PADDING, top = DEFAULT_MIN_SECTION_ITEM_PADDING_VERTICAL, bottom = DEFAULT_MIN_SECTION_ITEM_PADDING_VERTICAL)
  else
    PaddingValues(horizontal = DEFAULT_PADDING, vertical = DEFAULT_MIN_SECTION_ITEM_PADDING_VERTICAL),
  content: (@Composable RowScope.() -> Unit)
) {
  val modifier = Modifier
    .fillMaxWidth()
    .sizeIn(minHeight = minHeight)
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
  minHeight: Dp = DEFAULT_MIN_SECTION_ITEM_HEIGHT,
  padding: PaddingValues = PaddingValues(horizontal = DEFAULT_PADDING),
  disabled: Boolean = false,
  content: (@Composable RowScope.() -> Unit)
) {
  val modifier = Modifier
    .fillMaxWidth()
    .sizeIn(minHeight = minHeight)
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
        values.first { it.value == currentValue.value }.title + (if (label != null) " $label" else ""),
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
    Modifier.padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING, top = DEFAULT_PADDING_HALF).fillMaxWidth(0.9F),
    color = color,
    lineHeight = 18.sp,
    fontSize = 14.sp,
    textAlign = textAlign
  )
}

@Composable
fun SectionCustomFooter(padding: PaddingValues = PaddingValues(start = DEFAULT_PADDING, end = DEFAULT_PADDING, top = 5.dp), content: (@Composable () -> Unit)) {
  Row(
    Modifier.padding(padding)
  ) {
    content()
  }
}

@Composable
fun SectionDivider() {
  Divider(Modifier.padding(horizontal = 8.dp))
}

@Composable
fun SectionDividerSpaced(maxTopPadding: Boolean = false, maxBottomPadding: Boolean = true) {
  Divider(
    Modifier.padding(
      start = DEFAULT_PADDING_HALF,
      top = if (maxTopPadding) DEFAULT_PADDING + 18.dp else DEFAULT_PADDING + 2.dp,
      end = DEFAULT_PADDING_HALF,
      bottom = if (maxBottomPadding) DEFAULT_PADDING + 18.dp else DEFAULT_PADDING + 2.dp)
  )
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
