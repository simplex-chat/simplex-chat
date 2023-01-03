import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Check
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.ValueTitleDesc
import chat.simplex.app.views.helpers.ValueTitle

@Composable
fun SectionView(title: String? = null, padding: PaddingValues = PaddingValues(), content: (@Composable ColumnScope.() -> Unit)) {
  Column {
    if (title != null) {
      Text(
        title, color = HighOrLowlight, style = MaterialTheme.typography.body2,
        modifier = Modifier.padding(start = DEFAULT_PADDING, bottom = 5.dp), fontSize = 12.sp
      )
    }
    Surface(color = if (isInDarkTheme()) GroupDark else MaterialTheme.colors.background) {
      Column(Modifier.padding(padding).fillMaxWidth()) { content() }
    }
  }
}

@Composable
fun SectionView(
  title: String,
  icon: ImageVector,
  iconTint: Color = HighOrLowlight,
  leadingIcon: Boolean = false,
  padding: PaddingValues = PaddingValues(),
  content: (@Composable ColumnScope.() -> Unit)
) {
  Column {
    val iconSize = with(LocalDensity.current) { 21.sp.toDp() }
    Row(Modifier.padding(start = DEFAULT_PADDING, bottom = 5.dp), verticalAlignment = Alignment.CenterVertically) {
      if (leadingIcon) Icon(icon, null, Modifier.padding(end = 4.dp).size(iconSize), tint = iconTint)
      Text(title, color = HighOrLowlight, style = MaterialTheme.typography.body2, fontSize = 12.sp)
      if (!leadingIcon) Icon(icon, null, Modifier.padding(start = 4.dp).size(iconSize), tint = iconTint)
    }
    Surface(color = if (isInDarkTheme()) GroupDark else MaterialTheme.colors.background) {
      Column(Modifier.padding(padding).fillMaxWidth()) { content() }
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
    LazyColumn {
      items(values.size) { index ->
        val item = values[index]
        SectionItemViewSpaceBetween({ onSelected(item.value) }) {
          Text(item.title)
          if (currentValue.value == item.value) {
            Icon(Icons.Outlined.Check, item.title, tint = MaterialTheme.colors.primary)
          }
        }
        Spacer(Modifier.padding(horizontal = 4.dp))
      }
    }
  }
  SectionTextFooter(values.first { it.value == currentValue.value }.description)
}

@Composable
fun SectionItemView(
  click: (() -> Unit)? = null,
  minHeight: Dp = 46.dp,
  disabled: Boolean = false,
  content: (@Composable RowScope.() -> Unit)
) {
  val modifier = Modifier
    .fillMaxWidth()
    .sizeIn(minHeight = minHeight)
  Row(
    if (click == null || disabled) modifier.padding(horizontal = DEFAULT_PADDING) else modifier.clickable(onClick = click).padding(horizontal = DEFAULT_PADDING),
    verticalAlignment = Alignment.CenterVertically
  ) {
    content()
  }
}

@Composable
fun SectionItemViewSpaceBetween(
  click: (() -> Unit)? = null,
  minHeight: Dp = 46.dp,
  padding: PaddingValues = PaddingValues(horizontal = DEFAULT_PADDING),
  disabled: Boolean = false,
  content: (@Composable RowScope.() -> Unit)
) {
  val modifier = Modifier
    .fillMaxWidth()
    .sizeIn(minHeight = minHeight)
  Row(
    if (click == null || disabled) modifier.padding(padding) else modifier.clickable(onClick = click).padding(padding),
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
  icon: ImageVector? = null,
  iconTint: Color = HighOrLowlight,
  enabled: State<Boolean> = mutableStateOf(true),
  onSelected: () -> Unit
) {
  SectionItemView(click = if (enabled.value) onSelected else null) {
    if (icon != null) {
      Icon(
        icon,
        title,
        Modifier.padding(end = 8.dp),
        tint = iconTint
      )
    }
    Text(title, color = if (enabled.value) Color.Unspecified else HighOrLowlight)

    Spacer(Modifier.fillMaxWidth().weight(1f))

    Row(
      Modifier.padding(start = 10.dp),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.End
    ) {
      Text(
        values.first { it.value == currentValue.value }.title + (if (label != null) " $label" else ""),
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
        color = HighOrLowlight
      )
    }
  }
}

@Composable
fun SectionTextFooter(text: String) {
  Text(
    text,
    Modifier.padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING, top = DEFAULT_PADDING_HALF).fillMaxWidth(0.9F),
    color = HighOrLowlight,
    lineHeight = 18.sp,
    fontSize = 14.sp
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
fun SectionSpacer() {
  Spacer(Modifier.height(30.dp))
}

@Composable
fun InfoRow(title: String, value: String, icon: ImageVector? = null, iconTint: Color? = null) {
  SectionItemViewSpaceBetween {
    Row {
      val iconSize = with(LocalDensity.current) { 21.sp.toDp() }
      if (icon != null) Icon(icon, title, Modifier.padding(end = 8.dp).size(iconSize), tint = iconTint ?: HighOrLowlight)
      Text(title)
    }
    Text(value, color = HighOrLowlight)
  }
}

@Composable
fun InfoRowEllipsis(title: String, value: String, onClick: () -> Unit) {
  SectionItemViewSpaceBetween(onClick) {
    val configuration = LocalConfiguration.current
    Text(title)
    Text(
      value,
      Modifier
        .padding(start = 10.dp)
        .widthIn(max = (configuration.screenWidthDp / 2).dp),
      maxLines = 1,
      overflow = TextOverflow.Ellipsis,
      color = HighOrLowlight
    )
  }
}
