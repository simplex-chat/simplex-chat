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
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.ValueTitleDesc
import chat.simplex.app.views.helpers.ValueTitle

@Composable
fun SectionView(title: String? = null, content: (@Composable () -> Unit)) {
  Column {
    if (title != null) {
      Text(
        title, color = HighOrLowlight, style = MaterialTheme.typography.body2,
        modifier = Modifier.padding(start = 16.dp, bottom = 5.dp), fontSize = 12.sp
      )
    }
    Surface(color = if (isInDarkTheme()) GroupDark else MaterialTheme.colors.background) {
      Column(Modifier.padding(horizontal = 6.dp).fillMaxWidth()) { content() }
    }
  }
}

@Composable
fun <T> SectionViewSelectable(
  title: String?,
  currentValue: MutableState<T>,
  values: List<ValueTitleDesc<T>>,
  onSelected: (T) -> Unit,
) {
  SectionView(title) {
    LazyColumn(
      Modifier.padding(horizontal = 8.dp)
    ) {
      items(values.size) { index ->
        val item = values[index]
        SectionItemViewSpaceBetween({ onSelected(item.value) }, padding = PaddingValues()) {
          Text(item.title)
          if (currentValue.value == item.value) {
            Icon(Icons.Outlined.Check, item.title, tint = HighOrLowlight)
          }
        }
        Spacer(Modifier.padding(horizontal = 4.dp))
      }
    }
  }
  SectionTextFooter(values.first { it.value == currentValue.value }.description)
}

@Composable
fun SectionItemView(click: (() -> Unit)? = null, height: Dp = 46.dp, disabled: Boolean = false, content: (@Composable RowScope.() -> Unit)) {
  val modifier = Modifier
    .padding(horizontal = 8.dp)
    .fillMaxWidth()
    .height(height)
  Row(
    if (click == null || disabled) modifier else modifier.clickable(onClick = click),
    verticalAlignment = Alignment.CenterVertically
  ) {
    content()
  }
}

@Composable
fun SectionItemViewSpaceBetween(
  click: (() -> Unit)? = null,
  height: Dp = 46.dp,
  padding: PaddingValues = PaddingValues(horizontal = 8.dp),
  disabled: Boolean = false,
  content: (@Composable () -> Unit)
) {
  val modifier = Modifier
    .padding(padding)
    .fillMaxWidth()
    .height(height)
  Row(
    if (click == null || disabled) modifier else modifier.clickable(onClick = click),
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
    Modifier.padding(horizontal = 16.dp).padding(top = 5.dp).fillMaxWidth(0.9F),
    color = HighOrLowlight,
    fontSize = 12.sp
  )
}

@Composable
fun SectionCustomFooter(padding: PaddingValues = PaddingValues(start = 16.dp, end = 16.dp, top = 5.dp), content: (@Composable () -> Unit)) {
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
fun InfoRow(title: String, value: String) {
  SectionItemView {
    Row(
      Modifier.fillMaxWidth(),
      horizontalArrangement = Arrangement.SpaceBetween,
      verticalAlignment = Alignment.CenterVertically
    ) {
      Text(title)
      Text(value, color = HighOrLowlight)
    }
  }
}

@Composable
fun InfoRowEllipsis(title: String, value: String, onClick: () -> Unit) {
  SectionItemView {
    Row(
      Modifier.fillMaxWidth(),
      horizontalArrangement = Arrangement.SpaceBetween,
      verticalAlignment = Alignment.CenterVertically
    ) {
      val configuration = LocalConfiguration.current
      Text(title)
      Text(value,
        Modifier
          .padding(start = 10.dp)
          .widthIn(max = (configuration.screenWidthDp / 2).dp)
          .clickable(onClick = onClick),
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
        color = HighOrLowlight
      )
    }
  }
}
