package chat.simplex.common.views.helpers

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.background
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import chat.simplex.common.ui.theme.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource

@Composable
fun CloseSheetBar(close: (() -> Unit)?, showClose: Boolean = true, tintColor: Color = if (close != null) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,  arrangement: Arrangement.Vertical = Arrangement.Top, closeBarTitle: String? = null, barPaddingValues: PaddingValues = PaddingValues(horizontal = AppBarHorizontalPadding), endButtons: @Composable RowScope.() -> Unit = {}) {
  var rowModifier = Modifier
    .fillMaxWidth()
    .height(AppBarHeight * fontSizeSqrtMultiplier)

  if (!closeBarTitle.isNullOrEmpty()) {
    rowModifier = rowModifier.background(MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.97f))
  }

  Column(
    verticalArrangement = arrangement,
    modifier = Modifier
      .fillMaxWidth()
      .heightIn(min = AppBarHeight * fontSizeSqrtMultiplier)
  ) {
    Row(
      modifier = Modifier.padding(barPaddingValues),
      content = {
        Row(
          rowModifier,
          horizontalArrangement = Arrangement.SpaceBetween,
          verticalAlignment = Alignment.CenterVertically
        ) {
          if (showClose)  {
            NavigationButtonBack(tintColor = tintColor, onButtonClicked = close)
          } else {
            Spacer(Modifier)
          }
          if (!closeBarTitle.isNullOrEmpty()) {
            Row(
              horizontalArrangement = Arrangement.Center,
              verticalAlignment = Alignment.CenterVertically
            ) {
              Text(
                closeBarTitle,
                color = MaterialTheme.colors.onBackground,
                fontWeight = FontWeight.SemiBold,
              )
            }
          }
          Row {
            endButtons()
          }
        }
      }
    )
  }
}

@Composable
fun AppBarTitle(title: String, hostDevice: Pair<Long?, String>? = null,  withPadding: Boolean = true, bottomPadding: Dp = DEFAULT_PADDING * 1.5f + 8.dp) {
  val theme = CurrentColors.collectAsState()
  val titleColor = MaterialTheme.appColors.title
  val brush = if (theme.value.base == DefaultTheme.SIMPLEX)
    Brush.linearGradient(listOf(titleColor.darker(0.2f), titleColor.lighter(0.35f)), Offset(0f, Float.POSITIVE_INFINITY), Offset(Float.POSITIVE_INFINITY, 0f))
  else // color is not updated when changing themes if I pass null here
    Brush.linearGradient(listOf(titleColor, titleColor), Offset(0f, Float.POSITIVE_INFINITY), Offset(Float.POSITIVE_INFINITY, 0f))
  Column {
    Text(
      title,
      Modifier
        .fillMaxWidth()
        .padding(start = if (withPadding) DEFAULT_PADDING else 0.dp, end = if (withPadding) DEFAULT_PADDING else 0.dp,),
      overflow = TextOverflow.Ellipsis,
      style = MaterialTheme.typography.h1.copy(brush = brush),
      color = MaterialTheme.colors.primaryVariant,
      textAlign = TextAlign.Center
    )
    if (hostDevice != null) {
      HostDeviceTitle(hostDevice)
    }
    Spacer(Modifier.height(bottomPadding))
  }
}

@Composable
private fun HostDeviceTitle(hostDevice: Pair<Long?, String>, extraPadding: Boolean = false) {
  Row(Modifier.fillMaxWidth().padding(top = 5.dp, bottom = if (extraPadding) DEFAULT_PADDING * 2 else DEFAULT_PADDING_HALF), verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.Center) {
    Icon(painterResource(if (hostDevice.first == null) MR.images.ic_desktop else MR.images.ic_smartphone_300), null, Modifier.size(15.dp), tint = MaterialTheme.colors.secondary)
    Spacer(Modifier.width(10.dp))
    Text(hostDevice.second, color = MaterialTheme.colors.secondary)
  }
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
fun PreviewCloseSheetBar() {
  SimpleXTheme {
    CloseSheetBar(close = {})
  }
}
