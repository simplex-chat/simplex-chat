package chat.simplex.common.views.chatlist

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.layout.onSizeChanged
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.ui.theme.isInDarkTheme
import chat.simplex.common.views.helpers.fontSizeMultiplier
import chat.simplex.common.views.newchat.darkStops
import chat.simplex.common.views.newchat.gradientPoints
import chat.simplex.common.views.newchat.lightStops
import chat.simplex.res.MR

// Spec: spec/client/chat-list.md#GetStakeBanner
@Composable
fun GetStakeBanner(showDismiss: Boolean, onTap: () -> Unit, onDismiss: () -> Unit) {
  Box(Modifier.fillMaxWidth()) {
    Row(
      Modifier
        .bannerCard(onTap)
        // the end padding is the 8dp trailing inset plus the X's 36dp hit region
        .padding(start = 16.dp, end = 44.dp, top = 12.dp, bottom = 12.dp),
      verticalAlignment = Alignment.CenterVertically
    ) {
      Column(verticalArrangement = Arrangement.spacedBy(4.dp)) {
        Text(
          stringResource(MR.strings.invest_banner_title),
          style = MaterialTheme.typography.body1,
          fontWeight = FontWeight.SemiBold,
          color = MaterialTheme.colors.primary,
          maxLines = 2,
          overflow = TextOverflow.Ellipsis
        )
        Text(
          stringResource(MR.strings.invest_banner_subtitle),
          style = MaterialTheme.typography.body2,
          color = MaterialTheme.colors.onBackground,
          maxLines = 2,
          overflow = TextOverflow.Ellipsis
        )
      }
    }

    if (showDismiss) {
      BannerDismissButton(Modifier.align(Alignment.TopEnd), onDismiss)
    }
  }
}

@Composable
fun Modifier.bannerCard(onTap: () -> Unit): Modifier {
  // grows linearly with system font but never shrinks below the default, and the card grows further
  // when 2-line text wraps at very large fonts
  val cardHeight = (72.dp * fontSizeMultiplier).coerceAtLeast(72.dp)
  val isDark = isInDarkTheme()
  var cardSize by remember { mutableStateOf(IntSize.Zero) }
  val brush = remember(isDark, cardSize) { gradientBrush(isDark, cardSize) }
  return this
    .fillMaxWidth()
    .heightIn(min = cardHeight)
    .clip(RoundedCornerShape(16.dp))
    .background(brush)
    .clickable(onClick = onTap)
    .onSizeChanged { cardSize = it }
}

// Same X pattern as OneHandUICard: circle-clipped clickable region with inner padding for hit area.
@Composable
fun BannerDismissButton(modifier: Modifier, onDismiss: () -> Unit) {
  Icon(
    painterResource(MR.images.ic_close),
    contentDescription = stringResource(MR.strings.icon_descr_close_button),
    tint = if (isInDarkTheme()) MaterialTheme.colors.onBackground else MaterialTheme.colors.secondary,
    modifier = modifier
      .padding(end = 4.dp, top = 4.dp)
      .clip(CircleShape)
      .clickable(onClick = onDismiss)
      .padding(8.dp)
      .size(16.dp)
  )
}

// Geometry-aware gradient with asymmetric scale: start (dark) pushed further below the card than
// end (warm) is above, so card-middle lands at the bright/mid-transition stop, not the dark region.
private fun gradientBrush(isDark: Boolean, size: IntSize): Brush {
  val stops = if (isDark) darkStops else lightStops
  if (size.width == 0 || size.height == 0) return Brush.linearGradient(colorStops = stops)
  val w = size.width.toFloat()
  val h = size.height.toFloat()
  val startScale = if (isDark) 3.0f else 2.5f
  val endScale = if (isDark) 2.1f else 1.7f
  val gp = gradientPoints(h / w, 1.0f)
  val sx = 0.5f + (gp.startX - 0.5f) * startScale
  val sy = 0.5f + (gp.startY - 0.5f) * startScale
  val ex = 0.5f + (gp.endX - 0.5f) * endScale
  val ey = 0.5f + (gp.endY - 0.5f) * endScale
  return Brush.linearGradient(
    colorStops = stops,
    start = Offset(sx * w, sy * h),
    end = Offset(ex * w, ey * h)
  )
}
