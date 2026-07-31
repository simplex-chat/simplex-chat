package chat.simplex.common.views.badges

import androidx.compose.foundation.*
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
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.layout.onSizeChanged
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.painterResource
import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.darkStops
import chat.simplex.common.views.newchat.lightStops
import chat.simplex.res.MR

// Dumb chat-list card promoting the badges purchase flow. Both the show gate and the dismissal
// persistence live in ChatListView; this view just renders and reports taps.
@Composable
fun SupportSimpleXBanner(onTap: () -> Unit, onDismiss: () -> Unit) {
  val cardCornerRadius = 16.dp
  val cardHeight = 72.dp
  // matches OneHandUICard's segment icon leading so the text aligns with it in the list
  val cardLeadingPadding = 16.dp
  val cardTrailingPadding = 8.dp
  val heroWidth = 110.dp
  // shorter than the natural drawn height so ContentScale.Crop slices the phone body at card bottom
  val heroVisibleHeight = 108.dp
  // matches the OneHandUICard-style X: outer end pad 4 + circle click area 32 = 36 reserved on right
  val heroTrailingPadding = 36.dp
  val textToHeroGap = 6.dp

  val isDark = isInDarkTheme()
  var cardSize by remember { mutableStateOf(IntSize.Zero) }
  val brush = remember(isDark, cardSize) { gradientBrush(isDark, cardSize) }

  // Root Box is sized to the card; hero and X are children so they can render outside the card's
  // rounded-corner clip. Hero anchors bottom-end and its 108dp height extends 36dp above the card top.
  Box(Modifier.fillMaxWidth().height(cardHeight)) {
    Row(
      Modifier
        .fillMaxSize()
        .clip(RoundedCornerShape(cardCornerRadius))
        .background(brush)
        .clickable(onClick = onTap)
        .onSizeChanged { cardSize = it }
        .padding(start = cardLeadingPadding, end = cardTrailingPadding + heroWidth + heroTrailingPadding + textToHeroGap),
      verticalAlignment = Alignment.CenterVertically
    ) {
      Column(verticalArrangement = Arrangement.spacedBy(4.dp)) {
        Text(
          generalGetString(MR.strings.badges_banner_title),
          style = MaterialTheme.typography.body1,
          fontWeight = FontWeight.SemiBold,
          color = MaterialTheme.colors.primary
        )
        Text(
          generalGetString(MR.strings.badges_banner_subtitle),
          style = MaterialTheme.typography.body2,
          color = MaterialTheme.colors.onBackground
        )
      }
    }

    HeroThumbnail(
      heroWidth = heroWidth,
      heroVisibleHeight = heroVisibleHeight,
      cardHeight = cardHeight,
      trailingPadding = heroTrailingPadding,
      modifier = Modifier.align(Alignment.BottomEnd)
    )

    // Same X pattern as OneHandUICard: circle-clipped clickable region with inner padding for hit area.
    Icon(
      painterResource(MR.images.ic_close),
      contentDescription = generalGetString(MR.strings.icon_descr_close_button),
      tint = MaterialTheme.colors.secondary,
      modifier = Modifier
        .align(Alignment.TopEnd)
        .padding(end = 4.dp, top = 4.dp)
        .clip(CircleShape)
        .clickable {
          AlertManager.shared.showAlertMsg(
            title = generalGetString(MR.strings.badges_banner_title),
            text = generalGetString(MR.strings.badges_banner_dismiss_message),
            onConfirm = onDismiss
          )
        }
        .padding(8.dp)
        .size(16.dp)
    )
  }
}

@Composable
private fun HeroThumbnail(heroWidth: Dp, heroVisibleHeight: Dp, cardHeight: Dp, trailingPadding: Dp, modifier: Modifier) {
  if (BuildConfigCommon.SIMPLEX_ASSETS) {
    // draws at natural aspect, top-aligned in a shorter slot; ContentScale.Crop cuts the overflow at card bottom
    Image(
      painterResource(if (isInDarkTheme()) MR.images.phone_supporter_light else MR.images.phone_supporter),
      contentDescription = null,
      contentScale = ContentScale.Crop,
      alignment = Alignment.TopCenter,
      modifier = modifier.padding(end = trailingPadding).size(width = heroWidth, height = heroVisibleHeight)
    )
  } else {
    val badgeSize = 48.dp
    Image(
      painterResource(MR.images.badge_supporter),
      contentDescription = null,
      contentScale = ContentScale.Fit,
      modifier = modifier
        .padding(end = trailingPadding + 12.dp, top = (cardHeight - badgeSize) / 2, bottom = (cardHeight - badgeSize) / 2)
        .size(badgeSize)
    )
  }
}

// light: 2-color end-to-end (lightStops' 0.0–0.5 blue plateau would read as all-blue here).
// dark: full 5-stop darkStops — the intermediate blue-to-navy transitions land nicely.
private fun gradientBrush(isDark: Boolean, size: IntSize): Brush {
  if (size.width == 0 || size.height == 0) {
    return if (isDark) Brush.linearGradient(colorStops = darkStops)
    else Brush.linearGradient(colors = listOf(lightStops.first().second, lightStops.last().second))
  }
  val w = size.width.toFloat()
  val h = size.height.toFloat()
  return if (isDark) {
    Brush.linearGradient(
      colorStops = darkStops,
      start = Offset(0f, h),
      end = Offset(w, 0f)
    )
  } else {
    Brush.linearGradient(
      colors = listOf(lightStops.first().second, lightStops.last().second),
      start = Offset(0f, h),
      end = Offset(w, 0f)
    )
  }
}
