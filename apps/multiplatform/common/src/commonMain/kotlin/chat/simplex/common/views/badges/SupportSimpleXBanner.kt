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
import androidx.compose.ui.layout.Layout
import androidx.compose.ui.layout.onSizeChanged
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.painterResource
import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.darkStops
import chat.simplex.common.views.newchat.gradientPoints
import chat.simplex.common.views.newchat.lightStops
import chat.simplex.res.MR

// Dumb chat-list card promoting the badges purchase flow. Both the show gate and the dismissal
// persistence live in ChatListView; this view just renders and reports taps.
@Composable
fun SupportSimpleXBanner(onTap: () -> Unit, onDismiss: () -> Unit) {
  val cardCornerRadius = 16.dp
  // grows linearly with system font but never shrinks below the default so small-font users see the
  // same baseline; the card Row uses heightIn(min = cardHeight) and grows further when 2-line text
  // wraps at very large fonts. Hero stays fixed so its above-card overhang shrinks at very large fonts.
  val cardHeight = (72.dp * fontSizeMultiplier).coerceAtLeast(72.dp)
  // matches OneHandUICard's segment icon leading so the text aligns with it in the list
  val cardLeadingPadding = 16.dp
  val cardTrailingPadding = 8.dp
  val heroWidth = 110.dp
  // shorter than the natural drawn height so ContentScale.Crop slices the phone body at card bottom
  val heroVisibleHeight = 108.dp
  // hero right edge sits at the dismiss X's icon left edge (X: outer 4pt + inner-pad 8 + half of 16pt icon)
  val heroTrailingPadding = 28.dp
  val textToHeroGap = 6.dp

  val isDark = isInDarkTheme()
  var cardSize by remember { mutableStateOf(IntSize.Zero) }
  val brush = remember(isDark, cardSize) { gradientBrush(isDark, cardSize) }

  // Layout sizes to the card; hero is placed at y = cardHeight - heroHeight (negative → hero
  // overhangs above card at normal fonts, 0/positive → hero fits inside card at large fonts).
  Layout(content = {
    Box(Modifier.fillMaxWidth()) {
      Row(
        Modifier
          .fillMaxWidth()
          .heightIn(min = cardHeight)
          .clip(RoundedCornerShape(cardCornerRadius))
          .background(brush)
          .clickable(onClick = onTap)
          .onSizeChanged { cardSize = it }
          .padding(
            start = cardLeadingPadding,
            end = cardTrailingPadding + heroWidth + heroTrailingPadding + textToHeroGap,
            top = 12.dp,
            bottom = 12.dp
          ),
        verticalAlignment = Alignment.CenterVertically
      ) {
        Column(verticalArrangement = Arrangement.spacedBy(4.dp)) {
          Text(
            generalGetString(MR.strings.badges_banner_title),
            style = MaterialTheme.typography.body1,
            fontWeight = FontWeight.SemiBold,
            color = MaterialTheme.colors.primary,
            maxLines = 2,
            overflow = TextOverflow.Ellipsis
          )
          Text(
            generalGetString(MR.strings.badges_banner_subtitle),
            style = MaterialTheme.typography.body2,
            color = MaterialTheme.colors.onBackground,
            maxLines = 2,
            overflow = TextOverflow.Ellipsis
          )
        }
      }

      // Same X pattern as OneHandUICard: circle-clipped clickable region with inner padding for hit area.
      Icon(
        painterResource(MR.images.ic_close),
        contentDescription = generalGetString(MR.strings.icon_descr_close_button),
        tint = if (isDark) MaterialTheme.colors.onBackground else MaterialTheme.colors.secondary,
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

    HeroThumbnail(
      heroWidth = heroWidth,
      heroVisibleHeight = heroVisibleHeight,
      cardHeight = cardHeight,
      trailingPadding = heroTrailingPadding,
      modifier = Modifier
    )
  }) { measurables, constraints ->
    val cardPlaceable = measurables[0].measure(constraints)
    val heroPlaceable = measurables[1].measure(constraints.copy(minWidth = 0, minHeight = 0))
    layout(cardPlaceable.width, cardPlaceable.height) {
      cardPlaceable.place(0, 0)
      heroPlaceable.place(cardPlaceable.width - heroPlaceable.width, cardPlaceable.height - heroPlaceable.height)
    }
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

// Same geometry-aware gradient as OnboardingCardView / ConnectBannerCard: aspect drives the
// start/end points along a fixed 80° axis so the full 5-stop palette lands correctly at any size.
private fun gradientBrush(isDark: Boolean, size: IntSize): Brush {
  val stops = if (isDark) darkStops else lightStops
  if (size.width == 0 || size.height == 0) return Brush.linearGradient(colorStops = stops)
  val w = size.width.toFloat()
  val h = size.height.toFloat()
  val gp = gradientPoints(h / w, if (isDark) 2.1f else 1.7f)
  return Brush.linearGradient(
    colorStops = stops,
    start = Offset(gp.startX * w, gp.startY * h),
    end = Offset(gp.endX * w, gp.endY * h)
  )
}
