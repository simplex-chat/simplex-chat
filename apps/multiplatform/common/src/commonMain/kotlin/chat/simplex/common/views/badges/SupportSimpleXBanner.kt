package chat.simplex.common.views.badges

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.layout.Layout
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.painterResource
import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chatlist.BannerDismissButton
import chat.simplex.common.views.chatlist.bannerCard
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR

@Composable
fun SupportSimpleXBanner(
  title: String = generalGetString(MR.strings.badges_banner_title),
  subtitle: String = generalGetString(MR.strings.badges_banner_subtitle),
  onTap: () -> Unit,
  onDismiss: () -> Unit
) {
  // the card's own height, for centring the fallback hero; hero stays fixed so its above-card
  // overhang shrinks at very large fonts
  val cardHeight = (72.dp * fontSizeMultiplier).coerceAtLeast(72.dp)
  val cardTrailingPadding = 8.dp
  val heroWidth = 110.dp
  // shorter than the natural drawn height so ContentScale.Crop slices the phone body at card bottom
  val heroVisibleHeight = 108.dp
  // hero right edge sits at the dismiss X's icon left edge (X: outer 4pt + inner-pad 8 + half of 16pt icon)
  val heroTrailingPadding = 28.dp
  val textToHeroGap = 6.dp

  // Layout sizes to the card; hero is placed at y = cardHeight - heroHeight (negative → hero
  // overhangs above card at normal fonts, 0/positive → hero fits inside card at large fonts).
  Layout(content = {
    Box(Modifier.fillMaxWidth()) {
      Row(
        Modifier
          .bannerCard(onTap)
          .padding(
            start = 16.dp,
            end = cardTrailingPadding + heroWidth + heroTrailingPadding + textToHeroGap,
            top = 12.dp,
            bottom = 12.dp
          ),
        verticalAlignment = Alignment.CenterVertically
      ) {
        Column(verticalArrangement = Arrangement.spacedBy(4.dp)) {
          Text(
            title,
            style = MaterialTheme.typography.body1,
            fontWeight = FontWeight.SemiBold,
            color = MaterialTheme.colors.primary,
            maxLines = 2,
            overflow = TextOverflow.Ellipsis
          )
          Text(
            subtitle,
            style = MaterialTheme.typography.body2,
            color = MaterialTheme.colors.onBackground,
            maxLines = 2,
            overflow = TextOverflow.Ellipsis
          )
        }
      }

      BannerDismissButton(Modifier.align(Alignment.TopEnd), onDismiss)
    }

    HeroThumbnail(
      heroWidth = heroWidth,
      heroVisibleHeight = heroVisibleHeight,
      cardHeight = cardHeight,
      trailingPadding = heroTrailingPadding
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
private fun HeroThumbnail(heroWidth: Dp, heroVisibleHeight: Dp, cardHeight: Dp, trailingPadding: Dp) {
  if (BuildConfigCommon.SIMPLEX_ASSETS) {
    // draws at natural aspect, top-aligned in a shorter slot; ContentScale.Crop cuts the overflow at card bottom
    Image(
      painterResource(if (isInDarkTheme()) MR.images.phone_supporter_light else MR.images.phone_supporter),
      contentDescription = null,
      contentScale = ContentScale.Crop,
      alignment = Alignment.TopCenter,
      modifier = Modifier.padding(end = trailingPadding).size(width = heroWidth, height = heroVisibleHeight)
    )
  } else {
    val badgeSize = 48.dp
    Image(
      painterResource(MR.images.badge_supporter),
      contentDescription = null,
      contentScale = ContentScale.Fit,
      modifier = Modifier
        .padding(end = trailingPadding + 12.dp, top = (cardHeight - badgeSize) / 2, bottom = (cardHeight - badgeSize) / 2)
        .size(badgeSize)
    )
  }
}
