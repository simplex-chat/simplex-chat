package chat.simplex.common.views.badges

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
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
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.darkStops
import chat.simplex.common.views.newchat.gradientPoints
import chat.simplex.common.views.newchat.lightStops
import chat.simplex.common.views.onboarding.HowItWorks
import chat.simplex.common.views.onboarding.OnboardingActionButton
import chat.simplex.res.MR

// Entry point for badges management. Subsequent screens push via ModalManager; the enclosing
// modal (settings or the chat-list banner sheet) provides the sliding animation.
@Composable
fun BadgesSupportSimplexView() {
  // TODO [badges] gate on user badge status (no badge → this view, active → "Manage your badge")
  ColumnWithScrollBar(
    Modifier.padding(horizontal = 25.dp).padding(top = 28.dp, bottom = 20.dp),
    verticalArrangement = Arrangement.spacedBy(16.dp),
    horizontalAlignment = Alignment.CenterHorizontally,
    maxIntrinsicSize = true,
  ) {
    Text(
      stringResource(MR.strings.badges_support_simplex_title),
      style = MaterialTheme.typography.h1,
      fontWeight = FontWeight.Bold,
      color = MaterialTheme.colors.primary,
      textAlign = TextAlign.Center,
      modifier = Modifier.fillMaxWidth()
    )

    Text(
      stringResource(MR.strings.badges_support_simplex_body),
      style = MaterialTheme.typography.body1,
      textAlign = TextAlign.Center,
      modifier = Modifier.fillMaxWidth()
    )

    val primary = MaterialTheme.colors.primary
    TextButton({
      ModalManager.start.showModal { HowItWorks(user = chatModel.currentUser.value, onboardingStage = null, titleColor = primary) }
    }) {
      Row(verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(6.dp)) {
        Icon(painterResource(MR.images.ic_info), null, tint = MaterialTheme.colors.primary)
        Text(stringResource(MR.strings.badges_why_simplex_is_built), color = MaterialTheme.colors.primary, fontWeight = FontWeight.Medium)
      }
    }

    Spacer(Modifier.weight(1f))

    PhoneSupporterHero(Modifier.fillMaxWidth(0.55f))

    Spacer(Modifier.weight(1f))

    ChooseLevelButton()

    RedeemCodeButton(Modifier.padding(top = 4.dp))
  }
}

@Composable
private fun ChooseLevelButton() {
  OnboardingActionButton(
    modifier = if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING).fillMaxWidth() else Modifier,
    labelId = MR.strings.badges_choose_your_level,
    onboarding = null,
    onclick = {
      ModalManager.start.showModal { BadgesYourLevelView() }
    }
  )
}

@Composable
private fun RedeemCodeButton(modifier: Modifier = Modifier) {
  TextButton(
    onClick = { ModalManager.start.showModal { BadgesRedeemCodeView() } },
    modifier = modifier
  ) {
    Text(
      stringResource(MR.strings.badges_redeem_code_button),
      color = MaterialTheme.colors.primary,
      fontWeight = FontWeight.Medium
    )
  }
}

// Hero image reused across badges views and WhatsNewView v7.1. Fallback (no SIMPLEX_ASSETS) is a
// gradient card carrying the small supporter badge glyph.
@Composable
fun PhoneSupporterHero(modifier: Modifier = Modifier) {
  val isDark = isInDarkTheme()
  if (BuildConfigCommon.SIMPLEX_ASSETS) {
    Image(
      painterResource(if (isDark) MR.images.phone_supporter_light else MR.images.phone_supporter),
      contentDescription = null,
      contentScale = ContentScale.Fit,
      modifier = modifier.fillMaxWidth()
    )
  } else {
    var size by remember { mutableStateOf(IntSize.Zero) }
    val stops = if (isDark) darkStops else lightStops
    val scale = if (isDark) 1.5f else 1.2f
    val brush = remember(size, isDark) {
      if (size.width > 0 && size.height > 0) {
        val aspect = size.height.toFloat() / size.width.toFloat()
        val gp = gradientPoints(aspect, scale)
        Brush.linearGradient(
          colorStops = stops,
          start = Offset(gp.startX * size.width, gp.startY * size.height),
          end = Offset(gp.endX * size.width, gp.endY * size.height)
        )
      } else {
        Brush.linearGradient(colorStops = stops)
      }
    }
    Box(
      modifier
        .fillMaxWidth()
        .aspectRatio(1f)
        .clip(RoundedCornerShape(24.dp))
        .background(brush)
        .onSizeChanged { size = it },
      contentAlignment = Alignment.Center
    ) {
      Image(
        painterResource(MR.images.badge_supporter),
        contentDescription = null,
        contentScale = ContentScale.Fit,
        modifier = Modifier.size(96.dp)
      )
    }
  }
}
