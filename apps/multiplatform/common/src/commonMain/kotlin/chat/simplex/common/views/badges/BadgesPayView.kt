package chat.simplex.common.views.badges

import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.StringResource
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.OnboardingActionButton
import chat.simplex.res.MR

// TODO [badges]: replace with types produced by the badge purchase API when it lands.
enum class BadgePeriod {
  OneMonth,
  Subscribe;

  val icon: dev.icerock.moko.resources.ImageResource
    get() = when (this) {
      OneMonth -> MR.images.ic_calendar
      Subscribe -> MR.images.ic_refresh
    }

  val label: StringResource
    get() = when (this) {
      OneMonth -> MR.strings.badges_period_one_month
      Subscribe -> MR.strings.badges_period_subscribe
    }
}

@Composable
fun BadgesPayView(level: BadgeLevel) {
  var selectedPeriod by remember { mutableStateOf(BadgePeriod.Subscribe) }

  ColumnWithScrollBar(
    Modifier.background(MaterialTheme.colors.background).padding(horizontal = 25.dp).padding(top = 8.dp, bottom = 20.dp),
    verticalArrangement = Arrangement.spacedBy(16.dp),
    horizontalAlignment = Alignment.CenterHorizontally,
    maxIntrinsicSize = true,
  ) {
    Text(
      stringResource(level.title),
      style = MaterialTheme.typography.h1,
      fontWeight = FontWeight.Bold,
      color = MaterialTheme.colors.primary,
      textAlign = TextAlign.Center,
      modifier = Modifier.fillMaxWidth()
    )

    BadgeUserPreview(level = level, modifier = Modifier.padding(top = 4.dp))

    Text(
      stringResource(level.tagline),
      style = MaterialTheme.typography.body1,
      color = MaterialTheme.colors.onBackground,
      textAlign = TextAlign.Center,
      modifier = Modifier.fillMaxWidth().padding(top = 4.dp)
    )

    Spacer(Modifier.weight(1f).heightIn(min = 20.dp))

    // IntrinsicSize.Max + fillMaxHeight on children so both cards match the taller card's height
    // when 2-line labels at large fonts would otherwise size them differently.
    Row(
      Modifier.fillMaxWidth().height(IntrinsicSize.Max),
      horizontalArrangement = Arrangement.spacedBy(12.dp)
    ) {
      PeriodCard(BadgePeriod.OneMonth, selectedPeriod, Modifier.weight(1f).fillMaxHeight()) { selectedPeriod = it }
      PeriodCard(BadgePeriod.Subscribe, selectedPeriod, Modifier.weight(1f).fillMaxHeight()) { selectedPeriod = it }
    }

    Spacer(Modifier.weight(1f).heightIn(min = 20.dp))

    // Replicates TextButtonBelowOnboardingButton's exact structure (TextButton chip + 7.5dp outer
    // Modifier padding + 5dp Text vertical padding) so Pay's action-button-to-supporting-text
    // spacing matches Support Simplex and Your Level to the pixel. onClick = {} + no fontWeight
    // keeps it non-interactive-feeling and normal weight (not Medium/bold).
    Column(horizontalAlignment = Alignment.CenterHorizontally) {
      PayButton(level, selectedPeriod)
      TextButton(
        onClick = {},
        modifier = Modifier.padding(top = 7.5.dp, bottom = 7.5.dp).clip(CircleShape)
      ) {
        Text(
          stringResource(billingFooter(selectedPeriod)).format("July 22, 2026"),
          Modifier.padding(vertical = 5.dp),
          style = MaterialTheme.typography.body2,
          color = MaterialTheme.colors.secondary,
          textAlign = TextAlign.Center
        )
      }
    }
  }
}

@Composable
private fun PeriodCard(period: BadgePeriod, selectedPeriod: BadgePeriod, modifier: Modifier, onSelect: (BadgePeriod) -> Unit) {
  val isSelected = period == selectedPeriod
  val borderColor = if (isSelected) MaterialTheme.colors.primary else MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.92f)
  // Light: transparent so card matches page background. Dark: subtle gray tint for visible contrast.
  val cardBackground = if (isInDarkTheme()) MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.97f)
                       else MaterialTheme.colors.background
  val shape = RoundedCornerShape(16.dp)
  Column(
    modifier
      .clip(shape)
      .background(cardBackground, shape)
      .border(2.dp, borderColor, shape)
      .clickable { onSelect(period) }
      .padding(vertical = 30.dp, horizontal = 12.dp),
    horizontalAlignment = Alignment.CenterHorizontally,
    verticalArrangement = Arrangement.spacedBy(12.dp)
  ) {
    Icon(
      painterResource(period.icon),
      contentDescription = null,
      tint = if (isSelected) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
      modifier = Modifier.size(32.dp)
    )
    Text(stringResource(period.label), style = MaterialTheme.typography.h3, fontWeight = FontWeight.Bold, textAlign = TextAlign.Center)
  }
}

@Composable
private fun PayButton(level: BadgeLevel, selectedPeriod: BadgePeriod) {
  OnboardingActionButton(
    modifier = if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING).fillMaxWidth() else Modifier.widthIn(min = 300.dp),
    labelId = if (selectedPeriod == BadgePeriod.Subscribe) MR.strings.badges_pay_monthly else MR.strings.badges_pay_once,
    labelArg = level.priceAmount,
    onboarding = null,
    onclick = {
      // TODO [badges] wire to purchase API when it lands.
    }
  )
}

// TODO [badges] source the actual renewal/end date from the purchase state machine when wired.
private fun billingFooter(period: BadgePeriod): StringResource = when (period) {
  BadgePeriod.Subscribe -> MR.strings.badges_billing_footer_subscribe
  BadgePeriod.OneMonth -> MR.strings.badges_billing_footer_one_month
}
