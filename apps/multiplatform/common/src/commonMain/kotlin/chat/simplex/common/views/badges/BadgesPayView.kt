package chat.simplex.common.views.badges

import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
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

// Draft billing periods used by the badges UI while the API/state machine is still being designed.
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

    Row(
      Modifier.fillMaxWidth().padding(top = 12.dp),
      horizontalArrangement = Arrangement.spacedBy(12.dp)
    ) {
      PeriodCard(BadgePeriod.OneMonth, selectedPeriod, Modifier.weight(1f)) { selectedPeriod = it }
      PeriodCard(BadgePeriod.Subscribe, selectedPeriod, Modifier.weight(1f)) { selectedPeriod = it }
    }

    Spacer(Modifier.weight(1f).heightIn(min = 20.dp))

    // Plain Text (not TextButtonBelowOnboardingButton) because the billing footer is informational,
    // not an action — using TextButtonBelowOnboardingButton would force a Medium-weight bold look.
    // 15.5dp top padding matches the visual gap the TextButtonBelowOnboardingButton produces on
    // the other two badges views (7.5dp Modifier + 8dp TextButton chip inner padding).
    Column(horizontalAlignment = Alignment.CenterHorizontally) {
      PayButton(level, selectedPeriod)
      Text(
        stringResource(billingFooter(selectedPeriod)),
        style = MaterialTheme.typography.body2,
        color = MaterialTheme.colors.secondary,
        textAlign = TextAlign.Center,
        modifier = Modifier.fillMaxWidth().padding(top = 15.5.dp)
      )
    }
  }
}

@Composable
private fun PeriodCard(period: BadgePeriod, selectedPeriod: BadgePeriod, modifier: Modifier, onSelect: (BadgePeriod) -> Unit) {
  val isSelected = period == selectedPeriod
  val borderColor = if (isSelected) MaterialTheme.colors.primary else MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.92f)
  val shape = RoundedCornerShape(16.dp)
  Column(
    modifier
      .clip(shape)
      .background(MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.97f), shape)
      .border(2.dp, borderColor, shape)
      .clickable { onSelect(period) }
      .padding(vertical = 20.dp, horizontal = 12.dp),
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
    labelId = if (selectedPeriod == BadgePeriod.Subscribe) level.payMonthlyLabel else level.payOnceLabel,
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
