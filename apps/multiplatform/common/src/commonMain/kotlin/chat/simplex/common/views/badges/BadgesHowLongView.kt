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
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
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
  Monthly,
  Annual;

  val icon: dev.icerock.moko.resources.ImageResource
    get() = when (this) {
      OneMonth -> MR.images.ic_calendar
      Monthly -> MR.images.ic_refresh
      Annual -> MR.images.ic_refresh
    }

  val label: StringResource
    get() = when (this) {
      OneMonth -> MR.strings.badges_period_one_month
      Monthly -> MR.strings.badges_period_monthly
      Annual -> MR.strings.badges_period_annual
    }

  val months: Int
    get() = when (this) {
      OneMonth -> 1
      Monthly -> 1
      Annual -> 12
    }

  @Composable
  fun priceText(price: BadgePrice): String = when (price) {
    is BadgePrice.Loading -> "…"
    is BadgePrice.Unavailable -> "—"
    is BadgePrice.Price -> when (this) {
      OneMonth -> price.price
      Monthly -> stringResource(MR.strings.badges_price_monthly).format(price.price)
      Annual -> stringResource(MR.strings.badges_price_annual).format(price.price)
    }
  }

  fun payLabel(price: BadgePrice): Pair<StringResource, String?> = when (price) {
    is BadgePrice.Loading -> MR.strings.badges_price_loading to null
    is BadgePrice.Unavailable -> MR.strings.badges_price_unavailable to null
    is BadgePrice.Price -> when (this) {
      OneMonth -> MR.strings.badges_pay_once to price.price
      Monthly -> MR.strings.badges_pay_monthly to price.price
      Annual -> MR.strings.badges_pay_annual to price.price
    }
  }
}

@Composable
fun BadgesHowLongView(level: BadgeLevel, modalManager: ModalManager) {
  var selectedPeriod by remember { mutableStateOf(if (BadgePeriod.Monthly in badgePeriodsForSale) BadgePeriod.Monthly else BadgePeriod.OneMonth) }

  LaunchedEffect(Unit) { BadgeStore.load() }
  CloseWhenSupportGivesWay(modalManager)

  ColumnWithScrollBar(
    Modifier.background(MaterialTheme.colors.background).padding(horizontal = 25.dp).padding(top = 8.dp, bottom = 20.dp),
    verticalArrangement = Arrangement.spacedBy(16.dp),
    horizontalAlignment = Alignment.CenterHorizontally,
    maxIntrinsicSize = true,
  ) {
    Text(
      stringResource(MR.strings.badges_how_long_title),
      style = MaterialTheme.typography.h1,
      fontWeight = FontWeight.Bold,
      color = MaterialTheme.colors.primary,
      textAlign = TextAlign.Center,
      modifier = Modifier.fillMaxWidth()
    )

    Text(
      stringResource(level.summary),
      style = MaterialTheme.typography.body1,
      color = MaterialTheme.colors.secondary,
      textAlign = TextAlign.Center,
      modifier = Modifier.fillMaxWidth()
    )

    BadgeUserPreview(level = level, modifier = Modifier.padding(top = 4.dp))

    Spacer(Modifier.weight(1f).heightIn(min = 8.dp))

    // IntrinsicSize.Max + fillMaxHeight on children so the cards all match the tallest one -
    // only Annual carries a savings line, and prices wrap at large fonts.
    Row(
      Modifier.fillMaxWidth().height(IntrinsicSize.Max),
      horizontalArrangement = Arrangement.spacedBy(12.dp)
    ) {
      BadgePeriod.entries.forEach { period ->
        PeriodCard(level, period, selectedPeriod, Modifier.weight(1f).fillMaxHeight()) { selectedPeriod = it }
      }
    }

    Spacer(Modifier.weight(1f).heightIn(min = 8.dp))

    Column(horizontalAlignment = Alignment.CenterHorizontally) {
      ContinueButton(level, selectedPeriod, modalManager)
      BadgeBillingFooter(selectedPeriod)
    }
  }
}

// Replicates TextButtonBelowOnboardingButton spacing (7.5dp outer + 5dp inner) without a
// TextButton so the footer has no hover/click affordance.
@Composable
fun BadgeBillingFooter(period: BadgePeriod) {
  Box(Modifier.padding(top = 7.5.dp, bottom = 7.5.dp).padding(horizontal = 16.dp, vertical = 8.dp)) {
    Text(
      stringResource(billingFooter(period)).format(billingDate(period)),
      Modifier.padding(vertical = 5.dp),
      style = MaterialTheme.typography.body2,
      color = MaterialTheme.colors.secondary,
      textAlign = TextAlign.Center
    )
  }
}

@Composable
private fun PeriodCard(level: BadgeLevel, period: BadgePeriod, selectedPeriod: BadgePeriod, modifier: Modifier, onSelect: (BadgePeriod) -> Unit) {
  val isSelected = period == selectedPeriod
  val forSale = period in badgePeriodsForSale
  val textColor = if (forSale) Color.Unspecified else MaterialTheme.colors.secondary
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
      .clickable(enabled = forSale) { onSelect(period) }
      .padding(vertical = 20.dp, horizontal = 12.dp),
    horizontalAlignment = Alignment.CenterHorizontally,
    verticalArrangement = Arrangement.spacedBy(12.dp)
  ) {
    Icon(
      painterResource(period.icon),
      contentDescription = null,
      tint = if (isSelected) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
      modifier = Modifier.size(32.dp).alpha(if (forSale) 1f else 0.4f)
    )
    Text(stringResource(period.label), style = MaterialTheme.typography.body1, color = textColor, textAlign = TextAlign.Center)
    Text(period.priceText(BadgeStore.price(level, period)), style = MaterialTheme.typography.h3, fontWeight = FontWeight.SemiBold, color = textColor, textAlign = TextAlign.Center)
    val percent = savingsPercent(level, period)
    if (forSale && percent != null) {
      Text(
        stringResource(MR.strings.badges_savings).format("${percent}%"),
        style = MaterialTheme.typography.body2,
        color = if (isSelected) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
        textAlign = TextAlign.Center
      )
    }
  }
}

private fun savingsPercent(level: BadgeLevel, period: BadgePeriod): Int? =
  if (period == BadgePeriod.Annual) BadgeStore.annualSavings(level) else null

@Composable
private fun ContinueButton(level: BadgeLevel, selectedPeriod: BadgePeriod, modalManager: ModalManager) {
  OnboardingActionButton(
    modifier = if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING).fillMaxWidth() else Modifier.widthIn(min = 300.dp),
    labelId = MR.strings.badges_continue,
    onboarding = null,
    onclick = {
      modalManager.showModal { BadgesCheckOrderView(level, selectedPeriod, modalManager) }
    }
  )
}

private fun billingFooter(period: BadgePeriod): StringResource = when (period) {
  BadgePeriod.Monthly, BadgePeriod.Annual -> MR.strings.badges_billing_footer_subscribe
  BadgePeriod.OneMonth -> MR.strings.badges_billing_footer_one_month
}

// TODO [badges] from now only because a purchase is refused while a badge is held (refuseWhileBadgeHeld);
// a top-up must count from the end of the existing balance
private fun billingDate(period: BadgePeriod): String {
  val date = java.time.LocalDate.now().plusMonths(period.months.toLong())
  val formatter = java.time.format.DateTimeFormatter.ofLocalizedDate(java.time.format.FormatStyle.LONG)
  return date.format(formatter)
}
