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
import androidx.compose.ui.platform.ClipboardManager
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.text.AnnotatedString
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

  // OnboardingActionButton takes a resource id, so the price states map to (id, arg) here rather
  // than to a formatted string as on iOS
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
fun BadgesPayView(level: BadgeLevel) {
  var selectedPeriod by remember { mutableStateOf(BadgePeriod.Monthly) }
  val purchasing = remember { mutableStateOf(false) }
  val clipboard = LocalClipboardManager.current

  LaunchedEffect(Unit) { BadgeStore.load() }

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

    // IntrinsicSize.Max + fillMaxHeight on children so all three cards match the tallest one -
    // only Annual carries a savings line, and prices wrap at large fonts.
    Row(
      Modifier.fillMaxWidth().height(IntrinsicSize.Max),
      horizontalArrangement = Arrangement.spacedBy(12.dp)
    ) {
      PeriodCard(level, BadgePeriod.OneMonth, selectedPeriod, Modifier.weight(1f).fillMaxHeight()) { selectedPeriod = it }
      PeriodCard(level, BadgePeriod.Monthly, selectedPeriod, Modifier.weight(1f).fillMaxHeight()) { selectedPeriod = it }
      PeriodCard(level, BadgePeriod.Annual, selectedPeriod, Modifier.weight(1f).fillMaxHeight()) { selectedPeriod = it }
    }

    Spacer(Modifier.weight(1f).heightIn(min = 20.dp))

    // Replicates TextButtonBelowOnboardingButton spacing (7.5dp outer + 5dp inner) without a
    // TextButton so the footer has no hover/click affordance.
    Column(horizontalAlignment = Alignment.CenterHorizontally) {
      PayButton(level, selectedPeriod, purchasing, clipboard)
      Box(Modifier.padding(top = 7.5.dp, bottom = 7.5.dp).padding(horizontal = 16.dp, vertical = 8.dp)) {
        Text(
          stringResource(billingFooter(selectedPeriod)).format(stubBillingDate()),
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
private fun PeriodCard(level: BadgeLevel, period: BadgePeriod, selectedPeriod: BadgePeriod, modifier: Modifier, onSelect: (BadgePeriod) -> Unit) {
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
    Text(period.priceText(BadgeStore.price(level, period)), style = MaterialTheme.typography.body1, textAlign = TextAlign.Center)
    val percent = savingsPercent(level, period)
    if (percent != null) {
      Text(
        stringResource(MR.strings.badges_savings).format(percent),
        style = MaterialTheme.typography.caption,
        color = if (isSelected) MaterialTheme.colors.primary else MaterialTheme.colors.secondary,
        textAlign = TextAlign.Center
      )
    }
  }
}

private fun savingsPercent(level: BadgeLevel, period: BadgePeriod): Int? =
  if (period == BadgePeriod.Annual) BadgeStore.annualSavings(level) else null

@Composable
private fun PayButton(level: BadgeLevel, selectedPeriod: BadgePeriod, purchasing: MutableState<Boolean>, clipboard: ClipboardManager) {
  val price = BadgeStore.price(level, selectedPeriod)
  val (labelId, labelArg) = selectedPeriod.payLabel(price)
  OnboardingActionButton(
    modifier = if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING).fillMaxWidth() else Modifier.widthIn(min = 300.dp),
    labelId = labelId,
    labelArg = labelArg,
    onboarding = null,
    enabled = price.canPurchase && !purchasing.value,
    onclick = { purchase(level, selectedPeriod, purchasing, clipboard) }
  )
}

private fun purchase(level: BadgeLevel, period: BadgePeriod, purchasing: MutableState<Boolean>, clipboard: ClipboardManager) {
  val invoiceId = newBadgeInvoiceId()
  purchasing.value = true
  // not withBGApi: the purchase waits for the user in the Play sheet and would block chat API calls
  withLongRunningApi {
    try {
      val outcome = BadgeStore.purchase(level, period, invoiceId)
      purchasing.value = false
      when (outcome) {
        is BadgePurchaseOutcome.Purchased -> showPurchasedAlert(outcome.receipt, invoiceId, clipboard)
        is BadgePurchaseOutcome.Pending -> AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.badges_purchase_pending),
          text = generalGetString(MR.strings.badges_purchase_pending_desc)
        )
        is BadgePurchaseOutcome.Cancelled -> {}
      }
    } catch (e: Exception) {
      Log.e(TAG, "BadgesPayView.purchase: ${e.stackTraceToString()}")
      purchasing.value = false
      AlertManager.shared.showAlertMsg(
        title = generalGetString(MR.strings.badges_purchase_error),
        text = e.toString()
      )
    }
  }
}

// TODO [badges] store integration diagnostics - replaced by the issued badge once the service lands.
private fun showPurchasedAlert(receipt: BadgeStoreReceipt, invoiceId: String, clipboard: ClipboardManager) {
  val returnedInvoice = when (receipt.invoiceId) {
    null -> "none"
    invoiceId -> "yes"
    else -> "mismatch: ${receipt.invoiceId}"
  }
  val summary = listOf(
    "Product: ${receipt.productId}",
    "Invoice: $invoiceId",
    "Invoice returned by Google: $returnedInvoice",
    "Order: ${receipt.orderId ?: "none"}",
    "Token: ${receipt.token.length} bytes"
  ).joinToString("\n")
  // logged as well as shown: the log always lands even when the alert is missed
  Log.d(TAG, "badge purchase succeeded\n$summary")
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.badges_purchase_successful),
    text = summary,
    confirmText = "Copy token",
    onConfirm = {
      clipboard.setText(AnnotatedString(receipt.token))
      showToast(generalGetString(MR.strings.copied))
    },
    dismissText = generalGetString(MR.strings.ok),
    parseHtml = false
  )
}

private fun billingFooter(period: BadgePeriod): StringResource = when (period) {
  BadgePeriod.Monthly, BadgePeriod.Annual -> MR.strings.badges_billing_footer_subscribe
  BadgePeriod.OneMonth -> MR.strings.badges_billing_footer_one_month
}

// TODO [badges] source the actual date from the purchase state machine when wired.
private fun stubBillingDate(): String {
  val date = java.time.LocalDate.of(2026, 7, 22)
  val formatter = java.time.format.DateTimeFormatter.ofLocalizedDate(java.time.format.FormatStyle.LONG)
  return date.format(formatter)
}
