package chat.simplex.common.views.badges

import androidx.compose.foundation.background
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
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.OnboardingActionButton
import chat.simplex.common.views.usersettings.networkAndServers.ConditionsTextView
import chat.simplex.res.*

@Composable
fun BadgesCheckOrderView(level: BadgeLevel, period: BadgePeriod, modalManager: ModalManager, unwindToDepth: Int) {
  val purchasing = remember { mutableStateOf(false) }

  LaunchedEffect(Unit) { BadgeStore.load() }
  CloseWhenSupportGivesWay(modalManager, unwindToDepth)

  ColumnWithScrollBar(
    Modifier.padding(horizontal = 25.dp).padding(top = 8.dp, bottom = 20.dp),
    verticalArrangement = Arrangement.spacedBy(16.dp),
    horizontalAlignment = Alignment.CenterHorizontally,
    maxIntrinsicSize = true,
  ) {
    Text(
      stringResource(MR.strings.badges_check_your_order_title),
      style = MaterialTheme.typography.h1,
      fontWeight = FontWeight.Bold,
      color = MaterialTheme.colors.primary,
      textAlign = TextAlign.Center,
      modifier = Modifier.fillMaxWidth()
    )

    Column(
      Modifier
        .fillMaxWidth()
        .padding(top = 20.dp)
        .clip(RoundedCornerShape(16.dp))
        .background(sectionCardColor())
        .padding(horizontal = 24.dp, vertical = 12.dp)
    ) {
      OrderRow(MR.strings.badges_your_badge, stringResource(level.title))
      OrderRow(MR.strings.badges_order_duration, stringResource(period.label))
      Divider(Modifier.padding(top = 8.dp, bottom = 6.dp))
      OrderRow(MR.strings.badges_order_total, period.priceText(BadgeStore.price(level, period, compact = false)))
    }

    val price = BadgeStore.price(level, period, compact = false)
    if (price is BadgePrice.Price) {
      when (period) {
        BadgePeriod.Monthly -> Footnote(String.format(stringResource(MR.strings.badges_subscription_disclosure_monthly), price.price))
        BadgePeriod.Annual -> Footnote(String.format(stringResource(MR.strings.badges_subscription_disclosure_annual), price.price))
        BadgePeriod.OneMonth -> {}
      }
    }

    Spacer(Modifier.weight(1f).heightIn(min = 20.dp))

    Column(horizontalAlignment = Alignment.CenterHorizontally) {
      PayButton(level, period, purchasing)
      BadgeBillingFooter(period)
      Text(
        stringResource(MR.strings.badges_privacy_policy),
        Modifier.clickable { modalManager.showModal { BadgeConditionsView() } },
        style = MaterialTheme.typography.body2,
        color = MaterialTheme.colors.primary
      )
    }
  }
}

@Composable
private fun BadgeConditionsView() {
  ColumnWithScrollBar(Modifier.fillMaxSize().padding(horizontal = DEFAULT_PADDING)) {
    AppBarTitle(stringResource(MR.strings.badges_privacy_policy), enableAlphaChanges = false, withPadding = false, bottomPadding = DEFAULT_PADDING)
    Column(Modifier.weight(1f).padding(bottom = DEFAULT_PADDING, top = DEFAULT_PADDING_HALF)) {
      ConditionsTextView(chatModel.remoteHostId())
    }
  }
}

@Composable
private fun Footnote(text: String) {
  Text(
    text,
    style = MaterialTheme.typography.body2,
    color = MaterialTheme.colors.secondary
  )
}

@Composable
private fun OrderRow(title: StringResource, value: String) {
  Row(Modifier.fillMaxWidth().padding(vertical = 8.dp)) {
    Text(stringResource(title), style = MaterialTheme.typography.body1, color = MaterialTheme.colors.secondary)
    Spacer(Modifier.weight(1f))
    Text(value, style = MaterialTheme.typography.body1, fontWeight = FontWeight.Medium)
  }
}

@Composable
private fun PayButton(level: BadgeLevel, period: BadgePeriod, purchasing: MutableState<Boolean>) {
  val price = BadgeStore.price(level, period, compact = false)
  val (labelId, labelArg) = period.payLabel(price)
  OnboardingActionButton(
    modifier = if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING).fillMaxWidth() else Modifier.widthIn(min = 300.dp),
    labelId = labelId,
    labelArg = labelArg,
    onboarding = null,
    enabled = price.canPurchase && !purchasing.value && BadgeStore.canBuy(chatModel.currentUser.value?.userId),
    onclick = { purchase(level, period, purchasing) }
  )
}

private fun purchase(level: BadgeLevel, period: BadgePeriod, purchasing: MutableState<Boolean>) {
  purchasing.value = true
  // not withBGApi: the purchase waits for the user in the Play sheet and would block chat API calls
  withLongRunningApi {
    try {
      BadgeStore.purchase(level, period)
      purchasing.value = false
    } catch (e: Exception) {
      Log.e(TAG, "BadgesCheckOrderView.purchase: ${e.stackTraceToString()}")
      purchasing.value = false
      AlertManager.shared.showAlertMsg(
        title = generalGetString(MR.strings.badges_purchase_error),
        text = when (e) {
          is BadgeStoreError.ApiError -> chatModel.controller.redeemErrorText(e.err, purchase = true)
          else -> badgeStoreErrorText(e) ?: "${generalGetString(MR.strings.error_prefix)}: ${e.message ?: e}"
        }
      )
    }
  }
}

// the store rows act on this device's store, which a profile on a remote host cannot use
val badgeStoreLane: Boolean get() = badgeStoreAvailable && chatModel.remoteHostId() == null

@Composable
fun RestorePurchasesButton() {
  val restoring = remember { mutableStateOf(false) }
  Text(
    stringResource(MR.strings.badges_restore_purchases),
    Modifier.clickable(enabled = !restoring.value) { restoreBadgePurchases(restoring) },
    style = MaterialTheme.typography.body2,
    color = MaterialTheme.colors.primary
  )
}

fun restoreBadgePurchases(restoring: MutableState<Boolean>) {
  restoring.value = true
  withLongRunningApi {
    try {
      val found = BadgeStore.restorePurchases()
      if (!found) {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.badges_nothing_to_restore),
          text = generalGetString(MR.strings.badges_nothing_to_restore_desc)
        )
      }
    } catch (e: Exception) {
      Log.e(TAG, "RestorePurchasesButton: ${e.stackTraceToString()}")
      AlertManager.shared.showAlertMsg(
        title = generalGetString(MR.strings.badges_restore_failed),
        text = if (e is BadgeStoreError.ApiError) chatModel.controller.redeemErrorText(e.err, purchase = true)
          else badgeStoreErrorText(e) ?: "${generalGetString(MR.strings.error_prefix)}: ${e.message ?: e}"
      )
    }
    restoring.value = false
  }
}
