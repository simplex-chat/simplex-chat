package chat.simplex.common.views.badges

import androidx.compose.foundation.background
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
import chat.simplex.res.MR

@Composable
fun BadgesCheckOrderView(level: BadgeLevel, period: BadgePeriod, modalManager: ModalManager) {
  val purchasing = remember { mutableStateOf(false) }

  LaunchedEffect(Unit) { BadgeStore.load() }
  CloseWhenSupportGivesWay(modalManager)

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

    Spacer(Modifier.weight(1f).heightIn(min = 20.dp))

    Column(horizontalAlignment = Alignment.CenterHorizontally) {
      PayButton(level, period, purchasing)
      BadgeBillingFooter(period)
    }
  }

  if (purchasing.value) {
    Box(
      Modifier.fillMaxSize(),
      contentAlignment = Alignment.Center
    ) {
      Surface(Modifier.size(50.dp), color = MaterialTheme.colors.background.copy(0.9f), contentColor = LocalContentColor.current, shape = RoundedCornerShape(50)){}
      CircularProgressIndicator(
        Modifier
          .padding(horizontal = 2.dp)
          .size(30.dp),
        color = MaterialTheme.colors.secondary,
        strokeWidth = 3.dp
      )
    }
  }
}

@Composable
private fun OrderRow(title: StringResource, value: String) {
  Row(Modifier.fillMaxWidth().padding(vertical = 8.dp)) {
    Text(stringResource(title), style = MaterialTheme.typography.body1, fontWeight = FontWeight.Medium, color = MaterialTheme.colors.secondary)
    Spacer(Modifier.weight(1f))
    Text(value, style = MaterialTheme.typography.body1, fontWeight = FontWeight.SemiBold)
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
        text = if (e is BadgeStoreError.InvoiceRefused) chatModel.controller.redeemErrorText(e.err, purchase = true)
          else "${generalGetString(MR.strings.error_prefix)}: ${e.message ?: e}"
      )
    }
  }
}
