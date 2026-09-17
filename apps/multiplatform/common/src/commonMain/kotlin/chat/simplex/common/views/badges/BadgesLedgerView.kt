package chat.simplex.common.views.badges

import InfoRow
import SectionItemView
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.material.Icon
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import chat.simplex.common.model.BadgeState
import chat.simplex.common.model.StatementCreditType
import chat.simplex.common.model.StatementDebitType
import chat.simplex.common.model.StatementEntry
import chat.simplex.common.model.StatementEntryType
import chat.simplex.common.model.localTimestamp
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.platform.Log
import chat.simplex.common.platform.TAG
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.AppBarTitle
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.common.views.helpers.withBGApi
import chat.simplex.res.MR

@Composable
fun BadgesLedgerView(badgeState: BadgeState) {
  val entries = remember { mutableStateOf<List<StatementEntry>>(emptyList()) }

  LaunchedEffect(Unit) {
    val user = chatModel.currentUser.value ?: return@LaunchedEffect
    withBGApi {
      try {
        val ledger = chatModel.controller.apiGetBadgeLedger(chatModel.remoteHostId(), user.userId, badgeState.badgePurchaseId)
        withContext(Dispatchers.Main) { entries.value = ledger }
      } catch (e: Exception) {
        Log.e(TAG, "apiGetBadgeLedger: ${e.message}")
        AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error), e.message)
      }
    }
  }

  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.badges_ledger))
    SectionView {
      if (entries.value.isEmpty()) {
        SectionItemView {
          Text(stringResource(MR.strings.badges_ledger_no_entries), color = MaterialTheme.colors.secondary)
        }
      } else {
        entries.value.forEach { LedgerRow(it) }
      }
    }
  }
}

@Composable
private fun LedgerRow(entry: StatementEntry) {
  val expanded = remember(entry.entryId) { mutableStateOf(false) }
  SectionItemView(click = { expanded.value = !expanded.value }) {
    Row(
      Modifier.fillMaxWidth(),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.spacedBy(8.dp)
    ) {
      Text(entry.entryType.text, Modifier.weight(1f))
      Text(if (entry.changeMonths >= 0) "+${entry.changeMonths}" else entry.changeMonths.toString(), color = MaterialTheme.colors.secondary)
      Icon(
        painterResource(if (expanded.value) MR.images.ic_chevron_up else MR.images.ic_chevron_down),
        contentDescription = null,
        tint = MaterialTheme.colors.secondary,
        modifier = Modifier.size(20.dp)
      )
    }
  }
  if (expanded.value) {
    InfoRow(stringResource(MR.strings.badges_ledger_date), localTimestamp(entry.createdAt))
    InfoRow(stringResource(MR.strings.badges_ledger_balance), entry.balanceMonths.toString())
    InfoRow(stringResource(MR.strings.badges_ledger_balance_start), localTimestamp(entry.balanceStartTs))
    InfoRow(stringResource(MR.strings.badges_ledger_anchor), localTimestamp(entry.balanceAnchorTs))
    InfoRow(stringResource(MR.strings.badges_ledger_badge_type), entry.balanceBadgeType.text)
    val pausedSince = entry.wasPausedSince
    if (pausedSince != null) {
      InfoRow(stringResource(MR.strings.badges_ledger_paused_since), localTimestamp(pausedSince))
    }
    InfoRow(stringResource(MR.strings.badges_ledger_entry_id), entry.entryId)
    PayloadRow(entry.entryType)
  }
}

@Composable
private fun PayloadRow(entryType: StatementEntryType) {
  when (entryType) {
    is StatementEntryType.Credit -> when (val credit = entryType.credit) {
      is StatementCreditType.Payment -> {
        val invoiceId = credit.invoiceId
        if (invoiceId != null) InfoRow(stringResource(MR.strings.badges_ledger_invoice_id), invoiceId)
      }
      is StatementCreditType.Charge -> InfoRow(stringResource(MR.strings.badges_ledger_charge_id), credit.chargeId)
      is StatementCreditType.TransferIn -> InfoRow(stringResource(MR.strings.badges_ledger_from_purchase_key), credit.fromPurchaseKey)
      is StatementCreditType.Code, is StatementCreditType.Support, is StatementCreditType.Opening, is StatementCreditType.Unknown -> {}
    }
    is StatementEntryType.Debit -> when (val debit = entryType.debit) {
      is StatementDebitType.Upgrade -> InfoRow(stringResource(MR.strings.badges_ledger_to_purchase_key), debit.toPurchaseKey)
      is StatementDebitType.TransferOut -> InfoRow(stringResource(MR.strings.badges_ledger_to_purchase_key), debit.toPurchaseKey)
      is StatementDebitType.Refund, is StatementDebitType.Support, is StatementDebitType.Badge, is StatementDebitType.Lapse, is StatementDebitType.Unknown -> {}
    }
  }
}
