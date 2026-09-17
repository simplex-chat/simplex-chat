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
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import kotlinx.serialization.encodeToString
import chat.simplex.common.model.BadgeState
import chat.simplex.common.model.StatementCreditType
import chat.simplex.common.model.StatementDebitType
import chat.simplex.common.model.StatementEntry
import chat.simplex.common.model.StatementEntryType
import chat.simplex.common.model.json
import chat.simplex.common.model.localTimestamp
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.platform.Log
import chat.simplex.common.platform.TAG
import chat.simplex.common.platform.chatModel
import chat.simplex.common.platform.shareText
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.AppBarTitle
import chat.simplex.common.views.helpers.ModalView
import chat.simplex.common.views.helpers.ShareButton
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.common.views.helpers.withBGApi
import chat.simplex.res.MR

@Composable
fun BadgesLedgerView(badgeState: BadgeState, close: () -> Unit) {
  val entries = remember { mutableStateOf<List<StatementEntry>?>(null) }
  val clipboard = LocalClipboardManager.current

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

  ModalView(
    close,
    cardScreen = true,
    endButtons = {
      val loaded = entries.value
      if (!loaded.isNullOrEmpty()) {
        ShareButton { clipboard.shareText(ledgerShareText(loaded)) }
      }
    }
  ) {
    ColumnWithScrollBar {
      AppBarTitle(stringResource(MR.strings.badges_ledger))
      val loaded = entries.value
      if (loaded != null) {
        SectionView {
          if (loaded.isEmpty()) {
            SectionItemView {
              Text(stringResource(MR.strings.badges_ledger_no_entries), color = MaterialTheme.colors.secondary)
            }
          } else {
            loaded.forEach { LedgerRow(it) }
          }
        }
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
      Text(changeText(entry), color = MaterialTheme.colors.secondary)
      Icon(
        painterResource(if (expanded.value) MR.images.ic_chevron_up else MR.images.ic_chevron_down),
        contentDescription = null,
        tint = MaterialTheme.colors.secondary,
        modifier = Modifier.size(20.dp)
      )
    }
  }
  if (expanded.value) {
    entryFields(entry).forEach { (label, value) -> InfoRow(label, value) }
  }
}

private fun changeText(entry: StatementEntry): String =
  if (entry.changeMonths >= 0) "+${entry.changeMonths}" else entry.changeMonths.toString()

private fun entryFields(entry: StatementEntry): List<Pair<String, String>> {
  val fields = mutableListOf(
    generalGetString(MR.strings.badges_ledger_date) to localTimestamp(entry.createdAt),
    generalGetString(MR.strings.badges_ledger_balance) to entry.balanceMonths.toString(),
    generalGetString(MR.strings.badges_ledger_balance_start) to localTimestamp(entry.balanceStartTs),
    generalGetString(MR.strings.badges_ledger_anchor) to localTimestamp(entry.balanceAnchorTs),
    generalGetString(MR.strings.badges_ledger_badge_type) to entry.balanceBadgeType.text,
  )
  val pausedSince = entry.wasPausedSince
  if (pausedSince != null) {
    fields.add(generalGetString(MR.strings.badges_ledger_paused_since) to localTimestamp(pausedSince))
  }
  fields.add(generalGetString(MR.strings.badges_ledger_entry_id) to entry.entryId)
  payloadField(entry.entryType)?.let { fields.add(it) }
  return fields
}

private fun payloadField(entryType: StatementEntryType): Pair<String, String>? = when (entryType) {
  is StatementEntryType.Credit -> when (val credit = entryType.credit) {
    is StatementCreditType.Payment -> credit.invoiceId?.let { generalGetString(MR.strings.badges_ledger_invoice_id) to it }
    is StatementCreditType.Charge -> generalGetString(MR.strings.badges_ledger_charge_id) to credit.chargeId
    is StatementCreditType.TransferIn -> generalGetString(MR.strings.badges_ledger_from_purchase_key) to credit.fromPurchaseKey
    is StatementCreditType.Code, is StatementCreditType.Support, is StatementCreditType.Opening, is StatementCreditType.Unknown -> null
  }
  is StatementEntryType.Debit -> when (val debit = entryType.debit) {
    is StatementDebitType.Upgrade -> generalGetString(MR.strings.badges_ledger_to_purchase_key) to debit.toPurchaseKey
    is StatementDebitType.TransferOut -> generalGetString(MR.strings.badges_ledger_to_purchase_key) to debit.toPurchaseKey
    is StatementDebitType.Refund, is StatementDebitType.Support, is StatementDebitType.Badge, is StatementDebitType.Lapse, is StatementDebitType.Unknown -> null
  }
}

// the JSON as core sent it: English field names and ISO dates, for support
private fun ledgerShareText(entries: List<StatementEntry>): String = json.encodeToString(entries)
