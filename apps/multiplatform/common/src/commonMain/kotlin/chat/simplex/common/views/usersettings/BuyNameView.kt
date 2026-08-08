package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.input.ImeAction
import androidx.compose.ui.text.input.KeyboardCapitalization
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch

// Buying a name.
//
// Three steps, in the order the user experiences them: choose a name, see
// whether it is free and what it costs, pay. Payment goes through
// [NamePayment], which is the only place that knows about stores - when real
// billing lands, nothing on this screen changes.
//
// A name is bought outright for a fixed term. There is no subscription, so the
// screen says when it runs out rather than implying it renews itself.
@Composable
fun BuyNameView(rhId: Long?, close: () -> Unit) {
  val label = rememberSaveable { mutableStateOf("") }
  val years = rememberSaveable { mutableStateOf(1) }
  val quote = remember { mutableStateOf<CR.NameQuoted?>(null) }
  val checking = remember { mutableStateOf(false) }
  val buying = remember { mutableStateOf(false) }
  val scope = rememberCoroutineScope()
  val myLink = chatModel.userAddress.value?.connLinkContact?.simplexChatUri(short = true)
  // A name can be bought as a gift, so pointing it here is a default, not a
  // rule. Off means it is registered pointing nowhere, ready to be given away.
  val pointAtMe = rememberSaveable { mutableStateOf(true) }

  // Re-quote as the user types, but only once they have stopped.
  LaunchedEffect(label.value) {
    quote.value = null
    val l = label.value.trim().lowercase()
    if (l.length < 6) return@LaunchedEffect
    delay(400)
    checking.value = true
    quote.value = chatModel.controller.apiNameQuote(rhId, l)
    checking.value = false
  }

  fun doBuy() {
    scope.launch {
      buying.value = true
      try {
        val paid = NamePayment.purchase(years.value)
        if (paid == null) return@launch // cancelled: nothing charged, nothing registered
        // Bought to be used, so it points at this profile from the moment it
        // exists rather than needing a second trip through the detail screen.
        val link = if (pointAtMe.value) myLink else null
        val reg = chatModel.controller.apiNameBuy(rhId, label.value.trim().lowercase(), years.value, paid.token, link)
        if (reg != null) {
          val keySaved = chatModel.controller.apiNameStatus(rhId)?.nameKeySaved ?: true
          close()
          if (keySaved) {
            if (pointAtMe.value) offerSetPrimaryName(rhId, reg.nameFqdn)
            else AlertManager.shared.showAlertMsg(
              title = generalGetString(MR.strings.names_bought_title),
              text = generalGetString(MR.strings.names_bought_for_other).format(reg.nameFqdn),
            )
          } else {
            // C8: for most users this is the first backup the app has ever
            // offered, so it comes before anything cosmetic.
            AlertManager.shared.showAlertDialog(
              title = generalGetString(MR.strings.names_bought_title),
              text = generalGetString(MR.strings.names_bought_text).format(reg.nameFqdn),
              confirmText = generalGetString(MR.strings.names_bought_save_key),
              dismissText = generalGetString(MR.strings.names_bought_later),
              onConfirm = { ModalManager.start.showModalCloseable { c -> NameRecoveryKeyView(rhId, c) } },
              onDismiss = { if (pointAtMe.value) offerSetPrimaryName(rhId, reg.nameFqdn) },
            )
          }
        }
      } finally {
        buying.value = false
      }
    }
  }

  BuyNameLayout(
    label = label,
    years = years,
    quote = quote.value,
    checking = checking.value,
    buying = buying.value,
    pointAtMe = pointAtMe,
    buy = { confirmBuy(label.value, pointAtMe.value) { doBuy() } },
  )
}

private fun confirmBuy(rawLabel: String, pointAtMe: Boolean, proceed: () -> Unit) {
  val fqdn = rawLabel.trim().lowercase() + ".simplex"
  AlertManager.shared.showAlertDialog(
    title = if (pointAtMe) generalGetString(MR.strings.names_buy_confirm_title) else generalGetString(MR.strings.names_buy_confirm_other_title),
    // Pointing it here puts your address into a public record, so that is
    // confirmed rather than assumed. Buying it for someone else does not.
    text = if (pointAtMe) generalGetString(MR.strings.names_buy_confirm_text).format(fqdn)
    else generalGetString(MR.strings.names_buy_confirm_other_text).format(fqdn),
    confirmText = generalGetString(MR.strings.names_buy_confirm_action),
    onConfirm = proceed,
  )
}

@Composable
private fun BuyNameLayout(
  label: MutableState<String>,
  years: MutableState<Int>,
  quote: CR.NameQuoted?,
  checking: Boolean,
  buying: Boolean,
  pointAtMe: MutableState<Boolean>,
  buy: () -> Unit,
) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.names_buy_title))

    SectionView(stringResource(MR.strings.names_buy_choose).uppercase()) {
      SectionItemView(padding = PaddingValues(horizontal = DEFAULT_PADDING)) {
        Row(verticalAlignment = Alignment.CenterVertically) {
          TextField(
            value = label.value,
            // Not filtered: silently dropping a character looks like a broken
            // keyboard. Invalid input is shown and explained instead.
            onValueChange = { label.value = it.trim().lowercase() },
            placeholder = { Text(stringResource(MR.strings.names_buy_placeholder)) },
            singleLine = true,
            keyboardOptions = KeyboardOptions(capitalization = KeyboardCapitalization.None, imeAction = ImeAction.Done),
            modifier = Modifier.weight(1f),
          )
          Text(".simplex", color = MaterialTheme.colors.secondary)
          Spacer(Modifier.width(DEFAULT_PADDING_HALF))
          // Availability where the name is typed, not in a section below it.
          Box(Modifier.size(24.dp), contentAlignment = Alignment.Center) {
            when (val st = nameStatus(label.value, checking, quote)) {
              is NameStatusUi.Empty -> {}
              is NameStatusUi.Checking -> CircularProgressIndicator(Modifier.size(18.dp), strokeWidth = 2.dp, color = MaterialTheme.colors.secondary)
              is NameStatusUi.Available -> Icon(painterResource(MR.images.ic_check), stringResource(MR.strings.names_buy_available_a11y), tint = SimplexGreen)
              is NameStatusUi.Taken -> Icon(painterResource(MR.images.ic_close), stringResource(MR.strings.names_buy_taken), tint = Color.Red)
              // Fixable by typing: a warning, not a rejection.
              is NameStatusUi.TooShort, is NameStatusUi.BadChars, is NameStatusUi.Unknown ->
                Icon(painterResource(MR.images.ic_error), stringResource(MR.strings.names_buy_check_a11y), tint = WarningOrange)
            }
          }
        }
      }
    }
    when (val st = nameStatus(label.value, checking, quote)) {
      is NameStatusUi.Empty -> SectionTextFooter(stringResource(MR.strings.names_buy_rules))
      is NameStatusUi.BadChars -> SectionTextFooter(stringResource(MR.strings.names_buy_bad_chars), WarningOrange)
      is NameStatusUi.TooShort -> SectionTextFooter(stringResource(MR.strings.names_buy_too_short), WarningOrange)
      is NameStatusUi.Checking -> SectionTextFooter(stringResource(MR.strings.names_buy_checking))
      is NameStatusUi.Unknown -> SectionTextFooter(stringResource(MR.strings.names_buy_unknown), WarningOrange)
      is NameStatusUi.Taken -> SectionTextFooter(stringResource(MR.strings.names_buy_taken), Color.Red)
      is NameStatusUi.Available -> SectionTextFooter(stringResource(MR.strings.names_buy_available).format(priceText(st.priceCents, years.value)), SimplexGreen)
    }

    SectionDividerSpaced(maxTopPadding = true)
    SectionView {
      when {
        quote == null || !quote.nameAvailable -> {}
        else -> {
          SectionItemView(click = if (buying) null else buy, disabled = buying) {
            Text(
              if (buying) stringResource(MR.strings.names_buy_working)
              else stringResource(MR.strings.names_buy_action).format(priceText(quote.namePriceCents, years.value)),
              color = if (buying) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
            )
          }
        }
      }
    }


    SectionDividerSpaced(maxTopPadding = true)
    SectionView(stringResource(MR.strings.names_buy_term).uppercase()) {
      SectionItemView {
        Row(Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.SpaceBetween, verticalAlignment = Alignment.CenterVertically) {
          Text(stringResource(MR.strings.names_buy_years).format(years.value))
          Row {
            TextButton(onClick = { if (years.value > 1) years.value-- }, enabled = years.value > 1) { Text("−") }
            TextButton(onClick = { if (years.value < 10) years.value++ }, enabled = years.value < 10) { Text("+") }
          }
        }
      }
    }
    SectionTextFooter(stringResource(MR.strings.names_buy_no_subscription))

    SectionDividerSpaced(maxTopPadding = true)
    SectionView {
      SectionItemView {
        Row(Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.SpaceBetween, verticalAlignment = Alignment.CenterVertically) {
          Text(stringResource(MR.strings.names_buy_point_at_me))
          DefaultSwitch(checked = pointAtMe.value, onCheckedChange = { pointAtMe.value = it })
        }
      }
    }
    SectionTextFooter(
      if (pointAtMe.value) stringResource(MR.strings.names_buy_point_at_me_on)
      else stringResource(MR.strings.names_buy_point_at_me_off)
    )

    if (!NamePayment.isLive) {
      SectionDividerSpaced(maxTopPadding = true)
      SectionView(stringResource(MR.strings.names_buy_dev_section).uppercase()) {
        SectionItemView { Text(stringResource(MR.strings.names_buy_dev_warning), color = WarningOrange) }
      }
    }
    SectionBottomSpacer()
  }
}

// Label rule, mirroring simplexmq's nameLabelP and SetSimplexNameView: ASCII
// letters and digits, single internal hyphens, no leading or trailing hyphen.
private val labelRegex = Regex("[a-z0-9]+(-[a-z0-9]+)*")

private sealed class NameStatusUi {
  object Empty : NameStatusUi()
  object TooShort : NameStatusUi()
  object BadChars : NameStatusUi()
  object Checking : NameStatusUi()
  object Unknown : NameStatusUi()
  object Taken : NameStatusUi()
  class Available(val priceCents: Int) : NameStatusUi()
}

private fun nameStatus(raw: String, checking: Boolean, quote: CR.NameQuoted?): NameStatusUi {
  val l = raw.trim().lowercase()
  return when {
    l.isEmpty() -> NameStatusUi.Empty
    // Character rule is reported before length: "6 characters" is unhelpful
    // advice when the problem is a space or an underscore.
    !labelRegex.matches(l) -> NameStatusUi.BadChars
    l.length < 6 -> NameStatusUi.TooShort
    checking -> NameStatusUi.Checking
    quote == null -> NameStatusUi.Unknown
    quote.nameAvailable -> NameStatusUi.Available(quote.namePriceCents)
    else -> NameStatusUi.Taken
  }
}

private fun priceText(centsPerYear: Int, years: Int): String {
  val total = centsPerYear * years
  return "$${total / 100}.${(total % 100).toString().padStart(2, '0')}"
}
