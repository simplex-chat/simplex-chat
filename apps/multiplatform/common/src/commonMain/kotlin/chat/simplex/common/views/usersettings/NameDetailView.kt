package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontFamily
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.launch
import kotlinx.datetime.Instant

// One name: when it runs out, where it points, and what can be done with it.
//
// The link a name points at is the whole point of owning one, so a mismatch
// with the profile's current address is called out rather than left for the
// user to compare by eye.
@Composable
fun NameDetailView(rhId: Long?, fqdn: String, close: () -> Unit) {
  val info = remember { mutableStateOf<CR.NameInfo?>(null) }
  val busy = remember { mutableStateOf(false) }
  val scope = rememberCoroutineScope()

  suspend fun reload() { info.value = chatModel.controller.apiNameInfo(rhId, fqdn) }
  LaunchedEffect(Unit) { reload() }

  val myLink = chatModel.userAddress.value?.connLinkContact?.simplexChatUri(short = true)

  NameDetailLayout(
    fqdn = fqdn,
    info = info.value,
    myLink = myLink,
    busy = busy.value,
    repoint = {
      if (myLink != null) {
        AlertManager.shared.showAlertDialog(
          title = generalGetString(MR.strings.names_repoint_title),
          text = generalGetString(MR.strings.names_repoint_text).format(fqdn),
          confirmText = generalGetString(MR.strings.names_repoint_action),
          onConfirm = {
            withBGApi {
              busy.value = true
              try {
                if (chatModel.controller.apiNameSetLink(rhId, fqdn, myLink)) {
                  reload()
                  // Now that it points here, offer to show it on the profile.
                  if (chatModel.currentUser.value?.profile?.contactDomain?.domain != fqdn) {
                    offerSetPrimaryName(rhId, fqdn)
                  }
                }
              } finally { busy.value = false }
            }
          }
        )
      }
    },
    renew = {
      AlertManager.shared.showAlertDialog(
        title = generalGetString(MR.strings.names_renew_title),
        text = generalGetString(MR.strings.names_renew_text).format(fqdn),
        confirmText = generalGetString(MR.strings.names_renew_action),
        onConfirm = {
          withBGApi {
            busy.value = true
            try {
              val paid = NamePayment.purchaseFor("renew:$fqdn", 1) ?: return@withBGApi
              val r = chatModel.controller.apiNameRenew(rhId, fqdn, 1, paid.token)
              if (r != null) NamePayment.spent("renew:$fqdn")
              if (r != null) {
                reload()
                AlertManager.shared.showAlertMsg(
                  title = generalGetString(MR.strings.names_renew_done_title),
                  text = generalGetString(
                    if (r.nameReRegistered) MR.strings.names_renew_done_again else MR.strings.names_renew_done
                  ).format(fqdn),
                )
              }
            } finally { busy.value = false }
          }
        }
      )
    },
    gift = { ModalManager.start.showModalCloseable { c -> GiveNameView(rhId, fqdn, c) } },
  )
}

@Composable
private fun NameDetailLayout(
  fqdn: String,
  info: CR.NameInfo?,
  myLink: String?,
  busy: Boolean,
  repoint: () -> Unit,
  renew: () -> Unit,
  gift: () -> Unit,
) {
  ColumnWithScrollBar {
    AppBarTitle(fqdn)
    if (info != null && expiryDays(info.nameExpires.toLong()) < 0) {
      SectionTextFooter(stringResource(MR.strings.names_expired_note), Color.Red)
    }

    if (info == null) {
      SectionView { SectionItemView { Text(stringResource(MR.strings.names_incoming_loading), color = MaterialTheme.colors.secondary) } }
      SectionBottomSpacer()
      return@ColumnWithScrollBar
    }

    val links = info.nameContact + info.nameChannel
    val pointsAtMe = myLink != null && links.contains(myLink)

    SectionView(stringResource(MR.strings.names_detail_points_to).uppercase()) {
      if (links.isEmpty()) {
        SectionItemView { Text(stringResource(MR.strings.names_detail_no_link), color = WarningOrange) }
      } else {
        links.forEach { l ->
          SectionItemView { Text(l, fontFamily = FontFamily.Monospace, maxLines = 2, color = MaterialTheme.colors.secondary) }
        }
      }
    }
    if (!pointsAtMe && myLink != null) {
      // Worth surfacing: a name that does not point at this profile does not
      // bring anyone here, which is the only reason to own it.
      SectionTextFooter(stringResource(MR.strings.names_detail_mismatch), WarningOrange)
      SectionView {
        SectionItemView(click = if (busy) null else repoint, disabled = busy) {
          Text(
            if (busy) stringResource(MR.strings.names_repoint_working) else stringResource(MR.strings.names_repoint_action),
            color = if (busy) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
          )
        }
      }
    } else if (pointsAtMe) {
      SectionTextFooter(stringResource(MR.strings.names_detail_points_here), SimplexGreen)
    }

    SectionDividerSpaced(maxTopPadding = true)
    SectionView(stringResource(MR.strings.names_detail_validity).uppercase()) {
      SectionItemView {
        Row(Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.SpaceBetween) {
          Text(stringResource(MR.strings.names_detail_expires))
          Text(expiryText(info.nameExpires.toLong()), color = expiryColor(info.nameExpires.toLong()))
        }
      }
      SectionItemView {
        Row(Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.SpaceBetween) {
          Text(stringResource(MR.strings.names_detail_changes_left))
          Text(info.nameEditCredits.toString(), color = MaterialTheme.colors.secondary)
        }
      }
      // The app sends no expiry notifications, so the only reminder that will
      // actually reach the user is one in their own calendar.
      SectionItemView(click = {
        val ok = addCalendarReminder(
          generalGetString(MR.strings.names_remind_title).format(fqdn),
          generalGetString(MR.strings.names_remind_description),
          info.nameExpires.toLong(),
        )
        if (!ok) showToast(generalGetString(MR.strings.names_remind_none))
      }) {
        Text(stringResource(MR.strings.names_remind_action), color = MaterialTheme.colors.primary)
      }
    }
    // Nothing renews by itself, so say when it ends rather than implying it continues.
    SectionTextFooter(stringResource(MR.strings.names_detail_no_autorenew))

    SectionDividerSpaced(maxTopPadding = true)
    SectionView {
      // Offered whenever the name can still be recovered: while live, through
      // the grace period, and after it if no one else has taken it - past grace
      // this buys the name again, which is the same thing to the user.
      SettingsActionItem(
        painterResource(MR.images.ic_refresh),
        stringResource(MR.strings.names_renew_action),
        renew,
        textColor = MaterialTheme.colors.primary,
        iconColor = MaterialTheme.colors.primary,
        disabled = busy,
      )
      SettingsActionItem(
        painterResource(MR.images.ic_id_card),
        stringResource(MR.strings.names_give_title),
        gift,
        textColor = MaterialTheme.colors.primary,
        iconColor = MaterialTheme.colors.primary,
      )
    }
    SectionTextFooter(stringResource(MR.strings.names_give_footer))
    SectionBottomSpacer()
  }
}

// Owning a name and showing it on your profile are two different things: the
// wallet owns it, the profile claims it. This is the second step, and it is
// what makes the name appear on the SimpleX address screen.
fun offerSetPrimaryName(rhId: Long?, fqdn: String, onDone: () -> Unit = {}) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.names_primary_title),
    text = generalGetString(MR.strings.names_primary_text).format(fqdn),
    confirmText = generalGetString(MR.strings.names_primary_action),
    dismissText = generalGetString(MR.strings.names_bought_later),
    onConfirm = {
      withBGApi {
        try {
          val u = chatModel.controller.apiSetUserDomain(rhId, fqdn)
          chatModel.updateUser(u)
        } catch (e: Exception) {
          AlertManager.shared.showAlertMsg(generalGetString(MR.strings.names_primary_failed), e.message ?: "")
        }
        onDone()
      }
    },
    onDismiss = onDone,
  )
}

// Integer division truncates toward zero, so a name that expired an hour ago
// gave 0 and rendered as "Today" in orange - the exact window in which it is
// being released to anyone. floorDiv keeps expired negative.
private fun expiryDays(expires: Long): Long {
  val now = System.currentTimeMillis() / 1000
  return Math.floorDiv(expires - now, 86400L)
}

private fun expiryText(expires: Long): String {
  val d = expiryDays(expires)
  return when {
    d < 0 -> generalGetString(MR.strings.names_detail_expired)
    d == 0L -> generalGetString(MR.strings.names_detail_expires_today)
    d < 30 -> generalGetString(MR.strings.names_detail_expires_in_days).format(d)
    else -> Instant.fromEpochSeconds(expires).toString().substring(0, 10)
  }
}

private fun expiryColor(expires: Long): Color {
  val d = expiryDays(expires)
  return if (d < 0) Color.Red else if (d < 30) WarningOrange else Color.Unspecified
}
