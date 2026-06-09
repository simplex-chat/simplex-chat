package chat.simplex.common.views.newchat

import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import kotlin.reflect.KClass

// The shared gate every scanner routes through. If the scan is the accepted kind, run the
// screen's onMatch (which keeps that screen's own success/failure side effects and returns the
// bool the Android scanner uses for de-dup). Otherwise show the one wrong-type alert and return
// true, so the scanner caches the code and the alert shows once (it only de-dups on true).
suspend fun handleScan(
  rhId: Long?,
  raw: String,
  expected: KClass<out QRCodeType>,
  close: () -> Unit,
  onMatch: suspend (QRCodeType) -> Boolean,
): Boolean {
  val qr = parseQRCode(raw)
  return if (expected.isInstance(qr)) {
    onMatch(qr)
  } else {
    showWrongQRCodeAlert(rhId, qr, expected, close)
    true
  }
}

// One alert for every wrong scan: name exactly what was scanned and the real place to scan it,
// and (only for a connection link, with an active user) offer to connect with it directly.
private fun showWrongQRCodeAlert(rhId: Long?, scanned: QRCodeType, expected: KClass<out QRCodeType>, close: () -> Unit) {
  // Relay link: it cannot be used to connect — reuse the existing relay alert (title + message).
  if (scanned is QRCodeType.ConnectionLink && scanned.linkType == SimplexLinkType.relay) {
    AlertManager.shared.showAlertMsg(
      title = generalGetString(MR.strings.relay_address_alert_title),
      text = generalGetString(MR.strings.relay_address_alert_message),
    )
    return
  }
  // Unrecognised: we cannot name the kind. Keep each screen's own existing "invalid" wording —
  // the server screen says "Invalid server address!", the verify screen says "Incorrect security
  // code!" (it scans a code, not a link), everything else the generic "not a SimpleX link".
  if (scanned is QRCodeType.Unknown) {
    when (expected) {
      QRCodeType.ServerAddress::class -> AlertManager.shared.showAlertMsg(
        title = generalGetString(MR.strings.smp_servers_invalid_address),
        text = generalGetString(MR.strings.smp_servers_check_address),
      )
      QRCodeType.SecurityCode::class -> AlertManager.shared.showAlertMsg(
        title = generalGetString(MR.strings.incorrect_code),
      )
      else -> AlertManager.shared.showAlertMsg(
        title = generalGetString(MR.strings.invalid_qr_code),
        text = generalGetString(MR.strings.code_you_scanned_is_not_simplex_link_qr_code),
      )
    }
    return
  }
  // Recognised wrong kind: "<what it is>\n\n<where to scan it>".
  val hasUser = chatModel.currentUser.value != null
  val where = if (hasUser) scanned.whereToScan else generalGetString(MR.strings.qr_where_no_user)
  val message = listOfNotNull(scanned.description.ifEmpty { null }, where).joinToString("\n\n")
  // The only safe action is "Connect" for a connection link, and only when a user exists
  // (planAndConnect is a no-op without one, and the breadcrumb screens don't exist yet).
  if (hasUser && scanned is QRCodeType.ConnectionLink) {
    AlertManager.shared.showAlertDialog(
      title = generalGetString(MR.strings.wrong_qr_code),
      text = message,
      confirmText = generalGetString(MR.strings.qr_action_connect),
      onConfirm = {
        close()
        withBGApi { planAndConnect(rhId, scanned.text, close = null) }
      },
    )
  } else {
    AlertManager.shared.showAlertMsg(title = generalGetString(MR.strings.wrong_qr_code), text = message)
  }
}

// A short, complete sentence naming the kind (so it reads as its own line, not a fragment that
// runs into the instruction below). Reuses the existing link-type labels for connection links.
private val QRCodeType.description: String
  get() = when (this) {
    is QRCodeType.ConnectionLink -> String.format(generalGetString(MR.strings.qr_type_connection), linkType.description)
    is QRCodeType.ServerAddress -> generalGetString(MR.strings.qr_type_server_address)
    is QRCodeType.MigrationLink -> generalGetString(MR.strings.qr_type_migration_link)
    is QRCodeType.DesktopAddress -> generalGetString(MR.strings.qr_type_desktop_address)
    is QRCodeType.SecurityCode -> generalGetString(MR.strings.qr_type_security_code)
    is QRCodeType.Unknown -> ""
  }

// The real button sequence from the chat list to the scanner that accepts this kind.
private val QRCodeType.whereToScan: String?
  get() = when (this) {
    is QRCodeType.ConnectionLink -> if (linkType == SimplexLinkType.relay) null else generalGetString(MR.strings.qr_where_connection)
    is QRCodeType.ServerAddress -> generalGetString(MR.strings.qr_where_server)
    is QRCodeType.MigrationLink -> generalGetString(MR.strings.qr_where_migration)
    is QRCodeType.DesktopAddress -> generalGetString(MR.strings.qr_where_desktop)
    is QRCodeType.SecurityCode -> generalGetString(MR.strings.qr_where_security_code)
    is QRCodeType.Unknown -> null
  }
