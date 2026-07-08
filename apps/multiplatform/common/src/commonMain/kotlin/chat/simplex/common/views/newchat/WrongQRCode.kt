package chat.simplex.common.views.newchat

import chat.simplex.common.model.*
import chat.simplex.common.model.ServerAddress.Companion.parseServerAddress
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.migration.strHasSimplexFileLink
import chat.simplex.res.MR

// The QR-encoded form of a desktop session address always starts with this scheme
// (single slash; see simplexmq RCSignedInvitation encoding).
const val DESKTOP_ADDRESS_SCHEME = "xrcp:/"

// The kinds of QR code the app knows how to scan. Each scanner expects one of these; when a
// scanner's own parser rejects a scan, identifyQRCode reports which of the others it turned out to
// be. A security code has no scheme, so it is recognised by shape (see isSecurityCode) — every
// scanner detects it except the security-code scanner itself (see detectSecurityCode).
private sealed class ScannedQRCode {
  data class ConnectionLink(val linkType: SimplexLinkType): ScannedQRCode()
  object ServerAddress: ScannedQRCode()
  object MigrationLink: ScannedQRCode()
  object DesktopAddress: ScannedQRCode()
  object SecurityCode: ScannedQRCode()

  // "This is a <kind>." — names what was actually scanned.
  val description: String get() = when (this) {
    is ConnectionLink -> String.format(generalGetString(MR.strings.wrong_qr_is_connection_link), linkType.description)
    ServerAddress -> generalGetString(MR.strings.wrong_qr_is_server_address)
    MigrationLink -> generalGetString(MR.strings.wrong_qr_is_migration_link)
    DesktopAddress -> generalGetString(MR.strings.wrong_qr_is_desktop_address)
    SecurityCode -> generalGetString(MR.strings.wrong_qr_is_security_code)
  }

  // "<Where to scan it>." — the screen that accepts this kind, or null when there is no scanner for
  // it (a relay address is a connection link but cannot be used to connect from New chat).
  val whereToScan: String? get() = when (this) {
    is ConnectionLink -> if (linkType == SimplexLinkType.relay) null else generalGetString(MR.strings.wrong_qr_where_connection_link)
    ServerAddress -> generalGetString(MR.strings.wrong_qr_where_server_address)
    MigrationLink -> generalGetString(MR.strings.wrong_qr_where_migration_link)
    DesktopAddress -> generalGetString(MR.strings.wrong_qr_where_desktop_address)
    SecurityCode -> generalGetString(MR.strings.wrong_qr_where_security_code)
  }
}

// The "megaparser": try every known parser and return the first match, or null when the scan is not
// a SimpleX QR code at all. The scanner that already rejected this text passes it here, so the kind
// it expected either does not match (a link is not a server address) or was a false negative worth
// reporting anyway. No new parsing logic — these are the same parsers each scanner already uses.
private fun identifyQRCode(text: String, detectSecurityCode: Boolean): ScannedQRCode? {
  val t = text.trim()
  val md = parseToMarkdown(t)
  val linkType = if (md?.size == 1) (md[0].format as? Format.SimplexLink)?.linkType else null
  return when {
    linkType != null -> ScannedQRCode.ConnectionLink(linkType)
    parseServerAddress(t) != null -> ScannedQRCode.ServerAddress
    strHasSimplexFileLink(t) -> ScannedQRCode.MigrationLink
    t.startsWith(DESKTOP_ADDRESS_SCHEME) -> ScannedQRCode.DesktopAddress
    detectSecurityCode && isSecurityCode(t) -> ScannedQRCode.SecurityCode
    else -> null
  }
}

// A contact's security/verification code has no scheme; the QR encodes it verbatim. The core builds
// it as `verificationCode = unwords . chunks 5 . show . os2ip` — the decimal digits of a hash in
// space-separated groups of 5 (e.g. "61889 38426 ... 25"), ~77 digits. Recognise that exact shape:
// 2+ whitespace-separated groups of ASCII digits, 32+ digits total. Requiring the grouping keeps a
// bare long number from matching. Checked last, after every scheme-based parser.
private fun isSecurityCode(t: String): Boolean {
  val groups = t.split(Regex("\\s+")).filter { it.isNotEmpty() }
  return groups.size >= 2 &&
    groups.sumOf { it.length } >= 32 &&
    groups.all { g -> g.all { it in '0'..'9' } }
}

// Shown by every scanner when a scan is not the kind that screen expects: name what was actually
// scanned and where to scan it. When it is not a recognisable SimpleX QR code, fall back to
// [orElse] — a unified "not a SimpleX QR code" message by default, which the security-code scanner
// overrides with its own "incorrect code" (there the scan is the right kind but the wrong value).
fun showWrongQRCodeAlert(text: String, detectSecurityCode: Boolean = true, orElse: () -> Unit = ::showNotSimplexQRCodeAlert) {
  val qr = identifyQRCode(text, detectSecurityCode)
  if (qr == null) {
    orElse()
  } else {
    AlertManager.shared.showAlertMsg(
      title = generalGetString(MR.strings.wrong_qr_code),
      text = listOfNotNull(qr.description, qr.whereToScan).joinToString("\n\n")
    )
  }
}

private fun showNotSimplexQRCodeAlert() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.wrong_qr_code),
    text = generalGetString(MR.strings.code_you_scanned_is_not_simplex_link_qr_code)
  )
}
