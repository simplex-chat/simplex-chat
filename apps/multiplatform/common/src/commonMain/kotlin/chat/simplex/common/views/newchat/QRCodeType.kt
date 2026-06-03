package chat.simplex.common.views.newchat

import chat.simplex.common.model.*
import chat.simplex.common.model.ServerAddress.Companion.parseServerAddress
import chat.simplex.common.views.helpers.parseToMarkdown
import chat.simplex.common.views.migration.strHasSimplexFileLink

// One unified type for every kind of QR code (or pasted string) the app can scan.
// Each scanner declares the kind it accepts; everything else is reported to the user with
// one shared "wrong QR code" alert (see handleScan / showWrongQRCodeAlert).
sealed class QRCodeType {
  abstract val text: String

  // A SimpleX connection link (contact / invitation / group / channel / relay).
  data class ConnectionLink(override val text: String, val linkType: SimplexLinkType?): QRCodeType()
  // A SMP/XFTP chat server address (the UserServer is built by the caller that has rhId).
  data class ServerAddress(override val text: String): QRCodeType()
  // A database migration file link (move a profile to this device).
  data class MigrationLink(override val text: String): QRCodeType()
  // A desktop remote-control address (link a mobile to a desktop).
  data class DesktopAddress(override val text: String): QRCodeType()
  // A contact's security / verification code (used to verify end-to-end encryption).
  data class SecurityCode(override val text: String): QRCodeType()
  // Anything not recognised as one of the above — e.g. a non-SimpleX QR, or garbage.
  data class Unknown(override val text: String): QRCodeType()
}

// The QR-encoded form of a desktop session invitation always starts with this scheme
// (single slash; see simplexmq StrEncoding RCSignedInvitation).
const val DESKTOP_ADDRESS_SCHEME = "xrcp:/"

// Classify a scanned/pasted string by composing the existing local parsers in a fixed priority
// order, returning the first match, else Unknown. No new parsing logic, no rhId; the markdown and
// server parsers are FFI calls into the core library.
fun parseQRCode(raw: String): QRCodeType {
  val t = raw.trim()
  return when {
    strHasSimplexFileLink(t) -> QRCodeType.MigrationLink(t)            // simplex:/file | https://simplex.chat/file
    t.startsWith(DESKTOP_ADDRESS_SCHEME) -> QRCodeType.DesktopAddress(t)
    else -> {
      // Accept only when the whole string is exactly one SimpleX link, keeping the parsed
      // link so we can read its type for the wrong-type alert.
      val md = parseToMarkdown(t)
      val link = if (md?.size == 1) md[0].format as? Format.SimplexLink else null
      when {
        link != null -> QRCodeType.ConnectionLink(t, link.linkType)
        parseServerAddress(t) != null -> QRCodeType.ServerAddress(t)
        isSecurityCode(t) -> QRCodeType.SecurityCode(t)
        else -> QRCodeType.Unknown(t)
      }
    }
  }
}

// A contact's security / verification code QR encodes the raw code with no scheme. The core
// builds it as `verificationCode = T.pack . unwords . chunks 5 . show . os2ip` — decimal digits
// of a SHA-256 integer, in groups of 5 separated by spaces (e.g. "61889 38426 ... 25"). Recognise
// it by shape: ≥32 decimal digits once the grouping whitespace is stripped (no hex — `show` only
// emits decimal). Checked last (after every real link/address parser), so those never reach here;
// the scanned text keeps its spaces, so verifyCode still matches.
internal fun isSecurityCode(t: String): Boolean {
  val digits = t.filterNot { it.isWhitespace() }
  return digits.length >= 32 && digits.all { it in '0'..'9' }
}
