package chat.simplex.common.views.newchat

import chat.simplex.common.model.ScannedLinkType
import chat.simplex.common.model.SimplexLinkType
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR

// Shown when a scanner (or the Migrate paste field) is handed a valid SimpleX
// code of a kind it does not accept: name what it actually is and where to use
// it. The type comes from the core classifier (checkLink), so this is purely
// presentation — no parsing here.
private fun wrongQRCodeMessage(type: ScannedLinkType): String = when (type) {
  is ScannedLinkType.Connection ->
    if (type.linkType == SimplexLinkType.relay)
      String.format(generalGetString(MR.strings.wrong_qr_relay_address), type.linkType.description)
    else
      String.format(generalGetString(MR.strings.wrong_qr_connection_link), type.linkType.description)
  ScannedLinkType.Server -> generalGetString(MR.strings.wrong_qr_server_address)
  ScannedLinkType.FileDescription -> generalGetString(MR.strings.wrong_qr_migration_link)
  ScannedLinkType.DesktopCtrl -> generalGetString(MR.strings.wrong_qr_desktop_address)
  ScannedLinkType.VerificationCode -> generalGetString(MR.strings.wrong_qr_security_code)
}

// Title defaults to "Wrong QR code"; MigrateToDevice passes the neutral "Wrong
// link" title because its path is shared with the paste button.
fun showWrongQRCodeAlert(type: ScannedLinkType, title: String = generalGetString(MR.strings.wrong_qr_code)) {
  AlertManager.shared.showAlertMsg(title = title, text = wrongQRCodeMessage(type))
}
