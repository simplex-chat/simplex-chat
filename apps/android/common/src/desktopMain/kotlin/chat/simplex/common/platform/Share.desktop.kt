package chat.simplex.common.platform

import androidx.compose.ui.platform.ClipboardManager
import androidx.compose.ui.platform.UriHandler
import androidx.compose.ui.text.AnnotatedString
import chat.simplex.common.views.helpers.withApi
import java.io.File
import java.net.URI
import java.net.URLEncoder
import com.icerockdev.library.MR

actual fun UriHandler.sendEmail(subject: String, body: CharSequence) {
  val subjectEncoded = URLEncoder.encode(subject, "UTF-8").replace("+", "%20")
  val bodyEncoded = URLEncoder.encode(subject, "UTF-8").replace("+", "%20")
  openUri("mailto:?subject=$subjectEncoded&body=$bodyEncoded")
}

actual fun ClipboardManager.shareText(text: String) {
  setText(AnnotatedString(text))
  showToast(MR.strings.copied.localized())
}

actual fun shareFile(text: String, filePath: String) {
  withApi {
    FileChooserLauncher(false) { to: URI? ->
      if (to != null) {
        copyFileToFile(File(filePath), to) {}
      }
    }.launch(filePath)
  }
}