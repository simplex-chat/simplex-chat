package chat.simplex.common.platform

import androidx.compose.ui.platform.ClipboardManager
import androidx.compose.ui.platform.UriHandler

expect fun UriHandler.sendEmail(subject: String, body: CharSequence)

expect fun ClipboardManager.shareText(text: String)
expect fun shareFile(text: String, filePath: String)