package chat.simplex.app.views.helpers

import android.content.*
import androidx.core.content.ContextCompat

fun shareText(cxt: Context, text: String) {
  val sendIntent: Intent = Intent().apply {
    action = Intent.ACTION_SEND
    putExtra(Intent.EXTRA_TEXT, text)
    type = "text/plain"
  }
  val shareIntent = Intent.createChooser(sendIntent, null)
  cxt.startActivity(shareIntent)
}

fun copyText(cxt: Context, text: String) {
  val clipboard = ContextCompat.getSystemService(cxt, ClipboardManager::class.java)
  clipboard?.setPrimaryClip(ClipData.newPlainText("text", text))
}
