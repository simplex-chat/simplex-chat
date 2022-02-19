package chat.simplex.app.views.newchat

import android.graphics.Bitmap
import android.graphics.Color
import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Size
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.layout.onGloballyPositioned
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.toSize
import chat.simplex.app.ui.theme.SimpleXTheme
import com.google.zxing.BarcodeFormat
import com.google.zxing.EncodeHintType
import com.google.zxing.qrcode.QRCodeWriter

@Composable
fun QRCode(connReq: String) {
  Image(
    bitmap = qrCodeBitmap(connReq, 1024).asImageBitmap(),
    contentDescription = "QR Code"
  )
}

fun qrCodeBitmap(content: String, size: Int): Bitmap {
  val hints = hashMapOf<EncodeHintType, Int>().also { it[EncodeHintType.MARGIN] = 1 }
  val bits = QRCodeWriter().encode(content, BarcodeFormat.QR_CODE, size, size, hints)
  return Bitmap.createBitmap(size, size, Bitmap.Config.RGB_565).also {
    for (x in 0 until size) {
      for (y in 0 until size) {
        it.setPixel(x, y, if (bits[x, y]) Color.BLACK else Color.WHITE)
      }
    }
  }
}

@Preview
@Composable
fun PreviewQRCode() {
  SimpleXTheme {
    QRCode(connReq = "https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D")
  }
}
