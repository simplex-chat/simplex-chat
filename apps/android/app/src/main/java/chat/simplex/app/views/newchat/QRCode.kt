package chat.simplex.app.views.newchat

import android.graphics.Bitmap
import androidx.compose.foundation.Image
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import boofcv.alg.fiducial.qrcode.QrCodeEncoder
import boofcv.alg.fiducial.qrcode.QrCodeGeneratorImage
import boofcv.android.ConvertBitmap
import chat.simplex.app.R
import chat.simplex.app.ui.theme.SimpleXTheme

@Composable
fun QRCode(connReq: String, modifier: Modifier = Modifier) {
  Image(
    bitmap = qrCodeBitmap(connReq, 1024).asImageBitmap(),
    contentDescription = stringResource(R.string.image_descr_qr_code),
    modifier = modifier
  )
}

fun qrCodeBitmap(content: String, size: Int): Bitmap {
  val qrCode = QrCodeEncoder().addAutomatic(content).fixate()
  val renderer = QrCodeGeneratorImage(5)
  renderer.render(qrCode)
  return ConvertBitmap.grayToBitmap(renderer.gray, Bitmap.Config.RGB_565)
}

@Preview
@Composable
fun PreviewQRCode() {
  SimpleXTheme {
    QRCode(connReq = "https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D")
  }
}
