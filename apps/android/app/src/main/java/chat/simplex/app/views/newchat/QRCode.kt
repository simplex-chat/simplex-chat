package chat.simplex.app.views.newchat

import android.graphics.Bitmap
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Rect
import androidx.compose.ui.graphics.*
import androidx.compose.ui.layout.boundsInRoot
import androidx.compose.ui.layout.onGloballyPositioned
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalView
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.core.graphics.applyCanvas
import boofcv.alg.fiducial.qrcode.*
import boofcv.android.ConvertBitmap
import chat.simplex.app.R
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.*
import kotlinx.coroutines.launch

@Composable
fun QRCode(connReq: String, modifier: Modifier = Modifier, withLogo: Boolean = true, tintColor: Color = Color(0xff062d56)) {
  val view = LocalView.current
  val context = LocalContext.current
  val scope = rememberCoroutineScope()
  var rect by remember { mutableStateOf<Rect?>(null) }
  BoxWithConstraints(Modifier
    .onGloballyPositioned {
      rect = it.boundsInRoot()
    }
    .clickable {
      scope.launch {
        val r = rect
        if (r != null) {
          val image = Bitmap.createBitmap(r.width.toInt(), r.height.toInt(), Bitmap.Config.ARGB_8888).applyCanvas {
            translate(-r.left, -r.top)
            view.draw(this)
          }
          val file = saveTempImageUncompressed(image, false)
          if (file != null) {
            shareFile(context, "", file.absolutePath)
          }
        }
      }
    },
    contentAlignment = Alignment.Center
  ) {
    Image(
      bitmap = qrCodeBitmap(connReq, 1024).replaceColor(Color.Black.toArgb(), tintColor.toArgb()).asImageBitmap(),
      contentDescription = stringResource(R.string.image_descr_qr_code),
      modifier = modifier
    )
    if (withLogo) {
      Box(
        Modifier
          .size(maxWidth * 0.125f)
          .background(Color.White, RoundedCornerShape(100))
      )
      Image(
        painterResource(R.mipmap.icon_foreground),
        null,
        Modifier
          .size(maxWidth * 0.19f)
      )
    }
  }
}

fun qrCodeBitmap(content: String, size: Int): Bitmap {
  val qrCode = QrCodeEncoder().addAutomatic(content).setError(QrCode.ErrorLevel.M).fixate()
  val renderer = QrCodeGeneratorImage(5)
  renderer.render(qrCode)
  return ConvertBitmap.grayToBitmap(renderer.gray, Bitmap.Config.RGB_565)
}

fun Bitmap.replaceColor(from: Int, to: Int): Bitmap {
  val pixels = IntArray(width * height)
  getPixels(pixels, 0, width, 0, 0, width, height)
  var i = 0
  while (i < pixels.size) {
    if (pixels[i] == from) {
      pixels[i] = to
    }
    i++
  }
  setPixels(pixels, 0, width, 0, 0, width, height)
  return this
}

@Preview
@Composable
fun PreviewQRCode() {
  SimpleXTheme {
    QRCode(connReq = "https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D")
  }
}
