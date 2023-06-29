package chat.simplex.app.views.newchat

import android.graphics.Bitmap
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.*
import androidx.compose.ui.platform.*
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.core.graphics.*
import androidx.core.graphics.drawable.toBitmap
import boofcv.alg.drawing.FiducialImageEngine
import boofcv.alg.fiducial.qrcode.*
import boofcv.android.ConvertBitmap
import chat.simplex.app.R
import chat.simplex.app.SimplexApp
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.*
import kotlinx.coroutines.launch

@Composable
fun QRCode(
  connReq: String,
  modifier: Modifier = Modifier,
  tintColor: Color = Color(0xff062d56),
  withLogo: Boolean = true
) {
  val context = LocalContext.current
  val scope = rememberCoroutineScope()

  BoxWithConstraints {
    val maxWidthInPx = with(LocalDensity.current) { maxWidth.roundToPx() }
    val qr = remember(maxWidthInPx, connReq, tintColor, withLogo) {
      qrCodeBitmap(connReq, maxWidthInPx).replaceColor(Color.Black.toArgb(), tintColor.toArgb())
        .let { if (withLogo) it.addLogo() else it }
        .asImageBitmap()
    }
    Image(
      bitmap = qr,
      contentDescription = stringResource(R.string.image_descr_qr_code),
      modifier
        .clickable {
          scope.launch {
            val image = qrCodeBitmap(connReq, 1024).replaceColor(Color.Black.toArgb(), tintColor.toArgb())
              .let { if (withLogo) it.addLogo() else it }
            val file = saveTempImageUncompressed(image, false)
            if (file != null) {
              shareFile(context, "", file.absolutePath)
            }
          }
        }
    )
  }
}

fun qrCodeBitmap(content: String, size: Int = 1024): Bitmap {
  val qrCode = QrCodeEncoder().addAutomatic(content).setError(QrCode.ErrorLevel.L).fixate()
  /** See [QrCodeGeneratorImage.initialize] and [FiducialImageEngine.configure] for size calculation */
  val numModules = QrCode.totalModules(qrCode.version)
  val borderModule = 1
  // val calculatedFinalWidth = (pixelsPerModule * numModules) + 2 * (borderModule * pixelsPerModule)
  // size = (x * numModules) + 2 * (borderModule * x)
  // size / x = numModules + 2 * borderModule
  // x = size / (numModules + 2 * borderModule)
  val pixelsPerModule = size / (numModules + 2 * borderModule)
  // + 1 to make it with better quality
  val renderer = QrCodeGeneratorImage(pixelsPerModule + 1)
  renderer.borderModule = borderModule
  renderer.render(qrCode)
  return ConvertBitmap.grayToBitmap(renderer.gray, Bitmap.Config.RGB_565).scale(size, size)
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

fun Bitmap.addLogo(): Bitmap = applyCanvas {
  val radius = (width * 0.16f) / 2
  val paint = android.graphics.Paint()
  paint.color = android.graphics.Color.WHITE
  drawCircle(width / 2f, height / 2f, radius, paint)
  val logo = SimplexApp.context.resources.getDrawable(R.mipmap.icon_foreground, null).toBitmap()
  val logoSize = (width * 0.24).toInt()
  translate((width - logoSize) / 2f, (height - logoSize) / 2f)
  drawBitmap(logo, null, android.graphics.Rect(0, 0, logoSize, logoSize), null)
}

@Preview
@Composable
fun PreviewQRCode() {
  SimpleXTheme {
    QRCode(connReq = "https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D")
  }
}
