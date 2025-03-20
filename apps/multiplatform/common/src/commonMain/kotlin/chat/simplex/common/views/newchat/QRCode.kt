package chat.simplex.common.views.newchat

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.*
import androidx.compose.ui.platform.*
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.stringResource
import boofcv.alg.drawing.FiducialImageEngine
import boofcv.alg.fiducial.qrcode.*
import chat.simplex.common.model.CryptoFile
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import kotlinx.coroutines.launch

@Composable
fun SimpleXLinkQRCode(
  connReq: String,
  modifier: Modifier = Modifier,
  padding: PaddingValues = PaddingValues(horizontal = DEFAULT_PADDING * 2f, vertical = DEFAULT_PADDING_HALF),
  tintColor: Color = Color(0xff062d56),
  withLogo: Boolean = true,
  onShare: (() -> Unit)? = null,
) {
  QRCode(
    simplexChatLink(connReq),
    modifier,
    padding,
    tintColor,
    withLogo,
    onShare,
  )
}

fun simplexChatLink(uri: String): String {
  return if (uri.startsWith("https://simplex.chat/")) {
    uri.replace("https://simplex.chat/", "simplex:/")
  } else {
    uri
  }
}

@Composable
fun QRCode(
  connReq: String,
  modifier: Modifier = Modifier,
  padding: PaddingValues = PaddingValues(horizontal = DEFAULT_PADDING * 2f, vertical = DEFAULT_PADDING_HALF),
  tintColor: Color = Color(0xff062d56),
  withLogo: Boolean = true,
  onShare: (() -> Unit)? = null,
) {
  val scope = rememberCoroutineScope()
  val qr = remember(connReq, tintColor, withLogo) {
    qrCodeBitmap(connReq, 1024).replaceColor(Color.Black.toArgb(), tintColor.toArgb())
      .let { if (withLogo) it.addLogo() else it }
  }
  Box(Modifier.fillMaxWidth(), contentAlignment = Alignment.Center) {
    Image(
      bitmap = qr,
      contentDescription = stringResource(MR.strings.image_descr_qr_code),
      Modifier
        .padding(padding)
        .widthIn(max = 400.dp)
        .aspectRatio(1f)
        .then(modifier)
        .clickable {
          scope.launch {
            val image = qrCodeBitmap(connReq, 1024).replaceColor(Color.Black.toArgb(), tintColor.toArgb())
              .let { if (withLogo) it.addLogo() else it }
            val file = saveTempImageUncompressed(image, true)
            if (file != null) {
              shareFile("", CryptoFile.plain(file.absolutePath))
              onShare?.invoke()
            }
          }
        }
    )
  }
}

fun qrCodeBitmap(content: String, size: Int = 1024): ImageBitmap {
  val qrCode = QrCodeEncoder().addAutomatic(content).setError(QrCode.ErrorLevel.L).fixate()
  /** See [QrCodeGeneratorImage.initialize] and [FiducialImageEngine.configure] for size calculation */
  val numModules = QrCode.totalModules(qrCode.version)
  // Hide border on light themes to make it fit to the same place as camera in QRCodeScanner.
  // On dark themes better to show the border
  val borderModule = if (CurrentColors.value.colors.isLight) 0 else 1
  // val calculatedFinalWidth = (pixelsPerModule * numModules) + 2 * (borderModule * pixelsPerModule)
  // size = (x * numModules) + 2 * (borderModule * x)
  // size / x = numModules + 2 * borderModule
  // x = size / (numModules + 2 * borderModule)
  val pixelsPerModule = size / (numModules + 2 * borderModule)
  // + 1 to make it with better quality
  val renderer = QrCodeGeneratorImage(pixelsPerModule + 1)
  renderer.borderModule = borderModule
  renderer.render(qrCode)
  return renderer.gray.toImageBitmap().scale(size, size)
}

expect fun ImageBitmap.replaceColor(from: Int, to: Int): ImageBitmap

@Preview
@Composable
fun PreviewQRCode() {
  SimpleXTheme {
    QRCode(connReq = "simplex:/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D")
  }
}
