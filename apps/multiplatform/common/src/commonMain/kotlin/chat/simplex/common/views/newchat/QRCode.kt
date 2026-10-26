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
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.*
import kotlinx.coroutines.launch

@Composable
fun SimpleXCreatedLinkQRCode(
  connLink: CreatedConnLink,
  short: Boolean,
  modifier: Modifier = Modifier,
  padding: PaddingValues = PaddingValues(horizontal = DEFAULT_PADDING * 2f, vertical = DEFAULT_PADDING_HALF),
  tintColor: Color = Color(0xff062d56),
  withLogo: Boolean = true,
  onShare: (() -> Unit)? = null,
) {
  QRCode(
    connLink.simplexChatUri(short),
    small = short && connLink.connShortLink != null,
    modifier,
    padding,
    tintColor,
    withLogo,
    onShare,
  )
}

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
    small = connReq.count() < 200,
    modifier,
    padding,
    tintColor,
    withLogo,
    onShare,
  )
}

private const val QUIET_ZONE = 4

@Composable
fun QRCode(
  connReq: String,
  small: Boolean = false,
  modifier: Modifier = Modifier,
  padding: PaddingValues = PaddingValues(horizontal = DEFAULT_PADDING * 2f, vertical = DEFAULT_PADDING_HALF),
  tintColor: Color = Color(0xff062d56),
  withLogo: Boolean = true,
  onShare: (() -> Unit)? = null,
) {
  val scope = rememberCoroutineScope()
  val logoSize = if (small) 0.21f else 0.16f
  val errorLevel = if (small) QrCode.ErrorLevel.M else QrCode.ErrorLevel.L
  val qrCode = remember(connReq, errorLevel) { QrCodeEncoder().addAutomatic(connReq).setError(errorLevel).fixate() }
  val zoneScale = 1 + 2f * QUIET_ZONE / QrCode.totalModules(qrCode.version)
  val qr = remember(qrCode, tintColor, withLogo) {
    qrCodeBitmap(qrCode, 1024).replaceColor(Color.Black.toArgb(), tintColor.toArgb())
      .let { if (withLogo) it.addLogo(logoSize / zoneScale) else it }
  }
  Box(Modifier.fillMaxWidth(), contentAlignment = Alignment.Center) {
    Image(
      bitmap = qr,
      contentDescription = stringResource(MR.strings.image_descr_qr_code),
      Modifier
        .padding(padding)
        .widthIn(max = 400.dp)
        .fillMaxWidth(if (small) 0.63f * zoneScale else 1f)
        .aspectRatio(1f)
        .then(modifier)
        .clickable {
          scope.launch {
            val file = saveTempImageUncompressed(qr, true)
            if (file != null) {
              shareFile("", CryptoFile.plain(file.absolutePath))
              onShare?.invoke()
            }
          }
        }
    )
  }
}

fun qrCodeBitmap(qrCode: QrCode, size: Int = 1024): ImageBitmap {
  /** See [QrCodeGeneratorImage.initialize] and [FiducialImageEngine.configure] for size calculation */
  val numModules = QrCode.totalModules(qrCode.version)
  val borderModule = QUIET_ZONE
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
    QRCode(connReq = "https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D")
  }
}
