package chat.simplex.app.views.newchat

import android.graphics.ImageFormat.*
import android.util.Log
import android.view.ViewGroup
import androidx.camera.core.*
import androidx.camera.lifecycle.ProcessCameraProvider
import androidx.camera.view.PreviewView
import androidx.compose.runtime.*
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalLifecycleOwner
import androidx.compose.ui.viewinterop.AndroidView
import androidx.core.content.ContextCompat
import chat.simplex.app.TAG
import com.google.common.util.concurrent.ListenableFuture
import com.google.zxing.*
import com.google.zxing.common.HybridBinarizer
import com.google.zxing.qrcode.QRCodeReader
import java.nio.ByteBuffer
import java.util.*
import java.util.concurrent.*

@Composable
fun QRCodeScanner(onBarcode: (String) -> Unit) {
  val context = LocalContext.current
  val lifecycleOwner = LocalLifecycleOwner.current
  var preview by remember { mutableStateOf<Preview?>(null) }
  var lastAnalyzedTimeStamp = 0L
  var contactLink = ""

  AndroidView(
    factory = { AndroidViewContext ->
      PreviewView(AndroidViewContext).apply {
        this.scaleType = PreviewView.ScaleType.FILL_CENTER
        layoutParams = ViewGroup.LayoutParams(
          ViewGroup.LayoutParams.MATCH_PARENT,
          ViewGroup.LayoutParams.MATCH_PARENT,
        )
        implementationMode = PreviewView.ImplementationMode.COMPATIBLE
      }
    }
  ) { previewView ->
    val cameraSelector: CameraSelector = CameraSelector.Builder()
      .requireLensFacing(CameraSelector.LENS_FACING_BACK)
      .build()
    val cameraExecutor: ExecutorService = Executors.newSingleThreadExecutor()
    val cameraProviderFuture: ListenableFuture<ProcessCameraProvider> =
      ProcessCameraProvider.getInstance(context)

    cameraProviderFuture.addListener({
      preview = Preview.Builder().build().also {
        it.setSurfaceProvider(previewView.surfaceProvider)
      }
      val cameraProvider: ProcessCameraProvider = cameraProviderFuture.get()
      val qrReader = QRCodeReader()
      fun getQR(imageProxy: ImageProxy) {
        val currentTimeStamp = System.currentTimeMillis()
        if (currentTimeStamp - lastAnalyzedTimeStamp >= TimeUnit.SECONDS.toMillis(1)) {
          lastAnalyzedTimeStamp = currentTimeStamp
          val bitmap = imageProxyToBinaryBitmap(imageProxy)
          val decodeHints = EnumMap<DecodeHintType, Any>(
            DecodeHintType::class.java
          )
          decodeHints[DecodeHintType.POSSIBLE_FORMATS] = BarcodeFormat.QR_CODE
          try {
            val result = qrReader.decode(bitmap, decodeHints)
            if (result.text != contactLink) {
              // Make sure link is new and not a repeat
              contactLink = result.text
              onBarcode(contactLink)
            }
            imageProxy.close()
          } catch (nfe: NotFoundException) {
            // Comment log for too much noise
            // Log.d(TAG, "Failed to find QR code in current image.")
            imageProxy.close()
          } catch (fe: FormatException) {
            Log.d(TAG, "Failed to find QR code of expected format. Possible mis-detection.")
            imageProxy.close()
          } catch (ce: ChecksumException) {
            Log.d(TAG, "Invalid QR Code. Checksum failure.")
            imageProxy.close()
          } catch (e: Exception) {
            Log.d(TAG, "QR code detection failed for unknown reason: ${e.localizedMessage}")
            imageProxy.close()
          }
        } else {
          imageProxy.close()
        }
      }

      val imageAnalyzer = ImageAnalysis.Analyzer { proxy -> getQR(proxy) }
      val imageAnalysis: ImageAnalysis = ImageAnalysis.Builder()
        .setBackpressureStrategy(ImageAnalysis.STRATEGY_KEEP_ONLY_LATEST)
        .setImageQueueDepth(1)
        .build()
        .also { it.setAnalyzer(cameraExecutor, imageAnalyzer) }
      try {
        cameraProvider.unbindAll()
        cameraProvider.bindToLifecycle(lifecycleOwner, cameraSelector, preview, imageAnalysis)
      } catch (e: Exception) {
        Log.d(TAG, "CameraPreview: ${e.localizedMessage}")
      }
    }, ContextCompat.getMainExecutor(context))
  }
}

private fun imageProxyToBinaryBitmap(img: ImageProxy) : BinaryBitmap? {
  if (img.format == YUV_420_888 || img.format == YUV_422_888 || img.format == YUV_444_888) {
    val byteBuffer: ByteBuffer = img.planes[0].buffer
    val imageData = ByteArray(byteBuffer.capacity())
    try {
      byteBuffer.get(imageData)
      val source = PlanarYUVLuminanceSource(
        imageData,
        img.width, img.height,
        0, 0,
        img.width, img.height,
        false
      )
      return BinaryBitmap(HybridBinarizer(source))
    } catch (e: Exception) {
      e.printStackTrace()
    }
  }
  return null
}
