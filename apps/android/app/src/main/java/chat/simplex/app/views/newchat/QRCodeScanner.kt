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
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

@Composable
fun QRCodeScanner(onBarcode: (String) -> Unit) {
  val context = LocalContext.current
  val lifecycleOwner = LocalLifecycleOwner.current
  var preview by remember { mutableStateOf<Preview?>(null) }

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
    },
    update = { previewView ->
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
          val bitmap = imageProxyToBinaryBitmap(imageProxy)
          val decodeHints = EnumMap<DecodeHintType, Any>(
            DecodeHintType::class.java
          )
          decodeHints[DecodeHintType.TRY_HARDER] = true
          decodeHints[DecodeHintType.POSSIBLE_FORMATS] = BarcodeFormat.QR_CODE
          decodeHints[DecodeHintType.PURE_BARCODE] = false
          // TODO(Breaks here need to downsample bitmap)
          // val result = qrReader.decode(bitmap, decodeHints)
          // println("QR RESULT: ${result.text}")
          // onBarcode(result.text)
        }
        val imageAnalyzer = ImageAnalysis.Analyzer { proxy -> getQR(proxy)}
        val imageAnalysis: ImageAnalysis = ImageAnalysis.Builder()
        .setBackpressureStrategy(ImageAnalysis.STRATEGY_KEEP_ONLY_LATEST)
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
  )
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
