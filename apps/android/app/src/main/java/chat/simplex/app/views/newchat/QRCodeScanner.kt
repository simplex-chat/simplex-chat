package chat.simplex.app.views.newchat

import android.annotation.SuppressLint
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
import boofcv.abst.fiducial.QrCodeDetector
import boofcv.alg.color.ColorFormat
import boofcv.android.ConvertCameraImage
import boofcv.factory.fiducial.FactoryFiducial
import boofcv.struct.image.GrayU8
import chat.simplex.app.TAG
import com.google.common.util.concurrent.ListenableFuture
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
      val detector: QrCodeDetector<GrayU8> = FactoryFiducial.qrcode(null, GrayU8::class.java)
      fun getQR(imageProxy: ImageProxy) {
        val currentTimeStamp = System.currentTimeMillis()
        if (currentTimeStamp - lastAnalyzedTimeStamp >= TimeUnit.SECONDS.toMillis(1)) {
          detector.process(imageProxyToGrayU8(imageProxy))
          val found = detector.detections
          val qr = found.firstOrNull()
          if (qr != null) {
            if (qr.message != contactLink) {
              // Make sure link is new and not a repeat
              contactLink = qr.message
              onBarcode(contactLink)
            }
          }
        }
        imageProxy.close()
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



@SuppressLint("UnsafeOptInUsageError")
private fun imageProxyToGrayU8(img: ImageProxy) : GrayU8? {
  val image = img.image
  if (image != null) {
    val out = GrayU8()
    ConvertCameraImage.imageToBoof(image, ColorFormat.GRAY, out, null)
    return out
  }
  return null
}
