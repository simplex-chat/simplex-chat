package chat.simplex.app.views.newchat

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
import com.google.common.util.concurrent.ListenableFuture
import com.google.mlkit.vision.barcode.BarcodeScannerOptions
import com.google.mlkit.vision.barcode.BarcodeScanning
import com.google.mlkit.vision.barcode.common.Barcode
import com.google.mlkit.vision.common.InputImage
import java.util.concurrent.*

// Bar code scanner adapted from https://github.com/MakeItEasyDev/Jetpack-Compose-BarCode-Scanner

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
//    modifier = Modifier.fillMaxSize(),
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
        val barcodeAnalyser = BarCodeAnalyser { barcodes ->
          barcodes.firstOrNull()?.rawValue?.let(onBarcode)
        }
        val imageAnalysis: ImageAnalysis = ImageAnalysis.Builder()
          .setBackpressureStrategy(ImageAnalysis.STRATEGY_KEEP_ONLY_LATEST)
          .build()
          .also { it.setAnalyzer(cameraExecutor, barcodeAnalyser) }

        try {
          cameraProvider.unbindAll()
          cameraProvider.bindToLifecycle(lifecycleOwner, cameraSelector, preview, imageAnalysis)
        } catch (e: Exception) {
          Log.d("SIMPLEX", "CameraPreview: ${e.localizedMessage}")
        }
      }, ContextCompat.getMainExecutor(context))
    }
  )
}

class BarCodeAnalyser(
  private val onBarcodeDetected: (barcodes: List<Barcode>) -> Unit,
): ImageAnalysis.Analyzer {
  private var lastAnalyzedTimeStamp = 0L

  @ExperimentalGetImage
  override fun analyze(image: ImageProxy) {
    val currentTimestamp = System.currentTimeMillis()
    if (currentTimestamp - lastAnalyzedTimeStamp >= TimeUnit.SECONDS.toMillis(1)) {
      image.image?.let { imageToAnalyze ->
        val options = BarcodeScannerOptions.Builder()
          .setBarcodeFormats(Barcode.FORMAT_ALL_FORMATS)
          .build()
        val barcodeScanner = BarcodeScanning.getClient(options)
        val imageToProcess = InputImage.fromMediaImage(imageToAnalyze, image.imageInfo.rotationDegrees)

        barcodeScanner.process(imageToProcess)
          .addOnSuccessListener { barcodes ->
            if (barcodes.isNotEmpty()) {
              onBarcodeDetected(barcodes)
            } else {
              Log.d("SIMPLEX", "BarcodeAnalyser: No barcode Scanned")
            }
          }
          .addOnFailureListener { exception ->
            Log.d("SIMPLEX", "BarcodeAnalyser: Something went wrong $exception")
          }
          .addOnCompleteListener {
            image.close()
          }
      }
      lastAnalyzedTimeStamp = currentTimestamp
    } else {
      image.close()
    }
  }
}
