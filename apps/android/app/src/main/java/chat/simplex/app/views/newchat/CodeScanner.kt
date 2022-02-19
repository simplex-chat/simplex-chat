package chat.simplex.app.views.newchat

import android.Manifest
import android.util.Log
import android.view.ViewGroup
import android.widget.Toast
import androidx.camera.core.CameraSelector
import androidx.camera.core.ImageAnalysis
import androidx.camera.core.Preview
import androidx.camera.lifecycle.ProcessCameraProvider
import androidx.camera.view.PreviewView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.LocalLifecycleOwner
import androidx.compose.ui.unit.dp
import androidx.compose.ui.viewinterop.AndroidView
import androidx.core.content.ContextCompat
import com.google.accompanist.permissions.ExperimentalPermissionsApi
import com.google.accompanist.permissions.rememberPermissionState
import com.google.common.util.concurrent.ListenableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import android.annotation.SuppressLint
import androidx.camera.core.ImageProxy
import androidx.navigation.NavController
import chat.simplex.app.model.ChatController
import chat.simplex.app.views.helpers.CloseSheetBar
import chat.simplex.app.views.helpers.withApi
import com.google.mlkit.vision.barcode.BarcodeScannerOptions
import com.google.mlkit.vision.barcode.BarcodeScanning
import com.google.mlkit.vision.barcode.common.Barcode
import com.google.mlkit.vision.common.InputImage
import java.util.concurrent.TimeUnit

// Bar code scanner adapted from https://github.com/MakeItEasyDev/Jetpack-Compose-BarCode-Scanner

@ExperimentalPermissionsApi
@Composable
fun CodeScanner(ctrl: ChatController, nav: NavController) {
  Surface(color = MaterialTheme.colors.background) {
    Column(
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      CloseSheetBar({ nav.popBackStack() })

      Spacer(modifier = Modifier.height(10.dp))

      val cameraPermissionState = rememberPermissionState(permission = Manifest.permission.CAMERA)

      Button(
        onClick = {
          cameraPermissionState.launchPermissionRequest()
        }
      ) {
        Text(text = "Camera Permission")
      }

      Spacer(modifier = Modifier.height(10.dp))

      CameraPreview { connReqUri ->
        withApi {
          val res = ctrl.apiConnect(connReqUri)
          // check if it is valid
          nav.popBackStack()
        }
      }
    }
  }
}

@Composable
fun CameraPreview(onBarcode: (String) -> Unit) {
  val context = LocalContext.current
  val lifecycleOwner = LocalLifecycleOwner.current
  var preview by remember { mutableStateOf<Preview?>(null) }
  val barCodeVal = remember { mutableStateOf("") }

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
    modifier = Modifier
      .fillMaxSize(),
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
          barcodes.first().rawValue?.let(onBarcode)
//          barcodes.forEach { barcode ->
//            barcode.rawValue?.let { barcodeValue ->
//              barCodeVal.value = barcodeValue
//              Toast.makeText(context, barcodeValue, Toast.LENGTH_SHORT).show()
//            }
//          }
        }
        val imageAnalysis: ImageAnalysis = ImageAnalysis.Builder()
          .setBackpressureStrategy(ImageAnalysis.STRATEGY_KEEP_ONLY_LATEST)
          .build()
          .also {
            it.setAnalyzer(cameraExecutor, barcodeAnalyser)
          }

        try {
          cameraProvider.unbindAll()
          cameraProvider.bindToLifecycle(
            lifecycleOwner,
            cameraSelector,
            preview,
            imageAnalysis
          )
        } catch (e: Exception) {
          Log.d("TAG", "CameraPreview: ${e.localizedMessage}")
        }
      }, ContextCompat.getMainExecutor(context))
    }
  )
}

class BarCodeAnalyser(
  private val onBarcodeDetected: (barcodes: List<Barcode>) -> Unit,
): ImageAnalysis.Analyzer {
  private var lastAnalyzedTimeStamp = 0L

  @SuppressLint("UnsafeOptInUsageError")
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
              Log.d("SIMPLEX", "analyze: No barcode Scanned")
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
