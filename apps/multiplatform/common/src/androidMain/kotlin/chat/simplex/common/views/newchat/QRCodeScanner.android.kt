package chat.simplex.common.views.newchat

import android.Manifest
import android.annotation.SuppressLint
import android.content.pm.PackageManager
import android.util.Log
import android.view.ViewGroup
import androidx.camera.core.*
import androidx.camera.lifecycle.ProcessCameraProvider
import androidx.camera.view.PreviewView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clipToBounds
import androidx.compose.ui.platform.*
import androidx.compose.ui.unit.dp
import androidx.compose.ui.viewinterop.AndroidView
import androidx.core.content.ContextCompat
import boofcv.abst.fiducial.QrCodeDetector
import boofcv.alg.color.ColorFormat
import boofcv.android.ConvertCameraImage
import boofcv.factory.fiducial.FactoryFiducial
import boofcv.struct.image.GrayU8
import chat.simplex.common.platform.TAG
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import com.google.accompanist.permissions.PermissionStatus
import com.google.accompanist.permissions.rememberPermissionState
import com.google.common.util.concurrent.ListenableFuture
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import java.util.concurrent.*

// Adapted from learntodroid - https://gist.github.com/learntodroid/8f839be0b29d0378f843af70607bd7f5

@Composable
actual fun QRCodeScanner(
  showQRCodeScanner: MutableState<Boolean>,
  padding: PaddingValues,
  onBarcode: (String) -> Unit
) {
  val context = LocalContext.current
  val lifecycleOwner = LocalLifecycleOwner.current
  var preview by remember { mutableStateOf<Preview?>(null) }
  var lastAnalyzedTimeStamp = 0L
  var contactLink = ""

  val cameraProviderFuture by produceState<ListenableFuture<ProcessCameraProvider>?>(initialValue = null) {
    value = ProcessCameraProvider.getInstance(context)
  }

  DisposableEffect(lifecycleOwner) {
    onDispose {
      cameraProviderFuture?.get()?.unbindAll()
    }
  }

  Box(Modifier.fillMaxWidth(), contentAlignment = Alignment.Center) {
    val cameraPermissionState = rememberPermissionState(permission = Manifest.permission.CAMERA)
    val modifier = Modifier
      .padding(padding)
      .clipToBounds()
      .widthIn(max = 400.dp)
      .aspectRatio(1f)
    val showScanner = remember { showQRCodeScanner }
    if (showScanner.value && cameraPermissionState.status == PermissionStatus.Granted) {
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
        modifier = modifier
      ) { previewView ->
        val cameraSelector: CameraSelector = CameraSelector.Builder()
          .requireLensFacing(CameraSelector.LENS_FACING_BACK)
          .build()
        val cameraExecutor: ExecutorService = Executors.newSingleThreadExecutor()
        cameraProviderFuture?.addListener({
          preview = Preview.Builder().build().also {
            it.setSurfaceProvider(previewView.surfaceProvider)
          }
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
            cameraProviderFuture?.get()?.unbindAll()
            cameraProviderFuture?.get()?.bindToLifecycle(lifecycleOwner, cameraSelector, preview, imageAnalysis)
          } catch (e: Exception) {
            Log.d(TAG, "CameraPreview: ${e.localizedMessage}")
          }
        }, ContextCompat.getMainExecutor(context))
      }
    } else {
      val buttonColors = ButtonDefaults.buttonColors(
        backgroundColor = MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.9f),
        contentColor = MaterialTheme.colors.primary,
        disabledBackgroundColor = MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.9f),
        disabledContentColor = MaterialTheme.colors.primary,
      )
      var permissionRequested by rememberSaveable { mutableStateOf(false) }
      when {
        cameraPermissionState.status is PermissionStatus.Denied && !permissionRequested && showScanner.value -> {
          LaunchedEffect(Unit) {
            permissionRequested = true
            cameraPermissionState.launchPermissionRequest()
          }
        }
        cameraPermissionState.status is PermissionStatus.Denied -> {
          Button({ cameraPermissionState.launchPermissionRequest() }, modifier = modifier, colors = buttonColors) {
            Icon(painterResource(MR.images.ic_camera_enhance), null)
            Spacer(Modifier.width(DEFAULT_PADDING_HALF))
            Text(stringResource(MR.strings.enable_camera_access))
          }
        }
        cameraPermissionState.status == PermissionStatus.Granted -> {
          Button({ showQRCodeScanner.value = true }, modifier = modifier, colors = buttonColors) {
            Icon(painterResource(MR.images.ic_qr_code), null)
            Spacer(Modifier.width(DEFAULT_PADDING_HALF))
            Text(stringResource(MR.strings.tap_to_scan))
          }
        }
        !LocalContext.current.packageManager.hasSystemFeature(PackageManager.FEATURE_CAMERA_ANY) -> {
          Button({  }, enabled = false, modifier = modifier, colors = buttonColors) {
            Text(stringResource(MR.strings.camera_not_available))
          }
        }
      }
    }
  }
}

@SuppressLint("UnsafeOptInUsageError")
private fun imageProxyToGrayU8(img: ImageProxy) : GrayU8? {
  val image = img.image
  if (image != null) {
    val outImg = GrayU8()
    ConvertCameraImage.imageToBoof(image, ColorFormat.GRAY, outImg, null)
    return outImg
  }
  return null
}
