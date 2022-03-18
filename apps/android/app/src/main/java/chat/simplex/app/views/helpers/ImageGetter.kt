package chat.simplex.app.views.helpers

import android.Manifest
import android.app.Activity
import android.content.Context
import android.content.Intent
import android.content.pm.PackageManager
import android.graphics.*
import android.net.Uri
import android.provider.MediaStore
import android.util.Base64
import android.util.Log
import android.widget.Toast
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContract
import androidx.activity.result.contract.ActivityResultContracts
import androidx.annotation.CallSuper
import androidx.compose.foundation.layout.*
import androidx.compose.material.ModalBottomSheetState
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Collections
import androidx.compose.material.icons.outlined.PhotoCamera
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.onFocusChanged
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.unit.dp
import androidx.core.content.ContextCompat
import androidx.core.content.FileProvider
import chat.simplex.app.BuildConfig
import chat.simplex.app.TAG
import chat.simplex.app.views.newchat.ActionButton
import kotlinx.coroutines.launch
import java.io.ByteArrayOutputStream
import java.io.File
import java.lang.Integer.min

// Inspired by https://github.com/MakeItEasyDev/Jetpack-Compose-Capture-Image-Or-Choose-from-Gallery

fun bitmapToBase64(bitmap: Bitmap, squareCrop: Boolean = true): String {
  val stream = ByteArrayOutputStream()
  val height: Int
  val width: Int
  val xOffset: Int
  val yOffset: Int
  val baseScale = 96
  if (bitmap.height > bitmap.width) {
    height = baseScale
    width = height * bitmap.width / bitmap.height
    xOffset = 0
    yOffset = (height - width) / 2
  }
  else {
    width = baseScale
    height = width * bitmap.height / bitmap.width
    xOffset = (width - height) / 2
    yOffset = 0
  }
  var processedImage = Bitmap.createScaledBitmap(bitmap, width, height, false)
  val side = min(width, height)
  if (squareCrop) {
    processedImage = Bitmap.createBitmap(
      processedImage, xOffset, yOffset, side, side
    )
  }
  processedImage.compress(Bitmap.CompressFormat.JPEG, 75, stream)
  return "data:image/jpg;base64," + Base64.encodeToString(stream.toByteArray(), Base64.NO_WRAP)
}

fun base64ToBitmap(base64ImageString: String) : Bitmap {
  val imageString = base64ImageString
    .removePrefix("data:image/png;base64,")
    .removePrefix("data:image/jpg;base64,")
  val imageBytes = Base64.decode(imageString, Base64.NO_WRAP)
  return BitmapFactory.decodeByteArray(imageBytes, 0, imageBytes.size)
}

class CustomTakePicturePreview : ActivityResultContract<Void?, Bitmap?>() {
  private var uri: Uri? = null
  private var tmpFile: File? = null
  lateinit var externalContext: Context

  @CallSuper
  override fun createIntent(context: Context, input: Void?): Intent {
    externalContext = context
    tmpFile = File.createTempFile("image", ".bmp", context.filesDir)
    uri = FileProvider.getUriForFile(context, "${BuildConfig.APPLICATION_ID}.provider", tmpFile!!)
    return Intent(MediaStore.ACTION_IMAGE_CAPTURE)
      .putExtra(MediaStore.EXTRA_OUTPUT, uri)
  }

  override fun getSynchronousResult(
    context: Context,
    input: Void?
  ): SynchronousResult<Bitmap?>? = null

  override fun parseResult(resultCode: Int, intent: Intent?): Bitmap? {
    return if (resultCode == Activity.RESULT_OK && uri != null) {
      val source = ImageDecoder.createSource(externalContext.contentResolver, uri!!)
      val bitmap = ImageDecoder.decodeBitmap(source)
      tmpFile?.delete()
      bitmap
    } else {
      Log.e( TAG, "Getting image from camera cancelled or failed.")
      tmpFile?.delete()
      null
    }
  }
}

@Composable
fun GetImageOptions(
  bottomSheetModalState: ModalBottomSheetState,
  profileImageStr: MutableState<String?>,
) {
  val context = LocalContext.current
  val isCameraSelected = remember { mutableStateOf (false) }
  val coroutineScope = rememberCoroutineScope()
  val galleryLauncher = rememberLauncherForActivityResult(
    contract = ActivityResultContracts.GetContent()
  ) { uri: Uri? ->
    if (uri != null) {
      val source = ImageDecoder.createSource(context.contentResolver, uri)
      val bitmap = ImageDecoder.decodeBitmap(source)
      val base64Image = bitmapToBase64(bitmap)
      profileImageStr.value = base64Image
    }
  }

  val cameraLauncher = rememberLauncherForActivityResult(
    contract = CustomTakePicturePreview()
  ) { bitmap: Bitmap? ->
    if (bitmap != null) {
      val base64Image = bitmapToBase64(bitmap)
      profileImageStr.value = base64Image
    }
  }

  val permissionLauncher = rememberLauncherForActivityResult(
    contract = ActivityResultContracts.RequestPermission()
  ) { isGranted: Boolean ->
    if (isGranted) {
      if (isCameraSelected.value) {
        cameraLauncher.launch(null)
      } else {
        galleryLauncher.launch("image/*")
      }
      coroutineScope.launch {
        bottomSheetModalState.hide()
      }
    } else {
      Toast.makeText(context, "Permission Denied!", Toast.LENGTH_SHORT).show()
    }
  }


  Box(
    modifier = Modifier
      .fillMaxWidth()
      .wrapContentHeight()
      .onFocusChanged { focusState ->
        if (!focusState.hasFocus) {
          coroutineScope.launch { bottomSheetModalState.hide() }
        }
      }
  ) {
    Row(
      Modifier
        .fillMaxWidth()
        .padding(horizontal = 8.dp, vertical = 30.dp),
      horizontalArrangement = Arrangement.SpaceEvenly
    ) {

      ActionButton(null, "Use Camera", icon = Icons.Outlined.PhotoCamera) {
        when (PackageManager.PERMISSION_GRANTED) {
          ContextCompat.checkSelfPermission(
            context, Manifest.permission.CAMERA
          ) -> {
            cameraLauncher.launch(null)
            coroutineScope.launch {
              bottomSheetModalState.hide()
            }
          }
          else -> {
            isCameraSelected.value = true
            permissionLauncher.launch(Manifest.permission.CAMERA)
          }
        }
      }
      ActionButton(null, "From Gallery", icon = Icons.Outlined.Collections) {
        when (PackageManager.PERMISSION_GRANTED) {
          ContextCompat.checkSelfPermission(
            context, Manifest.permission.READ_EXTERNAL_STORAGE
          ) -> {
            galleryLauncher.launch("image/*")
            coroutineScope.launch {
              bottomSheetModalState.hide()
            }
          }
          else -> {
            isCameraSelected.value = false
            permissionLauncher.launch(Manifest.permission.READ_EXTERNAL_STORAGE)
          }
        }
      }
//      ActionButton(null, "Delete Image", icon = Icons.Filled.Delete) {
//        profileImageStr.value = null
//      }
    }
  }
}
