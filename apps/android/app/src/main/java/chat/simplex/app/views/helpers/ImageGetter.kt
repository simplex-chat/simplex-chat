package chat.simplex.app.views.helpers

import android.Manifest
import android.content.pm.PackageManager
import android.graphics.*
import android.net.Uri
import android.util.Base64
import android.widget.Toast
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.activity.result.launch
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
import chat.simplex.app.views.newchat.ActionButton
import kotlinx.coroutines.launch
import java.io.ByteArrayOutputStream
import java.lang.Integer.min

// Inspired by https://github.com/MakeItEasyDev/Jetpack-Compose-Capture-Image-Or-Choose-from-Gallery

fun bitmapToBase64(bitmap: Bitmap, squareCrop: Boolean = false): String {
  val stream = ByteArrayOutputStream()
  var height: Int
  var width: Int
  var xOffset: Int
  var yOffset: Int
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
  val resizedImage = Bitmap.createScaledBitmap(bitmap, width, height, false)
  val croppedImage = Bitmap.createBitmap(resizedImage, xOffset, yOffset, min(width, height), min(width, height))
  croppedImage.compress(Bitmap.CompressFormat.JPEG, 75, stream)
  return "data:image/jpg;base64," + Base64.encodeToString(stream.toByteArray(), Base64.NO_WRAP)
}

fun base64ToBitmap(base64ImageString: String) : Bitmap {
  val imageString = base64ImageString
    .removePrefix("data:image/png;base64,")
    .removePrefix("data:image/jpg;base64,")
  val imageBytes = Base64.decode(imageString, Base64.NO_WRAP)
  return BitmapFactory.decodeByteArray(imageBytes, 0, imageBytes.size)
}

@Composable
fun GetImageOptions(
  bottomSheetModalState: ModalBottomSheetState,
  profileImageStr: MutableState<String?>,
  updateProfileImage: (String) -> Unit
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
      withApi {
        updateProfileImage(base64Image)
      }
    }
  }

  val cameraLauncher = rememberLauncherForActivityResult(
    contract = ActivityResultContracts.TakePicturePreview()
  ) { bitmap: Bitmap? ->
    if (bitmap != null) {
      val base64Image = bitmapToBase64(bitmap)
      profileImageStr.value = base64Image
      withApi {
        updateProfileImage(base64Image)
      }
    }
  }

  val permissionLauncher = rememberLauncherForActivityResult(
    contract = ActivityResultContracts.RequestPermission()
  ) { isGranted: Boolean ->
    if (isGranted) {
      if (isCameraSelected.value) {
        cameraLauncher.launch()
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
        .padding(horizontal = 8.dp, vertical = 10.dp),
      horizontalArrangement = Arrangement.SpaceEvenly
    ) {

      ActionButton(null, "Use Camera", icon = Icons.Outlined.PhotoCamera) {
        when (PackageManager.PERMISSION_GRANTED) {
          ContextCompat.checkSelfPermission(
            context, Manifest.permission.CAMERA
          ) -> {
            cameraLauncher.launch()
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
