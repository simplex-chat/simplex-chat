package chat.simplex.app.views.helpers

import android.Manifest
import android.content.pm.PackageManager
import android.graphics.Bitmap
import android.graphics.ImageDecoder
import android.net.Uri
import android.util.Base64
import android.widget.Toast
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.activity.result.launch
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Delete
import androidx.compose.material.icons.outlined.Collections
import androidx.compose.material.icons.outlined.PhotoCamera
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.onFocusChanged
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.core.content.ContextCompat
import chat.simplex.app.views.newchat.ActionButton
import kotlinx.coroutines.launch
import java.io.ByteArrayOutputStream

// Inspired by https://github.com/MakeItEasyDev/Jetpack-Compose-Capture-Image-Or-Choose-from-Gallery

fun bitmapToBase64(bitmap: Bitmap): String {
  val stream = ByteArrayOutputStream()
  bitmap.compress(Bitmap.CompressFormat.JPEG, 25, stream)
  return Base64.encodeToString(stream.toByteArray(), Base64.DEFAULT)
}


@Composable
fun Base64ImageGetter(base64ImageString: MutableState<String?>) {
  val isCameraSelected = remember { mutableStateOf (false) }
  val context = LocalContext.current
  val bottomSheetModalState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden)
  val coroutineScope = rememberCoroutineScope()



  val galleryLauncher = rememberLauncherForActivityResult(
    contract = ActivityResultContracts.GetContent()
  ) { uri: Uri? ->
    if (uri != null) {
      val source = ImageDecoder.createSource(context.contentResolver, uri)
      val bitmap = ImageDecoder.decodeBitmap(source)
      base64ImageString.value = bitmapToBase64(bitmap)
    }
  }

  val cameraLauncher = rememberLauncherForActivityResult(
    contract = ActivityResultContracts.TakePicturePreview()
  ) { bitmap: Bitmap? ->
    if (bitmap != null) base64ImageString.value = bitmapToBase64(bitmap)
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

  ModalBottomSheetLayout(
    sheetContent = {
      Box(
        modifier = Modifier
          .fillMaxWidth()
          .wrapContentHeight()
          .onFocusChanged { coroutineScope.launch { bottomSheetModalState.hide() } }
      ) {
//          Row(Modifier
//            .fillMaxWidth().padding(horizontal = 8.dp, vertical = 6.dp).align(Alignment.End)
//          ) {
//            Icon(Icons.Outlined.Close,
//              "close",
//            modifier = Modifier
//              .height(20.dp)
//              .clickable {
//                coroutineScope.launch {
//                  bottomSheetModalState.hide()
//                }
//              }
//          )
//          }
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
            ActionButton(null, "Delete Image", icon = Icons.Filled.Delete) {
              // Todo delete image
              base64ImageString.value = null
            }
          }
        }
    },
    sheetState = bottomSheetModalState,
  ) {
    Box(
      contentAlignment = Alignment.Center,
      modifier = Modifier.fillMaxSize()
    ) {
      Button(
        onClick = {
          coroutineScope.launch {
            if (!bottomSheetModalState.isVisible) {
              bottomSheetModalState.show()
            } else {
              bottomSheetModalState.hide()
            }
          }
        },
        modifier = Modifier
          .padding(16.dp)
          .fillMaxWidth(),
      ) {
        Text(
          text = "Update Profile Image",
          modifier = Modifier.padding(8.dp),
          textAlign = TextAlign.Center,
        )
      }
    }
  }
}
