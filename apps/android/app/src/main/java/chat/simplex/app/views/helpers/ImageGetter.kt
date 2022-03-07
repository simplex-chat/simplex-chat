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
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.core.content.ContextCompat
import kotlinx.coroutines.launch
import java.io.ByteArrayOutputStream

// Inspired by https://github.com/MakeItEasyDev/Jetpack-Compose-Capture-Image-Or-Choose-from-Gallery

fun bitmapToBase64(bitmap: Bitmap): String {
  val stream = ByteArrayOutputStream()
  bitmap.compress(Bitmap.CompressFormat.PNG, 100, stream)
  return Base64.encodeToString(stream.toByteArray(), Base64.DEFAULT)
}


@Composable
fun Base64ImageGetter(base64ImageString: MutableState<String?>) {
  val isCameraSelected = remember { mutableStateOf<Boolean> (false) }
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
      ) {
        Column(
          verticalArrangement = Arrangement.SpaceEvenly,
          horizontalAlignment = Alignment.CenterHorizontally
        ) {
          Text(
            text = "Update Profile Image",
            modifier = Modifier
              .fillMaxWidth()
              .padding(15.dp),
            color = MaterialTheme.colors.primary,
            fontSize = 20.sp,
            fontWeight = FontWeight.Bold,
          )
          Divider(
            modifier = Modifier
              .height(1.dp)
          )
          Text(
            text = "Camera",
            modifier = Modifier
              .fillMaxWidth()
              .clickable {
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
              .padding(15.dp),
            fontSize = 18.sp,
          )
          Divider(
            modifier = Modifier
              .height(0.5.dp)
              .fillMaxWidth()
          )
          Text(
            text = "Choose from Gallery",
            modifier = Modifier
              .fillMaxWidth()
              .clickable {
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
              .padding(15.dp),
            fontSize = 18.sp,
          )
          Divider(
            modifier = Modifier
              .height(0.5.dp)
              .fillMaxWidth()
          )
          Text(
            text = "Cancel",
            modifier = Modifier
              .fillMaxWidth()
              .clickable {
                coroutineScope.launch {
                  bottomSheetModalState.hide()
                }
              }
              .padding(15.dp),
            fontSize = 18.sp,
          )
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
          text = "Take Picture",
          modifier = Modifier.padding(8.dp),
          textAlign = TextAlign.Center,
        )
      }
    }
  }
}
