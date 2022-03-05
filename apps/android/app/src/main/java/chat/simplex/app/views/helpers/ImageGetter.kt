package chat.simplex.app.views.helpers

import android.Manifest
import android.content.pm.PackageManager
import android.graphics.Bitmap
import android.graphics.ImageDecoder
import android.net.Uri
import android.widget.Toast
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.activity.result.launch
import androidx.compose.foundation.Image
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.core.content.ContextCompat
import kotlinx.coroutines.launch

// Inspired by https://github.com/MakeItEasyDev/Jetpack-Compose-Capture-Image-Or-Choose-from-Gallery


@Composable
fun ImageGetter(bitmap: MutableState<Bitmap?>) {
  val imageUri = remember { mutableStateOf<Uri?>(null) }
  val isCameraSelected = remember { mutableStateOf<Boolean> (false) }
  val context = LocalContext.current
  val bottomSheetModalState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden)
  val coroutineScope = rememberCoroutineScope()



  val galleryLauncher = rememberLauncherForActivityResult(
    contract = ActivityResultContracts.GetContent()
  ) { uri: Uri? ->
    imageUri.value = uri
    bitmap.value = null
  }

  val cameraLauncher = rememberLauncherForActivityResult(
    contract = ActivityResultContracts.TakePicturePreview()
  ) { btm: Bitmap? ->
    bitmap.value = btm
    imageUri.value = null
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
            text = "Add Photo!",
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
            text = "Take Photo",
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
                    isCameraSelected = false
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

  imageUri.value?.let {
        val source = ImageDecoder.createSource(context.contentResolver, it)
        ImageDecoder.decodeBitmap(source)
    }

    bitmap.value?.let { it ->
      Image(
        bitmap = it.asImageBitmap(),
        contentDescription = "Image",
        alignment = Alignment.TopCenter,
        modifier = Modifier
          .fillMaxWidth()
          .fillMaxHeight(0.45f)
          .padding(top = 10.dp),
        contentScale = ContentScale.Fit
      )
    }
  }



//@Composable
//fun ImageGetter(fromGallery: Boolean = false) {
//  var imageUri: Uri? = null
//  var bitmap: Bitmap? = null
//  val result = remember { mutableStateOf<Bitmap?>(null)}
//  if (fromGallery) getImageFromGallery(result)
//  else getImageFromCamera(result)
//}

//@Composable
//fun getImageFromGallery(result: MutableState<Bitmap?>) {
//  val context = LocalContext.current
//  val uri = remember { mutableStateOf<Uri?>(null) }
//  // TODO set up downsampling/scaling to avoid too large an image.
//  val bitmapOptions = BitmapFactory.Options()
//  bitmapOptions.outMimeType = "jpg"
//  val launcher = rememberLauncherForActivityResult(ActivityResultContracts.GetContent()) {
//    uri.value = it
//    if (uri.value != null) {
//      val inputStream = context.contentResolver.openInputStream(uri.value!!)
//      result.value = BitmapFactory.decodeStream(inputStream, null, bitmapOptions)
//    }
//  }
//  @Composable
//  fun LaunchGallery() {
//    SideEffect {
//      launcher.launch("image/*")
//    }
//  }
//  return LaunchGallery()
//}
//
//@Composable
//fun getImageFromCamera(result: MutableState<Bitmap?>) {
//  val launcher = rememberLauncherForActivityResult(ActivityResultContracts.TakePicturePreview()) {
//    result.value = it
//  }
//  @Composable
//  fun LaunchCamera() {
//    SideEffect {
//      launcher.launch(null)
//    }
//  }
//  return LaunchCamera()
//}
