package chat.simplex.app.views.helpers

import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.net.Uri
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.runtime.*
import androidx.compose.ui.platform.LocalContext

@Composable
fun ImageGetter(fromGallery: Boolean) {
  val result = remember { mutableStateOf<Bitmap?>(null)}
  if (fromGallery) getImageFromGallery(result)
  else getImageFromCamera(result)
}


@Composable
fun getImageFromGallery(result: MutableState<Bitmap?>) {
  val context = LocalContext.current
  val uri = remember { mutableStateOf<Uri?>(null) }
  // TODO set up downsampling/scaling to avoid too large an image.
  val bitmapOptions = BitmapFactory.Options()
  bitmapOptions.outMimeType = "jpg"
  val launcher = rememberLauncherForActivityResult(ActivityResultContracts.GetContent()) {
    uri.value = it
    if (uri.value != null) {
      val inputStream = context.contentResolver.openInputStream(uri.value!!)
      result.value = BitmapFactory.decodeStream(inputStream, null, bitmapOptions)
    }
  }
  @Composable
  fun LaunchGallery() {
    SideEffect {
      launcher.launch("image/*")
    }
  }
  return LaunchGallery()
}

@Composable
fun getImageFromCamera(result: MutableState<Bitmap?>) {
  val launcher = rememberLauncherForActivityResult(ActivityResultContracts.TakePicturePreview()) {
    result.value = it
  }
  @Composable
  fun LaunchCamera() {
    SideEffect {
      launcher.launch(null)
    }
  }
  return LaunchCamera()
}