package chat.simplex.app.views.helpers

import android.Manifest
import android.app.Activity
import android.content.*
import android.content.Intent.FLAG_ACTIVITY_NEW_TASK
import android.content.pm.PackageManager
import android.graphics.*
import android.net.Uri
import android.provider.MediaStore
import android.util.Base64
import android.util.Log
import android.widget.Toast
import androidx.activity.compose.ManagedActivityResultLauncher
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContract
import androidx.activity.result.contract.ActivityResultContracts
import androidx.annotation.CallSuper
import androidx.compose.foundation.layout.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.Saver
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.onFocusChanged
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import androidx.core.content.ContextCompat
import androidx.core.content.FileProvider
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.json
import chat.simplex.app.views.chat.PickFromGallery
import chat.simplex.app.views.newchat.ActionButton
import kotlinx.serialization.builtins.*
import java.io.ByteArrayOutputStream
import java.io.File
import kotlin.math.min
import kotlin.math.sqrt

// Inspired by https://github.com/MakeItEasyDev/Jetpack-Compose-Capture-Image-Or-Choose-from-Gallery
fun cropToSquare(image: Bitmap): Bitmap {
  var xOffset = 0
  var yOffset = 0
  val side = min(image.height, image.width)
  if (image.height < image.width) {
    xOffset = (image.width - side) / 2
  } else {
    yOffset = (image.height - side) / 2
  }
  return Bitmap.createBitmap(image, xOffset, yOffset, side, side)
}

fun resizeImageToStrSize(image: Bitmap, maxDataSize: Long): String {
  var img = image
  var str = compressImageStr(img)
  while (str.length > maxDataSize) {
    val ratio = sqrt(str.length.toDouble() / maxDataSize.toDouble())
    val clippedRatio = min(ratio, 2.0)
    val width = (img.width.toDouble() / clippedRatio).toInt()
    val height = img.height * width / img.width
    img = Bitmap.createScaledBitmap(img, width, height, true)
    str = compressImageStr(img)
  }
  return str
}

private fun compressImageStr(bitmap: Bitmap): String {
  val usePng = bitmap.hasAlpha()
  val ext = if (usePng) "png" else "jpg"
  return "data:image/$ext;base64," + Base64.encodeToString(compressImageData(bitmap, usePng).toByteArray(), Base64.NO_WRAP)
}

fun resizeImageToDataSize(image: Bitmap, usePng: Boolean, maxDataSize: Long): ByteArrayOutputStream {
  var img = image
  var stream = compressImageData(img, usePng)
  while (stream.size() > maxDataSize) {
    val ratio = sqrt(stream.size().toDouble() / maxDataSize.toDouble())
    val clippedRatio = min(ratio, 2.0)
    val width = (img.width.toDouble() / clippedRatio).toInt()
    val height = img.height * width / img.width
    img = Bitmap.createScaledBitmap(img, width, height, true)
    stream = compressImageData(img, usePng)
  }
  return stream
}

private fun compressImageData(bitmap: Bitmap, usePng: Boolean): ByteArrayOutputStream {
  val stream = ByteArrayOutputStream()
  bitmap.compress(if (!usePng) Bitmap.CompressFormat.JPEG else Bitmap.CompressFormat.PNG, 85, stream)
  return stream
}

val errorBitmapBytes = Base64.decode("iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAYAAACqaXHeAAAAAXNSR0IArs4c6QAAAKVJREFUeF7t1kENACEUQ0FQhnVQ9lfGO+xggITQdvbMzArPey+8fa3tAfwAEdABZQspQStgBssEcgAIkSAJkiAJljtEgiRIgmUCSZAESZAESZAEyx0iQRIkwTKBJEiCv5fgvTd1wDmn7QAP4AeIgA4oW0gJWgEzWCZwbQ7gAA7ggLKFOIADOKBMIAeAEAmSIAmSYLlDJEiCJFgmkARJkARJ8N8S/ADTZUewBvnTOQAAAABJRU5ErkJggg==", Base64.NO_WRAP)
val errorBitmap: Bitmap = BitmapFactory.decodeByteArray(errorBitmapBytes, 0, errorBitmapBytes.size)

fun base64ToBitmap(base64ImageString: String): Bitmap {
  val imageString = base64ImageString
    .removePrefix("data:image/png;base64,")
    .removePrefix("data:image/jpg;base64,")
  try {
    val imageBytes = Base64.decode(imageString, Base64.NO_WRAP)
    return BitmapFactory.decodeByteArray(imageBytes, 0, imageBytes.size)
  } catch (e: Exception) {
    Log.e(TAG, "base64ToBitmap error: $e")
    return errorBitmap
  }
}

class CustomTakePicturePreview(var uri: Uri?, var tmpFile: File?): ActivityResultContract<Void?, Uri?>() {
  @CallSuper
  override fun createIntent(context: Context, input: Void?): Intent {
    tmpFile = File.createTempFile("image", ".bmp", File(getAppFilesDirectory(SimplexApp.context)))
    // Since the class should return Uri, the file should be deleted somewhere else. And in order to be sure, delegate this to system
    tmpFile?.deleteOnExit()
    uri = FileProvider.getUriForFile(context, "${BuildConfig.APPLICATION_ID}.provider", tmpFile!!)
    return Intent(MediaStore.ACTION_IMAGE_CAPTURE)
      .putExtra(MediaStore.EXTRA_OUTPUT, uri)
  }

  override fun getSynchronousResult(
    context: Context,
    input: Void?
  ): SynchronousResult<Uri?>? = null

  override fun parseResult(resultCode: Int, intent: Intent?): Uri? {
    return if (resultCode == Activity.RESULT_OK && uri != null) {
      uri
    } else {
      Log.e(TAG, "Getting image from camera cancelled or failed.")
      null
    }
  }

  companion object {
    fun saver(): Saver<CustomTakePicturePreview, *> = Saver(
      save = { json.encodeToString(ListSerializer(String.serializer().nullable), listOf(it.uri?.toString(), it.tmpFile?.toString())) },
      restore = {
        val data = json.decodeFromString(ListSerializer(String.serializer().nullable), it)
        val uri = if (data[0] != null) Uri.parse(data[0]) else null
        val tmpFile = if (data[1] != null) File(data[1]) else null
        CustomTakePicturePreview(uri, tmpFile)
      }
    )
  }
}
//class GetGalleryContent: ActivityResultContracts.GetContent() {
//  override fun createIntent(context: Context, input: String): Intent {
//    super.createIntent(context, input)
//    return Intent(Intent.ACTION_PICK, MediaStore.Images.Media.EXTERNAL_CONTENT_URI)
//  }
//}
//@Composable
//fun rememberGalleryLauncher(cb: (Uri?) -> Unit): ManagedActivityResultLauncher<String, Uri?> =
//  rememberLauncherForActivityResult(contract = GetGalleryContent(), cb)
@Composable
fun rememberCameraLauncher(cb: (Uri?) -> Unit): ManagedActivityResultLauncher<Void?, Uri?> {
  val contract = rememberSaveable(stateSaver = CustomTakePicturePreview.saver()) {
    mutableStateOf(CustomTakePicturePreview(null, null))
  }
  return rememberLauncherForActivityResult(contract = contract.value, cb)
}

@Composable
fun rememberPermissionLauncher(cb: (Boolean) -> Unit): ManagedActivityResultLauncher<String, Boolean> =
  rememberLauncherForActivityResult(contract = ActivityResultContracts.RequestPermission(), cb)

@Composable
fun rememberGetContentLauncher(cb: (Uri?) -> Unit): ManagedActivityResultLauncher<String, Uri?> =
  rememberLauncherForActivityResult(contract = ActivityResultContracts.GetContent(), cb)

@Composable
fun rememberGetMultipleContentsLauncher(cb: (List<Uri>) -> Unit): ManagedActivityResultLauncher<String, List<Uri>> =
  rememberLauncherForActivityResult(contract = GetMultipleContentsAndMimeTypes(), cb)

class GetMultipleContentsAndMimeTypes: ActivityResultContracts.GetMultipleContents() {
  override fun createIntent(context: Context, input: String): Intent {
    val mimeTypes = input.split(";")
    return super.createIntent(context, mimeTypes[0]).apply {
      if (mimeTypes.isNotEmpty()) {
        putExtra(Intent.EXTRA_MIME_TYPES, mimeTypes.toTypedArray())
      }
    }
  }
}

fun ManagedActivityResultLauncher<Void?, Uri?>.launchWithFallback() {
  try {
    launch(null)
  } catch (e: ActivityNotFoundException) {
    // No Activity found to handle Intent android.media.action.IMAGE_CAPTURE
    // Means, no system camera app (Android 11+ requirement)
    // https://developer.android.com/about/versions/11/behavior-changes-11#media-capture
    Log.e(TAG, "Camera launcher: " + e.stackTraceToString())

    try {
      // Try to open any camera just to capture an image, will not be returned like with previous intent
      SimplexApp.context.startActivity(Intent(MediaStore.INTENT_ACTION_STILL_IMAGE_CAMERA).also { it.addFlags(FLAG_ACTIVITY_NEW_TASK) })
    } catch (e: ActivityNotFoundException) {
      // No camera apps available at all
      Log.e(TAG, "Camera launcher2: " + e.stackTraceToString())
    }
  }
}

@Composable
fun GetImageBottomSheet(
  imageBitmap: MutableState<Uri?>,
  onImageChange: (Bitmap) -> Unit,
  hideBottomSheet: () -> Unit
) {
  val context = LocalContext.current
  val processPickedImage = { uri: Uri? ->
    if (uri != null) {
      val bitmap = getBitmapFromUri(uri)
      if (bitmap != null) {
        imageBitmap.value = uri
        onImageChange(bitmap)
      }
    }
  }
  val galleryLauncher = rememberLauncherForActivityResult(contract = PickFromGallery(), processPickedImage)
  val galleryLauncherFallback = rememberGetContentLauncher(processPickedImage)
  val cameraLauncher = rememberCameraLauncher(processPickedImage)
  val permissionLauncher = rememberPermissionLauncher { isGranted: Boolean ->
    if (isGranted) {
      cameraLauncher.launchWithFallback()
      hideBottomSheet()
    } else {
      Toast.makeText(context, generalGetString(R.string.toast_permission_denied), Toast.LENGTH_SHORT).show()
    }
  }

  Box(
    modifier = Modifier
      .fillMaxWidth()
      .wrapContentHeight()
      .onFocusChanged { focusState ->
        if (!focusState.hasFocus) hideBottomSheet()
      }
  ) {
    Row(
      Modifier
        .fillMaxWidth()
        .padding(horizontal = 8.dp, vertical = 30.dp),
      horizontalArrangement = Arrangement.SpaceEvenly
    ) {
      ActionButton(null, stringResource(R.string.use_camera_button), icon = painterResource(R.drawable.ic_photo_camera)) {
        when (PackageManager.PERMISSION_GRANTED) {
          ContextCompat.checkSelfPermission(context, Manifest.permission.CAMERA) -> {
            cameraLauncher.launchWithFallback()
            hideBottomSheet()
          }
          else -> {
            permissionLauncher.launch(Manifest.permission.CAMERA)
          }
        }
      }
      ActionButton(null, stringResource(R.string.from_gallery_button), icon = painterResource(R.drawable.ic_image)) {
        try {
          galleryLauncher.launch(0)
        } catch (e: ActivityNotFoundException) {
          galleryLauncherFallback.launch("image/*")
        }
        hideBottomSheet()
      }
    }
  }
}
