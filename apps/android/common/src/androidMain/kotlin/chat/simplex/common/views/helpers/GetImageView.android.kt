package chat.simplex.common.views.helpers

import android.Manifest
import android.app.Activity
import android.content.*
import android.content.Intent.FLAG_ACTIVITY_NEW_TASK
import android.content.pm.PackageManager
import android.graphics.*
import android.net.Uri
import android.provider.MediaStore
import android.util.Base64
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
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.platform.LocalContext
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import androidx.core.content.ContextCompat
import androidx.core.content.FileProvider
import chat.simplex.common.helpers.APPLICATION_ID
import com.icerockdev.library.MR
import chat.simplex.common.model.json
import chat.simplex.common.platform.*
import chat.simplex.common.views.newchat.ActionButton
import kotlinx.serialization.builtins.*
import java.io.File
import java.net.URI

val errorBitmapBytes = Base64.decode("iVBORw0KGgoAAAANSUhEUgAAAEAAAABACAYAAACqaXHeAAAAAXNSR0IArs4c6QAAAKVJREFUeF7t1kENACEUQ0FQhnVQ9lfGO+xggITQdvbMzArPey+8fa3tAfwAEdABZQspQStgBssEcgAIkSAJkiAJljtEgiRIgmUCSZAESZAESZAEyx0iQRIkwTKBJEiCv5fgvTd1wDmn7QAP4AeIgA4oW0gJWgEzWCZwbQ7gAA7ggLKFOIADOKBMIAeAEAmSIAmSYLlDJEiCJFgmkARJkARJ8N8S/ADTZUewBvnTOQAAAABJRU5ErkJggg==", Base64.NO_WRAP)
val errorBitmap: Bitmap = BitmapFactory.decodeByteArray(errorBitmapBytes, 0, errorBitmapBytes.size)

class CustomTakePicturePreview(var uri: Uri?, var tmpFile: File?): ActivityResultContract<Void?, Uri?>() {
  @CallSuper
  override fun createIntent(context: Context, input: Void?): Intent {
    tmpFile = File.createTempFile("image", ".bmp", File(getAppFilesDirectory()))
    // Since the class should return Uri, the file should be deleted somewhere else. And in order to be sure, delegate this to system
    tmpFile?.deleteOnExit()
    uri = FileProvider.getUriForFile(context, "$APPLICATION_ID.provider", tmpFile!!)
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
      androidAppContext.startActivity(Intent(MediaStore.INTENT_ACTION_STILL_IMAGE_CAMERA).also { it.addFlags(FLAG_ACTIVITY_NEW_TASK) })
    } catch (e: ActivityNotFoundException) {
      // No camera apps available at all
      Log.e(TAG, "Camera launcher2: " + e.stackTraceToString())
    }
  }
}

@Composable
actual fun GetImageBottomSheet(
  imageBitmap: MutableState<URI?>,
  onImageChange: (ImageBitmap) -> Unit,
  hideBottomSheet: () -> Unit
) {
  val context = LocalContext.current
  val processPickedImage = { uri: Uri? ->
    if (uri != null) {
      val uri = URI(uri.toString())
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
      showToast(generalGetString(MR.strings.toast_permission_denied))
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
      ActionButton(null, stringResource(MR.strings.use_camera_button), icon = painterResource(MR.images.ic_photo_camera)) {
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
      ActionButton(null, stringResource(MR.strings.from_gallery_button), icon = painterResource(MR.images.ic_image)) {
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

class PickFromGallery: ActivityResultContract<Int, Uri?>() {
  override fun createIntent(context: Context, input: Int) =
    Intent(Intent.ACTION_PICK, MediaStore.Images.Media.INTERNAL_CONTENT_URI).apply {
      type = "image/*"
    }

  override fun parseResult(resultCode: Int, intent: Intent?): Uri? = intent?.data
}

class PickMultipleImagesFromGallery: ActivityResultContract<Int, List<Uri>>() {
  override fun createIntent(context: Context, input: Int) =
    Intent(Intent.ACTION_PICK, MediaStore.Images.Media.INTERNAL_CONTENT_URI).apply {
      putExtra(Intent.EXTRA_ALLOW_MULTIPLE, true)
      type = "image/*"
    }

  override fun parseResult(resultCode: Int, intent: Intent?): List<Uri> =
    if (intent?.data != null)
      listOf(intent.data!!)
    else if (intent?.clipData != null)
      with(intent.clipData!!) {
        val uris = ArrayList<Uri>()
        for (i in 0 until kotlin.math.min(itemCount, 10)) {
          val uri = getItemAt(i).uri
          if (uri != null) uris.add(uri)
        }
        if (itemCount > 10) {
          AlertManager.shared.showAlertMsg(MR.strings.images_limit_title, MR.strings.images_limit_desc)
        }
        uris
      }
    else
      emptyList()
}


class PickMultipleVideosFromGallery: ActivityResultContract<Int, List<Uri>>() {
  override fun createIntent(context: Context, input: Int) =
    Intent(Intent.ACTION_PICK, MediaStore.Video.Media.INTERNAL_CONTENT_URI).apply {
      putExtra(Intent.EXTRA_ALLOW_MULTIPLE, true)
      type = "video/*"
    }

  override fun parseResult(resultCode: Int, intent: Intent?): List<Uri> =
    if (intent?.data != null)
      listOf(intent.data!!)
    else if (intent?.clipData != null)
      with(intent.clipData!!) {
        val uris = ArrayList<Uri>()
        for (i in 0 until kotlin.math.min(itemCount, 10)) {
          val uri = getItemAt(i).uri
          if (uri != null) uris.add(uri)
        }
        if (itemCount > 10) {
          AlertManager.shared.showAlertMsg(MR.strings.videos_limit_title, MR.strings.videos_limit_desc)
        }
        uris
      }
    else
      emptyList()
}