package chat.simplex.app.views.chat

import android.Manifest
import android.content.ActivityNotFoundException
import android.content.pm.PackageManager
import android.graphics.Bitmap
import android.net.Uri
import android.widget.Toast
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.compose.runtime.*
import androidx.core.content.ContextCompat
import chat.simplex.app.SimplexApp
import chat.simplex.app.platform.resizeImageToStrSize
import chat.simplex.app.views.helpers.*
import chat.simplex.res.MR

@Composable
fun AttachmentSelection(
  composeState: MutableState<ComposeState>,
  attachmentOption: MutableState<AttachmentOption?>,
  processPickedFile: (Uri?, String?) -> Unit,
  processPickedMedia: (List<Uri>, String?) -> Unit
) {
  val cameraLauncher = rememberCameraLauncher { uri: Uri? ->
    if (uri != null) {
      val bitmap: Bitmap? = getBitmapFromUri(uri)
      if (bitmap != null) {
        val imagePreview = resizeImageToStrSize(bitmap, maxDataSize = 14000)
        composeState.value = composeState.value.copy(preview = ComposePreview.MediaPreview(listOf(imagePreview), listOf(UploadContent.SimpleImage(uri))))
      }
    }
  }
  val cameraPermissionLauncher = rememberPermissionLauncher { isGranted: Boolean ->
    if (isGranted) {
      cameraLauncher.launchWithFallback()
    } else {
      Toast.makeText(SimplexApp.context, generalGetString(MR.strings.toast_permission_denied), Toast.LENGTH_SHORT).show()
    }
  }
  val galleryImageLauncher = rememberLauncherForActivityResult(contract = PickMultipleImagesFromGallery()) { processPickedMedia(it, null) }
  val galleryImageLauncherFallback = rememberGetMultipleContentsLauncher { processPickedMedia(it, null) }
  val galleryVideoLauncher = rememberLauncherForActivityResult(contract = PickMultipleVideosFromGallery()) { processPickedMedia(it, null) }
  val galleryVideoLauncherFallback = rememberGetMultipleContentsLauncher { processPickedMedia(it, null) }

  val filesLauncher = rememberGetContentLauncher { processPickedFile(it, null) }
  LaunchedEffect(attachmentOption.value) {
    when (attachmentOption.value) {
      AttachmentOption.CameraPhoto -> {
        when (PackageManager.PERMISSION_GRANTED) {
          ContextCompat.checkSelfPermission(SimplexApp.context, Manifest.permission.CAMERA) -> {
            cameraLauncher.launchWithFallback()
          }
          else -> {
            cameraPermissionLauncher.launch(Manifest.permission.CAMERA)
          }
        }
        attachmentOption.value = null
      }
      AttachmentOption.GalleryImage -> {
        try {
          galleryImageLauncher.launch(0)
        } catch (e: ActivityNotFoundException) {
          galleryImageLauncherFallback.launch("image/*")
        }
        attachmentOption.value = null
      }
      AttachmentOption.GalleryVideo -> {
        try {
          galleryVideoLauncher.launch(0)
        } catch (e: ActivityNotFoundException) {
          galleryVideoLauncherFallback.launch("video/*")
        }
        attachmentOption.value = null
      }
      AttachmentOption.File -> {
        filesLauncher.launch("*/*")
        attachmentOption.value = null
      }
      else -> {}
    }
  }
}
