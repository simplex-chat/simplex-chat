package chat.simplex.common.views.chat

import android.Manifest
import android.content.ActivityNotFoundException
import android.content.pm.PackageManager
import android.net.Uri
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.compose.runtime.*
import androidx.compose.ui.graphics.ImageBitmap
import androidx.core.content.ContextCompat
import chat.simplex.common.helpers.toURI
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.*
import com.icerockdev.library.MR
import java.net.URI

@Composable
actual fun AttachmentSelection(
  composeState: MutableState<ComposeState>,
  attachmentOption: MutableState<AttachmentOption?>,
  processPickedFile: (URI?, String?) -> Unit,
  processPickedMedia: (List<URI>, String?) -> Unit
) {
  val cameraLauncher = rememberCameraLauncher { uri: Uri? ->
    if (uri != null) {
      val bitmap: ImageBitmap? = getBitmapFromUri(uri.toURI())
      if (bitmap != null) {
        val imagePreview = resizeImageToStrSize(bitmap, maxDataSize = 14000)
        composeState.value = composeState.value.copy(preview = ComposePreview.MediaPreview(listOf(imagePreview), listOf(UploadContent.SimpleImage(uri.toURI()))))
      }
    }
  }
  val cameraPermissionLauncher = rememberPermissionLauncher { isGranted: Boolean ->
    if (isGranted) {
      cameraLauncher.launchWithFallback()
    } else {
      showToast(generalGetString(MR.strings.toast_permission_denied))
    }
  }
  val galleryImageLauncher = rememberLauncherForActivityResult(contract = PickMultipleImagesFromGallery()) { processPickedMedia(it.map { it.toURI() }, null) }
  val galleryImageLauncherFallback = rememberGetMultipleContentsLauncher { processPickedMedia(it.map { it.toURI() }, null) }
  val galleryVideoLauncher = rememberLauncherForActivityResult(contract = PickMultipleVideosFromGallery()) { processPickedMedia(it.map { it.toURI() }, null) }
  val galleryVideoLauncherFallback = rememberGetMultipleContentsLauncher { processPickedMedia(it.map { it.toURI() }, null) }
  val filesLauncher = rememberGetContentLauncher { processPickedFile(it?.toURI(), null) }
  LaunchedEffect(attachmentOption.value) {
    when (attachmentOption.value) {
      AttachmentOption.CameraPhoto -> {
        when (PackageManager.PERMISSION_GRANTED) {
          ContextCompat.checkSelfPermission(androidAppContext, Manifest.permission.CAMERA) -> {
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