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
import androidx.activity.compose.ManagedActivityResultLauncher
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContract
import androidx.activity.result.contract.ActivityResultContracts
import androidx.annotation.CallSuper
import androidx.compose.foundation.layout.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.onFocusChanged
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import androidx.core.content.ContextCompat
import androidx.core.content.FileProvider
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.views.newchat.ActionButton
import java.io.ByteArrayOutputStream
import java.io.File
import kotlin.math.min
import kotlin.math.sqrt

@Composable
fun ChooseAttachmentView(
  context: Context,
  cameraPermissionLauncher: ManagedActivityResultLauncher<String, Boolean>,
  cameraLauncher: ManagedActivityResultLauncher<Void?, Bitmap?>,
  galleryLauncher: ManagedActivityResultLauncher<String, Uri?>,
  filesLauncher: ManagedActivityResultLauncher<String, Uri?>,
  hide: () -> Unit
) {
  Box(
    modifier = Modifier
      .fillMaxWidth()
      .wrapContentHeight()
      .onFocusChanged { focusState ->
//        if (!focusState.hasFocus) hideBottomSheet()
      }
  ) {
    Row(
      Modifier
        .fillMaxWidth()
        .padding(horizontal = 8.dp, vertical = 30.dp),
      horizontalArrangement = Arrangement.SpaceEvenly
    ) {
      ActionButton(null, stringResource(R.string.use_camera_button), icon = Icons.Outlined.PhotoCamera) {
        when (PackageManager.PERMISSION_GRANTED) {
          ContextCompat.checkSelfPermission(context, Manifest.permission.CAMERA) -> {
            cameraLauncher.launch(null)
          }
          else -> {
            cameraPermissionLauncher.launch(Manifest.permission.CAMERA)
          }
        }
        hide()
      }
      ActionButton(null, stringResource(R.string.from_gallery_button), icon = Icons.Outlined.Collections) {
        galleryLauncher.launch("image/*")
        hide()
      }
      ActionButton(null, stringResource(R.string.choose_file), icon = Icons.Outlined.InsertDriveFile) {
        filesLauncher.launch("*/*")
        hide()
      }
    }
  }
}
