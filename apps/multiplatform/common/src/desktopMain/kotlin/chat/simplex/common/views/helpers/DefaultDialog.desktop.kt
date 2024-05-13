package chat.simplex.common.views.helpers

import androidx.compose.foundation.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.input.key.*
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.*
import chat.simplex.common.DialogParams
import chat.simplex.common.platform.desktopPlatform
import chat.simplex.res.MR
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import java.awt.FileDialog
import java.io.File
import javax.swing.JFileChooser
import javax.swing.filechooser.FileFilter
import javax.swing.filechooser.FileNameExtensionFilter

@Composable
actual fun DefaultDialog(
  onDismissRequest: () -> Unit,
  content: @Composable () -> Unit
) {
  Dialog(
    undecorated = true,
    transparent = true,
    resizable = false,
    title = "",
    onCloseRequest = onDismissRequest,
    onPreviewKeyEvent = { event ->
      if (event.key == Key.Escape && event.type == KeyEventType.KeyUp) {
        onDismissRequest(); true
      } else false
    }
  ) {
    Surface(
      Modifier
        .border(border = BorderStroke(1.dp, MaterialTheme.colors.secondary.copy(alpha = 0.3F)), shape = RoundedCornerShape(8)),
      contentColor = LocalContentColor.current
    ) {
      content()
    }
  }
}

@Composable
fun FrameWindowScope.FileDialogChooser(
  title: String,
  isLoad: Boolean,
  params: DialogParams,
  onResult: (result: List<File>) -> Unit
) {
  if (desktopPlatform.isLinux() || desktopPlatform.isWindows()) {
    FileDialogChooserMultiple(title, isLoad, params.filename, params.allowMultiple, params.fileFilter, params.fileFilterDescription, onResult)
  } else {
    FileDialogAwt(title, isLoad, params.filename, params.allowMultiple, params.fileFilter, onResult)
  }
}

@Composable
fun FrameWindowScope.FileDialogChooserMultiple(
  title: String,
  isLoad: Boolean,
  filename: String?,
  allowMultiple: Boolean,
  fileFilter: ((File?) -> Boolean)? = null,
  fileFilterDescription: String? = null,
  onResult: (result: List<File>) -> Unit
) {
  val scope = rememberCoroutineScope()
  DisposableEffect(Unit) {
    val job = scope.launch(Dispatchers.Main) {
      val fileChooser = JFileChooser()
      fileChooser.dialogTitle = title
      fileChooser.isMultiSelectionEnabled = allowMultiple && isLoad
      fileChooser.isAcceptAllFileFilterUsed = fileFilter == null
      if (fileFilter != null && fileFilterDescription != null) {
        fileChooser.addChoosableFileFilter(object: FileFilter() {
          override fun accept(file: File?): Boolean = fileFilter(file)

          override fun getDescription(): String = fileFilterDescription
        })
      }
      val returned = if (isLoad) {
        fileChooser.showOpenDialog(window)
      } else {
        if (filename != null) {
          fileChooser.selectedFile = File(filename)
        } else {
          fileChooser.fileSelectionMode = JFileChooser.DIRECTORIES_ONLY
        }
        fileChooser.showSaveDialog(window)
      }
      val result = when (returned) {
        JFileChooser.APPROVE_OPTION -> {
          if (isLoad) {
            when {
              allowMultiple -> fileChooser.selectedFiles.filter { it.canRead() }
              fileChooser.selectedFile != null && fileChooser.selectedFile.canRead() -> listOf(fileChooser.selectedFile)
              else -> emptyList()
            }
          } else {
            if (!fileChooser.fileFilter.accept(fileChooser.selectedFile)) {
              val ext = (fileChooser.fileFilter as FileNameExtensionFilter).extensions[0]
              fileChooser.selectedFile = File(fileChooser.selectedFile.absolutePath + ".$ext")
            }
            listOf(fileChooser.selectedFile)
          }
        }
        else -> emptyList()
      }
      onResult(result)
    }
    onDispose {
      job.cancel()
    }
  }
}

/*
* Has graphic glitches on many Linux distributions, so use only on non-Linux systems. Also file filter doesn't work on Windows
* */
@Composable
private fun FrameWindowScope.FileDialogAwt(
  title: String,
  isLoad: Boolean,
  filename: String?,
  allowMultiple: Boolean,
  fileFilter: ((File?) -> Boolean)? = null,
  onResult: (result: List<File>) -> Unit
) = AwtWindow(
  create = {
    object: FileDialog(window, generalGetString(MR.strings.choose_file_title), if (isLoad) LOAD else SAVE) {
      override fun setVisible(value: Boolean) {
        super.setVisible(value)
        if (value) {
          if (files != null) {
            onResult(files.toList())
          } else {
            onResult(emptyList())
          }
        }
      }
    }.apply {
      this.title = title
      this.isMultipleMode = allowMultiple && isLoad
      if (!isLoad && filename != null) {
        this.file = filename
      }
      if (fileFilter != null) {
        this.setFilenameFilter { dir, file ->
          fileFilter(File(dir.absolutePath + File.separator + file))
        }
      }
    }
  },
  dispose = FileDialog::dispose
)
