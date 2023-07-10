package chat.simplex.common.views.helpers

import androidx.compose.runtime.*
import androidx.compose.ui.input.key.*
import androidx.compose.ui.window.*
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import java.awt.FileDialog
import java.io.File
import java.util.*
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
    title = "",
    onCloseRequest = onDismissRequest,
    onPreviewKeyEvent = { event ->
      if (event.key == Key.Escape && event.type == KeyEventType.KeyUp) {
        onDismissRequest(); true
      } else false
    }
  ) {
    content()
  }
}

@Composable
fun FrameWindowScope.FileDialogChooser(
  title: String,
  isLoad: Boolean,
  extensions: List<FileFilter> = emptyList(),
  onResult: (result: File?) -> Unit
) {
  if (isLinux()) {
    FileDialogChooserMultiple(title, isLoad, extensions) { onResult(it.firstOrNull()) }
  } else {
    FileDialogAwt(title, isLoad, onResult)
  }
}

@Composable
fun FrameWindowScope.FileDialogChooserMultiple(
  title: String,
  isLoad: Boolean,
  extensions: List<FileFilter> = emptyList(),
  onResult: (result: List<File>) -> Unit
) {
  val scope = rememberCoroutineScope()
  DisposableEffect(Unit) {
    val job = scope.launch(Dispatchers.Main) {
      val fileChooser = JFileChooser()
      fileChooser.dialogTitle = title
      fileChooser.isMultiSelectionEnabled = isLoad
      fileChooser.isAcceptAllFileFilterUsed = extensions.isEmpty()
      extensions.forEach { fileChooser.addChoosableFileFilter(it) }
      val returned = if (isLoad) {
        fileChooser.showOpenDialog(window)
      } else {
        fileChooser.fileSelectionMode = JFileChooser.DIRECTORIES_ONLY
        fileChooser.showSaveDialog(window)
      }
      val result = when (returned) {
        JFileChooser.APPROVE_OPTION -> {
          if (isLoad) {
            fileChooser.selectedFiles.filter { it.canRead() }
          } else {
            if (!fileChooser.fileFilter.accept(fileChooser.selectedFile)) {
              val ext = (fileChooser.fileFilter as FileNameExtensionFilter).extensions[0]
              fileChooser.selectedFile = File(fileChooser.selectedFile.absolutePath + ".$ext")
            }
            listOf(fileChooser.selectedFile)
          }
        }
        else -> listOf();
      }
      onResult(result)
    }
    onDispose {
      job.cancel()
    }
  }
}

/*
* Has graphic glitches on many Linux distributions, so use only on non-Linux systems
* */
@Composable
private fun FrameWindowScope.FileDialogAwt(
  title: String,
  isLoad: Boolean,
  onResult: (result: File?) -> Unit
) = AwtWindow(
  create = {
    object: FileDialog(window, "Choose a file", if (isLoad) LOAD else SAVE) {
      override fun setVisible(value: Boolean) {
        super.setVisible(value)
        if (value) {
          if (file != null) {
            onResult(File(directory).resolve(file))
          } else {
            onResult(null)
          }
        }
      }
    }.apply {
      this.title = title
    }
  },
  dispose = FileDialog::dispose
)

fun isLinux(): Boolean = System.getProperty("os.name", "generic").lowercase(Locale.ENGLISH) == "linux"
