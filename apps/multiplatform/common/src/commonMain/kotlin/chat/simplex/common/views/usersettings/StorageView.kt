package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemViewSpaceBetween
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import java.io.File
import java.io.IOException
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes

private class EntryUsage(val name: String, val bytes: Long)

private class RootUsage(val dir: File, val entries: List<EntryUsage>) {
  val bytes: Long = entries.sumOf { it.bytes }
}

@Composable
fun StorageView() {
  val usage by produceState<List<RootUsage>?>(initialValue = null) {
    value = withContext(Dispatchers.IO) { storageRoots().map(::rootUsage) }
  }
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.storage))
    val roots = usage
    if (roots == null) {
      DefaultProgressView(null)
    } else {
      roots.forEachIndexed { i, root ->
        if (i > 0) SectionDividerSpaced()
        SectionView(root.dir.path) {
          UsageRow(stringResource(MR.strings.storage_total), root.bytes)
          root.entries.forEach { UsageRow(it.name, it.bytes) }
        }
      }
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun UsageRow(name: String, bytes: Long) {
  SectionItemViewSpaceBetween {
    Text(name, Modifier.weight(1f))
    Text(formatBytes(bytes), Modifier.padding(start = DEFAULT_PADDING), color = MaterialTheme.colors.secondary)
  }
}

private fun storageRoots(): List<File> {
  val dirs = listOf(dataDir, preferencesDir, tmpDir).distinct()
  return dirs.filter { dir -> dirs.none { it != dir && dir.startsWith(it) } }
}

private fun rootUsage(root: File): RootUsage =
  RootUsage(
    root,
    root.listFiles().orEmpty()
      .map { EntryUsage(it.name, treeSize(it)) }
      .sortedByDescending { it.bytes }
  )

private fun treeSize(entry: File): Long {
  var bytes = 0L
  val path = try { entry.toPath() } catch (e: InvalidPathException) { Log.e(TAG, "StorageView treeSize: $e"); return 0L }
  Files.walkFileTree(path, object : SimpleFileVisitor<Path>() {
    override fun visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult {
      bytes += attrs.size()
      return FileVisitResult.CONTINUE
    }

    override fun visitFileFailed(file: Path, exc: IOException): FileVisitResult {
      Log.e(TAG, "StorageView visitFileFailed: $exc")
      return FileVisitResult.CONTINUE
    }

    override fun postVisitDirectory(dir: Path, exc: IOException?): FileVisitResult {
      if (exc != null) Log.e(TAG, "StorageView postVisitDirectory: $exc")
      return FileVisitResult.CONTINUE
    }
  })
  return bytes
}
