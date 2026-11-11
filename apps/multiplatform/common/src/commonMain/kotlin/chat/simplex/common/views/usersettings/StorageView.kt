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
import chat.simplex.res.*
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import java.io.IOException
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes

internal class EntryUsage(val name: String, val bytes: Long, val dir: Path?)

internal class RootUsage(val dir: Path, val entries: List<EntryUsage>) {
  val bytes: Long = entries.sumOf { it.bytes }
}

@Composable
fun ModalData.StorageView(dirs: List<Path> = storageRoots()) {
  var usage by remember { stateGetOrPutNullable<List<RootUsage>>("usage") { null } }
  LaunchedEffect(Unit) {
    if (usage == null) usage = withContext(Dispatchers.IO) { dirs.map(::rootUsage) }
  }
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.storage))
    val roots = usage
    if (roots == null) {
      DefaultProgressView(null)
    } else {
      roots.forEachIndexed { i, root ->
        if (i > 0) SectionDividerSpaced()
        SectionView(root.dir.toString()) {
          UsageRow(stringResource(MR.strings.storage_total), root.bytes)
          root.entries.forEach { entry ->
            val click = entry.dir?.let { dir -> { ModalManager.start.showModal(cardScreen = true) { StorageView(listOf(dir)) } } }
            UsageRow(entry.name, entry.bytes, click)
          }
        }
      }
    }
    SectionBottomSpacer()
  }
}

@Composable
private fun UsageRow(name: String, bytes: Long, click: (() -> Unit)? = null) {
  SectionItemViewSpaceBetween(click) {
    Text(name, Modifier.weight(1f))
    Text(formatBytes(bytes), Modifier.padding(start = DEFAULT_PADDING), color = MaterialTheme.colors.secondary)
  }
}

private fun storageRoots(): List<Path> {
  val dirs = listOf(dataDir, preferencesDir, tmpDir).map { it.normalize() }.distinct()
  return dirs.filter { dir -> dirs.none { it != dir && dir.startsWith(it) } }.map { it.toPath() }
}

internal fun rootUsage(root: Path, closedDirs: List<Path> = listOf(appFilesDir, tmpDir, remoteHostsDir, databaseExportDir).map { it.toPath() }): RootUsage =
  RootUsage(
    root,
    rootEntries(root)
      .map { path -> EntryUsage(path.fileName.toString(), treeSize(path), path.takeIf { opensAsFolder(path, closedDirs) }) }
      .sortedByDescending { it.bytes }
  )

private fun opensAsFolder(path: Path, closedDirs: List<Path>): Boolean =
  Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS) && closedDirs.none { it.toAbsolutePath().normalize() == path.toAbsolutePath().normalize() }

private fun rootEntries(root: Path): List<Path> =
  try {
    Files.newDirectoryStream(root).use { it.toList() }
  } catch (e: Exception) {
    Log.e(TAG, "StorageView rootEntries: ${e.javaClass.simpleName}")
    emptyList()
  }

private fun treeSize(path: Path): Long {
  var bytes = 0L
  Files.walkFileTree(path, object : SimpleFileVisitor<Path>() {
    override fun visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult {
      bytes += attrs.size()
      return FileVisitResult.CONTINUE
    }

    override fun visitFileFailed(file: Path, exc: IOException): FileVisitResult {
      Log.e(TAG, "StorageView visitFileFailed: ${exc.javaClass.simpleName}")
      return FileVisitResult.CONTINUE
    }

    override fun postVisitDirectory(dir: Path, exc: IOException?): FileVisitResult {
      if (exc != null) Log.e(TAG, "StorageView postVisitDirectory: ${exc.javaClass.simpleName}")
      return FileVisitResult.CONTINUE
    }
  })
  return bytes
}
