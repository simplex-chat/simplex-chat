package chat.simplex.app

import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path

private const val TEMP_DIR_PREFIX = "simplex-desktop-test"

internal fun withTempDir(parent: Path? = null, block: (Path) -> Unit) {
  val tmp = if (parent == null) Files.createTempDirectory(TEMP_DIR_PREFIX) else Files.createTempDirectory(parent, TEMP_DIR_PREFIX)
  try {
    block(tmp)
  } finally {
    Files.walk(tmp).use { paths ->
      paths.sorted(Comparator.reverseOrder()).forEach {
        try { Files.delete(it) } catch (_: IOException) {}
      }
    }
  }
}
