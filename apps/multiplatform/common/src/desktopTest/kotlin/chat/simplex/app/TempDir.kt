package chat.simplex.app

import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path

internal fun withTempDir(block: (Path) -> Unit) {
  val tmp = Files.createTempDirectory("simplex-desktop-test")
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
