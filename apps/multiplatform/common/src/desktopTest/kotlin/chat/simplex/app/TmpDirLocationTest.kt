package chat.simplex.app

import chat.simplex.common.platform.dataDir
import chat.simplex.common.platform.tmpDir
import kotlin.test.Test
import kotlin.test.assertTrue

class TmpDirLocationTest {
  @Test
  fun tmpDirIsInsideDataDir() {
    assertTrue(tmpDir.toPath().startsWith(dataDir.toPath()), "tmpDir $tmpDir must be inside dataDir $dataDir")
  }
}
