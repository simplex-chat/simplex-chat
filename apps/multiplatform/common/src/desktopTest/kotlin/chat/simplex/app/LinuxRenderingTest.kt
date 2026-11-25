package chat.simplex.app

import chat.simplex.common.platform.isSoftwareRenderer
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class LinuxRenderingTest {
  @Test
  fun mesaSoftwareRenderersAreSoftware() {
    assertTrue(isSoftwareRenderer("llvmpipe (LLVM 15.0.7, 256 bits)"))
    assertTrue(isSoftwareRenderer("softpipe"))
    assertTrue(isSoftwareRenderer("Software Rasterizer (swrast)"))
  }

  @Test
  fun gpuRenderersAreNotSoftware() {
    assertFalse(isSoftwareRenderer("Mesa Intel(R) UHD Graphics 620 (KBL GT2)"))
    assertFalse(isSoftwareRenderer("AMD Radeon RX 6600 (radeonsi, navi23, LLVM 15.0.7, DRM 3.49)"), "radeonsi names LLVM but renders on the GPU")
    assertFalse(isSoftwareRenderer("NVIDIA GeForce RTX 3060/PCIe/SSE2"))
  }
}
