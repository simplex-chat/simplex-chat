package chat.simplex.app.ui.harness

import org.junit.Ignore
import org.junit.Test

@Ignore("benchmark of reset, frame and capture costs; run manually with --tests '*HarnessMeasureTest*' after removing this line")
class HarnessMeasureTest : UiTest() {
  @Test fun measureResetsFramesAndCapture() = uiTest {
    val rt = Runtime.getRuntime()
    fun usedMb(): Long { System.gc(); Thread.sleep(200); System.gc(); return (rt.totalMemory() - rt.freeMemory()) / 1_048_576 }
    fun rssMb(): Long = java.io.File("/proc/self/status").readLines().first { it.startsWith("VmRSS:") }.split(Regex("\\s+"))[1].toLong() / 1024
    val resetMs = mutableListOf<Long>()
    for (i in 1..20) {
      val t = System.nanoTime()
      app("alice")
      poll(what = "the chat list") { seesText("Private notes") }
      resetMs += (System.nanoTime() - t) / 1_000_000
      if (i == 1 || i % 5 == 0) println("UITEST reset#$i ms=${resetMs.last()} heapMb=${usedMb()} rssMb=${rssMb()} threads=${Thread.activeCount()}")
    }
    println("UITEST reset ms: first=${resetMs.first()} median=${resetMs.drop(1).sorted()[resetMs.size / 2 - 1]} max=${resetMs.drop(1).max()}")
    val frames = 120
    val t = System.nanoTime()
    repeat(frames) { ui.mainClock.advanceTimeBy(16); ui.waitForIdle() }
    println("UITEST frame ms avg=${(System.nanoTime() - t) / 1_000_000.0 / frames}")
    val caps = (1..10).map { val c = System.nanoTime(); ui.captureToImage(); (System.nanoTime() - c) / 1_000_000 }
    println("UITEST captureToImage ms=${caps}")
  }
}
