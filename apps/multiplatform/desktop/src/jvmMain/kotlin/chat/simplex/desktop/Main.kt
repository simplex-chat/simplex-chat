package chat.simplex.desktop

import androidx.compose.animation.core.Animatable
import androidx.compose.animation.core.AnimationVector1D
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.*
import androidx.compose.foundation.lazy.LazyListState
import androidx.compose.runtime.*
import androidx.compose.ui.ExperimentalComposeUiApi
import androidx.compose.ui.Modifier
import androidx.compose.ui.input.pointer.*
import chat.simplex.common.platform.*
import chat.simplex.common.platform.DesktopPlatform
import chat.simplex.common.showApp
import chat.simplex.common.views.helpers.*
import kotlinx.coroutines.*
import java.io.File

fun main() {
  initHaskell()
  initApp()
  tmpDir.deleteRecursively()
  tmpDir.mkdir()
  return showApp()
}

@OptIn(ExperimentalComposeUiApi::class)
@Suppress("UnsafeDynamicallyLoadedCode")
private fun initHaskell() {
  val resourcesDir = File(System.getProperty("compose.application.resources.dir"))
  val vlcDir = File(resourcesDir.absolutePath + File.separator + "vlc")
  if (desktopPlatform == DesktopPlatform.WINDOWS_X86_64) {
    windowsLoadRequiredLibs(resourcesDir, vlcDir)
  } else {
    System.load(File(resourcesDir, "libapp-lib.${desktopPlatform.libExtension}").absolutePath)
  }
  // No picture without preloading it, only sound. However, with libs from AppImage it works without preloading
  //val libXcb = "libvlc_xcb_events.so.0.0.0"
  //System.load(File(File(vlcDir, "vlc"), libXcb).absolutePath)
  System.setProperty("jna.library.path", vlcDir.absolutePath)
  //discoverVlcLibs(File(File(vlcDir, "vlc"), "plugins").absolutePath)
  initHS()

  platform = object: PlatformInterface {
    @Composable
    override fun desktopScrollBarComponents(): Triple<Animatable<Float, AnimationVector1D>, Modifier, MutableState<Job>> {
      val scope = rememberCoroutineScope()
      val scrollBarAlpha = remember { Animatable(0f) }
      val scrollJob: MutableState<Job> = remember { mutableStateOf(Job()) }
      val modifier = remember {
        Modifier.pointerInput(Unit) {
          detectCursorMove {
            scope.launch {
              scrollBarAlpha.animateTo(1f)
            }
            scrollJob.value.cancel()
            scrollJob.value = scope.launch {
              delay(1000L)
              scrollBarAlpha.animateTo(0f)
            }
          }
        }
      }
      return Triple(scrollBarAlpha, modifier, scrollJob)
    }

    @Composable
    override fun desktopScrollBar(state: LazyListState, modifier: Modifier, scrollBarAlpha: Animatable<Float, AnimationVector1D>, scrollJob: MutableState<Job>, reversed: Boolean) {
      DesktopScrollBar(rememberScrollbarAdapter(scrollState = state), modifier, scrollBarAlpha, scrollJob, reversed)
    }

    @Composable
    override fun desktopScrollBar(state: ScrollState, modifier: Modifier, scrollBarAlpha: Animatable<Float, AnimationVector1D>, scrollJob: MutableState<Job>, reversed: Boolean) {
      DesktopScrollBar(rememberScrollbarAdapter(scrollState = state), modifier, scrollBarAlpha, scrollJob, reversed)
    }
  }
}

private fun windowsLoadRequiredLibs(libsTmpDir: File, vlcDir: File) {
  val mainLibs = arrayOf(
    "libcrypto-1_1-x64.dll",
    "libsimplex.dll",
    "libapp-lib.dll"
  )
  mainLibs.forEach {
    System.load(File(libsTmpDir, it).absolutePath)
  }
  val vlcLibs = arrayOf(
    "libvlccore.dll",
    "libvlc.dll",
    "axvlc.dll",
    "npvlc.dll"
  )
  vlcLibs.forEach {
    System.load(File(vlcDir, it).absolutePath)
  }
}
