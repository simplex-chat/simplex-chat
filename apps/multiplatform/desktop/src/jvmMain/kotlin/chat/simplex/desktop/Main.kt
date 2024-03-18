package chat.simplex.desktop

import androidx.compose.animation.core.Animatable
import androidx.compose.animation.core.AnimationVector1D
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.*
import androidx.compose.foundation.lazy.LazyListState
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.*
import androidx.compose.ui.ExperimentalComposeUiApi
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.input.pointer.*
import androidx.compose.ui.unit.dp
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

  platform = object : PlatformInterface {
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
      desktopScrollBar(rememberScrollbarAdapter(scrollState = state), modifier, scrollBarAlpha, scrollJob, reversed)
    }

    @Composable
    override fun desktopScrollBar(state: ScrollState, modifier: Modifier, scrollBarAlpha: Animatable<Float, AnimationVector1D>, scrollJob: MutableState<Job>, reversed: Boolean) {
      desktopScrollBar(rememberScrollbarAdapter(scrollState = state), modifier, scrollBarAlpha, scrollJob, reversed)
    }

    @Composable
    private fun desktopScrollBar(adapter: androidx.compose.foundation.v2.ScrollbarAdapter, modifier: Modifier, scrollBarAlpha: Animatable<Float, AnimationVector1D>, scrollJob: MutableState<Job>, reversed: Boolean) {
      val scope = rememberCoroutineScope()
      val interactionSource = remember { MutableInteractionSource() }
      val isHovered by interactionSource.collectIsHoveredAsState()
      val isDragged by interactionSource.collectIsDraggedAsState()
      LaunchedEffect(isHovered, isDragged) {
        scrollJob.value.cancel()
        if (isHovered || isDragged) {
          scrollBarAlpha.animateTo(1f)
        } else {
          scrollJob.value = scope.launch {
            delay(1000L)
            scrollBarAlpha.animateTo(0f)
          }
        }
      }
      VerticalScrollbar(
        modifier = modifier.graphicsLayer { alpha = scrollBarAlpha.value }
          .onPointerEvent(PointerEventType.Enter) {
            scrollJob.value.cancel()
            scope.launch {
              scrollBarAlpha.animateTo(1f)
            }
          },
        reverseLayout = reversed,
        style = LocalScrollbarStyle.current.copy(
          thickness = if (isHovered || isDragged) 10.dp else 6.dp,
          unhoverColor = if (MaterialTheme.colors.isLight) MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.7f) else MaterialTheme.colors.onBackground.mixWith(MaterialTheme.colors.background, 0.3f)
        ),
        adapter = adapter,
        interactionSource = interactionSource
      )
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
