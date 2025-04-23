package chat.simplex.common.views.helpers

import androidx.compose.animation.*
import androidx.compose.animation.core.*
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chatlist.StatusBarBackground
import chat.simplex.common.views.onboarding.OnboardingStage
import kotlinx.coroutines.flow.MutableStateFlow
import java.util.concurrent.atomic.AtomicBoolean
import kotlin.math.min
import kotlin.math.sqrt

@Composable
fun ModalView(
  close: () -> Unit,
  showClose: Boolean = true,
  showAppBar: Boolean = true,
  enableClose: Boolean = true,
  background: Color = Color.Unspecified,
  modifier: Modifier = Modifier,
  showSearch: Boolean = false,
  searchAlwaysVisible: Boolean = false,
  onSearchValueChanged: (String) -> Unit = {},
  endButtons: @Composable RowScope.() -> Unit = {},
  appBar: @Composable (BoxScope.() -> Unit)? = null,
  content: @Composable BoxScope.() -> Unit,
) {
  if (showClose && showAppBar) {
    BackHandler(enabled = enableClose, onBack = close)
  }
  val oneHandUI = remember { derivedStateOf { if (appPrefs.onboardingStage.state.value == OnboardingStage.OnboardingComplete) appPrefs.oneHandUI.state.value else false } }
  Surface(Modifier.fillMaxSize(), contentColor = LocalContentColor.current) {
    Box(if (background != Color.Unspecified) Modifier.background(background) else Modifier.themedBackground(bgLayerSize = LocalAppBarHandler.current?.backgroundGraphicsLayerSize, bgLayer = LocalAppBarHandler.current?.backgroundGraphicsLayer)) {
      Box(modifier = modifier) {
        content()
      }
      if (showAppBar) {
        if (oneHandUI.value) {
          StatusBarBackground()
        }
        Box(Modifier.align(if (oneHandUI.value) Alignment.BottomStart else Alignment.TopStart)) {
          if (appBar != null) {
            appBar()
          } else {
            DefaultAppBar(
              navigationButton = if (showClose) {
                { NavigationButtonBack(onButtonClicked = if (enableClose) close else null) }
              } else null,
              onTop = !oneHandUI.value,
              showSearch = showSearch,
              searchAlwaysVisible = searchAlwaysVisible,
              onSearchValueChanged = onSearchValueChanged,
              buttons = endButtons
            )
          }
        }
      }
    }
  }
}

enum class ModalPlacement {
  START, CENTER, END, FULLSCREEN
}

class ModalData(val keyboardCoversBar: Boolean = true) {
  private val state = mutableMapOf<String, MutableState<Any?>>()
  fun <T> stateGetOrPut (key: String, default: () -> T): MutableState<T> =
    state.getOrPut(key) { mutableStateOf(default() as Any) } as MutableState<T>

  fun <T> stateGetOrPutNullable (key: String, default: () -> T?): MutableState<T?> =
    state.getOrPut(key) { mutableStateOf(default() as Any?) } as MutableState<T?>

  val appBarHandler = AppBarHandler(null, null, keyboardCoversBar = keyboardCoversBar)
}

enum class ModalViewId {
  SECONDARY_CHAT
}

class ModalManager(private val placement: ModalPlacement? = null) {
  data class ModalViewHolder(
    val id: ModalViewId?,
    val animated: Boolean,
    val data: ModalData,
    val modal: @Composable ModalData.(close: () -> Unit) -> Unit
  )

  private val modalViews = arrayListOf<ModalViewHolder>()
  private val _modalCount = mutableStateOf(0)
  val modalCount: State<Int> = _modalCount
  private val toRemove = mutableSetOf<Int>()
  private var oldViewChanging = AtomicBoolean(false)
  // Don't use mutableStateOf() here, because it produces this if showing from SimpleXAPI.startChat():
  // java.lang.IllegalStateException: Reading a state that was created after the snapshot was taken or in a snapshot that has not yet been applied
  private var passcodeView: MutableStateFlow<(@Composable (close: () -> Unit) -> Unit)?> = MutableStateFlow(null)
  private var onTimePasscodeView: MutableStateFlow<(@Composable (close: () -> Unit) -> Unit)?> = MutableStateFlow(null)

  fun hasModalOpen(id: ModalViewId): Boolean = modalViews.any { it.id == id }

  fun isLastModalOpen(id: ModalViewId): Boolean = modalViews.lastOrNull()?.id == id

  fun showModal(settings: Boolean = false, showClose: Boolean = true, id: ModalViewId? = null, endButtons: @Composable RowScope.() -> Unit = {}, content: @Composable ModalData.() -> Unit) {
    showCustomModal(id = id) { close ->
      ModalView(close, showClose = showClose, endButtons = endButtons, content = { content() })
    }
  }

  fun showModalCloseable(settings: Boolean = false, showClose: Boolean = true, id: ModalViewId? = null, endButtons: @Composable RowScope.() -> Unit = {}, content: @Composable ModalData.(close: () -> Unit) -> Unit) {
    showCustomModal(id = id) { close ->
      ModalView(close, showClose = showClose, endButtons = endButtons, content = { content(close) })
    }
  }

  fun showCustomModal(animated: Boolean = true, keyboardCoversBar: Boolean = true, id: ModalViewId? = null, modal: @Composable ModalData.(close: () -> Unit) -> Unit) {
    Log.d(TAG, "ModalManager.showCustomModal")
    val data = ModalData(keyboardCoversBar = keyboardCoversBar)
    // Means, animation is in progress or not started yet. Do not wait until animation finishes, just remove all from screen.
    // This is useful when invoking close() and ShowCustomModal one after another without delay. Otherwise, screen will hold prev view
    if (toRemove.isNotEmpty()) {
      runAtomically { toRemove.removeIf { elem -> modalViews.removeAt(elem); true } }
    }
    // Make animated appearance only on Android (everytime) and on Desktop (when it's on the start part of the screen or modals > 0)
    // to prevent unneeded animation on different situations
    val anim = if (appPlatform.isAndroid) animated else animated && (modalCount.value > 0 || placement == ModalPlacement.START)
    modalViews.add(ModalViewHolder(id, anim, data, modal))
    _modalCount.value = modalViews.size - toRemove.size

    if (placement == ModalPlacement.CENTER) {
      ChatModel.chatId.value = null
    } else if (placement == ModalPlacement.END) {
      desktopExpandWindowToWidth(DEFAULT_START_MODAL_WIDTH * sqrt(appPrefs.fontScale.get()) + DEFAULT_MIN_CENTER_MODAL_WIDTH + DEFAULT_END_MODAL_WIDTH * sqrt(appPrefs.fontScale.get()))
    }
  }

  fun showPasscodeCustomModal(oneTime: Boolean, modal: @Composable (close: () -> Unit) -> Unit) {
    Log.d(TAG, "ModalManager.showPasscodeCustomModal, oneTime: $oneTime")
    if (oneTime) {
      onTimePasscodeView.value = modal
    } else {
      passcodeView.value = modal
    }
  }

  fun hasModalsOpen() = modalCount.value > 0

  val hasModalsOpen: Boolean
  @Composable get () = remember { modalCount }.value > 0

  fun openModalCount() = modalCount.value

  fun closeModal() {
    if (modalViews.isNotEmpty()) {
      val lastModal = modalViews.lastOrNull()
      if (lastModal != null) {
        if (lastModal.id == ModalViewId.SECONDARY_CHAT) chatModel.secondaryChatsContext.value = null
        if (!lastModal.animated)
          modalViews.removeAt(modalViews.lastIndex)
        else
          runAtomically { toRemove.add(modalViews.lastIndex - min(toRemove.size, modalViews.lastIndex)) }
      }
    }
    _modalCount.value = modalViews.size - toRemove.size
  }

  fun closeModals() {
    chatModel.secondaryChatsContext.value = null
    modalViews.clear()
    toRemove.clear()
    _modalCount.value = 0
  }

  fun closeModalsExceptFirst() {
    while (modalCount.value > 1) {
      closeModal()
    }
  }

  @OptIn(ExperimentalAnimationApi::class)
  @Composable
  fun showInView() {
    // Without animation
    if (modalCount.value > 0 && modalViews.lastOrNull()?.animated == false) {
      modalViews.lastOrNull()?.let {
        CompositionLocalProvider(LocalAppBarHandler provides adjustAppBarHandler(it.data.appBarHandler)) {
          it.modal(it.data, ::closeModal)
        }
      }
      return
    }
    AnimatedContent(targetState = modalCount.value,
      transitionSpec = {
        if (targetState > initialState) {
          fromEndToStartTransition()
        } else {
          fromStartToEndTransition()
        }.using(SizeTransform(clip = false))
      }
    ) {
      modalViews.getOrNull(it - 1)?.let {
        CompositionLocalProvider(LocalAppBarHandler provides adjustAppBarHandler(it.data.appBarHandler)) {
          it.modal(it.data, ::closeModal)
        }
      }
      // This is needed because if we delete from modalViews immediately on request, animation will be bad
      if (toRemove.isNotEmpty() && it == modalCount.value && transition.currentState == EnterExitState.Visible && !transition.isRunning) {
        runAtomically { toRemove.removeIf { elem -> modalViews.removeAt(elem); true } }
      }
    }
  }

  @Composable
  fun showPasscodeInView() {
    passcodeView.collectAsState().value?.invoke { passcodeView.value = null }
  }

  @Composable
  fun showOneTimePasscodeInView() {
    onTimePasscodeView.collectAsState().value?.invoke { onTimePasscodeView.value = null }
  }

  /**
  * Allows to modify a list without getting [ConcurrentModificationException]
  * */
  private fun runAtomically(atomicBoolean: AtomicBoolean = oldViewChanging, block: () -> Unit) {
    while (!atomicBoolean.compareAndSet(false, true)) {
      Thread.sleep(10)
    }
    block()
    atomicBoolean.set(false)
  }
//  private fun <T> animationSpecFromStart() = tween<T>(durationMillis = 150, easing = FastOutLinearInEasing)
//  private fun <T> animationSpecFromEnd() = tween<T>(durationMillis = 100, easing = FastOutSlowInEasing)

  companion object {
    private val shared = ModalManager()
    val start = if (appPlatform.isAndroid) shared else ModalManager(ModalPlacement.START)
    val center = if (appPlatform.isAndroid) shared else ModalManager(ModalPlacement.CENTER)
    val end = if (appPlatform.isAndroid) shared else ModalManager(ModalPlacement.END)
    val fullscreen = if (appPlatform.isAndroid) shared else ModalManager(ModalPlacement.FULLSCREEN)

    val floatingTerminal = if (appPlatform.isAndroid) shared else ModalManager(ModalPlacement.START)

    fun closeAllModalsEverywhere() {
      start.closeModals()
      center.closeModals()
      end.closeModals()
      fullscreen.closeModals()
      floatingTerminal.closeModals()
    }

    @OptIn(ExperimentalAnimationApi::class)
    fun fromStartToEndTransition() =
      slideInHorizontally(
        initialOffsetX = { fullWidth -> -fullWidth },
        animationSpec = animationSpec()
      ) with slideOutHorizontally(
        targetOffsetX = { fullWidth -> fullWidth },
        animationSpec = animationSpec()
      )

    @OptIn(ExperimentalAnimationApi::class)
    fun fromEndToStartTransition() =
      slideInHorizontally(
        initialOffsetX = { fullWidth -> fullWidth },
        animationSpec = animationSpec()
      ) with slideOutHorizontally(
        targetOffsetX = { fullWidth -> -fullWidth },
        animationSpec = animationSpec()
      )

    private fun <T> animationSpec() = tween<T>(durationMillis = 250, easing = FastOutSlowInEasing)
  }
}
