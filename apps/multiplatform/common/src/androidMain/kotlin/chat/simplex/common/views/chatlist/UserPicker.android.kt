package chat.simplex.common.views.chatlist

import SectionItemView
import androidx.compose.foundation.*
import androidx.compose.foundation.gestures.Orientation
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.DrawerDefaults.ScrimOpacity
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.graphics.*
import androidx.compose.ui.layout.onSizeChanged
import androidx.compose.ui.unit.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.User
import chat.simplex.common.model.UserInfo
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.OnboardingStage
import kotlinx.coroutines.CancellationException
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.MutableStateFlow

@Composable
actual fun UserPickerUsersSection(
  users: List<UserInfo>,
  stopped: Boolean,
  onUserClicked: (user: User) -> Unit,
) {
  val scrollState = rememberScrollState()
  val screenWidthDp = windowWidth()

  if (users.isNotEmpty()) {
    SectionItemView(
      padding = PaddingValues(
        top = if (windowOrientation() == WindowOrientation.PORTRAIT) DEFAULT_MIN_SECTION_ITEM_PADDING_VERTICAL else 0.dp
      ),
      disabled = stopped
    ) {
      Box {
        Row(
          modifier = Modifier.horizontalScroll(scrollState),
        ) {
          Spacer(Modifier.width(DEFAULT_PADDING))
          Row(horizontalArrangement = Arrangement.spacedBy(USER_PICKER_ROW_PADDING)) {
            users.forEach { u ->
              UserPickerUserBox(u, stopped, modifier = Modifier.userBoxWidth(u.user, users.size, screenWidthDp)) {
                onUserClicked(it)
                withBGApi {
                  delay(500)
                  scrollState.scrollTo(0)
                }
              }
            }
          }
          Spacer(Modifier.width(DEFAULT_PADDING))
        }
      }
    }
  }
}

private fun calculateFraction(pos: Float) =
  (pos / 1f).coerceIn(0f, 1f)

@Composable
actual fun PlatformUserPicker(modifier: Modifier, pickerState: MutableStateFlow<AnimatedViewState>, content: @Composable () -> Unit) {
  val pickerIsVisible = pickerState.collectAsState().value.isVisible()
  val dismissState = rememberDismissState(initialValue = if (pickerIsVisible) DismissValue.Default else DismissValue.DismissedToEnd) {
    if (it == DismissValue.DismissedToEnd && pickerState.value.isVisible()) {
      pickerState.value = AnimatedViewState.HIDING
    }
    true
  }
  val height = remember { mutableIntStateOf(0) }
  val heightValue = height.intValue
  val clickableModifier = if (pickerIsVisible) {
    Modifier.clickable(interactionSource = remember { MutableInteractionSource() }, indication = null, onClick = { pickerState.value = AnimatedViewState.HIDING })
  } else {
    Modifier
  }
  Box(
    Modifier
      .fillMaxSize()
      .then(clickableModifier)
      .drawBehind {
        val pos = when {
          dismissState.progress.from == DismissValue.Default && dismissState.progress.to == DismissValue.Default -> 1f
          dismissState.progress.from == DismissValue.DismissedToEnd && dismissState.progress.to == DismissValue.DismissedToEnd -> 0f
          dismissState.progress.to == DismissValue.Default -> dismissState.progress.fraction
          else -> 1 - dismissState.progress.fraction
        }
        val colors = CurrentColors.value.colors
        val resultingColor = if (colors.isLight) colors.onSurface.copy(alpha = ScrimOpacity) else Color.Black.copy(0.64f)
        val adjustedAlpha = resultingColor.alpha * calculateFraction(pos = pos)
        val shadingColor = resultingColor.copy(alpha = adjustedAlpha)

        if (pickerState.value.isVisible()) {
          platform.androidSetDrawerStatusAndNavBarColor(
            isLight = colors.isLight,
            drawerShadingColor = shadingColor,
            toolbarOnTop = !appPrefs.oneHandUI.get(),
            navBarColor = colors.background.mixWith(colors.onBackground, 0.97f)
          )
        } else if (ModalManager.start.modalCount.value == 0) {
          platform.androidSetDrawerStatusAndNavBarColor(
            isLight = colors.isLight,
            drawerShadingColor = shadingColor,
            toolbarOnTop = !appPrefs.oneHandUI.get(),
            navBarColor = (if (appPrefs.oneHandUI.get() && appPrefs.onboardingStage.get() == OnboardingStage.OnboardingComplete) {
              colors.background.mixWith(CurrentColors.value.colors.onBackground, 0.97f)
            } else {
              colors.background
            })
          )
        }
        drawRect(
          if (pos != 0f) resultingColor else Color.Transparent,
          alpha = calculateFraction(pos = pos)
        )
      }
      .graphicsLayer {
        if (heightValue == 0) {
          alpha = 0f
        }
        translationY = dismissState.offset.value
       },
    contentAlignment = Alignment.BottomCenter
  ) {
    Box(
      Modifier.onSizeChanged { height.intValue = it.height }
    ) {
      KeyChangeEffect(pickerIsVisible) {
        if (pickerState.value.isVisible()) {
          try {
            dismissState.animateTo(DismissValue.Default, userPickerAnimSpec())
          } catch (e: CancellationException) {
            Log.e(TAG, "Cancelled animateTo: ${e.stackTraceToString()}")
            pickerState.value = AnimatedViewState.GONE
          }
        } else {
          try {
            dismissState.animateTo(DismissValue.DismissedToEnd, userPickerAnimSpec())
          } catch (e: CancellationException) {
            Log.e(TAG, "Cancelled animateTo2: ${e.stackTraceToString()}")
            pickerState.value = AnimatedViewState.VISIBLE
          }
        }
      }
      val draggableModifier = if (height.intValue != 0)
        Modifier.draggableBottomDrawerModifier(
          state = dismissState,
          swipeDistance = height.intValue.toFloat(),
        )
      else Modifier
      Box(draggableModifier.then(modifier)) {
        content()
      }
    }
  }
}

private fun Modifier.draggableBottomDrawerModifier(
  state: DismissState,
  swipeDistance: Float,
): Modifier = this.swipeable(
  state = state,
  anchors = mapOf(0f to DismissValue.Default, swipeDistance to DismissValue.DismissedToEnd),
  thresholds = { _, _ -> FractionalThreshold(0.3f) },
  orientation = Orientation.Vertical,
  resistance = null
)

private fun Modifier.userBoxWidth(user: User, totalUsers: Int, windowWidth: Dp): Modifier {
  return if (totalUsers == 1) {
    this.width(windowWidth - DEFAULT_PADDING * 2)
  } else if (user.activeUser) {
    this.width(windowWidth - DEFAULT_PADDING - (USER_PICKER_ROW_PADDING * 3) - USER_PICKER_IMAGE_SIZE)
  } else {
    this.widthIn(max = windowWidth * 0.618f)
  }
}