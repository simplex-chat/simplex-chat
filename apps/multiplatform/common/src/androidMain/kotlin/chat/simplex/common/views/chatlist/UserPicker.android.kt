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
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.flow.MutableStateFlow

@Composable
actual fun UserPickerInactiveUsersSection(
  users: List<UserInfo>,
  stopped: Boolean,
  onShowAllProfilesClicked: () -> Unit,
  onUserClicked: (user: User) -> Unit,
) {
  val scrollState = rememberScrollState()

  if (users.isNotEmpty()) {
    SectionItemView(padding = PaddingValues(start = 16.dp, top = DEFAULT_MIN_SECTION_ITEM_PADDING_VERTICAL, bottom = DEFAULT_PADDING_HALF), disabled = stopped) {
      Box {
        Row(
          modifier = Modifier.padding(end = DEFAULT_PADDING + 30.dp).horizontalScroll(scrollState)
        ) {
          users.forEach { u ->
            UserPickerInactiveUserBadge(u, stopped) {
              onUserClicked(it)
            }
            Spacer(Modifier.width(20.dp))
          }
          Spacer(Modifier.width(60.dp))
        }
        Row(
          horizontalArrangement = Arrangement.End,
          modifier = Modifier
            .fillMaxWidth()
            .padding(end = DEFAULT_PADDING + 30.dp)
            .height(60.dp)
        ) {
          Canvas(modifier = Modifier.size(60.dp)) {
            drawRect(
              brush = Brush.horizontalGradient(
                colors = listOf(
                  Color.Transparent,
                  CurrentColors.value.colors.surface,
                )
              ),
            )
          }
        }
        Row(
          horizontalArrangement = Arrangement.End,
          verticalAlignment = Alignment.CenterVertically,
          modifier = Modifier
            .height(60.dp)
            .fillMaxWidth()
            .padding(end = DEFAULT_PADDING)
        ) {
          IconButton(
            onClick = onShowAllProfilesClicked,
            enabled = !stopped
          ) {
            Icon(
              painterResource(MR.images.ic_chevron_right),
              stringResource(MR.strings.your_chat_profiles),
              tint = MaterialTheme.colors.secondary,
              modifier = Modifier.size(34.dp)
            )
          }
        }
      }
    }
  } else {
    UserPickerOptionRow(
      painterResource(MR.images.ic_manage_accounts),
      stringResource(MR.strings.your_chat_profiles),
      onShowAllProfilesClicked
    )
  }
}

private fun calculateFraction(pos: Float) =
  (pos / 1f).coerceIn(0f, 1f)

@Composable
actual fun UserPickerScaffold(pickerState: MutableStateFlow<AnimatedViewState>, content: @Composable (modifier: Modifier) -> Unit) {
  val pickerIsVisible = pickerState.collectAsState().value.isVisible()
  val dismissState = rememberDismissState(initialValue = if (pickerIsVisible) DismissValue.Default else DismissValue.DismissedToEnd) {
    if (it == DismissValue.DismissedToEnd) {
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
            navBarColor = colors.surface
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
       }
      .then(clickableModifier),
    contentAlignment = Alignment.BottomCenter
  ) {
    Box(
      Modifier.onSizeChanged { height.intValue = it.height }
    ) {
      KeyChangeEffect(pickerIsVisible) {
        if (pickerState.value.isVisible()) {
          dismissState.animateTo(DismissValue.Default, userPickerAnimSpec())
        } else {
          dismissState.animateTo(DismissValue.DismissedToEnd, userPickerAnimSpec())
        }
      }
      content(
        if (height.intValue != 0)
          Modifier.draggableBottomDrawerModifier(
            state = dismissState,
            swipeDistance = height.intValue.toFloat(),
          )
        else Modifier
      )
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
