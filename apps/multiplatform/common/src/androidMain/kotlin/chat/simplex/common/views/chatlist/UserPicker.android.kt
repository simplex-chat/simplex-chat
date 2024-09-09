package chat.simplex.common.views.chatlist

import SectionItemView
import androidx.compose.animation.core.*
import androidx.compose.animation.*
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.DrawerDefaults.ScrimOpacity
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.layout.onGloballyPositioned
import androidx.compose.ui.platform.LocalDensity
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
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.launch
import kotlin.math.roundToInt

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
  val currentTheme by CurrentColors.collectAsState()
  val resultingColor by remember {
    derivedStateOf {
      if (currentTheme.colors.isLight) currentTheme.colors.onSurface.copy(alpha = ScrimOpacity) else Color.Black.copy(0.64f)
    }
  }
  var manualDrag by remember { mutableStateOf(0f) }
  val drawerProgress by animateFloatAsState(
    targetValue = manualDrag,
  )
  val drawerHeight = 457.dp
  val maxDragOffset = with(LocalDensity.current) { drawerHeight * density }


  LaunchedEffect(Unit) {
    snapshotFlow { drawerProgress }
      .collect {
        val colors = CurrentColors.value.colors
        val adjustedAlpha = resultingColor.alpha * calculateFraction(pos = it)
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
      }
  }


  LaunchedEffect(Unit) {
    snapshotFlow { currentTheme }
      .distinctUntilChanged()
      .collect {
        launch {
          pickerState.collect {
            manualDrag = if (pickerState.value.isVisible()) {
              1f
            } else {
              0f
            }
          }
        }
      }
  }

  val dismissState = rememberDismissState(initialValue = DismissValue.Default) {
    if (drawerProgress < 0.75) {
      pickerState.value = AnimatedViewState.HIDING
    }
    if (it == DismissValue.DismissedToEnd) {
      pickerState.value = AnimatedViewState.HIDING
    }
    true
  }

  LaunchedEffect(Unit) {
    snapshotFlow { dismissState.offset.value }
      .collect {
        if (pickerState.value.isVisible() || pickerState.value.isHiding()) {
          manualDrag = 1 - (it / maxDragOffset.value)
        }
      }
  }

  val swipeableModifier = DraggableBottomDrawerModifier(
    state = dismissState,
    swipeDistance = with(LocalDensity.current) { drawerHeight.toPx() },
  )

  Box(
    Modifier
      .drawBehind {
        drawRect(
          if (pickerState.value.isVisible() || pickerState.value.isHiding()) resultingColor else Color.Transparent,
          alpha = calculateFraction(pos = drawerProgress)
        )
      }
      .offset {
        IntOffset(
          x = 0,
          y = ((1f - drawerProgress) * drawerHeight.toPx()).roundToInt()
        )
      }
  ) {
    Box(
      Modifier
        .fillMaxSize()
        .clickable(interactionSource = remember { MutableInteractionSource() }, indication = null, onClick = { pickerState.value = AnimatedViewState.HIDING }),
      contentAlignment = Alignment.BottomStart
    ) {
      content(swipeableModifier)
    }
  }
}