package chat.simplex.common.views.chatlist

import SectionItemView
import androidx.compose.animation.*
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.DrawerDefaults.ScrimOpacity
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.User
import chat.simplex.common.model.UserInfo
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.platform.platform
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.OnboardingStage
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.launch

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

@Composable
actual fun UserPickerScaffold(pickerState: MutableStateFlow<AnimatedViewState>, content: @Composable () -> Unit) {
  val currentTheme by CurrentColors.collectAsState()
  val resultingColor by remember {
    derivedStateOf {
      if (currentTheme.colors.isLight) currentTheme.colors.onSurface.copy(alpha = ScrimOpacity) else Color.Black.copy(0.64f)
    }
  }
  val animatedColor = remember {
    androidx.compose.animation.core.Animatable(
      if (pickerState.value.isVisible()) Color.Transparent else resultingColor,
      Color.VectorConverter(resultingColor.colorSpace)
    )
  }

  LaunchedEffect(Unit) {
    launch {
      snapshotFlow { ModalManager.start.modalCount.value }
        .collect { modalCount ->
          val colors = CurrentColors.value.colors

          if (modalCount == 0 && pickerState.value.isVisible()) {
            platform.androidSetDrawerStatusAndNavBarColor(
              isLight = colors.isLight,
              drawerShadingColor = animatedColor,
              toolbarOnTop = !appPrefs.oneHandUI.get(),
              navBarColor = colors.surface
            )
          }
        }
    }
  }

  LaunchedEffect(Unit) {
    snapshotFlow { currentTheme }
      .distinctUntilChanged()
      .collect {
        launch {
          pickerState.collect {
            val newState = it
            val colors = CurrentColors.value.colors
            val toColor = if (colors.isLight) colors.onSurface.copy(alpha = ScrimOpacity) else Color.Black.copy(0.64f)

            animatedColor.animateTo(if (newState.isVisible()) toColor else Color.Transparent, newChatSheetAnimSpec()) {
                if (newState.isVisible()) {
                  platform.androidSetDrawerStatusAndNavBarColor(
                    isLight = colors.isLight,
                    drawerShadingColor = animatedColor,
                    toolbarOnTop = !appPrefs.oneHandUI.get(),
                    navBarColor = colors.surface
                  )
                } else if (newState.isHiding()) {
                  platform.androidSetDrawerStatusAndNavBarColor(
                    isLight = colors.isLight,
                    drawerShadingColor = animatedColor,
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
        }
      }
  }

  Box(Modifier.drawBehind { drawRect(animatedColor.value) }) {
    AnimatedVisibility(
      visible = pickerState.run { value.isVisible() },
      enter = if (appPlatform.isAndroid) {
        slideInVertically(
          initialOffsetY = { it },
          animationSpec = userPickerAnimSpec()
        ) + fadeIn(animationSpec = userPickerAnimSpec())
      } else {
        fadeIn()
      },
      exit = if (appPlatform.isAndroid) {
        slideOutVertically(
          targetOffsetY = { it },
          animationSpec = userPickerAnimSpec()
        ) + fadeOut(animationSpec = userPickerAnimSpec())
      } else {
        fadeOut()
      }
    ) {
      content()
    }
  }
}