package chat.simplex.common.views.onboarding

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.pager.HorizontalPager
import androidx.compose.foundation.pager.rememberPagerState
import androidx.compose.material.Icon
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.migration.MigrateToDeviceView
import chat.simplex.common.views.migration.MigrationToState
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
fun IntroCarouselView(chatModel: ChatModel) {
  CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
    ModalView({}, showClose = false, showAppBar = false) {
      IntroCarouselContent(chatModel)
    }
  }
  LaunchedEffect(Unit) {
    if (chatModel.migrationState.value != null && !ModalManager.fullscreen.hasModalsOpen()) {
      ModalManager.fullscreen.showCustomModal(animated = false) { close -> MigrateToDeviceView(close) }
    }
  }
}

@Composable
private fun IntroCarouselContent(chatModel: ChatModel) {
  val pagerState = rememberPagerState(initialPage = 0, initialPageOffsetFraction = 0f) { 3 }

  Column(Modifier.fillMaxSize()) {
    Box(
      modifier = Modifier
        .fillMaxWidth()
        .padding(top = DEFAULT_PADDING + 8.dp),
      contentAlignment = Alignment.Center,
    ) {
      SimpleXLogo(modifier = Modifier.widthIn(max = if (appPlatform.isAndroid) 250.dp else 500.dp))
    }

    Spacer(Modifier.height(24.dp))


    HorizontalPager(
      state = pagerState,
      modifier = Modifier
        .weight(1f)
        .fillMaxWidth(),
      pageNestedScrollConnection = LocalAppBarHandler.current!!.connection,
      verticalAlignment = Alignment.Top,
      userScrollEnabled = appPlatform.isAndroid,
    ) { page ->
      val headline = when (page) {
        0 -> stringResource(MR.strings.intro_headline_1)
        1 -> stringResource(MR.strings.intro_headline_2)
        else -> stringResource(MR.strings.intro_headline_3)
      }
      val subtitle = when (page) {
        0 -> stringResource(MR.strings.intro_subtitle_1)
        1 -> stringResource(MR.strings.intro_subtitle_2)
        else -> stringResource(MR.strings.intro_subtitle_3)
      }
      val showButtons = page == 2
      val introImage = when (page) {
        0 -> MR.images.intro_2
        1 -> MR.images.intro_2
        else -> MR.images.intro_2
      }
      IntroPage(
        headline = headline,
        subtitle = subtitle,
        centralContent = {
          if (BuildConfigCommon.USE_BRANDED_IMAGES) {
            Icon(
              painter = painterResource(introImage),
              contentDescription = null,
              modifier = Modifier.size(200.dp),
            )
          }
        },
        showButtons = showButtons,
        onCreateProfile = if (showButtons) {
          {}
        } else null,
        onMigrate = if (showButtons) {
          {
            chatModel.migrationState.value = MigrationToState.PasteOrScanLink
            ModalManager.fullscreen.showCustomModal { close -> MigrateToDeviceView(close) }
          }
        } else null,
      )
    }

    Box(
      modifier = Modifier
        .fillMaxWidth()
        .padding(vertical = DEFAULT_PADDING),
      contentAlignment = Alignment.Center,
    ) {
      PageIndicator(pageCount = 3, currentPage = pagerState.currentPage)
    }
  }
}

@Composable
private fun TextFallback() {
  Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
    Icon(
      painter = painterResource(MR.images.ic_chat),
      contentDescription = null,
      modifier = Modifier.size(120.dp),
      tint = MaterialTheme.colors.primary.copy(alpha = 0.6f),
    )
  }
}
