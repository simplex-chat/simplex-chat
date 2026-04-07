package chat.simplex.common.views.newchat

import androidx.compose.animation.core.animateFloatAsState
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.pager.HorizontalPager
import androidx.compose.foundation.pager.rememberPagerState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.layout.onSizeChanged
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.UserAddressView
import chat.simplex.res.MR
import kotlinx.coroutines.launch
import kotlin.math.cos
import kotlin.math.max
import kotlin.math.sin

// MARK: - Onboarding condition

@Composable
fun shouldShowOnboarding(): Boolean {
  val addressCreationCardShown = remember { appPrefs.addressCreationCardShown.state }
  val chats = chatModel.chats.value
  return !addressCreationCardShown.value && chats.isNotEmpty() && noConversationChatsYet(chats)
}

fun noConversationChatsYet(chats: List<Chat>): Boolean =
  chats.all { chat ->
    when (val c = chat.chatInfo) {
      is ChatInfo.Local -> true
      is ChatInfo.Direct -> c.contact.chatDeleted || c.contact.isContactCard
      is ChatInfo.Group -> false
      is ChatInfo.ContactRequest -> true
      is ChatInfo.ContactConnection -> true
      is ChatInfo.InvalidJSON -> true
    }
  }

// MARK: - Gradient math

private const val GRADIENT_ANGLE_RAD = 80.0 * Math.PI / 180.0

private data class GradientEndpoints(val startX: Float, val startY: Float, val endX: Float, val endY: Float)

private fun gradientPoints(aspectRatio: Float, scale: Float): GradientEndpoints {
  val r = aspectRatio.toDouble()
  val s = scale.toDouble()
  val dx = cos(GRADIENT_ANGLE_RAD)
  val dy = -sin(GRADIENT_ANGLE_RAD) / r
  val dLenSq = dx * dx + dy * dy
  val projections = doubleArrayOf(
    -0.5 * dx + (-0.5) * dy,
     0.5 * dx + (-0.5) * dy,
    -0.5 * dx + 0.5 * dy,
     0.5 * dx + 0.5 * dy
  )
  val tMin = projections.min()
  val tMax = projections.max()
  val startX = 0.5 + tMin * dx / dLenSq
  val startY = 0.5 + tMin * dy / dLenSq
  val endX = 0.5 + tMax * dx / dLenSq
  val endY = 0.5 + tMax * dy / dLenSq
  return GradientEndpoints(
    startX = (0.5 + (startX - 0.5) * s).toFloat(),
    startY = (0.5 + (startY - 0.5) * s).toFloat(),
    endX = (0.5 + (endX - 0.5) * s).toFloat(),
    endY = (0.5 + (endY - 0.5) * s).toFloat()
  )
}

private val lightStops = arrayOf(
  0.0f to Color(0xFFd2e8ff),
  0.5f to Color(0xFFcce9ff),
  0.9f to Color(0xFFdfffff),
  1.0f to Color(0xFFfffcea)
)

private val darkStops = arrayOf(
  0.4f to Color(0xFF040a24),
  0.72f to Color(0xFF3854ab),
  0.9f to Color(0xFFa8edf3),
  1.0f to Color(0xFFfff6e0)
)

// MARK: - Card component

@Composable
fun OnboardingCardView(
  imageName: dev.icerock.moko.resources.ImageResource,
  imageNameLight: dev.icerock.moko.resources.ImageResource,
  icon: dev.icerock.moko.resources.ImageResource,
  title: String,
  subtitle: String? = null,
  labelHeightRatio: Float,
  onClick: () -> Unit
) {
  var imageAreaSize by remember { mutableStateOf(IntSize.Zero) }
  val isDark = isInDarkTheme()
  val stops = if (isDark) darkStops else lightStops
  val scale = if (isDark) 1.5f else 1.2f

  val brush = remember(imageAreaSize, isDark) {
    if (imageAreaSize.width > 0 && imageAreaSize.height > 0) {
      val aspect = imageAreaSize.height.toFloat() / imageAreaSize.width.toFloat()
      val gp = gradientPoints(aspect, scale)
      Brush.linearGradient(
        colorStops = stops,
        start = Offset(gp.startX * imageAreaSize.width, gp.startY * imageAreaSize.height),
        end = Offset(gp.endX * imageAreaSize.width, gp.endY * imageAreaSize.height)
      )
    } else {
      Brush.linearGradient(colorStops = stops)
    }
  }

  val labelBg = MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.97f)
    .copy(alpha = appPrefs.inAppBarsAlpha.get())

  Box(
    Modifier
      .fillMaxSize()
      .clip(RoundedCornerShape(24.dp))
      .clickable(onClick = onClick)
  ) {
    BoxWithConstraints(Modifier.fillMaxSize()) {
      val labelHeightPx = with(LocalDensity.current) { (maxWidth * labelHeightRatio).roundToPx() }
      val imageHeightPx = max(constraints.maxHeight - labelHeightPx, 1)

      Column(Modifier.fillMaxSize()) {
        Box(
          Modifier
            .fillMaxWidth()
            .height(with(LocalDensity.current) { imageHeightPx.toDp() })
            .background(brush)
            .onSizeChanged { imageAreaSize = it }
        ) {
          if (BuildConfigCommon.SIMPLEX_ASSETS) {
            Image(
              painterResource(if (isDark) imageNameLight else imageName),
              contentDescription = null,
              contentScale = ContentScale.Fit,
              modifier = Modifier.fillMaxSize()
            )
          }
        }
        Box(
          Modifier
            .fillMaxWidth()
            .height(with(LocalDensity.current) { labelHeightPx.toDp() })
            .background(labelBg),
          contentAlignment = Alignment.Center
        ) {
          Column(horizontalAlignment = Alignment.CenterHorizontally) {
            Row(verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(8.dp)) {
              Icon(
                painterResource(icon),
                contentDescription = null,
                modifier = Modifier.size(24.dp),
                tint = MaterialTheme.colors.primary
              )
              Text(
                title,
                style = MaterialTheme.typography.body1.copy(fontWeight = FontWeight.Medium),
                color = MaterialTheme.colors.onBackground,
                maxLines = 1,
                overflow = TextOverflow.Ellipsis
              )
            }
            if (subtitle != null) {
              Text(
                subtitle,
                style = MaterialTheme.typography.caption,
                color = MaterialTheme.colors.onBackground.copy(alpha = 0.7f)
              )
            }
          }
        }
      }
    }
  }
}

// MARK: - Page header

@Composable
private fun PageHeader(title: String, showBack: Boolean, isLandscape: Boolean, onBack: (() -> Unit)? = null) {
  val titleView = @Composable {
    Text(
      title,
      style = MaterialTheme.typography.h1,
      maxLines = 1,
      overflow = TextOverflow.Ellipsis,
      textAlign = TextAlign.Center,
      modifier = Modifier.fillMaxWidth()
    )
  }
  if (isLandscape) {
    Box(Modifier.fillMaxWidth().padding(horizontal = DEFAULT_PADDING)) {
      if (showBack && onBack != null) {
        BackButton(onBack, Modifier.align(Alignment.CenterStart))
      }
      titleView()
    }
  } else {
    Column(Modifier.fillMaxWidth().padding(horizontal = DEFAULT_PADDING)) {
      if (showBack && onBack != null) {
        BackButton(onBack, Modifier.align(Alignment.Start))
      } else {
        Spacer(Modifier.height(AppBarHeight))
      }
      titleView()
    }
  }
}

@Composable
private fun BackButton(onClick: () -> Unit, modifier: Modifier = Modifier) {
  Row(
    modifier
      .clip(RoundedCornerShape(20.dp))
      .clickable(onClick = onClick)
      .padding(8.dp),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.spacedBy(4.dp)
  ) {
    Icon(
      painterResource(MR.images.ic_arrow_back_ios_new),
      contentDescription = stringResource(MR.strings.back),
      tint = MaterialTheme.colors.primary,
      modifier = Modifier.height(24.dp)
    )
    Text(stringResource(MR.strings.back), color = MaterialTheme.colors.primary)
  }
}

// MARK: - Card pair layout

@Composable
private fun CardPair(
  isLandscape: Boolean,
  padding: Dp,
  spacing: Dp,
  maxCardHeight: Dp,
  card1: @Composable () -> Unit,
  card2: @Composable () -> Unit
) {
  if (isLandscape) {
    Row(
      Modifier.padding(horizontal = padding),
      horizontalArrangement = Arrangement.spacedBy(spacing)
    ) {
      Box(Modifier.weight(1f).heightIn(max = maxCardHeight)) { card1() }
      Box(Modifier.weight(1f).heightIn(max = maxCardHeight)) { card2() }
    }
  } else {
    Column(
      Modifier.padding(horizontal = padding),
      verticalArrangement = Arrangement.spacedBy(spacing)
    ) {
      Box(Modifier.fillMaxWidth().heightIn(max = maxCardHeight)) { card1() }
      Box(Modifier.fillMaxWidth().heightIn(max = maxCardHeight)) { card2() }
    }
  }
}

// MARK: - Pager

private val DESKTOP_MAX_CONTENT_WIDTH = 500.dp

@Composable
fun ConnectOnboardingView() {
  val pagerState = rememberPagerState(initialPage = 0) { 2 }
  val scope = rememberCoroutineScope()

  // Desktop: animate cards when start modals open
  val startModalsOpen = appPlatform.isDesktop && ModalManager.start.hasModalsOpen
  val cardOffset by animateFloatAsState(if (startModalsOpen) 0.3f else 0f)
  val cardAlpha by animateFloatAsState(if (startModalsOpen) 0.3f else 1f)
  val modalSlide by animateFloatAsState(if (startModalsOpen) 0f else -1f)

  Box(Modifier.fillMaxSize()) {
    // Desktop: start modal area — slides from left
    if (appPlatform.isDesktop) {
      Box(
        Modifier
          .fillMaxHeight()
          .widthIn(max = DEFAULT_START_MODAL_WIDTH * fontSizeSqrtMultiplier)
          .graphicsLayer { translationX = modalSlide * size.width }
      ) {
        ModalManager.start.showInView()
      }
    }

    // Cards — shift right and fade on desktop when start modal is open
    Box(
      Modifier.fillMaxSize().graphicsLayer {
        if (appPlatform.isDesktop) {
          translationX = cardOffset * size.width
          alpha = cardAlpha
        }
      },
      contentAlignment = Alignment.Center
    ) {
      HorizontalPager(
        state = pagerState,
        modifier = if (appPlatform.isDesktop) Modifier.widthIn(max = DESKTOP_MAX_CONTENT_WIDTH) else Modifier.fillMaxWidth(),
        userScrollEnabled = !appPlatform.isDesktop
      ) { page ->
        val cardClickOverride: (() -> Unit)? = if (appPlatform.isDesktop && startModalsOpen) {
          { ModalManager.start.closeModals() }
        } else null

        when (page) {
          0 -> TalkToSomeonePage(
            onLetSomeoneConnect = cardClickOverride ?: { scope.launch { pagerState.animateScrollToPage(1) }; Unit },
            onConnectViaLink = cardClickOverride ?: {
              ModalManager.start.showModalCloseable { close ->
                NewChatView(chatModel.currentRemoteHost.value, NewChatOption.CONNECT, showQRCodeScanner = appPlatform.isAndroid, close = close)
              }
            }
          )
          1 -> ConnectWithSomeonePage(
            onBack = cardClickOverride ?: { scope.launch { pagerState.animateScrollToPage(0) }; Unit },
            onInviteSomeone = cardClickOverride ?: {
              ModalManager.start.showModalCloseable { close ->
                NewChatView(chatModel.currentRemoteHost.value, NewChatOption.INVITE, close = close)
              }
            },
            onCreateAddress = cardClickOverride ?: {
              ModalManager.start.showModalCloseable { close ->
                UserAddressView(chatModel = chatModel, shareViaProfile = false, autoCreateAddress = true, close = close)
              }
            }
          )
        }
      }
    }
  }
}

// MARK: - Screen 1

@Composable
private fun TalkToSomeonePage(
  onLetSomeoneConnect: () -> Unit,
  onConnectViaLink: () -> Unit
) {
  BoxWithConstraints(Modifier.fillMaxSize()) {
    val isLandscape = maxWidth > maxHeight && appPlatform.isAndroid
    val padding = DEFAULT_PADDING
    val spacing = DEFAULT_PADDING
    val cardWidth = if (isLandscape) (maxWidth - padding * 2 - spacing) / 2 else maxWidth - padding * 2
    val maxCardHeight = cardWidth * 0.75f

    Column(
      Modifier.fillMaxSize(),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      PageHeader(
        title = stringResource(MR.strings.talk_to_someone),
        showBack = false,
        isLandscape = isLandscape
      )

      Spacer(Modifier.weight(1f).defaultMinSize(minHeight = DEFAULT_PADDING))

      CardPair(isLandscape, padding, spacing, maxCardHeight,
        card1 = {
          OnboardingCardView(
            imageName = MR.images.card_let_someone_connect_to_you_alpha,
            imageNameLight = MR.images.card_let_someone_connect_to_you_alpha_light,
            icon = MR.images.ic_add_link,
            title = stringResource(MR.strings.let_someone_connect_to_you),
            labelHeightRatio = 0.132f,
            onClick = onLetSomeoneConnect
          )
        },
        card2 = {
          OnboardingCardView(
            imageName = MR.images.card_connect_via_link_alpha,
            imageNameLight = MR.images.card_connect_via_link_alpha_light,
            icon = MR.images.ic_qr_code,
            title = stringResource(MR.strings.connect_via_link_or_qr_code),
            labelHeightRatio = 0.132f,
            onClick = onConnectViaLink
          )
        }
      )

      Spacer(Modifier.weight(1f).defaultMinSize(minHeight = DEFAULT_PADDING))
    }
  }
}

// MARK: - Screen 2

@Composable
private fun ConnectWithSomeonePage(
  onBack: () -> Unit,
  onInviteSomeone: () -> Unit,
  onCreateAddress: () -> Unit
) {
  BoxWithConstraints(Modifier.fillMaxSize()) {
    val isLandscape = maxWidth > maxHeight && appPlatform.isAndroid
    val padding = DEFAULT_PADDING
    val spacing = DEFAULT_PADDING
    val cardWidth = if (isLandscape) (maxWidth - padding * 2 - spacing) / 2 else maxWidth - padding * 2
    val maxCardHeight = cardWidth * 0.75f

    Column(
      Modifier.fillMaxSize(),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      PageHeader(
        title = stringResource(MR.strings.connect_with_someone),
        showBack = true,
        isLandscape = isLandscape,
        onBack = onBack
      )

      Spacer(Modifier.weight(1f).defaultMinSize(minHeight = DEFAULT_PADDING))

      CardPair(isLandscape, padding, spacing, maxCardHeight,
        card1 = {
          OnboardingCardView(
            imageName = MR.images.card_invite_someone_privately_alpha,
            imageNameLight = MR.images.card_invite_someone_privately_alpha_light,
            icon = MR.images.ic_add_link,
            title = stringResource(MR.strings.invite_someone_privately),
            subtitle = stringResource(MR.strings.a_link_for_one_person),
            labelHeightRatio = 0.195f,
            onClick = onInviteSomeone
          )
        },
        card2 = {
          OnboardingCardView(
            imageName = MR.images.card_create_your_public_address_alpha,
            imageNameLight = MR.images.card_create_your_public_address_alpha_light,
            icon = MR.images.ic_qr_code,
            title = stringResource(MR.strings.create_your_public_address),
            subtitle = stringResource(MR.strings.for_anyone_to_reach_you),
            labelHeightRatio = 0.195f,
            onClick = onCreateAddress
          )
        }
      )

      Spacer(Modifier.weight(1f).defaultMinSize(minHeight = DEFAULT_PADDING))
    }
  }
}
