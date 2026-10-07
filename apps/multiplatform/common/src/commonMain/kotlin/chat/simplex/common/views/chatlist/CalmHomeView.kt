package chat.simplex.common.views.chatlist

import androidx.compose.animation.*
import androidx.compose.animation.core.*
import androidx.compose.foundation.*
import androidx.compose.foundation.gestures.awaitEachGesture
import androidx.compose.foundation.gestures.awaitFirstDown
import androidx.compose.foundation.gestures.waitForUpOrCancellation
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.*
import androidx.compose.ui.draw.*
import androidx.compose.ui.geometry.*
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.drawscope.Stroke
import androidx.compose.ui.graphics.drawscope.clipPath
import androidx.compose.ui.graphics.drawscope.rotate
import androidx.compose.ui.hapticfeedback.HapticFeedbackType
import androidx.compose.ui.input.pointer.*
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.platform.LocalHapticFeedback
import androidx.compose.ui.semantics.*
import androidx.compose.ui.text.SpanStyle
import androidx.compose.ui.text.buildAnnotatedString
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.topPaddingToContent
import chat.simplex.common.views.contacts.onRequestAccepted
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlin.math.*

private val CALM_SIZE = 128.dp
private val calmEaseOut = CubicBezierEasing(0f, 0f, 0.58f, 1f)
private val calmEaseInOut = CubicBezierEasing(0.42f, 0f, 0.58f, 1f)
private val calmOvershoot = CubicBezierEasing(0.34f, 1.56f, 0.64f, 1f)

// width, height, corner (fraction of the shorter side), rotation: circle, rounded square, rounded diamond, capsule
private val calmShapes = listOf(
  floatArrayOf(1f, 1f, 0.5f, 0f),
  floatArrayOf(1f, 1f, 0.34f, 0f),
  floatArrayOf(0.78f, 0.78f, 0.3f, 45f),
  floatArrayOf(1.08f, 0.68f, 0.5f, 0f),
)

class CalmPeek(
  val title: String,
  val subtitle: String,
  val image: String? = null,
  val icon: ImageResource = MR.images.ic_account_circle_filled,
  val sender: String? = null,
  val text: String? = null,
  val identity: String? = null,
  val identityName: String? = null,
  val incognito: Boolean = false,
  val footer: String? = null,
)

// waiting means the chat shows an unread badge and its notifications are not off, same as Chat.unreadTag
private fun calmChatWaiting(chat: Chat): Boolean =
  (chat.chatInfo is ChatInfo.Direct && !chat.chatInfo.contact.nextAcceptContactRequest || chat.chatInfo is ChatInfo.Group) &&
      chat.chatInfo.chatSettings?.enableNtfs != MsgFilter.None && chat.unreadTag

private fun calmRequest(chat: Chat): Boolean =
  chat.chatInfo is ChatInfo.ContactRequest || (chat.chatInfo as? ChatInfo.Direct)?.contact?.nextAcceptContactRequest == true

// sorted by id: chatModel.users is reordered by the profile picker, and the shape must not change with it
private fun calmUsers(chatModel: ChatModel) = chatModel.users.filter { it.user.activeUser || !it.user.hidden }.sortedBy { it.user.userId }

private fun calmShapeIndex(users: List<UserInfo>) = max(0, users.indexOfFirst { it.user.activeUser })

private fun calmColors(theme: DefaultTheme): Pair<Color, Color?> = when (theme) {
  DefaultTheme.LIGHT -> Color(0xFF0B0C12) to null
  DefaultTheme.BLACK -> Color(0xFF1C1D22) to Color(0x4D70F0F9)
  else -> Color.Black to Color(0x4D70F0F9)
}

@Composable
fun CalmHomeView(chatModel: ChatModel, stopped: Boolean, oneHandUI: Boolean, onNewChat: () -> Unit, onAllChats: () -> Unit) {
  val user = chatModel.currentUser.value ?: return
  val scope = rememberCoroutineScope()
  val users = calmUsers(chatModel)
  val shapeIndex = calmShapeIndex(users)
  val chats = chatModel.chats.value
  val waiting = chats.filter(::calmChatWaiting)
  val request = chats.firstOrNull(::calmRequest)
  val previewMode = chatModel.notificationPreviewMode.value
  val hidePreviews = previewMode == NotificationPreviewMode.HIDDEN
  val otherProfileWaiting = users.any { !it.user.activeUser && it.user.showNtfs && it.unreadCount > 0 }
  // system animations off (Android) is read from the frame clock, desktop has no such setting
  val reduceMotion by produceState(false) { value = coroutineContext[MotionDurationScale]?.scaleFactor == 0f }
  // on Android the list stays composed behind an open chat, on desktop it is visible next to it
  val visible = if (appPlatform.isDesktop) isAppVisibleAndFocused() else chatModel.chatId.value == null
  val animate = visible && !reduceMotion
  var peeking by remember { mutableStateOf(false) }
  var peekId by remember { mutableStateOf<String?>(null) }
  var switching by remember { mutableStateOf(false) }
  val flash = remember { mutableStateOf<Pair<String, String?>?>(null) }
  LaunchedEffect(flash.value) {
    if (flash.value != null) {
      delay(1600)
      flash.value = null
    }
  }
  // the peek follows a chat, not a position, because the list reorders when messages arrive
  val currentChat = waiting.firstOrNull { it.id == peekId } ?: waiting.firstOrNull()
  val peekChat = if (peeking) currentChat else null
  var incognitoName by remember { mutableStateOf<Pair<String, String?>?>(null) }
  LaunchedEffect(peekChat?.id) {
    val contact = (peekChat?.chatInfo as? ChatInfo.Direct)?.contact
    if (contact != null && contact.contactConnIncognito) {
      incognitoName = peekChat.id to chatModel.controller.apiContactInfo(peekChat.remoteHostId, contact.contactId)?.second?.displayName
    }
  }
  val peek = when {
    !peeking -> null
    peekChat == null -> {
      val others = users.filter { !it.user.activeUser }.joinToString(", ") { it.user.displayName }
      CalmPeek(
        user.displayName,
        stringResource(MR.strings.calm_home_new_chats_see),
        image = user.image,
        identity = if (others.isEmpty()) null else stringResource(MR.strings.calm_home_swipe_to_be, others),
        identityName = others
      )
    }
    else -> calmChatPeek(
      peekChat, user, incognitoName?.takeIf { it.first == peekChat.id }?.second,
      hideNames = hidePreviews,
      showText = chatModel.showChatPreviews.value && previewMode == NotificationPreviewMode.MESSAGE,
      index = max(0, waiting.indexOf(peekChat)), total = waiting.size
    )
  }
  val description = listOfNotNull(
    stringResource(MR.strings.calm_home_you_are, user.displayName),
    when {
      waiting.isEmpty() -> stringResource(MR.strings.calm_home_nobody_waiting)
      hidePreviews -> stringResource(MR.strings.calm_home_someone_waiting)
      else -> stringResource(MR.strings.calm_home_n_waiting, waiting.size)
    },
    if (otherProfileWaiting) stringResource(MR.strings.calm_home_other_profile_messages) else null,
    if (request != null) stringResource(MR.strings.calm_home_request_waiting) else null,
  ).joinToString(". ")

  fun openChat(chat: Chat) {
    if (stopped || chatModel.chatId.value == chat.id) return
    scope.launch {
      when (chat.chatInfo) {
        is ChatInfo.Direct -> directChatAction(chat.remoteHostId, chat.chatInfo.contact, chatModel)
        is ChatInfo.Group -> groupChatAction(chat.remoteHostId, chat.chatInfo.groupInfo, chatModel)
        else -> {}
      }
    }
  }
  fun openRequest() {
    if (stopped || request == null) return
    when (request.chatInfo) {
      is ChatInfo.ContactRequest -> contactRequestAlertDialog(request.remoteHostId, request.chatInfo, chatModel) { onRequestAccepted(it) }
      // a request that came as a contact is accepted inside the chat
      else -> openChat(request)
    }
  }

  Box(
    Modifier
      .fillMaxSize()
      .padding(
        top = topPaddingToContent(false),
        bottom = WindowInsets.navigationBars.asPaddingValues().calculateBottomPadding() + if (oneHandUI) AppBarHeight * fontSizeSqrtMultiplier else 0.dp
      )
  ) {
    CalmHomeLayout(
      theme = CurrentColors.collectAsState().value.base,
      shapeIndex = shapeIndex,
      description = description,
      waiting = waiting.isNotEmpty(),
      breathMillis = when {
        waiting.isEmpty() || !animate -> 0
        // a faster breath would tell anyone watching how many people wrote
        hidePreviews || waiting.size == 1 -> 2600
        waiting.size == 2 -> 1900
        else -> 1350
      },
      otherProfileWaiting = otherProfileWaiting,
      requestWaiting = request != null,
      orbit = animate,
      canSwitchProfile = !stopped && users.size > 1,
      peek = peek,
      flash = flash.value,
      allChatsAtTop = oneHandUI,
      onPeek = {
        peekId = if (it) waiting.firstOrNull()?.id else null
        peeking = it
      },
      onPeekCycle = { dir ->
        if (waiting.size > 1) {
          peekId = waiting[(max(0, waiting.indexOfFirst { it.id == peekId }) + dir).mod(waiting.size)].id
        }
      },
      onOpenWaiting = {
        if (!stopped && currentChat != null) {
          peeking = false
          openChat(currentChat)
        }
      },
      onSwitchProfile = { dir ->
        if (!stopped && users.size > 1 && !switching) {
          val next = users[(shapeIndex + dir).mod(users.size)].user
          switching = true
          withBGApi {
            try {
              switchToUser(next)
            } finally {
              switching = false
            }
            if (chatModel.currentUser.value?.userId == next.userId) {
              val n = chatModel.chats.value.count(::calmChatWaiting)
              flash.value = next.displayName to when {
                n == 0 -> generalGetString(MR.strings.calm_home_nobody_waiting_here)
                hidePreviews -> generalGetString(MR.strings.calm_home_someone_waiting)
                else -> String.format(generalGetString(MR.strings.calm_home_n_waiting_here), n)
              }
            }
          }
        }
      },
      onTap = {
        when {
          request != null && waiting.isEmpty() && !stopped -> openRequest()
          waiting.isNotEmpty() -> flash.value = generalGetString(MR.strings.calm_home_hold_to_read) to null
          else -> flash.value = String.format(generalGetString(MR.strings.calm_home_you_are), user.displayName) to
              generalGetString(if (otherProfileWaiting) MR.strings.calm_home_other_profile_messages else MR.strings.calm_home_nobody_waiting)
        }
      },
      onNewChat = onNewChat,
      onRequest = ::openRequest,
      onAllChats = onAllChats,
    )
  }
}

@Composable
private fun calmChatPeek(chat: Chat, user: User, incognitoName: String?, hideNames: Boolean, showText: Boolean, index: Int, total: Int): CalmPeek {
  val cInfo = chat.chatInfo
  val groupInfo = cInfo.groupInfo_
  val incognito = groupInfo?.membership?.memberIncognito ?: (cInfo is ChatInfo.Direct && cInfo.contact.contactConnIncognito)
  val myName = when {
    groupInfo != null && incognito -> groupInfo.membership.memberProfile.displayName
    incognito -> incognitoName
    else -> user.displayName
  }
  val name = if (hideNames) stringResource(MR.strings.calm_home_someone) else cInfo.displayName
  // like the chat list preview: no deleted or live text, and in a group only the message that mentions you, unless all messages notify
  val ci = chat.chatItems.lastOrNull()?.takeIf {
    showText && !it.chatDir.sent && it.meta.itemDeleted == null && !it.isDeletedContent && !it.meta.isLive &&
        (groupInfo == null || cInfo.chatSettings?.enableNtfs == MsgFilter.All || it.meta.userMention)
  }
  val mc = ci?.content?.msgContent
  return CalmPeek(
    title = name,
    subtitle = stringResource(
      when {
        incognito -> MR.strings.calm_home_joined_incognito
        groupInfo != null && chat.chatStats.unreadMentions > 0 -> MR.strings.calm_home_group_mentioned
        groupInfo != null -> MR.strings.chat_banner_group
        else -> MR.strings.calm_home_contact
      }
    ),
    image = if (hideNames) null else cInfo.image,
    icon = if (groupInfo != null) MR.images.ic_supervised_user_circle_filled else MR.images.ic_account_circle_filled,
    sender = if (groupInfo != null) ci?.memberDisplayName else null,
    text = if (mc is MsgContent.MCChat) mc.chatLink.displayName else ci?.text(cInfo.isChannel)?.ifEmpty { null },
    // an incognito name belongs to one chat, so it is not shown when names are hidden
    identity = myName?.takeIf { !(hideNames && incognito) }?.let {
      stringResource(if (incognito) MR.strings.calm_home_knows_you_as_incognito else MR.strings.calm_home_knows_you_as, name, it)
    },
    identityName = myName,
    incognito = incognito,
    // with hidden previews the footer does not say how many are waiting
    footer = if (total > 1 && !hideNames) stringResource(MR.strings.calm_home_n_of_m, index + 1, total) else stringResource(MR.strings.calm_home_let_go_to_close),
  )
}

@Composable
fun CalmHomeLayout(
  theme: DefaultTheme,
  shapeIndex: Int,
  description: String,
  waiting: Boolean,
  breathMillis: Int,
  otherProfileWaiting: Boolean,
  requestWaiting: Boolean,
  orbit: Boolean,
  canSwitchProfile: Boolean,
  peek: CalmPeek?,
  flash: Pair<String, String?>?,
  allChatsAtTop: Boolean,
  onPeek: (Boolean) -> Unit,
  onPeekCycle: (Int) -> Unit,
  onOpenWaiting: () -> Unit,
  onSwitchProfile: (Int) -> Unit,
  onTap: () -> Unit,
  onNewChat: () -> Unit,
  onRequest: () -> Unit,
  onAllChats: () -> Unit,
) {
  val (fill, rim) = calmColors(theme)
  BoxWithConstraints(Modifier.fillMaxSize()) {
    // room for the All chats button, and for the flash text below the shape
    val topReserve = if (allChatsAtTop) 72.dp else 0.dp
    val bottomReserve = if (allChatsAtTop) 0.dp else 72.dp
    val centerY = max(min(maxHeight * 0.56f, maxHeight - bottomReserve - 136.dp), topReserve + CALM_SIZE / 2)
    val pressed = remember { mutableStateOf(false) }
    val swipe = remember { mutableStateOf(0) }
    val pressSpec = if (pressed.value) tween<Float>(120, easing = calmEaseOut) else tween(420, easing = calmOvershoot)
    val press = animateFloatAsState(if (pressed.value) 1f else 0f, pressSpec)
    val swipeShift = animateFloatAsState(swipe.value.toFloat(), pressSpec)

    // breathing runs only while someone is waiting, eases in and out, and keeps its phase when the period changes
    val breathAmp = animateFloatAsState(if (breathMillis > 0) 1f else 0f, tween(400))
    val breathFading by remember { derivedStateOf { breathAmp.value > 0f } }
    val breathPeriod = remember { mutableStateOf(2600) }
    val breathPhase = remember { mutableStateOf(0f) }
    SideEffect { if (breathMillis > 0) breathPeriod.value = breathMillis }
    if (breathMillis > 0 || breathFading) {
      LaunchedEffect(Unit) {
        var last = withFrameNanos { it }
        while (true) {
          val now = withFrameNanos { it }
          breathPhase.value = (breathPhase.value + (now - last) / 1_000_000f / breathPeriod.value) % 1f
          last = now
        }
      }
    }

    if (requestWaiting) {
      val angle = if (orbit) rememberInfiniteTransition().animateFloat(0f, 2 * PI.toFloat(), infiniteRepeatable(tween(11000, easing = LinearEasing))) else null
      val requestDescription = stringResource(MR.strings.calm_home_request_waiting)
      Box(
        Modifier
          .align(Alignment.TopCenter)
          .offset {
            val r = 104.dp.toPx()
            val a = angle?.value ?: 0f
            IntOffset((r * sin(a)).roundToInt(), (centerY.toPx() - r * cos(a) - 22.dp.toPx()).roundToInt())
          }
          .size(44.dp)
          .clip(CircleShape)
          .clickable(onClick = onRequest)
          .semantics { contentDescription = requestDescription },
        contentAlignment = Alignment.Center
      ) {
        Box(Modifier.size(18.dp).background(fill, CircleShape).border(1.5.dp, rim ?: Color.Transparent, CircleShape))
      }
    }

    val onPeekState = rememberUpdatedState(onPeek)
    val onPeekCycleState = rememberUpdatedState(onPeekCycle)
    val onOpenWaitingState = rememberUpdatedState(onOpenWaiting)
    val onSwitchProfileState = rememberUpdatedState(onSwitchProfile)
    val onTapState = rememberUpdatedState(onTap)
    val onNewChatState = rememberUpdatedState(onNewChat)
    val haptic = LocalHapticFeedback.current
    val openNextLabel = stringResource(MR.strings.calm_home_open_next)
    val switchProfileLabel = stringResource(MR.strings.calm_home_switch_profile)
    val newChatLabel = stringResource(MR.strings.new_chat)
    val allChatsLabel = stringResource(MR.strings.calm_home_all_chats)
    Box(
      Modifier
        .align(Alignment.TopCenter)
        .offset(y = centerY - CALM_SIZE / 2)
        .size(CALM_SIZE)
        .semantics {
          contentDescription = description
          role = Role.Button
          onClick { if (waiting) onOpenWaiting() else onTap(); true }
          customActions = listOfNotNull(
            if (waiting) CustomAccessibilityAction(openNextLabel) { onOpenWaiting(); true } else null,
            if (canSwitchProfile) CustomAccessibilityAction(switchProfileLabel) { onSwitchProfile(1); true } else null,
            CustomAccessibilityAction(newChatLabel) { onNewChat(); true },
            CustomAccessibilityAction(allChatsLabel) { onAllChats(); true },
          )
        }
        .pointerInput(Unit) {
          awaitEachGesture {
            val down = awaitFirstDown()
            // right click on desktop peeks at once, like a long press
            var held = currentEvent.buttons.isSecondaryPressed
            var moved = false
            var ended = false
            var position = down.position
            var cycleX = down.position.x
            pressed.value = true
            if (held) onPeekState.value(true)
            suspend fun AwaitPointerEventScope.track() {
              while (!ended) {
                val change = awaitPointerEvent().changes.firstOrNull { it.id == down.id }
                if (change == null || !change.pressed) {
                  ended = true
                  return
                }
                change.consume()
                position = change.position
                val d = position - down.position
                if (!held) {
                  if (d.getDistance() > viewConfiguration.touchSlop) moved = true
                  swipe.value = if (d.x > 30.dp.toPx()) 1 else if (d.x < -30.dp.toPx()) -1 else 0
                } else if (d.y < -80.dp.toPx()) {
                  ended = true
                  onOpenWaitingState.value()
                } else if (abs(position.x - cycleX) > 70.dp.toPx()) {
                  haptic.performHapticFeedback(HapticFeedbackType.TextHandleMove)
                  onPeekCycleState.value(if (position.x < cycleX) 1 else -1)
                  cycleX = position.x
                }
              }
            }
            if (!held) withTimeoutOrNull(320) { track() }
            if (!ended && !moved && !held) {
              held = true
              haptic.performHapticFeedback(HapticFeedbackType.LongPress)
              onPeekState.value(true)
            }
            track()
            pressed.value = false
            swipe.value = 0
            val d = position - down.position
            if (held) {
              onPeekState.value(false)
            } else if (moved) {
              if (abs(d.x) > 50.dp.toPx() && abs(d.y) < 60.dp.toPx()) onSwitchProfileState.value(if (d.x < 0) 1 else -1)
            } else {
              val second = withTimeoutOrNull(viewConfiguration.doubleTapTimeoutMillis) { awaitFirstDown() }
              if (second == null) {
                onTapState.value()
              } else {
                waitForUpOrCancellation()
                onNewChatState.value()
              }
            }
          }
        }
        .graphicsLayer {
          val p = breathPhase.value
          val b = 1f + 0.13f * breathAmp.value * calmEaseInOut.transform(if (p < 0.5f) p * 2 else (1 - p) * 2)
          val s = swipeShift.value
          val sx = 1 + 0.07f * press.value
          val sy = 1 - 0.1f * press.value
          scaleX = b * (sx + (0.92f - sx) * abs(s))
          scaleY = b * (sy + (1.04f - sy) * abs(s))
          translationX = 26.dp.toPx() * s
        }
    ) {
      // without motion (system setting, or the window is not focused) a still ring says someone is waiting
      CalmShape(shapeIndex, theme, Modifier.fillMaxSize(), ring = if (waiting && breathMillis == 0) MaterialTheme.colors.onBackground else null)
    }

    if (otherProfileWaiting) {
      Box(Modifier.align(Alignment.TopCenter).offset(y = centerY + 76.dp).size(8.dp).alpha(0.7f).background(MaterialTheme.colors.secondary, CircleShape))
    }

    AnimatedContent(
      flash,
      Modifier.align(Alignment.TopCenter).offset(y = centerY + 92.dp).fillMaxWidth().padding(horizontal = 24.dp),
      transitionSpec = { fadeIn() togetherWith fadeOut() },
      contentKey = { it }
    ) { f ->
      if (f != null) {
        Column(Modifier.fillMaxWidth().semantics { liveRegion = LiveRegionMode.Polite }, horizontalAlignment = Alignment.CenterHorizontally) {
          Text(f.first, color = MaterialTheme.colors.onBackground, fontSize = 17.sp, fontWeight = FontWeight.SemiBold, textAlign = TextAlign.Center)
          f.second?.let {
            Text(it, Modifier.padding(top = 2.dp), color = MaterialTheme.colors.secondary, fontSize = 14.sp, textAlign = TextAlign.Center)
          }
        }
      }
    }

    // the card sits above the shape; on short screens it keeps a usable height and covers the shape instead
    val peekHeight = max(centerY - CALM_SIZE / 2 - 20.dp - topReserve, min(220.dp, maxHeight - topReserve))
    val rise = with(LocalDensity.current) { 16.dp.roundToPx() }
    val pop = CubicBezierEasing(0.34f, 1.3f, 0.64f, 1f)
    Box(Modifier.offset(y = topReserve).fillMaxWidth().height(peekHeight).padding(horizontal = 14.dp), contentAlignment = Alignment.BottomCenter) {
      AnimatedContent(
        peek,
        transitionSpec = {
          (fadeIn(tween(200)) + slideInVertically(tween(280, easing = pop)) { rise } +
              scaleIn(tween(280, easing = pop), initialScale = 0.96f, transformOrigin = TransformOrigin(0.5f, 1f))) togetherWith fadeOut(tween(150))
        },
        contentKey = { it != null }
      ) { p ->
        if (p != null) CalmPeekCard(p, shapeIndex, theme)
      }
    }

    Column(
      Modifier
        .align(if (allChatsAtTop) Alignment.TopCenter else Alignment.BottomCenter)
        .padding(top = if (allChatsAtTop) 14.dp else 0.dp, bottom = if (allChatsAtTop) 0.dp else 14.dp)
        .clip(RoundedCornerShape(12.dp))
        .clickable(role = Role.Button, onClick = onAllChats)
        .padding(horizontal = 18.dp, vertical = 12.dp)
        .alpha(0.75f),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      if (!allChatsAtTop) {
        Box(Modifier.padding(bottom = 8.dp).size(36.dp, 4.dp).background(MaterialTheme.colors.onBackground.copy(alpha = 0.12f), RoundedCornerShape(2.dp)))
      }
      Text(allChatsLabel, color = MaterialTheme.colors.secondary, fontSize = 14.sp, fontWeight = FontWeight.Medium)
    }
  }
}

@Composable
private fun CalmPeekCard(p: CalmPeek, shapeIndex: Int, theme: DefaultTheme) {
  val shape = RoundedCornerShape(24.dp)
  Column(
    Modifier
      .fillMaxWidth()
      .shadow(16.dp, shape)
      .background(MaterialTheme.colors.surface, shape)
      .padding(start = 16.dp, top = 16.dp, end = 16.dp, bottom = 12.dp),
    verticalArrangement = Arrangement.spacedBy(10.dp)
  ) {
    Row(verticalAlignment = Alignment.CenterVertically) {
      ProfileImage(38.dp, p.image, p.icon)
      Column(Modifier.padding(start = 10.dp)) {
        Text(p.title, color = MaterialTheme.colors.onSurface, fontSize = 17.sp, fontWeight = FontWeight.Bold, maxLines = 1, overflow = TextOverflow.Ellipsis)
        Text(p.subtitle, color = MaterialTheme.colors.secondary, fontSize = 13.sp)
      }
    }
    if (p.text != null) {
      // the message gives up its height first when the screen is short
      Box(Modifier.fillMaxWidth(0.86f).weight(1f, fill = false)) {
        Column(Modifier.background(MaterialTheme.colors.onSurface.copy(alpha = 0.06f), RoundedCornerShape(18.dp)).padding(horizontal = 13.dp, vertical = 9.dp)) {
          if (p.sender != null) {
            Text(p.sender, Modifier.padding(bottom = 2.dp), color = MaterialTheme.colors.secondary, fontSize = 12.sp, maxLines = 1, overflow = TextOverflow.Ellipsis)
          }
          Text(p.text, color = MaterialTheme.colors.onSurface, fontSize = 16.sp, lineHeight = 21.sp, maxLines = 4, overflow = TextOverflow.Ellipsis)
        }
      }
    }
    if (p.identity != null) {
      Divider()
      Row(verticalAlignment = Alignment.CenterVertically) {
        if (p.incognito) {
          Box(Modifier.size(14.dp).border(2.dp, Indigo, CircleShape))
        } else {
          CalmShape(shapeIndex, theme, Modifier.size(14.dp), mark = MaterialTheme.colors.onSurface)
        }
        val nameStyle = SpanStyle(color = MaterialTheme.colors.onSurface, fontWeight = FontWeight.SemiBold)
        val text = buildAnnotatedString {
          append(p.identity)
          val start = p.identityName?.let { p.identity.lastIndexOf(it) } ?: -1
          if (start >= 0) {
            val end = start + p.identityName!!.length
            addStyle(nameStyle, start, end)
            val rest = (end until p.identity.length).firstOrNull { p.identity[it].isLetter() }
            if (p.incognito && rest != null) addStyle(SpanStyle(color = Indigo), rest, p.identity.length)
          }
        }
        Text(text, Modifier.padding(start = 8.dp), color = MaterialTheme.colors.secondary, fontSize = 14.sp)
      }
    }
    if (p.footer != null) {
      Row {
        Text(stringResource(MR.strings.calm_home_slide_up_to_reply), color = MaterialTheme.colors.secondary, fontSize = 12.sp)
        Spacer(Modifier.weight(1f))
        Text(p.footer, color = MaterialTheme.colors.secondary, fontSize = 12.sp)
      }
    }
  }
}

@Composable
private fun CalmShape(shapeIndex: Int, theme: DefaultTheme, modifier: Modifier, ring: Color? = null, mark: Color? = null) {
  val target = calmShapes[shapeIndex.mod(calmShapes.size)]
  val spec = tween<Float>(500, easing = CubicBezierEasing(0.34f, 1.4f, 0.64f, 1f))
  val width = animateFloatAsState(target[0], spec)
  val height = animateFloatAsState(target[1], spec)
  val corner = animateFloatAsState(target[2], spec)
  val rotation = animateFloatAsState(target[3], spec)
  val (fill, rim) = calmColors(theme)
  Canvas(modifier) {
    val w = size.width * width.value
    val h = size.height * height.value
    val topLeft = Offset((size.width - w) / 2, (size.height - h) / 2)
    val radius = min(w, h) * corner.value
    val path = Path().apply {
      addRoundRect(RoundRect(Rect(topLeft, Size(w, h)), CornerRadius(radius)))
    }
    rotate(rotation.value) {
      if (mark != null) {
        drawPath(path, mark)
        return@rotate
      }
      if (ring != null) {
        val gap = 7.5.dp.toPx()
        drawRoundRect(ring, topLeft - Offset(gap, gap), Size(w + 2 * gap, h + 2 * gap), CornerRadius(radius + gap), Stroke(3.dp.toPx()))
      }
      drawPath(path, fill)
      clipPath(path) {
        // the glint keeps its size and stays upright on every shape
        rotate(-rotation.value) {
          val glint = topLeft + Offset(size.width * 0.22f, size.height * 0.16f)
          rotate(-28f, glint + Offset(size.width * 0.13f, size.height * 0.075f)) {
            drawOval(Color.White.copy(alpha = 0.2f), glint, Size(size.width * 0.26f, size.height * 0.15f))
          }
        }
        if (rim != null) drawPath(path, rim, style = Stroke(3.dp.toPx()))
      }
    }
  }
}

@Composable
fun CalmHomeButton(onClick: () -> Unit) {
  val description = stringResource(MR.strings.calm_home_back)
  IconButton(onClick, Modifier.semantics { contentDescription = description }) {
    CalmShape(calmShapeIndex(calmUsers(chatModel)), CurrentColors.collectAsState().value.base, Modifier.size(30.dp))
  }
}
