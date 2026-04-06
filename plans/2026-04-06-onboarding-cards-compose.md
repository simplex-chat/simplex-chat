# Onboarding Cards — Compose (Android/Desktop) Implementation Plan

References the layout specification in `plans/2026-04-06-onboarding-cards-ios.md`.

## Scope

Same as iOS: Screens 1 and 2 with paging transition. Modal sheets for deeper views. No banner, no standalone onboarding variants.

## New file

`common/src/commonMain/kotlin/chat/simplex/common/views/newchat/OnboardingCards.kt`

## Assets

8 card stub SVGs needed in `assets/default/MR/images/` (same names as the real PNGs, with `.svg` extension):
- `card_let_someone_connect_to_you_alpha.svg` / `_light.svg`
- `card_connect_via_link_alpha.svg` / `_light.svg`
- `card_invite_someone_privately_alpha.svg` / `_light.svg`
- `card_create_your_public_address_alpha.svg` / `_light.svg`

Real PNGs already generated in art repo `multiplatform/resources/MR/images/`.

## Onboarding condition (shared by Android and Desktop)

Placed in `ConnectOnboardingView.kt` as top-level functions, accessible from both `ChatListView.kt` and `App.kt`:

```kotlin
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
```

`shouldShowOnboarding` is `@Composable` (reads reactive state) and public — called from both `ChatListView.kt` and `App.kt`. `noConversationChatsYet` is a pure function, also public (used by auto-dismiss LaunchedEffect).

### Auto-dismiss

```kotlin
LaunchedEffect(chatModel.chats.value.size) {
  if (!noConversationChatsYet(chatModel.chats.value)) {
    appPrefs.addressCreationCardShown.set(true)
  }
}
```

Placed in `ChatListWithLoadingScreen`.

## Android integration

### In `ChatListView.kt` — `ChatListWithLoadingScreen` (line 291)

Change from:
```kotlin
private fun BoxScope.ChatListWithLoadingScreen(searchText, listState) {
  if (!chatModel.desktopNoUserNoRemote) { ChatList(...) }
  if (chatModel.chats.value.isEmpty() && ...) { Text("Loading/empty") }
}
```

To:
```kotlin
private fun BoxScope.ChatListWithLoadingScreen(searchText, listState) {
  val chats = chatModel.chats.value
  when {
    chats.isEmpty() && !chatModel.switchingUsersAndHosts.value
      && !chatModel.desktopNoUserNoRemote && chatModel.chatRunning.value == null -> {
      Text(stringResource(MR.strings.loading_chats), Modifier.align(Alignment.Center), color = MaterialTheme.colors.secondary)
    }
    shouldShowOnboarding() -> {
      if (appPlatform.isAndroid) {
        ConnectOnboardingView()
      }
      // Desktop: empty — overlay in DesktopScreen handles it
    }
    !chatModel.desktopNoUserNoRemote -> {
      ChatList(searchText = searchText, listState)
    }
  }
  // Auto-dismiss
  LaunchedEffect(chats.size) {
    if (chats.isNotEmpty() && !noConversationChatsYet(chats)) {
      appPrefs.addressCreationCardShown.set(true)
    }
  }
}
```

Toolbar is a sibling in the parent `Box` (lines 150-174), stays visible.

## Desktop integration

### Layout: full-width overlay with custom modal animation

Cards display in a full-width overlay (centered in window). On desktop, cards are ALWAYS vertical — the window is tall enough, and vertical layout leaves natural space on the left for the modal slide-in animation.

Cards have a max width (~500dp) and center horizontally in the overlay.

### In `App.kt` — `DesktopScreen` (line 410)

Add a full-width overlay as a sibling Box (same z-order pattern as user picker scrim at line 432). Place before `ModalManager.fullscreen.showInView()`:

```kotlin
if (shouldShowOnboarding()) {
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  Box(
    Modifier
      .fillMaxSize()
      .background(MaterialTheme.colors.background)
      .padding(
        top = if (!oneHandUI.value) AppBarHeight * fontSizeSqrtMultiplier else 0.dp,
        bottom = if (oneHandUI.value) AppBarHeight * fontSizeSqrtMultiplier else 0.dp
      )
  ) {
    ConnectOnboardingView()
  }
}
```

- Full-width overlay, cards centered in window
- `.background(MaterialTheme.colors.background)` hides panel content
- Toolbar padding: `AppBarHeight * fontSizeSqrtMultiplier`
- Z-order: above panels, below `ModalManager.fullscreen`

### Desktop modal animation: left-to-right slide with card dissolve

When a card tap opens a deeper view (connect via link, invite, address) on desktop:

1. The deeper view slides in from the LEFT edge, moving left-to-right (opposite of standard)
2. Simultaneously, the cards shift RIGHT and fade to ~30% opacity
3. The deeper view fills the left portion; faded cards are ghosted on the right
4. Closing: reverse — deeper view slides left, cards restore position and full opacity

Implementation: inside `ConnectOnboardingView`, manage desktop modal content as local state (NOT via `ModalManager`). On Android, card taps use `ModalManager.start` as usual.

```kotlin
// Desktop only — local modal state
var desktopModalContent by remember { mutableStateOf<(@Composable (close: () -> Unit) -> Unit)?>(null) }
val showModal = desktopModalContent != null
val modalOffset by animateFloatAsState(if (showModal) 0f else -1f)
val cardOffsetFraction by animateFloatAsState(if (showModal) 0.3f else 0f)
val cardAlpha by animateFloatAsState(if (showModal) 0.3f else 1f)

Box(Modifier.fillMaxSize()) {
  // Cards — shifted and faded
  Box(Modifier.fillMaxSize().graphicsLayer {
    translationX = cardOffsetFraction * size.width
    alpha = cardAlpha
  }) {
    HorizontalPager(...) { /* card pages */ }
  }

  // Desktop modal — slides from left
  if (desktopModalContent != null) {
    Box(Modifier.fillMaxSize().graphicsLayer {
      translationX = modalOffset * size.width
    }) {
      desktopModalContent?.invoke { desktopModalContent = null }
    }
  }
}
```

Card tap actions choose path by platform:
```kotlin
val openModal: (@Composable (close: () -> Unit) -> Unit) -> Unit = { content ->
  if (appPlatform.isDesktop) {
    desktopModalContent = content
  } else {
    ModalManager.start.showModalCloseable { close -> content(close) }
  }
}
```

### Suppress "No selected chat" in `CenterPartOfScreen` (line 373)

```kotlin
null -> {
  if (!shouldShowOnboarding() && !rememberUpdatedState(ModalManager.center.hasModalsOpen()).value) {
    Box(...) { Text(stringResource(...)) }
  } else if (!shouldShowOnboarding()) {
    ModalManager.center.showInView()
  }
}
```

When onboarding active: center panel shows nothing (overlay handles cards).

### Desktop HorizontalPager: tap only

`userScrollEnabled = !appPlatform.isDesktop` — disables mouse swipe on desktop, tap/button navigation only.

## ConnectOnboardingView composable

### Structure

```kotlin
@Composable
fun ConnectOnboardingView() {
  val pagerState = rememberPagerState(initialPage = 0) { 2 }
  val scope = rememberCoroutineScope()

  HorizontalPager(state = pagerState, userScrollEnabled = true) { page ->
    when (page) {
      0 -> TalkToSomeonePage(
        onLetSomeoneConnect = { scope.launch { pagerState.animateScrollToPage(1) } },
        onConnectViaLink = { ModalManager.start.showModalCloseable { close ->
          NewChatView(chatModel.currentRemoteHost.value, NewChatOption.CONNECT, showQRCodeScanner = appPlatform.isAndroid, close = close)
        }}
      )
      1 -> ConnectWithSomeonePage(
        onBack = { scope.launch { pagerState.animateScrollToPage(0) } },
        onInviteSomeone = { ModalManager.start.showModalCloseable { close ->
          NewChatView(chatModel.currentRemoteHost.value, NewChatOption.INVITE, close = close)
        }},
        onCreateAddress = { ModalManager.start.showModalCloseable { close ->
          UserAddressView(chatModel = chatModel, shareViaProfile = false, autoCreateAddress = true, close = close)
        }}
      )
    }
  }
}
```

### Page layout

Each page uses `BoxWithConstraints` to compute card dimensions:

```kotlin
@Composable
private fun TalkToSomeonePage(onLetSomeoneConnect: () -> Unit, onConnectViaLink: () -> Unit) {
  BoxWithConstraints(Modifier.fillMaxSize()) {
    val isLandscape = maxWidth > maxHeight
    val padding = 16.dp
    val spacing = 16.dp
    val cardWidth = if (isLandscape) (maxWidth - padding * 2 - spacing) / 2 else maxWidth - padding * 2
    val maxCardHeight = cardWidth * 0.75f

    Column(Modifier.fillMaxSize(), horizontalAlignment = Alignment.CenterHorizontally) {
      pageHeader("Talk to someone", showBack = false, isLandscape = isLandscape)
      Spacer(Modifier.weight(1f).defaultMinSize(minHeight = 16.dp))
      cardPair(isLandscape, padding, spacing, maxCardHeight) {
        // card1 and card2
      }
      Spacer(Modifier.weight(1f).defaultMinSize(minHeight = 16.dp))
    }
  }
}
```

### pageHeader composable

Shared by both pages. No duplication:

```kotlin
@Composable
private fun pageHeader(title: String, showBack: Boolean, isLandscape: Boolean, onBack: (() -> Unit)? = null) {
  val titleView = @Composable {
    Text(
      stringResource(title),
      style = MaterialTheme.typography.h1,  // largeTitle equivalent
      fontWeight = FontWeight.Bold,
      maxLines = 1,
      overflow = TextOverflow.Ellipsis,
      textAlign = TextAlign.Center,
      modifier = Modifier.fillMaxWidth()
    )
  }
  if (isLandscape) {
    Box(Modifier.fillMaxWidth().padding(horizontal = 16.dp)) {
      if (showBack && onBack != null) {
        backButton(onBack, Modifier.align(Alignment.CenterStart))
      }
      titleView()
    }
  } else {
    Column(Modifier.fillMaxWidth().padding(horizontal = 16.dp)) {
      if (showBack && onBack != null) {
        backButton(onBack, Modifier.align(Alignment.Start))
      } else {
        Spacer(Modifier.height(AppBarHeight))
      }
      titleView()
    }
  }
}
```

Back button spacer uses `AppBarHeight` (56.dp) to match the platform's back button area, not iOS's 44pt.

### cardPair composable

Shared layout helper, no card duplication:

```kotlin
@Composable
private fun cardPair(
  isLandscape: Boolean,
  padding: Dp,
  spacing: Dp,
  maxCardHeight: Dp,
  card1: @Composable () -> Unit,
  card2: @Composable () -> Unit
) {
  if (isLandscape) {
    Row(Modifier.padding(horizontal = padding), horizontalArrangement = Arrangement.spacedBy(spacing)) {
      Box(Modifier.weight(1f).heightIn(max = maxCardHeight)) { card1() }
      Box(Modifier.weight(1f).heightIn(max = maxCardHeight)) { card2() }
    }
  } else {
    Column(Modifier.padding(horizontal = padding), verticalArrangement = Arrangement.spacedBy(spacing)) {
      Box(Modifier.fillMaxWidth().heightIn(max = maxCardHeight)) { card1() }
      Box(Modifier.fillMaxWidth().heightIn(max = maxCardHeight)) { card2() }
    }
  }
}
```

### OnboardingCardView composable

```kotlin
@Composable
fun OnboardingCardView(
  imageName: ImageResource,
  imageNameLight: ImageResource,
  icon: ImageResource,
  title: String,
  subtitle: String? = null,
  labelHeightRatio: Float,
  onClick: () -> Unit
)
```

Key Compose details (from layout-compose.md checklist):
- **Image:** `contentScale = ContentScale.Fit`, `Modifier.fillMaxSize()` — scaled AND centered ✓
- **Gradient:** `Brush.linearGradient(colorStops, start, end)` with pixel Offsets computed from image area measured size via `Modifier.onSizeChanged` or `BoxWithConstraints`
- **Gradient math:** identical to iOS — same function ported to Kotlin, same angle/scale/aspect-ratio correction
- **Corner radius:** `RoundedCornerShape(24.dp)` with `Modifier.clip()`
- **Dark/light:** `if (isInDarkTheme()) imageNameLight else imageName` for image, gradient stops selected by theme
- **Conditional assets:** `if (BuildConfigCommon.SIMPLEX_ASSETS) { Image(...) }`
- **Clickable:** `Modifier.clip(RoundedCornerShape(24.dp)).clickable(onClick = onClick)` — clip first so ripple is bounded

#### Label stripe background

Use the same pattern as the toolbar (from DefaultTopAppBar.kt line 43-65):
```kotlin
MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.97f)
  .copy(alpha = appPrefs.inAppBarsAlpha.get())
```

This exactly matches the toolbar appearance, including the user's bar transparency preference.

#### Gradient in Compose

```kotlin
// Compute inside BoxWithConstraints or onSizeChanged callback
val imageAreaSize = Size(width, imageHeight)
val (startUnit, endUnit) = gradientPoints(
  aspectRatio = imageAreaSize.height / imageAreaSize.width,
  scale = if (isInDarkTheme()) 1.5f else 1.2f
)
val brush = Brush.linearGradient(
  colorStops = if (isInDarkTheme()) darkStops else lightStops,
  start = Offset(startUnit.x * imageAreaSize.width, startUnit.y * imageAreaSize.height),
  end = Offset(endUnit.x * imageAreaSize.width, endUnit.y * imageAreaSize.height)
)
```

### Card icons (Moko resource names)

Screen 1:
- "Let someone connect to you" — `MR.images.ic_add_link`
- "Connect via link or QR code" — `MR.images.ic_qr_code`

Screen 2:
- "Invite someone privately" — `MR.images.ic_add_link`
- "Create your public address" — `MR.images.ic_qr_code`

### Strings

8 new entries in `strings.xml` (`MR/base/strings.xml`):
```xml
<string name="talk_to_someone">Talk to someone</string>
<string name="let_someone_connect_to_you">Let someone connect to you</string>
<string name="connect_via_link_or_qr">Connect via link or QR code</string>
<string name="connect_with_someone">Connect with someone</string>
<string name="invite_someone_privately">Invite someone privately</string>
<string name="a_link_for_one_person">A link for one person to connect</string>
<string name="create_your_public_address">Create your public address</string>
<string name="for_anyone_to_reach_you">For anyone to reach you</string>
```

## Files changed

- `ChatListView.kt` — add `shouldShowOnboarding`, `noConversationChatsYet`, modify `ChatListWithLoadingScreen`, add auto-dismiss `LaunchedEffect`
- `App.kt` — add desktop overlay in `DesktopScreen`, suppress "No selected chat" in `CenterPartOfScreen`
- `MR/base/strings.xml` — 8 new strings
- **New:** `OnboardingCards.kt` — `ConnectOnboardingView`, `OnboardingCardView`, `TalkToSomeonePage`, `ConnectWithSomeonePage`, `pageHeader`, `cardPair`, `shouldShowOnboarding`, `noConversationChatsYet`, gradient math
- **New:** 8 stub SVGs in `assets/default/MR/images/`
