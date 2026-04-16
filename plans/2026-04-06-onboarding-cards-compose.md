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

### Architecture

The overlay is the PRIMARY UI surface during onboarding. ALL interaction happens inside it — card taps, toolbar button modals, everything. `ModalManager.start` renders INTO the overlay instead of into the start panel.

### Overlay structure in `DesktopScreen` (App.kt)

Two visual layers in the overlay, both full-width:

1. **Background layer:** covers center+end area only (padded left by start panel width). Opaque `MaterialTheme.colors.background`. Hides center panel content ("No selected chat") while leaving start panel fully visible underneath.

2. **Content layer:** full window width, no background. Cards render here, centered in the full window. Clicks outside cards fall through to the start panel below.

Both layers have top/bottom padding for toolbar height (`AppBarHeight * fontSizeSqrtMultiplier`).

```kotlin
if (shouldShowOnboarding()) {
  val oneHandUI = remember { appPrefs.oneHandUI.state }
  val toolbarPadding = AppBarHeight * fontSizeSqrtMultiplier
  val topPad = if (!oneHandUI.value) toolbarPadding else 0.dp
  val bottomPad = if (oneHandUI.value) toolbarPadding else 0.dp

  // Background — center+end only
  Box(
    Modifier
      .fillMaxSize()
      .padding(start = DEFAULT_START_MODAL_WIDTH * fontSizeSqrtMultiplier, top = topPad, bottom = bottomPad)
      .background(MaterialTheme.colors.background)
  )

  // Content — full width, cards centered
  Box(
    Modifier
      .fillMaxSize()
      .padding(top = topPad, bottom = bottomPad),
    contentAlignment = Alignment.Center
  ) {
    ConnectOnboardingView()
  }
}
```

Z-order: above panels and vertical divider, below `ModalManager.fullscreen`.

### Start panel modal redirection

During onboarding, `ModalManager.start.showInView()` renders INSIDE the overlay instead of in the start panel Box.

In `DesktopScreen`:
```kotlin
// Start panel modals — normal location
Box(Modifier.widthIn(max = DEFAULT_START_MODAL_WIDTH * fontSizeSqrtMultiplier)) {
  if (!shouldShowOnboarding()) {
    ModalManager.start.showInView()
  }
  SwitchingUsersView()
}
```

Inside `ConnectOnboardingView`, on desktop:
- Watch `ModalManager.start.hasModalsOpen`
- When a start modal opens (from toolbar + button, avatar, or card tap):
  1. Cards shift RIGHT and fade to ~30% opacity (animated)
  2. `ModalManager.start.showInView()` renders on the LEFT side of the overlay with left-to-right slide animation
  3. This is the FIRST modal opening — it slides left-to-right
  4. Subsequent modals within the start modal stack open right-to-left as usual (standard `ModalManager` behavior inside the rendered area)
- When all start modals close: reverse animation — modal area slides left, cards restore position and opacity
- Clicking a faded card triggers `ModalManager.start.closeModals()` to dismiss and restore cards. This requires swapping card `onClick` handlers when `startModalsOpen` is true — each card's onClick becomes `{ ModalManager.start.closeModals() }` instead of its normal action.

```kotlin
// Inside ConnectOnboardingView, desktop only:
val startModalsOpen = ModalManager.start.hasModalsOpen
val cardOffset by animateFloatAsState(if (startModalsOpen) 0.3f else 0f)
val cardAlpha by animateFloatAsState(if (startModalsOpen) 0.3f else 1f)
val modalSlide by animateFloatAsState(if (startModalsOpen) 0f else -1f)

Box(Modifier.fillMaxSize()) {
  // Modal area — slides from left
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

  // Cards — shift right and fade when modal open
  Box(
    Modifier
      .fillMaxSize()
      .graphicsLayer {
        if (appPlatform.isDesktop) {
          translationX = cardOffset * size.width
          alpha = cardAlpha
        }
      }
  ) {
    HorizontalPager(...) { /* pages */ }
  }
}
```

Card taps use `ModalManager.start.showModalCloseable` on ALL platforms — same code. On Android, the modal renders in the normal start panel modal area. On desktop during onboarding, the modal renders inside the overlay via the redirected `showInView()`.

### Card tap actions — same on all platforms

```kotlin
val openConnectViaLink = {
  ModalManager.start.showModalCloseable { close ->
    NewChatView(chatModel.currentRemoteHost.value, NewChatOption.CONNECT, ..., close = close)
  }
}
```

No platform branching needed. `ModalManager.start` handles the modal lifecycle. Only the rendering location changes.

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

When onboarding active: center panel shows nothing (overlay covers it visually).

### Desktop HorizontalPager: tap only

`userScrollEnabled = !appPlatform.isDesktop` — disables mouse swipe on desktop.

## Revision 2 — Bug fixes from initial implementation

### Fix 1: `fillMaxSize()` overrides `widthIn(max:)`

In both page composables, the Column has `Modifier.fillMaxSize().widthIn(max = 500.dp)`. `fillMaxSize()` sets width to maximum, overriding the `widthIn` constraint.

Fix: `Modifier.fillMaxHeight().widthIn(max = 500.dp)` — only fill height, let widthIn cap the width.

### Fix 2: Modifier order — background before padding

In the overlay Box: `.fillMaxSize().background(color).padding(...)` paints background over toolbar area.

Fix: `.fillMaxSize().padding(...).background(color)` — padding first, background only fills content area.

Superseded by the two-layer approach above — background layer is separate from content layer.

### Fix 3: "You have no chats" text dropped

The `when` block in `ChatListWithLoadingScreen` replaced two independent `if` blocks with mutually exclusive branches, dropping the "You have no chats" case.

Fix: revert to the original `if` block structure, adding onboarding as the first check:
```kotlin
private fun BoxScope.ChatListWithLoadingScreen(searchText, listState) {
  if (shouldShowOnboarding()) {
    if (appPlatform.isAndroid) {
      ConnectOnboardingView()
    }
  } else {
    if (!chatModel.desktopNoUserNoRemote) {
      ChatList(searchText = searchText, listState)
    }
    if (chatModel.chats.value.isEmpty() && !chatModel.switchingUsersAndHosts.value && !chatModel.desktopNoUserNoRemote) {
      Text(stringResource(
        if (chatModel.chatRunning.value == null) MR.strings.loading_chats else MR.strings.you_have_no_chats
      ), Modifier.align(Alignment.Center), color = MaterialTheme.colors.secondary)
    }
  }
  // Auto-dismiss
  LaunchedEffect(chatModel.chats.value.size) { ... }
}
```

This preserves the original loading/empty behavior exactly. The onboarding branch is checked first — when active, it replaces everything. When inactive, original code runs unchanged.

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
