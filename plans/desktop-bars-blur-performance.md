# Plan: make the desktop bars blur lightweight

## Context

Settings > Appearance > Blur blurs the app bars: `blurredBackgroundModifier` puts a `BlurEffect` on the bar's layer
and draws a copy of the scrolled content inside it, so the blur is recomputed every frame. On desktop this made
scrolling visibly laggy at the default radius of 50.

## Measurements

Measured on the software renderer (`SKIKO_RENDER_API=SOFTWARE`), 1376x768 window, density 1, scrolling a chat of 70
messages. Frames were counted by capturing the window repeatedly and counting distinct images.

| | blur off | blur 50 |
|---|---|---|
| visible frames per second while scrolling | 14.5 | 5.2 - 8.7 |

A standalone harness that replays the same drawing through Skiko's `RenderNode` puts the cost of one bar at 29 ms
(radius 30), 42 ms (radius 50) and 71 ms (radius 100). Two bars are drawn in a chat, so at the default radius the
bars alone cost about 80 ms per frame.

## Cause

Skia's raster blur runs at full resolution: `Raster8888BlurAlgorithm` only rescales its input above sigma 135
(`SkBlurEngine.cpp`), and every radius the slider offers is below that. Each bar therefore blurs
`width x (bar + 6 sigma)` pixels per frame, which at radius 100 is the full window width by 404 rows.

The obvious remedy, blurring a downscaled copy the way Skia's own GPU path does, cannot be expressed by scaling the
layer: **a layer's image filter is evaluated in device space**, so any scale on the layer or the canvas is applied to
the sigma as well. Narrowing a bar eightfold and blurring it with an eighth of the sigma measured 68.5 ms against
68.4 ms for the untouched blur, and looked wrong, because the reduced sigma ended up applied at full resolution.
This is why the blur appeared weak enough to leave text under the bar readable.

## Fix

Draw the bar into an offscreen Skia surface that is up to 8x narrower, blur it there, where the surface's own
resolution is the device resolution and the sigma is not scaled back up, then stretch the finished blur across the
bar. Only the width is reduced: a bar is `AppBarHeight` tall, and taking rows away from it visibly weakens the blur,
which the harness confirmed and which was visible in the app.

`drawBarsBlurred` is an `expect`/`actual` because it needs Skia surfaces directly. Android is untouched: it blurs
through a `RenderEffect` on the layer, where the GPU blur rescales internally, so it never had this cost.

Blurring a copy has one consequence for when the bars draw. Previously the bar recorded `drawLayer(graphicsLayer)`, a
live reference that replays whatever the content layer holds at playback, so the bar did not have to be redrawn when
the content scrolled. Reading the content into a surface fixes it at the moment the bar draws instead, so the bar has
to be redrawn whenever that copy is re-recorded, which `AppBarHandler.contentVersion` provides: `copyViewToAppBar`
bumps it after each recording and the bar reads it. Without it the blur inside a chat lagged behind the content,
while the chat list looked correct because its bar happened to be invalidated by other state.

The surfaces are kept per size rather than one pair overall, because the desktop window's panes have different
widths and their bars are drawn in the same frame, which would otherwise reallocate both surfaces for every bar.

How far the copy is reduced follows from the blur itself. The width is divided until the blur's own sigma would fall
below 1.5, past which the narrowed copy is barely blurred and stretching it back out shows the steps of the narrowing
rather than a blur. The rows are halved once, with the vertical sigma halved to match. The rule has to be written in
terms of the reduced sigma rather than the radius: stopping as soon as the reduced sigma reached 4, as a first version
did, left the small radiuses barely narrowed at all and made radius 10 cost more than radius 50.

The blur is the surface's own layer paint rather than a copy between two surfaces, which is both one allocation fewer
and, measured with the surfaces reused as the app reuses them, 3-11% quicker.

Per bar, against the unmodified blur, with surfaces reused:

| radius | before | after | difference from the original |
|---|---|---|---|
| 10 | 17.9 ms | 2.7 ms | 0.41 / 255 |
| 50 | 40.9 ms | 3.0 ms | 0.23 / 255 |
| 100 | 72.7 ms | 5.0 ms | 0.27 / 255 |

The difference column is the root mean square difference over the bar, before the bar's own tint is applied, so what
reaches the screen is smaller still. Redrawing the bar and stretching the blur back over it cost 0.5 ms of those
figures whatever the radius, so that is the floor this approach can reach.

## Keeping the bars in step with the content

A layer's own filter follows the content it is attached to for free. Blurring a copy does not: the copy is taken when
the bar draws, so the bar has to be redrawn whenever the content it copies has moved. `AppBarHandler.contentVersion`
carries that, and the scroll containers bump it from a collector on the state they actually scroll.

Two details are load-bearing. The bump must not happen while drawing: an earlier version bumped it inside
`copyViewToAppBar`, and because Compose Desktop redraws the whole scene per frame, the content's draw invalidated the
bar, which requested another frame, which drew the content again. Measured idle, with no interaction: **96% of a core**
against 0% with the blur off and 0% for the unmodified app. And the version has to come from the state the container
actually scrolls, not from the handler's own: a chat supplies its own `LazyListState`, so watching the handler's left
the chat's blur stale while the chat list looked correct.

## Rejected

Reusing the blurred copy while the content is unchanged was measured and dropped: it saves nothing while scrolling,
which is the case that lags, and the chat wallpaper is recorded into its own layer without touching the content
version, so a cached blur could outlast a wallpaper change.

Blurring the bars of both panes in one pass does not apply: only `DefaultAppBar` is blurred, one per screen, and the
two on a desktop window belong to panes of different widths side by side rather than stacked.

## Verification

Built as an AppImage and checked by eye across the whole slider range at bar alpha 0.5, where the blur is most
exposed, on top of the chat list and inside a chat.
