# Desktop call server: pick a free port when `localhost:50395` is busy

Branch: `nd/fix-call-bind-port` · code commit `587b79779` · PR [#6963](https://github.com/simplex-chat/simplex-chat/pull/6963).

## 1. Problem statement

On Desktop, a WebRTC call runs in the system browser, served by an embedded `NanoWSD` HTTP+WebSocket server. `startServer()` bound that server to a hard-coded port, `SERVER_PORT = 50395` (`apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/views/call/CallView.desktop.kt:23`). If port 50395 was already in use — another instance of the app, a leftover server thread, or any unrelated process — `NanoHTTPD.start()` propagated the bind failure and the call could not start:

```
java.net.BindException: Address already in use: bind
	at java.base/sun.nio.ch.Net.bind0(Native Method)
	at java.base/sun.nio.ch.Net.bind(Unknown Source)
	at java.base/sun.nio.ch.Net.bind(Unknown Source)
	at java.base/sun.nio.ch.NioSocketImpl.bind(Unknown Source)
	at java.base/java.net.ServerSocket.bind(Unknown Source)
	at java.base/java.net.ServerSocket.bind(Unknown Source)
	at org.nanohttpd.protocols.http.ServerRunnable.run(ServerRunnable.java:63)
	at java.base/java.lang.Thread.run(Unknown Source)
```

A call should not be a single point of contention on one fixed TCP port. When 50395 is taken, the call should bind a different port and proceed.

Scope: Desktop only. Android renders the call in an in-process `WebView` via `WebViewAssetLoader` — no local server, no port — and is unaffected.

## 2. Solution summary

Three changes, all in `CallView.desktop.kt`, plus a one-line spec note. Total diff: 2 files, +24 / −15.

1. **`startServer` retries on a free port.** It gains a `port: Int = SERVER_PORT` parameter (used only by the retry; the single existing call site is unchanged by the default). `server.start()` is wrapped: on `BindException`, log a warning, stop the half-initialised server, and recurse once with `port = 0` — which makes the OS assign any free port. The recursion terminates because `port == 0` rethrows (the kernel does not hand out a busy ephemeral port).
2. **`WebRTCController` opens the browser at the port actually bound.** Previously it opened `http://localhost:50395/simplex/call/` *before* calling `startServer`; now it starts the server first and uses `server.listeningPort` for the URL — which equals `50395` in the normal case, and equals the OS-assigned port after a fallback.
3. **Spec note** in `apps/multiplatform/spec/services/calls.md` describing the fallback.

```diff
+import java.net.BindException

   val server = remember {
-    try {
-      uriHandler.openUri("http://${SERVER_HOST}:$SERVER_PORT/simplex/call/")
-    } catch (e: Exception) {
-      ... endCall() ...
-    }
-    startServer(onResponse)
+    startServer(onResponse).apply {
+      try {
+        uriHandler.openUri("http://${SERVER_HOST}:${listeningPort}/simplex/call/")
+      } catch (e: Exception) {
+        ... endCall() ...
+      }
+    }
   }

-fun startServer(onResponse: (WVAPIMessage) -> Unit): NanoWSD {
-  val server = object: NanoWSD(SERVER_HOST, SERVER_PORT) { /* unchanged */ }
-  server.start(60_000_000)
+fun startServer(onResponse: (WVAPIMessage) -> Unit, port: Int = SERVER_PORT): NanoWSD {
+  val server = object: NanoWSD(SERVER_HOST, port) { /* unchanged */ }
+  try {
+    server.start(60_000_000)
+  } catch (e: BindException) {
+    if (port == 0) throw e
+    Log.w(TAG, "Call server port $port is busy, using a random port: ${e.message}")
+    server.stop()
+    return startServer(onResponse, port = 0)
+  }
   return server
 }
```

The `NanoWSD` object body (request handling, resource serving) is untouched.

## 3. Root cause / how NanoHTTPD binds

`startServer()` builds an anonymous `NanoWSD(SERVER_HOST, SERVER_PORT)` and calls `server.start(60_000_000)`. Inside `NanoHTTPD.start(timeout)`:

```java
this.myServerSocket = this.getServerSocketFactory().create();
this.myServerSocket.setReuseAddress(true);
ServerRunnable serverRunnable = createServerRunnable(timeout);
this.myThread = new Thread(serverRunnable);
this.myThread.start();
while (!serverRunnable.hasBinded() && serverRunnable.getBindException() == null) { Thread.sleep(10L); }
if (serverRunnable.getBindException() != null) throw serverRunnable.getBindException();
```

`ServerRunnable.run()` does the actual `myServerSocket.bind(new InetSocketAddress(hostname, myPort))` on its own thread; if that throws (port in use → `java.net.BindException`, a subclass of `IOException`), it stores the exception, returns, and the accept loop is never entered. `start()` observes the stored exception and rethrows it — which is why the stack trace in the report shows `ServerRunnable.run` rather than `NanoHTTPD.start`: it is the same exception object, captured at the failed `bind`.

`setReuseAddress(true)` already handles the benign case (a just-closed server in `TIME_WAIT`), so the only way `start()` fails this way is a genuine conflict: something else is listening on `50395`. Pre-fix that exception escaped `startServer` → escaped the `remember {}` initialiser in `WebRTCController` → the call view could not establish its control channel.

A fixed port is also unnecessary on the wire. The browser page only ever connects back to *the origin it was served from*: `apps/multiplatform/common/src/commonMain/resources/assets/www/desktop/ui.js` opens `new WebSocket(`ws://${location.host}`)`, and `call.html` references its assets with root-relative paths (`/desktop/style.css`, `/call.js`, …). So the page follows whatever host:port the Kotlin side opened in the browser — there is no second place that hard-codes `50395`.

## 4. The fix in detail

### 4.1 Retry on `port = 0`

`NanoHTTPD`/`NanoWSD` accept port `0`, the standard "let the OS pick a free ephemeral port" convention; after `start()`, `getListeningPort()` (Kotlin: `listeningPort`) returns the concrete port the kernel assigned. So the retry needs no port-scanning loop and no arbitrary range — one fallback attempt, guaranteed to find a free port if one exists at all.

```kotlin
fun startServer(onResponse: (WVAPIMessage) -> Unit, port: Int = SERVER_PORT): NanoWSD {
  val server = object: NanoWSD(SERVER_HOST, port) { /* unchanged */ }
  try {
    server.start(60_000_000)
  } catch (e: BindException) {
    if (port == 0) throw e
    Log.w(TAG, "Call server port $port is busy, using a random port: ${e.message}")
    server.stop()
    return startServer(onResponse, port = 0)
  }
  return server
}
```

- `port: Int = SERVER_PORT` — the parameter exists for the recursive retry. `startServer` has exactly one caller in the tree (`WebRTCController`), and the default keeps that call site byte-identical. A default-valued parameter used for internal recursion is a routine Kotlin idiom (`fun f(x, acc = init)`).
- `catch (e: BindException)` — deliberately narrower than `IOException`. The reported failure mode is specifically "address already in use"; any *other* `start()` failure (e.g. an I/O error creating the socket) is not something a different port fixes, so it propagates exactly as before. Surgical: handle the bug, nothing else.
- `if (port == 0) throw e` — terminates the recursion. If even the OS-assigned port fails to bind, that is a pathological condition (no ephemeral ports at all); rethrow rather than loop, preserving the original "give up" behaviour on the second failure.
- `server.stop()` — `start()` assigns `myServerSocket` (an unbound `ServerSocket`) and `myThread` (which has already exited, having caught the bind error) *before* failing. `stop()` closes that orphaned socket and joins the dead thread. Pre-fix this leak was transient (the exception terminated the call attempt); now that the call *recovers* instead of failing, the half-initialised server must be released explicitly. `stop()` is the same call the existing `onDispose` already makes on the live server.

### 4.2 Start the server before opening the browser

The browser URL must carry the port the server actually bound, which is only known after `start()`. So the order in `WebRTCController`'s `remember {}` is inverted: `startServer` first, then `uriHandler.openUri`.

```kotlin
val server = remember {
  startServer(onResponse).apply {
    try {
      uriHandler.openUri("http://${SERVER_HOST}:${listeningPort}/simplex/call/")
    } catch (e: Exception) {
      Log.e(TAG, "Unable to open browser: ${e.stackTraceToString()}")
      AlertManager.shared.showAlertMsg(
        title = generalGetString(MR.strings.unable_to_open_browser_title),
        text = generalGetString(MR.strings.unable_to_open_browser_desc)
      )
      endCall()
    }
  }
}
```

- `.apply { … }` keeps the whole thing one memoized expression that yields the `NanoWSD` (as before), with no `val server = …; …; server` shadowing, and reads as "start the server, then (side effect) open the browser at its port". `listeningPort` resolves on the `apply` receiver.
- In the normal case `listeningPort == 50395`, so the opened URL is character-for-character what it was pre-fix — the browser keeps its per-origin permissions (camera/mic are granted to `localhost:50395`). Only a fallback changes the origin, and only for that call.
- Side benefit: pre-fix the browser was launched *before* the server's `start()` returned, so the page could (briefly) hit a not-yet-listening socket and rely on its own retry; now the server is provably listening before the browser is told about it. Strictly safer ordering.
- The error handling (alert + `endCall()`) is preserved verbatim; only its position moved.

### 4.3 Spec note

`apps/multiplatform/spec/services/calls.md` (the file the code links back to) gains one sentence on the NanoWSD bullet — "If that port is already in use it falls back to an OS-assigned free port (`port 0`); `WebRTCController` reads `server.listeningPort` for the browser URL" — and the WebRTCController bullet now reads "Starts the server, then opens `http://localhost:<listeningPort>/simplex/call/` (normally `50395`)".

## 5. Why this specific shape — alternatives considered

- **Always bind `port = 0`, drop the fixed port.** Simplest possible code, no retry. Rejected: browser permissions (camera/mic, autoplay) are scoped per *origin* = `scheme://host:port`. A port that changes every call would re-prompt the user for camera/mic on every call. Keeping `50395` as the primary value preserves the granted permission; `0` is the *fallback*, used only on conflict.
- **Scan a fixed range (`50395, 50396, … 50404`).** More "predictable-ish" than an ephemeral port, but it can still be exhausted, needs a loop with an off-by-one boundary, and re-introduces the very problem (a finite set of fixed ports) in miniature. `port = 0` delegates the search to the kernel — one call, can't be exhausted while any port is free. Standard idiom; NanoHTTPD supports it directly.
- **Catch `IOException` instead of `BindException`.** Broader than the bug. A non-bind `start()` failure isn't fixed by retrying on another port; let it propagate. A narrow catch makes the diff describe exactly the failure it handles.
- **Extract the `NanoWSD` object into a local `fun newServer(port)` and `try { newServer(SERVER_PORT)… } catch { newServer(0)… }`.** Functionally equivalent, but it re-indents the ~25-line object body for no behavioural reason — a noisier diff. The default-parameter + tail-recursion form leaves the object body byte-identical and adds only the retry wrapper.
- **Move the browser-open into a `LaunchedEffect`.** Cleaner separation of "construct" vs "side effect", but it defers the launch past first composition (a behaviour change beyond the bug) and adds an effect to reason about. The pre-fix code already opened the browser inside `remember {}`; `.apply { }` keeps that timing while removing the only real wart (the open ran *before* the server existed).
- **Update every doc that mentions `localhost:50395`** (`product/flows/calling.md`, `product/glossary.md`, `product/rules.md`, `product/views/call.md`). Out of scope here: `50395` is still the primary port and those are higher-level narrative docs; only `spec/services/calls.md` (which the code references and which describes the exact mechanism) is updated. A follow-up can sweep the rest if desired.

## 6. Verification

- `./gradlew :common:compileKotlinDesktop` → `BUILD SUCCESSFUL` (only pre-existing deprecation warnings; nothing in the changed file).
- A full Linux x86_64 AppImage was built from this branch and launched (Compose software renderer in the test VM); the desktop app starts normally.
- Manual, normal path: starting a call opens the system browser at `http://localhost:50395/simplex/call/` exactly as before; the WebSocket connects and the call proceeds.
- Manual, fallback path: occupy the port first (e.g. `python3 -m http.server 50395`, or `nc -l 50395`) and then start a call → the log shows `Call server port 50395 is busy, using a random port: …`, the browser is opened at the OS-assigned port, the page's `ws://${location.host}` WebSocket connects to that same port, and the call proceeds.

## 7. Risk and rollback

- **Blast radius**: `startServer` and the `remember {}` initialiser in `WebRTCController`, Desktop only. Android (WebView, no server) is untouched; iOS is unrelated.
- The fallback branch executes only when `50395` is genuinely occupied — rare. The common path is unchanged except for the start-then-open ordering, which is strictly safer.
- Per-origin browser permissions are preserved on the common path (port unchanged); a fallback resets them for that one call — a clear improvement over the call failing outright.
- **Rollback**: `git revert 587b79779` (and drop the commit before merge if desired). No data, schema, or protocol surface is touched.
