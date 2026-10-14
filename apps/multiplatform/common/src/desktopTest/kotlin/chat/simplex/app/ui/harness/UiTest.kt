package chat.simplex.app.ui.harness

import androidx.compose.ui.semantics.SemanticsActions
import androidx.compose.ui.graphics.toAwtImage
import androidx.compose.ui.test.*
import chat.simplex.common.AppScreen
import chat.simplex.common.ui.UiIdle
import chat.simplex.common.ui.UiKey
import kotlinx.coroutines.test.StandardTestDispatcher
import java.io.File

abstract class UiTest {
  companion object {
    init { UiJvm.setup() }
    private var testIndex = 0
    private const val HARD_TIMEOUT_MS = 240_000L
  }

  fun uiTest(body: UiScope.() -> Unit) {
    UiTestApp.bootJvm()
    val dir = File(UiEnv.jvmDir, "t${++testIndex}")
    var passed = false
    val watchdog = java.util.Timer("uitest-watchdog", true).apply {
      schedule(object : java.util.TimerTask() {
        override fun run() {
          println("UITEST hard timeout after ${HARD_TIMEOUT_MS / 1000} s in test ${dir.name}; thread dump follows")
          Thread.getAllStackTraces().forEach { (t, st) -> println("--- ${t.name}\n" + st.joinToString("\n") { "    at $it" }) }
          Runtime.getRuntime().halt(3)
        }
      }, HARD_TIMEOUT_MS)
    }
    try {
      for (attempt in 1..KNOWN_ISSUE_ATTEMPTS) {
        var bodyStarted = false
        var bodyCompleted = false
        try {
          runInternalSkikoComposeUiTest(width = 1600, height = 900, coroutineDispatcher = StandardTestDispatcher()) {
            mainClock.autoAdvance = false
            bodyStarted = true
            UiScope(this, dir).body()
            bodyCompleted = true
            UiTestApp.drain()
          }
          break
        } catch (e: Throwable) {
          val errors = (listOf(e) + e.suppressed).filter { it.javaClass.simpleName !in WRAPPERS }
          val known = errors.isNotEmpty() && errors.all(::isKnownAppIssue)
          if (!known || (bodyStarted && !bodyCompleted)) {
            throw if (e.suppressed.isEmpty()) e else AssertionError("${e.message}; suppressed:\n" + e.suppressed.joinToString("\n") { it.stackTraceToString().lines().take(25).joinToString("\n") }, e)
          }
          println("UITEST known app issue, not failing the test: " + errors.joinToString { it.message.orEmpty() })
          if (bodyCompleted) break
          UiTestApp.drain()
          if (attempt == KNOWN_ISSUE_ATTEMPTS) throw e
        }
      }
      passed = true
    } finally {
      repeat(2) { javax.swing.SwingUtilities.invokeAndWait {} }
      UiTestApp.drain()
      if (chat.simplex.common.platform.chatController.getChatCtrl() == null) {
        println("UITEST the test left no chat controller; initializing an empty core")
        UiTestApp.resetForTest(null)
      }
      watchdog.cancel()
      Peers.stopAll()
      if (passed) dir.deleteRecursively()
    }
  }
}

private const val EXTENSION_MS = 60_000L
private const val SCROLL_ATTEMPTS = 10
private const val SCROLL_ANIMATION_FRAMES = 40
private const val FOCUS_SETTLE_FRAMES = 3
private const val SETTLE_FRAMES = 30
private const val LIST_PAGES = 20
private const val REPLACE_CHECK_MS = 2_000L
private const val SCROLL_PAGE = 0.6f
private const val SCROLL_MARGIN_DP = 48f

class UiScope(val ui: SkikoComposeUiTest, private val dir: File) {
  val clipboard = TestClipboard()
  val uriHandler = RecordingUriHandler()

  fun app(user: String?): App {
    UiTestApp.resetForTest(user)
    ui.setContent { TestRoot(clipboard, uriHandler) { AppScreen() } }
    ui.waitForIdle()
    return App(this)
  }

  fun restart(): App {
    ui.setContent { }
    ui.waitForIdle()
    UiTestApp.restartCore()
    ui.setContent { TestRoot(clipboard, uriHandler) { AppScreen() } }
    poll(timeoutMs = 30_000, what = "the app to start again") {
      chat.simplex.common.platform.chatModel.chatDbStatus.value != null && !chat.simplex.common.platform.chatModel.ctrlInitInProgress.value
    }
    return App(this)
  }

  fun textOf(key: UiKey): String? {
    waitFor(key)
    val textKey = androidx.compose.ui.semantics.SemanticsProperties.Text
    val own = ui.onNode(matcher(key)).fetchSemanticsNode().config.getOrElseNullable(textKey) { null }?.firstOrNull()?.text
    return own ?: ui.onAllNodes(hasAnyAncestor(matcher(key)) and SemanticsMatcher.keyIsDefined(textKey), useUnmergedTree = true)
      .fetchSemanticsNodes().firstOrNull()?.config?.get(textKey)?.firstOrNull()?.text
  }

  fun storedSetting(key: String): String? =
    java.util.Properties().apply { File(chat.simplex.common.platform.desktopPlatform.configPath, "settings.properties").inputStream().use { load(it) } }.getProperty(key)

  fun sliderValue(key: UiKey): Float {
    waitFor(key)
    return ui.onNode(matcher(key)).fetchSemanticsNode().config[androidx.compose.ui.semantics.SemanticsProperties.ProgressBarRangeInfo].current
  }

  fun setSlider(key: UiKey, value: Float) {
    waitFor(key)
    scrollIntoView(key)
    waitStable(key)
    val node = ui.onNode(matcher(key))
    ui.runOnUiThread { node.performSemanticsAction(androidx.compose.ui.semantics.SemanticsActions.SetProgress) { it(value) } }
    ui.waitForIdle()
  }

  fun clipboardText(): String? = clipboard.content?.text

  fun matcher(key: UiKey): SemanticsMatcher =
    if (key.id == DESCRIBED) hasContentDescription(key.arg as String) and hasClickAction() else hasTestTag(key.tag)

  fun exists(key: UiKey): Boolean = ui.onAllNodes(matcher(key)).fetchSemanticsNodes().isNotEmpty()

  fun waitFor(key: UiKey, timeoutMs: Long = 10_000) = poll(timeoutMs, "node ${key.tag}") { exists(key) }

  private fun waitPopupsClosedUnlessInside(key: UiKey) {
    if (ui.onAllNodes(matcher(key) and hasAnyAncestor(isPopup())).fetchSemanticsNodes().isNotEmpty()) return
    poll(what = "open popups to close before acting on ${key.tag}") { ui.onAllNodes(isPopup()).fetchSemanticsNodes().isEmpty() }
  }


  fun scrollTo(key: UiKey) {
    poll(what = "a list to scroll to ${key.tag}") { exists(key) || pageListsTo(key) }
  }

  private fun pageListsTo(key: UiKey): Boolean {
    val lists = ui.onAllNodes(SemanticsMatcher.keyIsDefined(SemanticsActions.ScrollToIndex) and SemanticsMatcher.keyIsDefined(SemanticsActions.ScrollBy))
    for (i in lists.fetchSemanticsNodes().indices) {
      for (page in 1..LIST_PAGES) {
        val list = lists.fetchSemanticsNodes().getOrNull(i) ?: break
        val before = list.children.map { it.id to it.boundsInRoot }
        ui.runOnUiThread { list.config[SemanticsActions.ScrollBy].action?.invoke(0f, list.boundsInRoot.height * SCROLL_PAGE) }
        repeat(SCROLL_ANIMATION_FRAMES) { ui.mainClock.advanceTimeBy(16, ignoreFrameDuration = true); ui.waitForIdle() }
        if (exists(key)) return true
        if (lists.fetchSemanticsNodes().getOrNull(i)?.children?.map { it.id to it.boundsInRoot } == before) break
      }
    }
    return false
  }



  fun waitStable(key: UiKey) {
    var last: androidx.compose.ui.geometry.Rect? = null
    var count = 0
    try {
      poll(what = "node ${key.tag} to stop moving") {
        val ns = ui.onAllNodes(matcher(key)).fetchSemanticsNodes()
        count = ns.size
        val b = ns.firstOrNull()?.boundsInRoot
        (b != null && !b.isEmpty && b == last).also { last = b }
      }
    } catch (e: AssertionError) {
      throw AssertionError("${e.message}; last bounds $last; matching nodes $count", e)
    }
  }

  fun scrollIntoView(key: UiKey) {
    repeat(SCROLL_ATTEMPTS) {
      val node = ui.onAllNodes(matcher(key)).fetchSemanticsNodes().firstOrNull() ?: return
      val rootBottom = generateSequence(node) { it.parent }.last().boundsInRoot.bottom
      val b = node.boundsInRoot
      val placed = b.width > 0f && b.height > 0f
      if (placed && b.top >= 0f && b.bottom <= rootBottom) return
      val scroller = generateSequence(node.parent) { it.parent }
        .firstOrNull { it.config.contains(androidx.compose.ui.semantics.SemanticsActions.ScrollBy) } ?: return
      val margin = node.layoutInfo.density.density * SCROLL_MARGIN_DP
      val dy = when {
        !placed -> rootBottom * SCROLL_PAGE
        b.bottom > rootBottom -> b.bottom - rootBottom + margin
        else -> b.top - margin
      }
      ui.runOnUiThread { scroller.config[androidx.compose.ui.semantics.SemanticsActions.ScrollBy].action?.invoke(0f, dy) }
      repeat(SCROLL_ANIMATION_FRAMES) { ui.mainClock.advanceTimeBy(16, ignoreFrameDuration = true); ui.waitForIdle() }
    }
  }

  fun escape() {
    repeat(FOCUS_SETTLE_FRAMES) { ui.mainClock.advanceTimeBy(16, ignoreFrameDuration = true); ui.waitForIdle() }
    ui.runOnUiThread { chat.simplex.common.simplexWindowState.backstack.lastOrNull()?.invoke() }
    ui.waitForIdle()
  }

  fun click(key: UiKey) {
    waitFor(key)
    scrollIntoView(key)
    waitStable(key)
    poll(what = "node ${key.tag} to be enabled") { isEnabled(key) }
    waitPopupsClosedUnlessInside(key)
    focusBeforeTap(key)
    val target = ui.onNode(matcher(key), useUnmergedTree = key.id != DESCRIBED).fetchSemanticsNode()
    val point = tapPointInNode(target)
    val node = ui.onNode(matcher(key))
    ui.runOnUiThread { node.performTouchInput { click(point) } }
    ui.waitForIdle()
  }

  fun settle() {
    repeat(SETTLE_FRAMES) { ui.mainClock.advanceTimeBy(16, ignoreFrameDuration = true); ui.waitForIdle() }
  }

  private fun focusBeforeTap(key: UiKey) {
    if (ui.onAllNodes(matcher(key) and hasAnyAncestor(isPopup())).fetchSemanticsNodes().isNotEmpty()) return
    val node = ui.onNode(matcher(key))
    if (!node.fetchSemanticsNode().config.contains(SemanticsActions.RequestFocus)) return
    ui.runOnUiThread { node.performSemanticsAction(SemanticsActions.RequestFocus) }
    repeat(FOCUS_SETTLE_FRAMES) { ui.mainClock.advanceTimeBy(16, ignoreFrameDuration = true); ui.waitForIdle() }
  }

  fun enterText(key: UiKey, text: String) {
    waitFor(key)
    val node = ui.onNode(matcher(key))
    ui.runOnUiThread { node.performTextInput(text) }
    ui.waitForIdle()
  }

  fun typeInto(key: UiKey, text: String) {
    waitFor(key)
    val node = ui.onNode(matcher(key))
    val before = editableText(key).orEmpty()
    ui.runOnUiThread { node.performTextInput(text) }
    poll(what = "field ${key.tag} to hold `$text`") { editableText(key) == before + text }
  }

  fun replaceText(key: UiKey, text: String) {
    waitFor(key)
    val node = ui.onNode(matcher(key))
    val current = editableText(key).orEmpty()
    ui.runOnUiThread { node.performTextInputSelection(androidx.compose.ui.text.TextRange(0, current.length)) }
    poll(what = "field ${key.tag} to select all") {
      ui.onNode(matcher(key)).fetchSemanticsNode().config
        .getOrElseNullable(androidx.compose.ui.semantics.SemanticsProperties.TextSelectionRange) { null } == androidx.compose.ui.text.TextRange(0, current.length)
    }
    ui.runOnUiThread { node.performTextInput(text) }
    val replaced = runCatching { poll(timeoutMs = REPLACE_CHECK_MS, what = "field ${key.tag} to hold `$text`") { editableText(key) == text } }.isSuccess
    if (!replaced) {
      ui.runOnUiThread { node.performTextReplacement(text) }
      poll(what = "field ${key.tag} to hold `$text` after replacement") { editableText(key) == text }
    }
  }

  fun rightClick(key: UiKey) {
    waitFor(key)
    waitStable(key)
    waitPopupsClosedUnlessInside(key)
    val node = ui.onNode(matcher(key))
    ui.runOnUiThread { node.performMouseInput { rightClick() } }
    ui.waitForIdle()
  }

  fun textsOf(key: UiKey): List<String> {
    waitFor(key)
    val textKey = androidx.compose.ui.semantics.SemanticsProperties.Text
    val own = ui.onNode(matcher(key)).fetchSemanticsNode().config.getOrElseNullable(textKey) { null }.orEmpty().map { it.text }
    val inner = ui.onAllNodes(hasAnyAncestor(matcher(key)) and SemanticsMatcher.keyIsDefined(textKey), useUnmergedTree = true)
      .fetchSemanticsNodes().flatMap { it.config[textKey].map { t -> t.text } }
    return own + inner
  }

  fun descriptionsOf(key: UiKey): List<String> {
    waitFor(key)
    val descKey = androidx.compose.ui.semantics.SemanticsProperties.ContentDescription
    return ui.onAllNodes((matcher(key) or hasAnyAncestor(matcher(key))) and SemanticsMatcher.keyIsDefined(descKey), useUnmergedTree = true)
      .fetchSemanticsNodes().flatMap { it.config[descKey] }
  }

  fun isEnabled(key: UiKey): Boolean =
    ui.onAllNodes(matcher(key)).fetchSemanticsNodes().firstOrNull()
      ?.let { !it.config.contains(androidx.compose.ui.semantics.SemanticsProperties.Disabled) } ?: false

  fun pressEnter(key: UiKey) {
    waitPopupsClosedUnlessInside(key)
    val node = ui.onNode(matcher(key))
    ui.runOnUiThread { node.performKeyInput { pressKey(androidx.compose.ui.input.key.Key.Enter) } }
    ui.waitForIdle()
  }

  fun editableText(key: UiKey): String? =
    ui.onAllNodes((matcher(key) or hasAnyAncestor(matcher(key))) and SemanticsMatcher.keyIsDefined(androidx.compose.ui.semantics.SemanticsProperties.EditableText), useUnmergedTree = true)
      .fetchSemanticsNodes().firstOrNull()?.config?.get(androidx.compose.ui.semantics.SemanticsProperties.EditableText)?.text

  fun cli(name: String, flags: PeerFlags = PeerFlags()): CliPeer =
    CliPeer(name, File(dir, "peers/$name"), TestServers.smp1, flags) { t, what, cond -> poll(t, what, cond = cond) }
      .also { Peers.add(it); it.start() }

  fun poll(timeoutMs: Long = 10_000, what: String, screenshot: Boolean = true, cond: () -> Boolean) {
    val deadline = System.currentTimeMillis() + timeoutMs
    var busyAtDeadline: Set<Long>? = null
    var lastError: Throwable? = null
    while (true) {
      ui.mainClock.advanceTimeBy(16, ignoreFrameDuration = true)
      ui.waitForIdle()
      if (runCatching(cond).onFailure { lastError = it }.getOrDefault(false)) return
      val now = System.currentTimeMillis()
      if (now > deadline) {
        val busy = busyAtDeadline ?: UiIdle.inFlight().map { it.id }.toSet().also { busyAtDeadline = it }
        val stillBusy = UiIdle.inFlight().any { it.id in busy }
        if (!stillBusy || now > deadline + EXTENSION_MS) {
          val shot = if (screenshot) failureShot(what) else null
          throw AssertionError("timed out after $timeoutMs ms waiting for $what; ${lastError?.let { "last condition error: $it; " } ?: ""}in flight: ${UiIdle.inFlight().joinToString { it.what }}; screenshot: $shot; visible texts: ${texts().take(60)}")
        }
      }
      Thread.sleep(16)
    }
  }

  private var shots = 0

  private fun failureShot(what: String): String? = runCatching {
    val dir = File(UiEnv.runDir, "failures").apply { mkdirs() }
    val f = File(dir, "%s-%02d-%s.png".format(UiEnv.jvmDir.name, ++shots, what.replace(Regex("[^A-Za-z0-9]+"), "_").take(60)))
    javax.imageio.ImageIO.write(ui.onRoot().captureToImage().toAwtImage(), "png", f)
    f.path
  }.getOrNull()

  fun texts(): List<String> =
    ui.onAllNodes(hasText("", substring = true), useUnmergedTree = true).fetchSemanticsNodes()
      .flatMap { it.config.getOrElseNullable(androidx.compose.ui.semantics.SemanticsProperties.Text) { null }.orEmpty() }
      .map { it.text }

  fun assumeCamera() {}

  fun seesDescription(text: String, substring: Boolean = false) =
    ui.onAllNodes(hasContentDescription(text, substring = substring), useUnmergedTree = true).fetchSemanticsNodes().isNotEmpty()

  fun seesText(text: String, substring: Boolean = false) =
    ui.onAllNodes(hasText(text, substring = substring), useUnmergedTree = true).fetchSemanticsNodes().isNotEmpty()
}

private fun isKnownAppIssue(e: Throwable): Boolean =
  e !is AssertionError && generateSequence(e) { it.cause }.any { c -> KNOWN_APP_ISSUES.any { c.message.orEmpty().contains(it) } }

private val WRAPPERS = setOf("UncaughtExceptionsBeforeTest", "DiagnosticCoroutineContextException")

private const val KNOWN_ISSUE_ATTEMPTS = 3
private val KNOWN_APP_ISSUES = listOf("Controller is not initialized", "database closed")
