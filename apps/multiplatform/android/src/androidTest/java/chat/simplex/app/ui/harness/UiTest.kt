package chat.simplex.app.ui.harness

import android.widget.EditText
import androidx.compose.ui.semantics.SemanticsActions
import androidx.compose.ui.semantics.SemanticsProperties
import androidx.compose.ui.test.*
import androidx.compose.ui.test.junit4.ComposeTestRule
import androidx.compose.ui.test.junit4.createEmptyComposeRule
import androidx.test.core.app.ActivityScenario
import androidx.test.espresso.Espresso
import androidx.test.espresso.action.ViewActions
import androidx.test.espresso.matcher.ViewMatchers
import chat.simplex.app.MainActivity
import chat.simplex.common.model.AppPreferences
import chat.simplex.common.platform.androidAppContext
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.UiKey
import chat.simplex.common.ui.UiIdle
import android.content.Context
import org.junit.Rule
import android.app.Activity
import android.app.Instrumentation
import android.content.Intent
import androidx.test.espresso.intent.Intents
import androidx.test.espresso.intent.matcher.IntentMatchers
import androidx.core.content.FileProvider
import chat.simplex.common.helpers.APPLICATION_ID
import java.io.File

abstract class UiTest {
  @get:Rule val rule: ComposeTestRule = createEmptyComposeRule()
  @get:Rule val knownIssue = KnownIssueRule()

  fun uiTest(body: UiScope.() -> Unit) {
    val watchdog = Watchdog.start(HARD_TIMEOUT_MS)
    try {
      UiTestApp.bootOnce()
      AppCrashes.install()
      AppCrashes.clear()
      withKnownIssue(knownIssue.issue) {
        val scope = UiScope(rule)
        try {
          scope.body()
          scope.checkEnd()
        } finally {
          scope.finish()
        }
        AppCrashes.failIfAny()
      }
    } finally {
      watchdog.cancel()
    }
  }

  private companion object {
    const val HARD_TIMEOUT_MS = 300_000L
  }
}

class Watchdog private constructor(private val thread: Thread) {
  fun cancel() = thread.interrupt()

  companion object {
    fun start(timeoutMs: Long): Watchdog = Watchdog(Thread {
      try {
        Thread.sleep(timeoutMs)
      } catch (e: InterruptedException) {
        return@Thread
      }
      val dump = Thread.getAllStackTraces().entries.joinToString("\n\n") { (t, st) -> "\"${t.name}\" ${t.state}\n" + st.joinToString("\n") { "  at $it" } }
      dump.lines().forEach { android.util.Log.e("UITEST", it) }
      android.util.Log.e("UITEST", "hard timeout after $timeoutMs ms; killing the process so the runner can resume")
      android.os.Process.killProcess(android.os.Process.myPid())
    }.apply { isDaemon = true; name = "uitest-watchdog"; start() })
  }
}

class UiScope(val rule: ComposeTestRule) {
  private var scenario: ActivityScenario<MainActivity>? = null
  private val peers = mutableListOf<CliPeer>()
  private var intentsActive = false

  fun app(user: String?): App {
    Timing.phase("reset") { UiTestApp.resetForTest(user) }
    Timing.phase("launch") { launch() }
    return App(this)
  }

  fun restart(
    ready: () -> Boolean = {
      val m = chat.simplex.common.platform.chatModel
      m.chatDbStatus.value != null && !m.ctrlInitInProgress.value && m.chatRunning.value == true && m.currentUser.value != null && m.users.isNotEmpty()
    },
    what: String = "the core to start again with chat running"
  ): App {
    UiTestApp.restartCore()
    UiTestApp.onMain { CommonPrefs.reload() }
    poll(timeoutMs = 60_000, what = what) { ready() }
    Timing.phase("close") { close() }
    Timing.phase("launch") { launch() }
    return App(this)
  }

  fun exchangeDir(): File = File(androidAppContext.filesDir, "uitest-exchange").apply { mkdirs() }

  fun saveFileAs(file: File, action: () -> Unit) = answerIntent(listOf(Intent.ACTION_CREATE_DOCUMENT), file, action)

  fun openFile(file: File, action: () -> Unit) = answerIntent(listOf(Intent.ACTION_GET_CONTENT, Intent.ACTION_PICK), file, action)

  private fun answerIntent(intentActions: List<String>, file: File, action: () -> Unit) {
    val uri = FileProvider.getUriForFile(androidAppContext, "$APPLICATION_ID.provider", file)
    val result = Instrumentation.ActivityResult(Activity.RESULT_OK, Intent().setData(uri))
    intentActions.forEach { Intents.intending(IntentMatchers.hasAction(it)).respondWith(result) }
    val before = Intents.getIntents().count { it.action in intentActions }
    action()
    poll(what = "the app to ask for a file with ${intentActions.joinToString(" or ")}") { Intents.getIntents().count { it.action in intentActions } > before }
  }

  fun keyboardShown(): Boolean {
    var shown = false
    UiTestApp.onMain {
      val decor = androidx.test.runner.lifecycle.ActivityLifecycleMonitorRegistry.getInstance()
        .getActivitiesInStage(androidx.test.runner.lifecycle.Stage.RESUMED).firstOrNull()?.window?.decorView
      shown = decor != null && androidx.core.view.ViewCompat.getRootWindowInsets(decor)?.isVisible(androidx.core.view.WindowInsetsCompat.Type.ime()) == true
    }
    return shown
  }

  fun hideKeyboard() {
    runCatching { poll(timeoutMs = KEYBOARD_SHOW_MS, what = "the soft keyboard to show", screenshot = false) { keyboardShown() } }
    if (!keyboardShown()) return
    Espresso.closeSoftKeyboard()
    poll(what = "the soft keyboard to hide") { !keyboardShown() }
  }

  fun assumeCamera() {
    val cameras = androidx.test.platform.app.InstrumentationRegistry.getInstrumentation().targetContext
      .getSystemService(android.hardware.camera2.CameraManager::class.java).cameraIdList
    org.junit.Assume.assumeTrue("the device has no camera for the QR scanner", cameras.isNotEmpty())
  }

  fun checkEnd() {
    failOnPeerErrors(peers)
    waitForAppWork()
    val stuck = UiTestApp.drain()
    if (stuck.isNotEmpty()) throw AssertionError("app work still running when the test ended: ${stuck.joinToString()}")
    AppCrashes.failIfAny()
  }

  fun finish() {
    peers.forEach { it.dispose() }
    runCatching { SocksProxy.stop() }
    Timing.phase("close") { close() }
    Timing.phase("drain") { UiTestApp.drain() }
  }

  private fun launch() {
    Intents.init()
    intentsActive = true
    Intents.intending(org.hamcrest.CoreMatchers.not(IntentMatchers.isInternal())).respondWith(Instrumentation.ActivityResult(Activity.RESULT_CANCELED, null))
    scenario = ActivityScenario.launch(MainActivity::class.java)
    rule.waitForIdle()
  }

  private fun close() {
    if (intentsActive) Intents.release()
    intentsActive = false
    scenario?.close()
    scenario = null
  }

  fun cli(name: String, flags: PeerFlags = PeerFlags()): CliPeer =
    CliPeer(name, flags) { t, what, cond -> poll(t, what, cond = cond) }.also { peers += it; Timing.phase("peer $name") { it.start() } }

  fun poll(timeoutMs: Long = 10_000, what: String, screenshot: Boolean = true, cond: () -> Boolean) {
    val start = System.currentTimeMillis()
    val deadline = start + timeoutMs
    var busyAtDeadline: Set<Long>? = null
    var lastError: Throwable? = null
    var workEnded = false
    while (true) {
      rule.mainClock.advanceTimeBy(48, ignoreFrameDuration = true)
      rule.waitForIdle()
      AppCrashes.failIfAny()
      if (runCatching(cond).onFailure { lastError = it }.getOrDefault(false)) {
        Timing.wait(what, System.currentTimeMillis() - start)
        return
      }
      val now = System.currentTimeMillis()
      val busy = if (now > deadline) busyAtDeadline ?: UiIdle.inFlight().map { it.id }.toSet().also { busyAtDeadline = it } else emptySet()
      val idle = now > deadline && UiIdle.inFlight().none { it.id in busy }
      if (now > deadline && ((idle && workEnded) || now > deadline + EXTENSION_MS)) {
        val shot = if (screenshot) FailureShots.take(what) else null
        throw AssertionError("timed out after $timeoutMs ms waiting for $what; ${lastError?.let { "last condition error: $it; " } ?: ""}in flight: ${UiIdle.inFlight().joinToString { it.what }}; screenshot: $shot; visible texts: ${texts().take(60)}")
      }
      workEnded = idle
      Thread.sleep(50)
    }
  }

  fun matcher(key: UiKey): SemanticsMatcher =
    if (key.id == DESCRIBED) hasContentDescription(key.arg as String) and hasClickAction() else hasTestTag(key.tag)
  fun exists(key: UiKey): Boolean = rule.onAllNodes(matcher(key)).fetchSemanticsNodes().isNotEmpty()
  fun waitFor(key: UiKey, timeoutMs: Long = 10_000) = poll(timeoutMs, "node ${key.tag}") { exists(key) }

  private fun waitPopupsClosedUnlessInside(key: UiKey) {
    if (rule.onAllNodes(matcher(key) and hasAnyAncestor(isPopup())).fetchSemanticsNodes().isNotEmpty()) return
    poll(what = "open popups to close before acting on ${key.tag}") { rule.onAllNodes(isPopup()).fetchSemanticsNodes().isEmpty() }
  }

  fun settle() {
    rule.mainClock.advanceTimeBy(SETTLE_MS, ignoreFrameDuration = true)
    rule.waitForIdle()
  }


  fun scrollTo(key: UiKey) {
    poll(what = "a list to scroll to ${key.tag}") {
      exists(key) || rule.onAllNodes(SemanticsMatcher.keyIsDefined(SemanticsActions.ScrollToIndex)).let { lists ->
        lists.fetchSemanticsNodes().indices.any { i -> runCatching { lists[i].performScrollToNode(matcher(key)) }.isSuccess && exists(key) }
      }
    }
  }

  fun scrollChatTowardOlder(pages: Int = 1): Boolean = scrollChat(pages, towardOlder = true)

  fun scrollChatTowardNewer(pages: Int = 1): Boolean = scrollChat(pages, towardOlder = false)

  private fun scrollChat(pages: Int, towardOlder: Boolean): Boolean {
    val list = rule.onNode(SemanticsMatcher.keyIsDefined(SemanticsActions.ScrollToIndex) and SemanticsMatcher.keyIsDefined(SemanticsActions.ScrollBy) and hasAnyDescendant(chatItemNode))
    repeat(pages) {
      val items = rule.onAllNodes(chatItemNode).fetchSemanticsNodes()
      val tracked = (if (towardOlder) items.minByOrNull { it.boundsInRoot.top } else items.maxByOrNull { it.boundsInRoot.bottom })
        ?: throw AssertionError("no chat items in view to scroll the chat")
      val tag = tracked.config[SemanticsProperties.TestTag]
      val before = tracked.boundsInRoot
      val node = list.fetchSemanticsNode()
      val dy = node.boundsInRoot.height * CHAT_SCROLL_PAGE
      rule.runOnUiThread { node.config[SemanticsActions.ScrollBy].action?.invoke(0f, if (towardOlder) dy else -dy) }
      settle()
      val after = rule.onAllNodes(hasTestTag(tag)).fetchSemanticsNodes().firstOrNull()?.boundsInRoot
      if (after == before) return false
      if (after != null && (if (towardOlder) after.top <= before.top else after.bottom >= before.bottom)) {
        throw AssertionError("scrolling the chat toward ${if (towardOlder) "older" else "newer"} messages moved $tag the other way: $before -> $after")
      }
    }
    return true
  }

  fun waitStable(key: UiKey) {
    var last: androidx.compose.ui.geometry.Rect? = null
    try {
      poll(what = "node ${key.tag} to stop moving") {
        val b = rule.onAllNodes(matcher(key)).fetchSemanticsNodes().firstOrNull()?.boundsInRoot
        (b != null && !b.isEmpty && b == last).also { last = b }
      }
    } catch (e: AssertionError) {
      throw AssertionError("${e.message}; last bounds $last", e)
    }
  }

  fun click(key: UiKey) {
    waitFor(key)
    runCatching { rule.onNode(matcher(key)).performScrollTo() }
    centerInScrollParent(key)
    waitStable(key)
    poll(what = "node ${key.tag} to be enabled") { isEnabled(key) }
    waitPopupsClosedUnlessInside(key)
    val node = rule.onNode(matcher(key), useUnmergedTree = key.id != DESCRIBED).fetchSemanticsNode()
    val point = tapPointInNode(node)
    rule.onNode(matcher(key)).performTouchInput { click(point) }
    rule.waitForIdle()
  }

  private fun centerInScrollParent(key: UiKey) {
    val node = rule.onAllNodes(matcher(key)).fetchSemanticsNodes().firstOrNull() ?: return
    var parent = node.parent
    while (parent != null && !parent.config.contains(SemanticsActions.ScrollBy)) parent = parent.parent
    val scroller = parent ?: return
    val dy = node.boundsInRoot.center.y - scroller.boundsInRoot.center.y
    if (kotlin.math.abs(dy) < 1f) return
    rule.runOnUiThread { scroller.config[SemanticsActions.ScrollBy].action?.invoke(0f, dy) }
    rule.waitForIdle()
  }

  fun rightClick(key: UiKey) {
    waitFor(key)
    waitStable(key)
    waitPopupsClosedUnlessInside(key)
    rule.onNode(matcher(key)).performTouchInput { longClick() }
    rule.waitForIdle()
  }

  fun escape() {
    Espresso.pressBack()
    rule.waitForIdle()
  }

  private fun isMessageField(key: UiKey) = key.id == Ui.Chat.Message.id

  private fun messageEditText(key: UiKey) =
    Espresso.onView(org.hamcrest.CoreMatchers.allOf(ViewMatchers.isAssignableFrom(EditText::class.java), ViewMatchers.withTagValue(org.hamcrest.CoreMatchers.equalTo<Any>(key.tag))))

  private fun setMessageField(key: UiKey, value: String) {
    waitFor(key)
    waitStable(key)
    settle()
    messageEditText(key).perform(CommitText(value))
    poll(what = "the message field to hold `$value`") { editableText(key) == value }
  }

  fun enterText(key: UiKey, text: String) {
    waitFor(key)
    rule.onNode(matcher(key)).performTextInput(text)
    rule.waitForIdle()
  }

  fun typeInto(key: UiKey, text: String) {
    if (isMessageField(key)) {
      waitFor(key)
      setMessageField(key, editableText(key).orEmpty() + text)
      return
    }
    waitFor(key)
    val before = editableText(key).orEmpty()
    rule.onNode(matcher(key)).performTextInput(text)
    poll(what = "field ${key.tag} to hold `$text`") { editableText(key) == before + text }
  }

  fun replaceText(key: UiKey, text: String) {
    if (isMessageField(key)) {
      setMessageField(key, text)
      return
    }
    waitFor(key)
    rule.onNode(matcher(key)).performTextReplacement(text)
    poll(what = "field ${key.tag} to hold `$text`") { editableText(key) == text }
  }

  fun pressEnter(key: UiKey) {
    if (isMessageField(key)) click(key.part("send")) else rule.onNode(matcher(key)).performImeAction()
    rule.waitForIdle()
  }

  fun isEnabled(key: UiKey): Boolean =
    rule.onAllNodes(matcher(key)).fetchSemanticsNodes().firstOrNull()?.let { !it.config.contains(SemanticsProperties.Disabled) } ?: false

  fun editableText(key: UiKey): String? {
    if (isMessageField(key)) {
      var t: String? = null
      messageEditText(key).check { v, _ -> t = (v as? EditText)?.text?.toString() }
      return t
    }
    return rule.onAllNodes((matcher(key) or hasAnyAncestor(matcher(key))) and SemanticsMatcher.keyIsDefined(SemanticsProperties.EditableText), useUnmergedTree = true)
      .fetchSemanticsNodes().firstOrNull()?.config?.get(SemanticsProperties.EditableText)?.text
  }

  fun hintOf(key: UiKey): String? {
    if (!isMessageField(key)) return if (editableText(key).isNullOrEmpty()) textsOf(key).firstOrNull() else null
    waitFor(key)
    var hint: String? = null
    messageEditText(key).check { v, _ -> hint = (v as? EditText)?.takeIf { it.text.isNullOrEmpty() }?.hint?.toString() }
    return hint
  }

  fun textsOf(key: UiKey): List<String> {
    waitFor(key)
    val own = rule.onNode(matcher(key)).fetchSemanticsNode().config.getOrElseNullable(SemanticsProperties.Text) { null }.orEmpty().map { it.text }
    val inner = rule.onAllNodes(hasAnyAncestor(matcher(key)) and SemanticsMatcher.keyIsDefined(SemanticsProperties.Text), useUnmergedTree = true)
      .fetchSemanticsNodes().flatMap { it.config[SemanticsProperties.Text].map { t -> t.text } }
    return own + inner
  }

  fun textOf(key: UiKey): String? = textsOf(key).firstOrNull()

  fun descriptionsOf(key: UiKey): List<String> {
    waitFor(key)
    return rule.onAllNodes((matcher(key) or hasAnyAncestor(matcher(key))) and SemanticsMatcher.keyIsDefined(SemanticsProperties.ContentDescription), useUnmergedTree = true)
      .fetchSemanticsNodes().flatMap { it.config[SemanticsProperties.ContentDescription] }
  }

  fun texts(): List<String> =
    rule.onAllNodes(hasText("", substring = true), useUnmergedTree = true).fetchSemanticsNodes()
      .flatMap { it.config.getOrElseNullable(SemanticsProperties.Text) { null }.orEmpty() }.map { it.text }

  fun seesDescription(text: String, substring: Boolean = false) =
    rule.onAllNodes(hasContentDescription(text, substring = substring), useUnmergedTree = true).fetchSemanticsNodes().isNotEmpty()

  fun seesText(text: String, substring: Boolean = false) =
    rule.onAllNodes(hasText(text, substring = substring), useUnmergedTree = true).fetchSemanticsNodes().isNotEmpty()

  fun storedSetting(key: String): String? =
    androidAppContext.getSharedPreferences(AppPreferences.SHARED_PREFS_ID, Context.MODE_PRIVATE).all[key]?.toString()

  fun sliderValue(key: UiKey): Float {
    waitFor(key)
    return rule.onNode(matcher(key)).fetchSemanticsNode().config[SemanticsProperties.ProgressBarRangeInfo].current
  }

  fun setSlider(key: UiKey, value: Float) {
    waitFor(key)
    runCatching { rule.onNode(matcher(key)).performScrollTo() }
    centerInScrollParent(key)
    waitStable(key)
    rule.onNode(matcher(key)).performSemanticsAction(SemanticsActions.SetProgress) { it(value) }
    rule.waitForIdle()
  }

  fun clipboardText(): String? =
    Intents.getIntents().asReversed().firstNotNullOfOrNull { i ->
      val inner = if (i.action == Intent.ACTION_CHOOSER) i.getParcelableExtra(Intent.EXTRA_INTENT) as? Intent else i
      inner?.getStringExtra(Intent.EXTRA_TEXT)
    } ?: (androidAppContext.getSystemService(Context.CLIPBOARD_SERVICE) as android.content.ClipboardManager).primaryClip?.getItemAt(0)?.text?.toString()

  fun setClipboardText(text: String?) = UiTestApp.onMain {
    val cm = androidAppContext.getSystemService(Context.CLIPBOARD_SERVICE) as android.content.ClipboardManager
    if (text != null) cm.setPrimaryClip(android.content.ClipData.newPlainText("uitest", text))
    else if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.P) cm.clearPrimaryClip()
    else error("clearing the clipboard needs Android 9")
  }
}

object FailureShots {
  private var n = 0

  fun take(what: String): String? = runCatching {
    val inst = androidx.test.platform.app.InstrumentationRegistry.getInstrumentation()
    val dir = java.io.File(androidAppContext.getExternalFilesDir(null) ?: error("no external files dir"), "failures").apply { mkdirs() }
    val name = "%02d-%s.png".format(++n, what.replace(Regex("[^A-Za-z0-9]+"), "_").take(60))
    val bmp = inst.uiAutomation.takeScreenshot() ?: error("UiAutomation.takeScreenshot returned null")
    java.io.File(dir, name).outputStream().use { bmp.compress(android.graphics.Bitmap.CompressFormat.PNG, 100, it) }
    name
  }.onFailure { android.util.Log.e("UITEST", "failure screenshot: $it") }.getOrNull()
}

object Timing {
  private const val TAG = "UITEST"

  fun <T> phase(name: String, body: () -> T): T {
    val start = System.currentTimeMillis()
    return body().also { android.util.Log.i(TAG, "phase ${System.currentTimeMillis() - start} ms: $name") }
  }

  fun wait(what: String, ms: Long) {
    if (ms >= 500) android.util.Log.i(TAG, "wait $ms ms: $what")
  }
}

private const val SETTLE_MS = 500L
private const val CHAT_SCROLL_PAGE = 0.8f
private val CHAT_ITEM_TAG = "${Ui.Chat.Item(0).id}["
private val chatItemNode = SemanticsMatcher("a chat item") { it.config.getOrElseNullable(SemanticsProperties.TestTag) { null }?.startsWith(CHAT_ITEM_TAG) == true }
private const val KEYBOARD_SHOW_MS = 1_500L
private const val END_WORK_MS = 30_000L
private const val EXTENSION_MS = 60_000L

private class CommitText(private val value: String) : androidx.test.espresso.ViewAction {
  override fun getConstraints(): org.hamcrest.Matcher<android.view.View> = ViewMatchers.isAssignableFrom(EditText::class.java)
  override fun getDescription() = "type `$value` through the input connection, as a keyboard does"
  override fun perform(uiController: androidx.test.espresso.UiController, view: android.view.View) {
    val editText = view as EditText
    editText.requestFocus()
    val ic = editText.onCreateInputConnection(android.view.inputmethod.EditorInfo()) ?: error("the message field has no input connection")
    ic.beginBatchEdit()
    ic.setSelection(0, editText.text.length)
    ic.commitText(value, 1)
    ic.endBatchEdit()
    uiController.loopMainThreadUntilIdle()
  }
}

private fun UiScope.waitForAppWork() {
  try {
    poll(timeoutMs = END_WORK_MS, what = "app work to finish", screenshot = false) { UiIdle.inFlight().isEmpty() }
  } catch (e: AssertionError) {
    if (e.message?.startsWith("timed out") != true) throw e
  }
}
