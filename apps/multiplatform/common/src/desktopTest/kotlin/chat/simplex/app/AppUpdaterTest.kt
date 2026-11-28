package chat.simplex.app

import androidx.compose.foundation.layout.Column
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.test.ComposeUiTest
import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.SemanticsMatcher
import androidx.compose.ui.test.SemanticsNodeInteraction
import androidx.compose.ui.test.assertCountEquals
import androidx.compose.ui.test.filter
import androidx.compose.ui.test.isToggleable
import androidx.compose.ui.test.onAllNodesWithText
import androidx.compose.ui.test.onFirst
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.compose.ui.test.performScrollTo
import androidx.compose.ui.test.runComposeUiTest
import androidx.compose.ui.unit.DpSize
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.WindowState
import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.model.ChatController
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.Contact
import chat.simplex.common.platform.AppUpdatesChannel
import chat.simplex.common.platform.PlatformInterface
import chat.simplex.common.platform.desktopPlatform
import chat.simplex.common.platform.platform
import chat.simplex.common.platform.reloadedChatCtrl
import chat.simplex.common.platform.tmpDir
import chat.simplex.common.platform.uiReloadRequested
import chat.simplex.common.simplexWindowState
import chat.simplex.common.ui.theme.SimpleXTheme
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.AdvancedSettingsAppSection
import chat.simplex.common.views.usersettings.DeveloperView
import com.sun.net.httpserver.HttpServer
import kotlinx.coroutines.Job
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.JsonArray
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.addJsonObject
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.put
import kotlinx.serialization.json.putJsonArray
import java.io.ByteArrayOutputStream
import java.io.File
import java.net.InetSocketAddress
import java.nio.file.Files
import java.security.KeyPairGenerator
import java.security.MessageDigest
import java.security.PrivateKey
import java.security.Signature
import java.util.Base64
import java.util.Locale
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.CopyOnWriteArrayList
import java.util.zip.ZipEntry
import java.util.zip.ZipOutputStream
import kotlin.test.AfterTest
import kotlin.test.BeforeTest
import kotlin.test.Test
import kotlin.test.assertContentEquals
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNotNull
import kotlin.test.assertNull
import kotlin.test.assertTrue

private const val APP_RELEASES = "/repos/simplex-chat/simplex-chat/releases"
private const val TEST_RELEASES = "/repos/simplex-chat/simplex-chat-test/releases"
private const val RESOURCES_DIR = "compose.application.resources.dir"
private const val RUNNING_UI_UPDATE = "simplex.ui_update"

@OptIn(ExperimentalTestApi::class)
class AppUpdaterTest {
  private fun release(tag: String, preRelease: Boolean = false) =
    GitHubRelease(
      tagName = tag,
      htmlUrl = "https://github.com/simplex-chat/simplex-chat/releases/tag/$tag",
      name = tag,
      draft = false,
      preRelease = preRelease,
      body = "",
      publishedAt = "2026-11-17T00:00:00Z",
      assets = emptyList()
    )

  @Test
  fun testInternalRelease() {
    val newest = release("7.1.4-175005")
    val older = release("7.1.4-175004")
    assertEquals(newest, chooseInternalRelease(175, listOf(newest, older)))
    assertEquals(newest, chooseInternalRelease(175004, listOf(older, newest)))
    assertNull(chooseInternalRelease(175005, listOf(newest, older)))
    assertNull(chooseInternalRelease(175006, listOf(newest, older)))
    assertNull(chooseInternalRelease(175, listOf(release("v7.1.4"), release("7.1-beta.10"), release("7.1.4-internal.3"))))
    assertNull(chooseInternalRelease(175, emptyList()))
  }

  @Test
  fun testStableRelease() {
    val beta = release("v7.2.0-beta.1", preRelease = true)
    val stable = release("v7.1.4")
    val releases = listOf(beta, stable, release("v7.1.3"))
    assertEquals(stable, chooseStableRelease(SemVer(7, 1, 3), releases))
    assertNull(chooseStableRelease(SemVer(7, 1, 4), releases))
    assertNull(chooseStableRelease(SemVer(7, 2, 0, "beta", 1), releases))
  }

  @Test
  fun testBetaRelease() {
    val beta = release("v7.2.0-beta.1", preRelease = true)
    val releases = listOf(beta, release("v7.1.4"), release("v7.1.3"))
    assertEquals(beta, chooseBetaRelease(SemVer(7, 1, 3), releases))
    assertEquals(beta, chooseBetaRelease(SemVer(7, 1, 4), releases))
    assertNull(chooseBetaRelease(SemVer(7, 2, 0, "beta", 1), releases))
  }

  private val keys = KeyPairGenerator.getInstance("Ed25519").generateKeyPair()
  private val otherKeys = KeyPairGenerator.getInstance("Ed25519").generateKeyPair()
  private val base = uiBase("core-1", listOf("a-2.jar"), listOf("JAVA_VERSION=\"17\""), emptyList())
  private val otherBase = uiBase("core-2", listOf("a-2.jar"), listOf("JAVA_VERSION=\"17\""), emptyList())
  private val jar = byteArrayOf(1, 2, 3)

  private fun sha256(bytes: ByteArray) = MessageDigest.getInstance("SHA-256").digest(bytes).joinToString("") { "%02x".format(it) }

  private fun bundle(code: Int, signedBase: String = base, key: PrivateKey = keys.private, jarName: String = "ui-1.jar"): Map<String, ByteArray> {
    val manifest = """{"version_code":$code,"base":"$signedBase","jars":{"$jarName":"${sha256(jar)}"}}""".toByteArray()
    val signature = Signature.getInstance("Ed25519").run {
      initSign(key)
      update(manifest)
      sign()
    }
    return mapOf("ui.json" to manifest, "ui.json.sig" to signature, jarName to jar)
  }

  private fun bundleDir(code: Int, signedBase: String = base, root: File = Files.createTempDirectory("ui-updates").toFile()): File {
    val dir = root.resolve("$code")
    dir.mkdirs()
    bundle(code, signedBase).forEach { (name, bytes) -> dir.resolve(name).writeBytes(bytes) }
    return dir
  }

  private fun bundleZip(root: File, files: Map<String, ByteArray>): File {
    val zip = root.resolve("ui.zip")
    ZipOutputStream(zip.outputStream()).use { out ->
      files.forEach { (name, bytes) ->
        out.putNextEntry(ZipEntry(name))
        out.write(bytes)
        out.closeEntry()
      }
    }
    return zip
  }

  @Test
  fun testUiBase() {
    val jars = listOf("c-3-1234567890abcdef.jar", "b-1.0-0123456789abcdef0123456789abcdef.jar", "a-2.jar")
    val runtime = listOf("JAVA_VERSION=\"17.0.13\"", "MODULES=\"java.base\"")
    val options = listOf("-Djpackage.app-version=7.1.4", "-Dcompose.application.resources.dir=\$APPDIR/resources")
    assertEquals("920b1c4905c17ebda03c93a8f659b3ef21d7041f7a0149250da18ae328ad01f6", uiBase("core-1", jars, runtime, options))
  }

  @Test
  fun testCopyLimited() {
    val out = ByteArrayOutputStream()
    assertEquals(sha256(jar), copyLimited(jar.inputStream(), out, 3))
    assertContentEquals(jar, out.toByteArray())
    assertNull(copyLimited(jar.inputStream(), ByteArrayOutputStream(), 2))
  }

  @Test
  fun testUiUpdateVerification() {
    val dir = bundleDir(176)
    assertEquals(listOf(dir.resolve("ui-1.jar")), verifiedUiUpdate(dir, keys.public, base, 175)?.jars)
    assertNull(verifiedUiUpdate(dir, keys.public, base, 176))
    assertNull(verifiedUiUpdate(dir, keys.public, otherBase, 175))
    assertNull(verifiedUiUpdate(dir, otherKeys.public, base, 175))
    assertNull(verifiedUiUpdate(bundleDir(176, signedBase = otherBase), keys.public, base, 175))
    dir.resolve("ui-1.jar").appendBytes(byteArrayOf(4))
    assertNull(verifiedUiUpdate(dir, keys.public, base, 175))
  }

  @Test
  fun testUiUpdateTrial() {
    val dir = bundleDir(176)
    assertEquals(UiUpdateState.STAGED, uiUpdateState(dir))
    assertNotNull(startableUiUpdate(dir, keys.public, base, 175))
    assertEquals(UiUpdateState.STARTED, uiUpdateState(dir))
    assertNull(startableUiUpdate(dir, keys.public, base, 175))
    assertEquals(UiUpdateState.FAILED, uiUpdateState(dir))
    assertNull(startableUiUpdate(dir, keys.public, base, 175))
    assertEquals(UiUpdateState.FAILED, uiUpdateState(dir))

    val confirmed = bundleDir(177)
    setUiUpdateState(confirmed, UiUpdateState.CONFIRMED)
    assertNotNull(startableUiUpdate(confirmed, keys.public, base, 175))
    assertEquals(UiUpdateState.RESTARTED, uiUpdateState(confirmed))
    setUiUpdateState(confirmed, UiUpdateState.CONFIRMED)
    assertNotNull(startableUiUpdate(confirmed, keys.public, base, 175))
    assertNull(startableUiUpdate(confirmed, keys.public, base, 175))
    assertEquals(UiUpdateState.FAILED, uiUpdateState(confirmed))

    val superseded = bundleDir(178)
    assertNull(startableUiUpdate(superseded, keys.public, base, 178))
    assertFalse(superseded.exists())

    val removed = bundleDir(179)
    setUiUpdateState(removed, UiUpdateState.REMOVED)
    assertNull(startableUiUpdate(removed, keys.public, base, 175))
    assertFalse(removed.exists())
  }

  @Test
  fun testStageUiUpdate() {
    val root = Files.createTempDirectory("ui-updates").toFile()
    val files = bundle(176) + mapOf("state" to "CONFIRMED".toByteArray(), "ui-2.jar" to jar, "lib/ui-3.jar" to jar)
    assertTrue(stageUiUpdate(bundleZip(root, files), root, keys.public, base, 175))
    val dir = root.resolve("176")
    assertEquals(setOf("ui.json", "ui.json.sig", "ui-1.jar"), dir.list()!!.toSet())
    assertEquals(UiUpdateState.STAGED, uiUpdateState(dir))
    assertEquals(setOf("ui.zip", "176"), root.list()!!.toSet())

    val traversal = Files.createTempDirectory("ui-updates").toFile()
    assertFalse(stageUiUpdate(bundleZip(traversal, bundle(176, jarName = "../ui-2.jar")), traversal, keys.public, base, 175))
    assertEquals(setOf("ui.zip"), traversal.list()!!.toSet())

    val changed = Files.createTempDirectory("ui-updates").toFile()
    assertFalse(stageUiUpdate(bundleZip(changed, bundle(176) + mapOf("ui-1.jar" to byteArrayOf(4))), changed, keys.public, base, 175))
    assertEquals(setOf("ui.zip"), changed.list()!!.toSet())

    val other = Files.createTempDirectory("ui-updates").toFile()
    assertFalse(stageUiUpdate(bundleZip(other, bundle(176, key = otherKeys.private)), other, keys.public, base, 175))
    assertEquals(setOf("ui.zip"), other.list()!!.toSet())
  }

  private val server = HttpServer.create(InetSocketAddress("127.0.0.1", 0), 0)
  private val serverUrl get() = "http://127.0.0.1:${server.address.port}"
  private val served = ConcurrentHashMap<String, () -> ByteArray>()
  private val requests = CopyOnWriteArrayList<String>()
  private val testRoot = Files.createTempDirectory("app-updates").toFile()
  private val nextCode = BuildConfigCommon.DESKTOP_VERSION_CODE + 1
  private val internalTag = "${BuildConfigCommon.DESKTOP_VERSION_NAME}-$nextCode"
  private val installedBase by lazy { assertNotNull(installedUiUpdateBase()) }
  private val packages = listOf(desktopPlatform.githubAssetName, "simplex-desktop-ubuntu-22_04-x86_64.deb", "simplex-desktop-ubuntu-22_04-aarch64.deb").map { it to 1L }
  private val apiUrl = gitHubApiUrl
  private val updatesDir = uiUpdatesDir
  private val updateKey = uiUpdateKeyBase64
  private val uiJars = installedUiJars
  private val resourcesDir: String? = System.getProperty(RESOURCES_DIR)
  private val runningUiUpdate: String? = System.getProperty(RUNNING_UI_UPDATE)
  private val savedPlatform = platform
  private val chatCtrl = ChatController.getChatCtrl()
  private val chatRunning = ChatModel.chatRunning.value
  private val appBarHeight = AppBarHandler.appBarMaxHeightPx
  private val locale = Locale.getDefault()
  private val updateChannel = appPrefs.appUpdateChannel.get()
  private val skippedUpdate = appPrefs.appSkippedUpdate.get()
  private val socksProxy = appPrefs.networkUseSocksProxy.get()
  private val allowInternalBuilds = appPrefs.allowInternalBuilds.get()
  private val developerTools = appPrefs.developerTools.get()

  @BeforeTest
  fun setUp() {
    Locale.setDefault(Locale.US)
    simplexWindowState.windowState = WindowState(size = DpSize(1200.dp, 800.dp))
    server.createContext("/") { exchange ->
      val path = exchange.requestURI.path
      requests += path
      val body = served[path]?.invoke()
      exchange.sendResponseHeaders(if (body == null) 404 else 200, body?.size?.toLong() ?: -1L)
      exchange.responseBody.use { out -> if (body != null) out.write(body) }
    }
    server.start()
    gitHubApiUrl = serverUrl
    uiUpdatesDir = testRoot.resolve("ui_updates")
    uiUpdateKeyBase64 = Base64.getEncoder().encodeToString(keys.public.encoded)
    val app = testRoot.resolve("app").apply { mkdirs() }
    app.resolve("SimpleX.cfg").writeText("[JavaOptions]\njava-options=-D$RESOURCES_DIR=\$APPDIR/resources\n")
    installedUiJars = { listOf(app.resolve("common.jar")) }
    val resources = testRoot.resolve("resources").apply { mkdirs() }
    resources.resolve("core-hash").writeText("core-1\n")
    System.setProperty(RESOURCES_DIR, resources.path)
    appPrefs.appSkippedUpdate.set("")
    appPrefs.networkUseSocksProxy.set(false)
  }

  @AfterTest
  fun tearDown() {
    stopUpdateChecker()
    server.stop(0)
    AlertManager.shared.hideAllAlerts()
    gitHubApiUrl = apiUrl
    uiUpdatesDir = updatesDir
    uiUpdateKeyBase64 = updateKey
    installedUiJars = uiJars
    restoreProperty(RESOURCES_DIR, resourcesDir)
    restoreProperty(RUNNING_UI_UPDATE, runningUiUpdate)
    platform = savedPlatform
    ChatController.setChatCtrl(chatCtrl)
    ChatModel.chatRunning.value = chatRunning
    ChatModel.activeCall.value = null
    AppBarHandler.appBarMaxHeightPx = appBarHeight
    appPrefs.appUpdateChannel.set(updateChannel)
    appPrefs.appSkippedUpdate.set(skippedUpdate)
    appPrefs.networkUseSocksProxy.set(socksProxy)
    appPrefs.allowInternalBuilds.set(allowInternalBuilds)
    appPrefs.developerTools.set(developerTools)
    Locale.setDefault(locale)
    testRoot.deleteRecursively()
  }

  @Test
  fun stableChannelOffersTheAppRelease() {
    served[APP_RELEASES] = { releases(releaseJson("v999.1.0-beta.1", preRelease = true), releaseJson("v999.0.0")) }
    updateCheck(AppUpdatesChannel.STABLE) {
      assertEquals(listOf(APP_RELEASES), requests.toList())
      onNodeWithText("Update available: v999.0.0").assertExists()
    }
  }

  @Test
  fun betaChannelOffersTheAppPreRelease() {
    served[APP_RELEASES] = { releases(releaseJson("v999.1.0-beta.1", preRelease = true), releaseJson("v999.0.0")) }
    updateCheck(AppUpdatesChannel.BETA) {
      assertEquals(listOf(APP_RELEASES), requests.toList())
      onNodeWithText("Update available: v999.1.0-beta.1").assertExists()
    }
  }

  @Test
  fun internalChannelOffersTheTestBuild() {
    served[TEST_RELEASES] = { releases(releaseJson(internalTag)) }
    updateCheck(AppUpdatesChannel.INTERNAL) {
      assertEquals(listOf(TEST_RELEASES), requests.toList())
      onNodeWithText("Update available: $internalTag").assertExists()
    }
  }

  @Test
  fun disabledChannelRequestsNothing() {
    appPrefs.appUpdateChannel.set(AppUpdatesChannel.DISABLED)
    runBlocking { setupUpdateChecker().join() }
    checkForUpdate()
    assertEquals(listOf(), requests.toList())
    assertFalse(AlertManager.shared.hasAlertsShown())
  }

  @Test
  fun channelChangedDuringTheCheckShowsNoUpdate() {
    served[APP_RELEASES] = {
      appPrefs.appUpdateChannel.set(AppUpdatesChannel.BETA)
      releases(releaseJson("v999.0.0"))
    }
    updateCheck(AppUpdatesChannel.STABLE) {
      assertEquals(listOf(APP_RELEASES), requests.toList())
      assertFalse(AlertManager.shared.hasAlertsShown())
    }
  }

  @Test
  fun matchingUiUpdateIsStagedAndReloadOffered() {
    serveUiUpdate(uiZip(bundle(nextCode, installedBase)))
    updateCheck(AppUpdatesChannel.INTERNAL) {
      val staged = uiUpdatesDir.resolve("$nextCode")
      assertEquals(listOf("$nextCode"), uiUpdatesDir.list()!!.toList())
      assertEquals(setOf("ui.json", "ui.json.sig", "ui-1.jar"), staged.list()!!.toSet())
      assertEquals(UiUpdateState.STAGED, uiUpdateState(staged))
      assertFalse(tmpDir.resolve(uiZipName(installedBase)).exists())
      assertReloadOffered()
      AlertManager.shared.hideAllAlerts()
      checkForUpdate()
      waitForIdle()
      assertReloadOffered()
      assertEquals(listOf(TEST_RELEASES, "/${uiZipName(installedBase)}", TEST_RELEASES), requests.toList())
    }
  }

  @Test
  fun refusedReloadNowShowsTheReasonAndKeepsTheChat() {
    ChatController.setChatCtrl(1003)
    ChatModel.chatRunning.value = true
    ChatModel.activeCall.value = call(Contact.sampleData)
    serveUiUpdate(uiZip(bundle(nextCode, installedBase)))
    updateCheck(AppUpdatesChannel.INTERNAL) {
      press("Reload now")
      waitUntil(timeoutMillis = 5_000) { onAllNodesWithText("Reload the UI after the call ends.").fetchSemanticsNodes().isNotEmpty() }
      onNodeWithText("UI not reloaded").assertExists()
      onAllNodesWithText("Reload now").assertCountEquals(0)
      assertEquals(1003L, ChatController.getChatCtrl())
      assertNull(reloadedChatCtrl)
      assertFalse(uiReloadRequested)
    }
  }

  @Test
  fun uiUpdateLargerThanItsAssetIsNotStaged() {
    val zip = uiZip(bundle(nextCode, installedBase))
    serveUiUpdate(zip, size = zip.size - 1L)
    updateCheck(AppUpdatesChannel.INTERNAL) {
      assertPackageOffered()
      assertEquals(listOf(TEST_RELEASES, "/${uiZipName(installedBase)}"), requests.toList())
    }
  }

  @Test
  fun uiUpdateOverTheSizeLimitIsNotDownloaded() {
    serveUiUpdate(uiZip(bundle(nextCode, installedBase)), size = UI_UPDATE_MAX_SIZE + 1)
    updateCheck(AppUpdatesChannel.INTERNAL) {
      assertPackageOffered()
      assertEquals(listOf(TEST_RELEASES), requests.toList())
    }
  }

  @Test
  fun uiUpdateWithAChangedJarIsNotStaged() {
    serveUiUpdate(uiZip(bundle(nextCode, installedBase) + ("ui-1.jar" to byteArrayOf(4))))
    updateCheck(AppUpdatesChannel.INTERNAL) { assertPackageOffered() }
  }

  @Test
  fun uiUpdateSignedByAnotherKeyIsNotStaged() {
    serveUiUpdate(uiZip(bundle(nextCode, installedBase, key = otherKeys.private)))
    updateCheck(AppUpdatesChannel.INTERNAL) { assertPackageOffered() }
  }

  @Test
  fun uiUpdateSignedForAnotherBaseIsNotStaged() {
    serveUiUpdate(uiZip(bundle(nextCode, otherBase)))
    updateCheck(AppUpdatesChannel.INTERNAL) { assertPackageOffered() }
  }

  @Test
  fun uiUpdateOfAnotherBaseIsNotDownloaded() {
    serveUiUpdate(uiZip(bundle(nextCode, otherBase)), name = uiZipName(otherBase))
    updateCheck(AppUpdatesChannel.INTERNAL) {
      assertPackageOffered()
      assertEquals(listOf(TEST_RELEASES), requests.toList())
    }
  }

  @Test
  fun channelChangedDuringTheDownloadShowsNoReload() {
    val zip = uiZip(bundle(nextCode, installedBase))
    serveUiUpdate(zip)
    served["/${uiZipName(installedBase)}"] = {
      appPrefs.appUpdateChannel.set(AppUpdatesChannel.STABLE)
      zip
    }
    updateCheck(AppUpdatesChannel.INTERNAL) {
      assertEquals(UiUpdateState.STAGED, uiUpdateState(uiUpdatesDir.resolve("$nextCode")))
      assertFalse(AlertManager.shared.hasAlertsShown())
    }
  }

  @Test
  fun leavingInternalRemovesUiUpdates() {
    val older = bundleDir(nextCode, root = uiUpdatesDir)
    val running = bundleDir(nextCode + 1, root = uiUpdatesDir)
    System.setProperty(RUNNING_UI_UPDATE, running.path)
    served[APP_RELEASES] = { releases() }
    appPrefs.appUpdateChannel.set(AppUpdatesChannel.STABLE)
    runBlocking { setupUpdateChecker().join() }
    assertFalse(older.exists())
    assertEquals(UiUpdateState.REMOVED, uiUpdateState(running))
    assertEquals(listOf(APP_RELEASES), requests.toList())
  }

  @Test
  fun internalChannelKeepsUiUpdates() {
    val staged = bundleDir(nextCode, root = uiUpdatesDir)
    served[TEST_RELEASES] = { releases() }
    appPrefs.appUpdateChannel.set(AppUpdatesChannel.INTERNAL)
    runBlocking { setupUpdateChecker().join() }
    assertEquals(UiUpdateState.STAGED, uiUpdateState(staged))
    assertEquals(listOf(TEST_RELEASES), requests.toList())
  }

  @Test
  fun confirmingAStartedUiUpdateKeepsTheOlderOnes() {
    val older = bundleDir(nextCode, root = uiUpdatesDir)
    val running = runningUiUpdate(nextCode + 1, UiUpdateState.STARTED)
    confirmUiUpdate()
    assertEquals(UiUpdateState.CONFIRMED, uiUpdateState(running))
    assertTrue(older.exists())
  }

  @Test
  fun confirmingARestartedUiUpdateRemovesTheOlderOnes() {
    val older = bundleDir(nextCode, root = uiUpdatesDir)
    val running = runningUiUpdate(nextCode + 1, UiUpdateState.RESTARTED)
    val newer = bundleDir(nextCode + 2, root = uiUpdatesDir)
    confirmUiUpdate()
    assertEquals(UiUpdateState.CONFIRMED, uiUpdateState(running))
    assertFalse(older.exists())
    assertTrue(newer.exists())
  }

  @Test
  fun confirmingKeepsAFailedUiUpdateFailed() {
    val running = runningUiUpdate(nextCode, UiUpdateState.FAILED)
    confirmUiUpdate()
    assertEquals(UiUpdateState.FAILED, uiUpdateState(running))
  }

  @Test
  fun failingMarksOnlyTheRunningUiUpdate() {
    val other = bundleDir(nextCode, root = uiUpdatesDir)
    val running = runningUiUpdate(nextCode + 1, UiUpdateState.STARTED)
    failRunningUiUpdate()
    assertEquals(UiUpdateState.FAILED, uiUpdateState(running))
    assertEquals(UiUpdateState.STAGED, uiUpdateState(other))
  }

  @Test
  fun withoutARunningUiUpdateConfirmingAndFailingChangeNothing() {
    System.clearProperty(RUNNING_UI_UPDATE)
    val started = bundleDir(nextCode, root = uiUpdatesDir)
    setUiUpdateState(started, UiUpdateState.STARTED)
    confirmUiUpdate()
    failRunningUiUpdate()
    assertEquals(UiUpdateState.STARTED, uiUpdateState(started))
  }

  @Test
  fun usingTheInstalledUiRemovesTheOtherUpdatesAndFailsTheRunningOne() {
    val older = bundleDir(nextCode, root = uiUpdatesDir)
    val running = runningUiUpdate(nextCode + 1, UiUpdateState.CONFIRMED)
    val newer = bundleDir(nextCode + 2, root = uiUpdatesDir)
    useInstalledUi()
    assertFalse(older.exists())
    assertFalse(newer.exists())
    assertEquals(UiUpdateState.FAILED, uiUpdateState(running))
  }

  @Test
  fun channelsLackInternalWhileItIsNotAllowed() = runComposeUiTest {
    appPrefs.allowInternalBuilds.set(false)
    appPrefs.appUpdateChannel.set(AppUpdatesChannel.STABLE)
    showSettings { AdvancedSettingsAppSection(showSettingsModal = { { } }, withAuth = { _, _, block -> block() }) }
    press("Stable")
    onNodeWithText("Beta").assertExists()
    onAllNodesWithText("Internal").assertCountEquals(0)
  }

  @Test
  fun allowingInternalBuildsListsTheInternalChannel() = runComposeUiTest {
    appPrefs.developerTools.set(true)
    appPrefs.allowInternalBuilds.set(false)
    appPrefs.appUpdateChannel.set(AppUpdatesChannel.STABLE)
    showSettings {
      Column {
        AdvancedSettingsAppSection(showSettingsModal = { { } }, withAuth = { _, _, block -> block() })
        DeveloperView { _, _, block -> block() }
      }
    }
    switchBeside("Allow internal builds").performClick()
    waitForIdle()
    assertTrue(appPrefs.allowInternalBuilds.get())
    press("Stable")
    onAllNodesWithText("Internal").assertCountEquals(1)
  }

  @Test
  fun disallowingInternalBuildsMovesInternalToStable() {
    val checks = CopyOnWriteArrayList<Job>()
    platform = object : PlatformInterface {
      override fun desktopSetupUpdateChecker() {
        checks += setupUpdateChecker()
      }
    }
    served[APP_RELEASES] = { releases() }
    disallowInternalBuilds(AppUpdatesChannel.INTERNAL)
    runBlocking { checks.single().join() }
    assertEquals(AppUpdatesChannel.STABLE, appPrefs.appUpdateChannel.get())
    assertEquals(listOf(APP_RELEASES), requests.toList())
  }

  @Test
  fun disallowingInternalBuildsKeepsAnotherChannel() {
    var checks = 0
    platform = object : PlatformInterface {
      override fun desktopSetupUpdateChecker() {
        checks++
      }
    }
    disallowInternalBuilds(AppUpdatesChannel.BETA)
    assertEquals(0, checks)
    assertEquals(AppUpdatesChannel.BETA, appPrefs.appUpdateChannel.get())
  }

  private fun updateCheck(channel: AppUpdatesChannel, test: ComposeUiTest.() -> Unit) = runComposeUiTest {
    appPrefs.appUpdateChannel.set(channel)
    setContent { AlertManager.shared.showInView() }
    checkForUpdate()
    waitForIdle()
    test()
  }

  private fun disallowInternalBuilds(channel: AppUpdatesChannel) = runComposeUiTest {
    appPrefs.developerTools.set(true)
    appPrefs.allowInternalBuilds.set(true)
    appPrefs.appUpdateChannel.set(channel)
    showSettings { DeveloperView { _, _, block -> block() } }
    switchBeside("Allow internal builds").performClick()
    waitForIdle()
    assertFalse(appPrefs.allowInternalBuilds.get())
  }

  private fun releaseJson(tag: String, preRelease: Boolean = false, assets: List<Pair<String, Long>> = packages) = buildJsonObject {
    put("tag_name", tag)
    put("html_url", "https://github.com/simplex-chat/simplex-chat/releases/tag/$tag")
    put("name", tag)
    put("draft", false)
    put("prerelease", preRelease)
    put("body", "")
    put("published_at", "2026-11-28T00:00:00Z")
    putJsonArray("assets") {
      for ((name, size) in assets) addJsonObject {
        put("browser_download_url", "$serverUrl/$name")
        put("name", name)
        put("size", size)
      }
    }
  }

  private fun releases(vararg release: JsonObject): ByteArray = JsonArray(release.toList()).toString().toByteArray()

  private fun uiZipName(base: String) = "simplex-desktop-ui-$base.zip"

  private fun uiZip(files: Map<String, ByteArray>): ByteArray = bundleZip(testRoot, files).readBytes()

  private fun runningUiUpdate(code: Int, state: UiUpdateState): File =
    bundleDir(code, root = uiUpdatesDir).also { dir ->
      setUiUpdateState(dir, state)
      System.setProperty(RUNNING_UI_UPDATE, dir.path)
    }

  private fun serveUiUpdate(zip: ByteArray, name: String = uiZipName(installedBase), size: Long = zip.size.toLong()) {
    served["/$name"] = { zip }
    served[TEST_RELEASES] = { releases(releaseJson(internalTag, assets = packages + (name to size))) }
  }

  private fun ComposeUiTest.assertReloadOffered() {
    onNodeWithText("Reload now").assertExists()
    onAllNodesWithText("Update available: $internalTag").assertCountEquals(0)
  }

  private fun ComposeUiTest.assertPackageOffered() {
    onNodeWithText("Update available: $internalTag").assertExists()
    onAllNodesWithText("Reload now").assertCountEquals(0)
    assertEquals(listOf(), uiUpdatesDir.list().orEmpty().toList())
  }

  private fun ComposeUiTest.showSettings(content: @Composable () -> Unit) = setContent {
    SimpleXTheme {
      AppBarHandler.appBarMaxHeightPx = with(LocalDensity.current) { AppBarHeight.roundToPx() }
      CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler(), content = content)
    }
  }

  private fun ComposeUiTest.press(text: String) {
    onNodeWithText(text).performClick()
    waitForIdle()
  }

  private fun ComposeUiTest.switchBeside(text: String): SemanticsNodeInteraction {
    onNodeWithText(text).performScrollTo()
    val row = onNodeWithText(text).fetchSemanticsNode().boundsInRoot
    return onAllNodes(isToggleable())
      .filter(SemanticsMatcher("a switch beside $text") { it.boundsInRoot.center.y in row.top..row.bottom })
      .assertCountEquals(1)
      .onFirst()
  }

  private fun restoreProperty(name: String, value: String?) {
    if (value == null) System.clearProperty(name) else System.setProperty(name, value)
  }
}
