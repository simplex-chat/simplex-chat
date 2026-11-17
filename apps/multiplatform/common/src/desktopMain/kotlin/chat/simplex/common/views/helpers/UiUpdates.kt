package chat.simplex.common.views.helpers

import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.otherInstanceRunning
import chat.simplex.common.platform.dataDir
import chat.simplex.common.platform.reloadedInstanceLock
import chat.simplex.common.platform.uiReloadRequested
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import java.io.File
import java.io.InputStream
import java.io.OutputStream
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method
import java.net.JarURLConnection
import java.net.URL
import java.net.URLClassLoader
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import java.security.KeyFactory
import java.security.MessageDigest
import java.security.PublicKey
import java.security.Signature
import java.security.spec.X509EncodedKeySpec
import java.util.Base64
import java.util.Collections
import java.util.Enumeration
import java.util.jar.JarFile
import java.util.zip.ZipFile

const val UI_UPDATE_CONFIRM_DELAY = 10_000L
const val UI_UPDATE_MAX_SIZE = 256L shl 20

private const val UI_UPDATE_PROPERTY = "simplex.ui_update"
private const val UI_UPDATE_MANIFEST = "ui.json"
private const val UI_UPDATE_SIGNATURE = "ui.json.sig"
private const val UI_UPDATE_SIGNED_MAX_SIZE = 64 shl 10
private const val UI_UPDATE_STATE = "state"
private const val UI_UPDATE_STAGING = "staging"
private const val CORE_HASH = "core-hash"
private const val JAVA_OPTION = "java-options="
private const val APP_VERSION_OPTION = "-Djpackage.app-version="
private const val CORE_BINDINGS = "chat/simplex/common/platform/CoreKt.class"
private val UI_JAR_CLASSES = listOf(
  "chat/simplex/desktop/MainKt.class",
  "chat/simplex/common/BuildConfigCommon.class",
  "org/nanohttpd/protocols/http/NanoHTTPD.class"
)
private val UI_UPDATE_JAR = Regex("[A-Za-z0-9_][A-Za-z0-9._-]*\\.jar")
private val JAR_CONTENT_HASH = Regex("(-[0-9a-f]{16,32})?\\.jar$")
private val uiUpdateJson = Json { ignoreUnknownKeys = true }
private val uiUpdatesDir = File(dataDir, "ui_updates")

@Serializable
private class UiUpdateManifest(
  @SerialName("version_code") val versionCode: Int,
  val base: String,
  val jars: Map<String, String>
)

class UiUpdate(val versionCode: Int, val jars: List<File>)

enum class UiUpdateState { STAGED, STARTED, CONFIRMED, RESTARTED, FAILED, REMOVED }

fun startUiUpdate(mainClass: String, args: Array<String>): Boolean {
  val main = uiUpdateMain(mainClass) ?: return false
  runUi(main, args)
  return true
}

fun uiReloadable(): Boolean = installedUiJars() != null

fun runReloadedUis(mainClass: String, args: Array<String>) {
  if (UiUpdate::class.java.classLoader !== ClassLoader.getSystemClassLoader()) return
  while (uiReloadRequested) {
    uiReloadRequested = false
    val runtime = Runtime.getRuntime()
    System.gc()
    println("I: UI reload, heap used ${(runtime.totalMemory() - runtime.freeMemory()) shr 20} MB")
    runUi(uiUpdateMain(mainClass) ?: installedUiMain(mainClass) ?: return, args)
  }
}

private fun runUi(main: Method, args: Array<String>) {
  Thread.currentThread().contextClassLoader = main.declaringClass.classLoader
  try {
    main.invoke(null, args)
  } catch (e: InvocationTargetException) {
    throw e.targetException
  }
}

private fun installedUiMain(mainClass: String): Method? {
  val installedUi = installedUiJars() ?: return null
  System.clearProperty(UI_UPDATE_PROPERTY)
  return uiMain(installedUi, installedUi, mainClass)
}

private fun uiMain(jars: List<File>, installedUi: List<File>, mainClass: String): Method {
  val loader = URLClassLoader(jars.map { it.toURI().toURL() }.toTypedArray(), InstalledLibraries(installedUi))
  return Class.forName(mainClass, true, loader).getMethod("main", Array<String>::class.java)
}

fun runningUiUpdateDir(): File? = System.getProperty(UI_UPDATE_PROPERTY)?.let(::File)

fun confirmUiUpdate() {
  val dir = runningUiUpdateDir() ?: return
  when (uiUpdateState(dir)) {
    UiUpdateState.STARTED -> setUiUpdateState(dir, UiUpdateState.CONFIRMED)
    UiUpdateState.RESTARTED -> {
      setUiUpdateState(dir, UiUpdateState.CONFIRMED)
      uiUpdateDirs().filter { it.name.toInt() < dir.name.toInt() }.forEach { it.deleteRecursively() }
    }
    else -> {}
  }
}

fun failRunningUiUpdate() {
  runningUiUpdateDir()?.let { setUiUpdateState(it, UiUpdateState.FAILED) }
}

fun useInstalledUi() {
  val running = runningUiUpdateDir()
  uiUpdateDirs().filter { it != running }.forEach { it.deleteRecursively() }
  failRunningUiUpdate()
}

fun removeUiUpdates() {
  val running = runningUiUpdateDir()
  uiUpdateDirs().forEach { dir ->
    if (dir == running || !dir.deleteRecursively()) setUiUpdateState(dir, UiUpdateState.REMOVED)
  }
}

fun installedUiUpdateBase(): String? =
  if (uiUpdateKey() == null) null else installedUiJars()?.let(::installedUiBase)

fun uiUpdateDir(versionCode: Int): File = File(uiUpdatesDir, "$versionCode")

fun stageDownloadedUiUpdate(zip: File, base: String): Boolean {
  val key = uiUpdateKey() ?: return false
  return stageUiUpdate(zip, uiUpdatesDir, key, base, BuildConfigCommon.DESKTOP_VERSION_CODE)
}

private fun uiUpdateMain(mainClass: String): Method? {
  try {
    if (UiUpdate::class.java.classLoader !== ClassLoader.getSystemClassLoader()) return null
    val key = uiUpdateKey() ?: return null
    val dirs = uiUpdateDirs()
    if (dirs.isEmpty() || (reloadedInstanceLock == null && otherInstanceRunning())) return null
    val installedUi = installedUiJars() ?: return null
    val base = installedUiBase(installedUi) ?: return null
    for (dir in dirs) {
      val update = startableUiUpdate(dir, key, base, BuildConfigCommon.DESKTOP_VERSION_CODE) ?: continue
      try {
        val main = uiMain(update.jars, installedUi, mainClass)
        System.setProperty(UI_UPDATE_PROPERTY, dir.path)
        return main
      } catch (e: Throwable) {
        println("E: UI update ${dir.name} failed to load: $e")
        setUiUpdateState(dir, UiUpdateState.FAILED)
      }
    }
  } catch (e: Throwable) {
    println("E: UI update not started: ${e.stackTraceToString()}")
  }
  return null
}

internal fun startableUiUpdate(dir: File, key: PublicKey, base: String, installedCode: Int): UiUpdate? {
  val state = uiUpdateState(dir)
  when (state) {
    UiUpdateState.FAILED -> return null
    UiUpdateState.STARTED, UiUpdateState.RESTARTED -> {
      setUiUpdateState(dir, UiUpdateState.FAILED)
      return null
    }
    UiUpdateState.REMOVED -> {
      dir.deleteRecursively()
      return null
    }
    UiUpdateState.STAGED, UiUpdateState.CONFIRMED -> {}
  }
  val update = verifiedUiUpdate(dir, key, base, installedCode)
  if (update == null) {
    dir.deleteRecursively()
  } else {
    setUiUpdateState(dir, if (state == UiUpdateState.STAGED) UiUpdateState.STARTED else UiUpdateState.RESTARTED)
  }
  return update
}

internal fun verifiedUiUpdate(dir: File, key: PublicKey, base: String, installedCode: Int): UiUpdate? =
  try {
    verifiedManifest(File(dir, UI_UPDATE_MANIFEST).readBytes(), File(dir, UI_UPDATE_SIGNATURE).readBytes(), key, base, installedCode)
      ?.takeIf { m -> m.jars.all { (name, digest) -> File(dir, name).inputStream().use { copyLimited(it, OutputStream.nullOutputStream(), UI_UPDATE_MAX_SIZE) } == digest } }
      ?.let { m -> UiUpdate(m.versionCode, m.jars.keys.map { File(dir, it) }) }
  } catch (e: Exception) {
    println("E: UI update ${dir.name} not verified: $e")
    null
  }

internal fun stageUiUpdate(zip: File, root: File, key: PublicKey, base: String, installedCode: Int): Boolean {
  val staging = File(root, UI_UPDATE_STAGING)
  staging.deleteRecursively()
  ZipFile(zip).use { z ->
    val manifest = z.signedEntry(UI_UPDATE_MANIFEST) ?: return false
    val signature = z.signedEntry(UI_UPDATE_SIGNATURE) ?: return false
    val update = verifiedManifest(manifest, signature, key, base, installedCode) ?: return false
    staging.mkdirs()
    var remaining = UI_UPDATE_MAX_SIZE
    for ((name, digest) in update.jars) {
      val jar = File(staging, name)
      val written = z.getEntry(name)?.let { entry -> z.getInputStream(entry).use { input -> jar.outputStream().use { copyLimited(input, it, remaining) } } }
      if (written != digest) {
        staging.deleteRecursively()
        return false
      }
      remaining -= jar.length()
    }
    File(staging, UI_UPDATE_MANIFEST).writeBytes(manifest)
    File(staging, UI_UPDATE_SIGNATURE).writeBytes(signature)
    Files.move(staging.toPath(), File(root, "${update.versionCode}").toPath(), StandardCopyOption.ATOMIC_MOVE)
  }
  return true
}

internal fun copyLimited(input: InputStream, output: OutputStream, limit: Long): String? {
  val digest = MessageDigest.getInstance("SHA-256")
  val buffer = ByteArray(1 shl 16)
  var size = 0L
  while (true) {
    val n = input.read(buffer)
    if (n < 0) return digest.digest().toHex()
    size += n
    if (size > limit) return null
    digest.update(buffer, 0, n)
    output.write(buffer, 0, n)
  }
}

internal fun uiUpdateState(dir: File): UiUpdateState {
  val state = File(dir, UI_UPDATE_STATE)
  if (!state.exists()) return UiUpdateState.STAGED
  val name = state.readText()
  return UiUpdateState.entries.firstOrNull { it.name == name } ?: UiUpdateState.FAILED
}

internal fun setUiUpdateState(dir: File, state: UiUpdateState) {
  File(dir, UI_UPDATE_STATE).writeText(state.name)
}

internal fun uiBase(coreHash: String, dependencies: List<String>, runtime: List<String>, options: List<String>): String {
  val lines = listOf("core $coreHash") +
    dependencies.map { "jar ${it.replace(JAR_CONTENT_HASH, "")}" }.sorted() +
    runtime.map { "runtime $it" } +
    options.filterNot { it.startsWith(APP_VERSION_OPTION) }.map { "option $it" }
  return MessageDigest.getInstance("SHA-256").digest(lines.joinToString("") { "$it\n" }.toByteArray()).toHex()
}

private fun verifiedManifest(manifest: ByteArray, signature: ByteArray, key: PublicKey, base: String, installedCode: Int): UiUpdateManifest? {
  val verifier = Signature.getInstance("Ed25519")
  verifier.initVerify(key)
  verifier.update(manifest)
  if (!verifier.verify(signature)) return null
  return uiUpdateJson.decodeFromString<UiUpdateManifest>(manifest.decodeToString())
    .takeIf { m -> m.base == base && m.versionCode > installedCode && m.jars.isNotEmpty() && m.jars.keys.all { UI_UPDATE_JAR.matches(it) } }
}

private fun ZipFile.signedEntry(name: String): ByteArray? =
  getEntry(name)
    ?.let { entry -> getInputStream(entry).use { it.readNBytes(UI_UPDATE_SIGNED_MAX_SIZE + 1) } }
    ?.takeIf { it.size <= UI_UPDATE_SIGNED_MAX_SIZE }

private fun uiUpdateDirs(): List<File> =
  uiUpdatesDir.listFiles { f -> f.isDirectory && f.name.toIntOrNull() != null }.orEmpty().sortedByDescending { it.name.toInt() }

private fun uiUpdateKey(): PublicKey? =
  BuildConfigCommon.DESKTOP_UI_UPDATE_KEY.takeIf { it.isNotEmpty() }?.let { key ->
    try {
      KeyFactory.getInstance("Ed25519").generatePublic(X509EncodedKeySpec(Base64.getDecoder().decode(key)))
    } catch (e: Exception) {
      println("E: UI update key: $e")
      null
    }
  }

private fun installedUiJars(): List<File>? =
  UI_JAR_CLASSES.map { name ->
    (ClassLoader.getSystemResource(name)?.openConnection() as? JarURLConnection)?.jarFileURL?.toURI()?.let(::File) ?: return null
  }

private fun installedUiBase(installedUi: List<File>): String? {
  val coreHash = System.getProperty("compose.application.resources.dir")?.let { File(it, CORE_HASH) }?.takeIf { it.isFile }?.readText()?.trim() ?: return null
  val runtime = File(System.getProperty("java.home"), "release").takeIf { it.isFile }?.readLines()?.filter { it.isNotBlank() } ?: return null
  val cfg = installedUi.first().parentFile.listFiles { f -> f.extension == "cfg" }?.singleOrNull() ?: return null
  val ui = installedUi.map { it.name }.toSet()
  val dependencies = System.getProperty("java.class.path").split(File.pathSeparator).map { File(it).name }.filter { it !in ui }
  val options = cfg.readLines().filter { it.startsWith(JAVA_OPTION) }.map { it.removePrefix(JAVA_OPTION) }
  return uiBase(coreHash, dependencies, runtime, options)
}

private fun ByteArray.toHex(): String = joinToString("") { "%02x".format(it) }

private class InstalledLibraries(jars: List<File>) : ClassLoader(null) {
  private val system = ClassLoader.getSystemClassLoader()
  private val hiddenNames = jars.map { it.name }
  private val hidden = jars.flatMapTo(HashSet()) { jar -> JarFile(jar).use { j -> j.entries().toList().map { it.name } } } - CORE_BINDINGS

  private fun hides(name: String) = name in hidden

  override fun loadClass(name: String, resolve: Boolean): Class<*> =
    if (hides(name.replace('.', '/') + ".class")) throw ClassNotFoundException(name) else system.loadClass(name)

  override fun getResource(name: String): URL? = if (hides(name)) null else system.getResource(name)

  override fun getResources(name: String): Enumeration<URL> =
    Collections.enumeration(system.getResources(name).toList().filterNot { url -> hiddenNames.any { url.path.endsWith("$it!/$name") } })
}
