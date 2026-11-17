package chat.simplex.app

import chat.simplex.common.views.helpers.*
import java.io.File
import java.nio.file.Files
import java.security.KeyPairGenerator
import java.security.MessageDigest
import java.security.PrivateKey
import java.security.Signature
import java.util.zip.ZipEntry
import java.util.zip.ZipOutputStream
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNotNull
import kotlin.test.assertNull
import kotlin.test.assertTrue

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
  private val base = uiBase("core-1", listOf("a-2.jar"))
  private val otherBase = uiBase("core-2", listOf("a-2.jar"))
  private val jar = byteArrayOf(1, 2, 3)

  private fun sha256(bytes: ByteArray) = MessageDigest.getInstance("SHA-256").digest(bytes).joinToString("") { "%02x".format(it) }

  private fun bundle(code: Int, signedBase: String = base, key: PrivateKey = keys.private): Map<String, ByteArray> {
    val manifest = """{"version_code":$code,"base":"$signedBase","jars":{"ui-1.jar":"${sha256(jar)}"}}""".toByteArray()
    val signature = Signature.getInstance("Ed25519").run {
      initSign(key)
      update(manifest)
      sign()
    }
    return mapOf("ui.json" to manifest, "ui.json.sig" to signature, "ui-1.jar" to jar)
  }

  private fun bundleDir(code: Int, signedBase: String = base): File {
    val dir = Files.createTempDirectory("ui-updates").toFile().resolve("$code")
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
    assertEquals("edccfa7920449a4d5a3cac37746f82d760a61616c2f034fac4c93d1b63a7e45c", uiBase("core-1", jars))
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
    assertNotNull(startableUiUpdate(confirmed, keys.public, base, 175))
    assertEquals(UiUpdateState.CONFIRMED, uiUpdateState(confirmed))

    val superseded = bundleDir(178)
    assertNull(startableUiUpdate(superseded, keys.public, base, 178))
    assertFalse(superseded.exists())
  }

  @Test
  fun testStageUiUpdate() {
    val root = Files.createTempDirectory("ui-updates").toFile()
    val files = bundle(176) + mapOf("state" to "CONFIRMED".toByteArray(), "../ui-2.jar" to jar, "lib/ui-3.jar" to jar)
    assertTrue(stageUiUpdate(bundleZip(root, files), root, keys.public, base, 175))
    val dir = root.resolve("176")
    assertEquals(setOf("ui.json", "ui.json.sig", "ui-1.jar"), dir.list()!!.toSet())
    assertEquals(UiUpdateState.STAGED, uiUpdateState(dir))
    assertFalse(root.resolve("staging").exists())
    assertFalse(root.parentFile.resolve("ui-2.jar").exists())

    val other = Files.createTempDirectory("ui-updates").toFile()
    assertFalse(stageUiUpdate(bundleZip(other, bundle(176, key = otherKeys.private)), other, keys.public, base, 175))
    assertEquals(setOf("ui.zip"), other.list()!!.toSet())
  }
}
