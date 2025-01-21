package chat.simplex.common.views.helpers

import SectionItemView
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.style.TextAlign
import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatController.getNetCfg
import chat.simplex.common.model.json
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.WarningOrange
import chat.simplex.common.views.onboarding.ReadMoreButton
import chat.simplex.res.MR
import kotlinx.coroutines.*
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.Transient
import okhttp3.OkHttpClient
import okhttp3.Request
import java.io.Closeable
import java.io.File
import java.net.InetSocketAddress
import java.net.Proxy
import kotlin.math.min

data class SemVer(
  val major: Int,
  val minor: Int,
  val patch: Int,
  val preRelease: String? = null,
  val buildNumber: Int? = null,
): Comparable<SemVer?> {

  val isNotStable: Boolean = preRelease != null

  override fun compareTo(other: SemVer?): Int {
    if (other == null) return 1
    return when {
      major != other.major -> major.compareTo(other.major)
      minor != other.minor -> minor.compareTo(other.minor)
      patch != other.patch -> patch.compareTo(other.patch)
      preRelease != null && other.preRelease != null -> {
        val pr = preRelease.compareTo(other.preRelease, ignoreCase = true)
        when {
          pr != 0 -> pr
          buildNumber != null && other.buildNumber != null -> buildNumber.compareTo(other.buildNumber)
          buildNumber != null -> -1
          other.buildNumber != null -> 1
          else -> 0
        }
      }
      preRelease != null -> -1
      other.preRelease != null -> 1
      else -> 0
    }
  }

  companion object {
    private val regex = Regex("^(\\d+)\\.(\\d+)\\.(\\d+)(?:-([A-Za-z]+)\\.(\\d+))?\$")
    fun from(tagName: String): SemVer? {
      val trimmed = tagName.trimStart { it == 'v' }
      val redacted = when {
        trimmed.contains('-') && trimmed.substringBefore('-').count { it == '.' } == 1 -> "${trimmed.substringBefore('-')}.0-${trimmed.substringAfter('-')}"
        trimmed.substringBefore('-').count { it == '.' } == 1 -> "${trimmed}.0"
        else -> trimmed
      }
      val group = regex.matchEntire(redacted)?.groups
      return if (group != null) {
        SemVer(
          major = group[1]?.value?.toIntOrNull() ?: return null,
          minor = group[2]?.value?.toIntOrNull() ?: return null,
          patch = group[3]?.value?.toIntOrNull() ?: return null,
          preRelease = group[4]?.value,
          buildNumber = group[5]?.value?.toIntOrNull(),
        )
      } else {
        null
      }
    }

    fun fromCurrentVersionName(): SemVer? {
      val currentVersionName = if (appPlatform.isAndroid) BuildConfigCommon.ANDROID_VERSION_NAME else BuildConfigCommon.DESKTOP_VERSION_NAME
      return from(currentVersionName)
    }
  }
}

@Serializable
data class GitHubRelease(
  @SerialName("tag_name")
  val tagName: String,
  @SerialName("html_url")
  val htmlUrl: String,
  val name: String,
  val draft: Boolean,
  @SerialName("prerelease")
  private val preRelease: Boolean,
  val body: String,
  @SerialName("published_at")
  val publishedAt: String,
  val assets: List<GitHubAsset>
) {
  @Transient
  val semVer: SemVer? = SemVer.from(tagName)

  val isConsideredBeta: Boolean = preRelease || semVer == null || semVer.isNotStable
}

@Serializable
data class GitHubAsset(
  @SerialName("browser_download_url")
  val browserDownloadUrl: String,
  val name: String,
  val size: Long,

  val isAppImage: Boolean = name.lowercase().contains(".appimage")
)

fun showAppUpdateNotice() {
  AlertManager.shared.showAlertDialogButtonsColumn(
    generalGetString(MR.strings.app_check_for_updates_notice_title),
    text = generalGetString(MR.strings.app_check_for_updates_notice_desc),
    buttons = {
      Column {
        SectionItemView({
          AlertManager.shared.hideAlert()
          appPrefs.appUpdateChannel.set(AppUpdatesChannel.STABLE)
          setupUpdateChecker()
        }) {
          Text(generalGetString(MR.strings.app_check_for_updates_stable), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }

        SectionItemView({
          AlertManager.shared.hideAlert()
          appPrefs.appUpdateChannel.set(AppUpdatesChannel.BETA)
          setupUpdateChecker()
        }) {
          Text(generalGetString(MR.strings.app_check_for_updates_beta), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }

        SectionItemView({
          AlertManager.shared.hideAlert()
          appPrefs.appUpdateChannel.set(AppUpdatesChannel.DISABLED)
        }) {
          Text(generalGetString(MR.strings.app_check_for_updates_notice_disable), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
      }
    }
  )
}

private var updateCheckerJob: Job = Job()
fun setupUpdateChecker() = withLongRunningApi {
  updateCheckerJob.cancel()
  if (appPrefs.appUpdateChannel.get() == AppUpdatesChannel.DISABLED) {
    return@withLongRunningApi
  }
  checkForUpdate()
  createUpdateJob()
}

private fun createUpdateJob() {
  updateCheckerJob = withLongRunningApi {
    delay(24 * 60 * 60 * 1000)
    checkForUpdate()
    createUpdateJob()
  }
}


fun checkForUpdate() {
  Log.d(TAG, "Checking for update")
  val currentSemVer = SemVer.fromCurrentVersionName()
  if (currentSemVer == null) {
    Log.e(TAG, "Current SemVer cannot be parsed")
    return
  }
  val client = setupHttpClient()
  try {
    val request = Request.Builder().url("https://api.github.com/repos/simplex-chat/simplex-chat/releases").addHeader("User-agent", "curl").build()
    client.newCall(request).execute().use { response ->
      response.body?.use {
        val body = it.string()
        val releases = json.decodeFromString<List<GitHubRelease>>(body)
        val release = when (appPrefs.appUpdateChannel.get()) {
          AppUpdatesChannel.STABLE -> releases.firstOrNull { r -> !r.draft && !r.isConsideredBeta && currentSemVer < r.semVer }
          AppUpdatesChannel.BETA -> releases.firstOrNull { r -> !r.draft && currentSemVer < r.semVer }
          AppUpdatesChannel.DISABLED -> return
        }

        if (release == null || release.tagName == appPrefs.appSkippedUpdate.get()) {
          Log.d(TAG, "Skipping update because of the same version or skipped version")
          return
        }
        val assets = chooseGitHubReleaseAssets(release)
        // No need to show an alert if no suitable packages were found. But for Flatpak users it's useful to see release notes anyway
        if (assets.isEmpty() && !isRunningFromFlatpak()) {
          Log.d(TAG, "No assets to download for current system")
          return
        }
        val lines = ArrayList<String>()
        for (line in release.body.lines()) {
          if (line == "Commits:") break
          lines.add(line)
        }
        val text = lines.joinToString("\n")
        AlertManager.shared.showAlertDialogButtonsColumn(
          generalGetString(MR.strings.app_check_for_updates_update_available).format(release.name),
          text = text,
          textAlign = TextAlign.Start,
          dismissible = false,
          belowTextContent = {
            ReadMoreButton(release.htmlUrl)
          },
          buttons = {
            Column {
              for (asset in assets) {
                SectionItemView({
                  AlertManager.shared.hideAlert()
                  chatModel.updatingProgress.value = 0f
                  withLongRunningApi {
                    try {
                      downloadAsset(asset)
                    } finally {
                      chatModel.updatingProgress.value = null
                    }
                  }
                }) {
                  Text(
                    generalGetString(MR.strings.app_check_for_updates_button_download).format(
                      if (asset.name.length > 34) "…" + asset.name.substringAfter("simplex-desktop-") else asset.name,
                      formatBytes(asset.size)), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary
                  )
                }
              }

              SectionItemView({
                AlertManager.shared.hideAlert()
                skipRelease(release)
              }) {
                Text(generalGetString(MR.strings.app_check_for_updates_button_skip), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = WarningOrange)
              }

              SectionItemView({
                AlertManager.shared.hideAlert()
              }) {
                Text(generalGetString(MR.strings.app_check_for_updates_button_remind_later), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
              }
            }
          }
        )
      }
    }
  } catch (e: Exception) {
    Log.e(TAG, "Failed to get the latest release: ${e.stackTraceToString()}")
  }
}

private fun setupHttpClient(): OkHttpClient {
  val netCfg = getNetCfg()
  var proxy: Proxy? = null
  if (netCfg.useSocksProxy && netCfg.socksProxy != null) {
    val hostname = netCfg.socksProxy.substringBefore(":").ifEmpty { "localhost" }
    val port = netCfg.socksProxy.substringAfter(":").toIntOrNull()
    if (port != null) {
      proxy = Proxy(Proxy.Type.SOCKS, InetSocketAddress(hostname, port))
    }
  }
  return OkHttpClient.Builder().proxy(proxy).followRedirects(true).build()
}

private fun skipRelease(release: GitHubRelease) {
  appPrefs.appSkippedUpdate.set(release.tagName)
}

private suspend fun downloadAsset(asset: GitHubAsset) {
  withContext(Dispatchers.Main) {
    showToast(generalGetString(MR.strings.app_check_for_updates_download_started))
  }
  val progressListener = object: ProgressListener {
    override fun update(bytesRead: Long, contentLength: Long, done: Boolean) {
      if (contentLength != -1L) {
        chatModel.updatingProgress.value = if (done) 1f else bytesRead / contentLength.toFloat()
      }
    }
  }
  val client = setupHttpClient().newBuilder()
    .addNetworkInterceptor { chain ->
      val originalResponse = chain.proceed(chain.request())
      val body = originalResponse.body
      if (body != null) {
        originalResponse.newBuilder().body(ProgressResponseBody(body, progressListener)).build()
      } else {
        originalResponse
      }
    }
    .build()

  try {
    val request = Request.Builder().url(asset.browserDownloadUrl).addHeader("User-agent", "curl").build()
    val call = client.newCall(request)
    chatModel.updatingRequest = Closeable {
      call.cancel()
      withApi {
        showToast(generalGetString(MR.strings.app_check_for_updates_canceled))
      }
    }
    call.execute().use { response ->
      response.body?.use { body ->
        body.byteStream().use { stream ->
          createTmpFileAndDelete { file ->
            // It's important to close output stream (with use{}), otherwise, Windows cannot rename the file
            file.outputStream().use { output ->
              stream.copyTo(output)
            }
            val newFile = File(file.parentFile, asset.name)
            file.renameTo(newFile)

            AlertManager.shared.showAlertDialogButtonsColumn(
              generalGetString(MR.strings.app_check_for_updates_download_completed_title),
              dismissible = false,
              buttons = {
                Column {
                  // It's problematic to install .deb package because it requires either root or GUI package installer which is not available on
                  // Debian by default. Let the user install it manually only
                  if (!asset.name.lowercase().endsWith(".deb")) {
                    SectionItemView({
                      AlertManager.shared.hideAlert()
                      chatModel.updatingProgress.value = -1f
                      withLongRunningApi {
                        try {
                          installAppUpdate(newFile)
                        } finally {
                          chatModel.updatingProgress.value = null
                        }
                      }
                    }) {
                      Text(generalGetString(MR.strings.app_check_for_updates_button_install), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
                    }
                  }

                  SectionItemView({
                    desktopOpenDir(newFile.parentFile)
                  }) {
                    Text(generalGetString(MR.strings.app_check_for_updates_button_open), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
                  }

                  SectionItemView({
                    AlertManager.shared.hideAlert()
                    newFile.delete()
                  }) {
                    Text(generalGetString(MR.strings.cancel_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = Color.Red)
                  }
                }
              }
            )
          }
        }
      }
    }
  } catch (e: Exception) {
    Log.e(TAG, "Failed to download the asset from release: ${e.stackTraceToString()}")
  }
}

private fun isRunningFromAppImage(): Boolean = System.getenv("APPIMAGE") != null

private fun isRunningFromFlatpak(): Boolean = System.getenv("container") == "flatpak"

private fun chooseGitHubReleaseAssets(release: GitHubRelease): List<GitHubAsset> {
  val res = if (isRunningFromFlatpak()) {
    // No need to show download options for Flatpak users
    emptyList()
  } else if (!isRunningFromAppImage() && Runtime.getRuntime().exec("which dpkg").onExit().join().exitValue() == 0) {
    // Show all available .deb packages and user will choose the one that works on his system (for Debian derivatives)
    release.assets.filter { it.name.lowercase().endsWith(".deb") }
  } else {
    release.assets.filter { it.name == desktopPlatform.githubAssetName }
  }
  return res
}

private suspend fun installAppUpdate(file: File) = withContext(Dispatchers.IO) {
  when {
    desktopPlatform.isLinux() -> {
      val process = Runtime.getRuntime().exec("xdg-open ${file.absolutePath}").onExit().join()
      val startedInstallation = process.exitValue() == 0 && process.children().count() > 0
      if (!startedInstallation) {
        Log.e(TAG, "Error starting installation: ${process.inputReader().use { it.readLines().joinToString("\n") }}${process.errorStream.use { String(it.readAllBytes()) }}")
        // Failed to start installation. show directory with the file for manual installation
        desktopOpenDir(file.parentFile)
      } else {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.app_check_for_updates_installed_successfully_title),
          text = generalGetString(MR.strings.app_check_for_updates_installed_successfully_desc)
        )
        file.delete()
      }
    }
    desktopPlatform.isWindows() -> {
      val process = Runtime.getRuntime().exec("msiexec /i ${file.absolutePath}"/* /qb */).onExit().join()
      val startedInstallation = process.exitValue() == 0
      if (!startedInstallation) {
        Log.e(TAG, "Error starting installation: ${process.inputReader().use { it.readLines().joinToString("\n") }}${process.errorStream.use { String(it.readAllBytes()) }}")
        // Failed to start installation. show directory with the file for manual installation
        desktopOpenDir(file.parentFile)
      } else {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.app_check_for_updates_installed_successfully_title),
          text = generalGetString(MR.strings.app_check_for_updates_installed_successfully_desc)
        )
        file.delete()
      }
    }
    desktopPlatform.isMac() -> {
      // Default mount point if no other DMGs were mounted before
      var volume = "/Volumes/SimpleX"
      try {
        val process = Runtime.getRuntime().exec("hdiutil mount ${file.absolutePath}").onExit().join()
        val startedInstallation = process.exitValue() == 0
        val lines = process.inputReader().use { it.readLines() }
        // This is needed for situations when mount point has non-default path. 
        // For example, when a user already had mounted SimpleX.dmg before and default mount point is not available.
        // Mac will make volume like /Volumes/SimpleX 1
        val lastLine = lines.lastOrNull()?.substringAfterLast('\t')
        if (!startedInstallation || lastLine == null || !lastLine.lowercase().contains("/volumes/")) {
          Log.e(TAG, "Error starting installation: ${process.inputReader().use { it.readLines().joinToString("\n") }}${process.errorStream.use { String(it.readAllBytes()) }}")
          // Failed to start installation. show directory with the file for manual installation
          desktopOpenDir(file.parentFile)
          return@withContext
        }
        volume = lastLine
        File("/Applications/SimpleX.app").renameTo(File("/Applications/SimpleX-old.app"))
        val process2 = Runtime.getRuntime().exec(arrayOf("cp", "-R", "${volume}/SimpleX.app", "/Applications")).onExit().join()
        val copiedSuccessfully = process2.exitValue() == 0
        if (!copiedSuccessfully) {
          Log.e(TAG, "Error copying the app: ${process2.inputReader().use { it.readLines().joinToString("\n") }}${process2.errorStream.use { String(it.readAllBytes()) }}")
          // Failed to start installation. show directory with the file for manual installation
          desktopOpenDir(file.parentFile)
        } else {
          AlertManager.shared.showAlertMsg(
            title = generalGetString(MR.strings.app_check_for_updates_installed_successfully_title),
            text = generalGetString(MR.strings.app_check_for_updates_installed_successfully_desc)
          )
          file.delete()
        }
      } finally {
        try {
          Runtime.getRuntime().exec(arrayOf("hdiutil", "unmount", volume)).onExit().join()
        } finally {
          if (!File("/Applications/SimpleX.app").exists()) {
            File("/Applications/SimpleX-old.app").renameTo(File("/Applications/SimpleX.app"))
          } else {
            Runtime.getRuntime().exec("rm -rf /Applications/SimpleX-old.app").onExit().join()
          }
        }
      }
    }
  }
  Unit
}
