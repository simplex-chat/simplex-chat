package chat.simplex.common.views.helpers

import SectionItemView
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.text.style.TextAlign
import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatController.getNetCfg
import chat.simplex.common.model.json
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.WarningOrange
import chat.simplex.res.MR
import kotlinx.coroutines.*
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import okhttp3.OkHttpClient
import okhttp3.Request
import java.io.File
import java.net.InetSocketAddress
import java.net.Proxy

@Serializable
data class GitHubRelease(
  val id: Long,
  @SerialName("html_url")
  val htmlUrl: String,
  val name: String,
  val draft: Boolean,
  val prerelease: Boolean,
  val body: String,
  @SerialName("published_at")
  val publishedAt: String,
  val assets: List<GitHubAsset>
)

@Serializable
data class GitHubAsset(
  @SerialName("browser_download_url")
  val browserDownloadUrl: String,
  val name: String,
  val size: Long,

  val isAppImage: Boolean = name.lowercase().contains(".appimage")
)

private var updateCheckerJob: Job = Job()
fun setupUpdateChecker() = withLongRunningApi {
  updateCheckerJob.cancel()
  if (appPrefs.appUpdateChannel.get() == AppUpdatesChannel.NONE) {
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
  val client = setupHttpClient()
  try {
    val request = Request.Builder().url("https://api.github.com/repos/simplex-chat/simplex-chat/releases").addHeader("User-agent", "curl").build()
    client.newCall(request).execute().use { response ->
      response.body?.use {
        val body = it.string()
        val releases = json.decodeFromString<List<GitHubRelease>>(body).filterNot { it.draft }
        val release = when (appPrefs.appUpdateChannel.get()) {
          AppUpdatesChannel.STABLE -> releases.firstOrNull { !it.prerelease }
          AppUpdatesChannel.BETA -> releases.firstOrNull()
          AppUpdatesChannel.NONE -> return
        } ?: return
        val currentVersionName = "v" + (if (appPlatform.isAndroid) BuildConfigCommon.ANDROID_VERSION_NAME else BuildConfigCommon.DESKTOP_VERSION_NAME)
        val redactedCurrentVersionName = when {
          currentVersionName.contains('-') && currentVersionName.substringBefore('-').count { it == '.' } == 1 -> "${currentVersionName.substringBefore('-')}.0-${currentVersionName.substringAfter('-')}"
          currentVersionName.substringBefore('-').count { it == '.' } == 1 -> "${currentVersionName}.0"
          else -> currentVersionName
        }
        // LALAL
//        if (release.id == appPrefs.appSkippedUpdate.get() || release.name == currentVersionName || release.name == redactedCurrentVersionName) {
//          return
//        }
        val assets = chooseGitHubReleaseAssets(release).ifEmpty { return }
        AlertManager.shared.showAlertDialogButtonsColumn(
          generalGetString(MR.strings.app_check_for_updates_update_available).format(release.name),
          text = release.body,
          buttons = {
            val uriHandler = LocalUriHandler.current
            Column {
              for (asset in assets) {
                SectionItemView({
                  AlertManager.shared.hideAlert()
                  downloadAsset(asset)
                }) {
                  Text(generalGetString(MR.strings.app_check_for_updates_button_download).format("…" + asset.name.substringAfter("simplex-desktop-"), formatBytes(asset.size)), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
                }
              }

              SectionItemView({
                uriHandler.openUriCatching(release.htmlUrl)
              }) {
                Text(generalGetString(MR.strings.app_check_for_updates_button_read_more), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
              }

              SectionItemView({
                AlertManager.shared.hideAlert()
                skipRelease(release)
              }) {
                Text(generalGetString(MR.strings.app_check_for_updates_button_skip), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = WarningOrange)
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
  appPrefs.appSkippedUpdate.set(release.id)
}

private fun downloadAsset(asset: GitHubAsset) {
  showToast(generalGetString(MR.strings.app_check_for_updates_download_started))
  withLongRunningApi {
    val client = setupHttpClient()
    try {
      val request = Request.Builder().url(asset.browserDownloadUrl).addHeader("User-agent", "curl").build()
      client.newCall(request).execute().use { response ->
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
                text = generalGetString(MR.strings.app_check_for_updates_download_completed_desc),
                buttons = {
                  Column {
                    SectionItemView({
                      AlertManager.shared.hideAlert()
                      withLongRunningApi {
                        installAppUpdate(newFile)
                      }
                    }) {
                      Text(generalGetString(MR.strings.app_check_for_updates_button_install), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
                    }
                    SectionItemView({
                      AlertManager.shared.hideAlert()
                      desktopOpenDir(newFile.parentFile)
                    }) {
                      Text(generalGetString(MR.strings.app_check_for_updates_button_open), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
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
}


private fun chooseGitHubReleaseAssets(release: GitHubRelease): List<GitHubAsset> {
  val process = Runtime.getRuntime().exec("which dpkg").onExit().join()
  val isDebianBased = process.exitValue() == 0
  // Show all available .deb packages and user will choose the one that works on his system
  val res = if (isDebianBased) {
    release.assets.filter { it.name.lowercase().endsWith(".deb") }
  } else {
    release.assets.filter { it.name == desktopPlatform.githubAssetName }
  }
  return res
}

private suspend fun installAppUpdate(file: File) {
  when {
    desktopPlatform.isLinux() -> {
      val process = Runtime.getRuntime().exec("xdg-open ${file.absolutePath}").onExit().join()
      val startedInstallation = process.exitValue() == 0 && process.children().count() > 0
      if (!startedInstallation) {
        Log.e(TAG, "Error starting installation: ${process.inputReader().use { it.readLines().joinToString("\n") }}${process.errorStream.use { String(it.readAllBytes()) }}")
        // Failed to start installation. show directory with the file for manual installation
        desktopOpenDir(file.parentFile)
      } else {
        withApi {
          showToast(generalGetString(MR.strings.app_check_for_updates_installed_successfully))
        }
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
        withApi {
          showToast(generalGetString(MR.strings.app_check_for_updates_installed_successfully))
        }
        file.delete()
      }
    }
    desktopPlatform.isMac() -> {
      try {
        val process = Runtime.getRuntime().exec("hdiutil mount ${file.absolutePath}").onExit().join()
        val startedInstallation = process.exitValue() == 0
        if (!startedInstallation) {
          Log.e(TAG, "Error starting installation: ${process.inputReader().use { it.readLines().joinToString("\n") }}${process.errorStream.use { String(it.readAllBytes()) }}")
          // Failed to start installation. show directory with the file for manual installation
          desktopOpenDir(file.parentFile)
          return
        }
        var process2 = Runtime.getRuntime().exec("cp -R /Volumes/SimpleX/SimpleX.app /Applications").onExit().join()
        val copiedSuccessfully = process2.exitValue() == 0
        if (!copiedSuccessfully) {
          Log.e(TAG, "Error copying the app: ${process.inputReader().use { it.readLines().joinToString("\n") }}${process.errorStream.use { String(it.readAllBytes()) }}")
          // Failed to start installation. show directory with the file for manual installation
          desktopOpenDir(file.parentFile)
        } else {
          withApi {
            showToast(generalGetString(MR.strings.app_check_for_updates_installed_successfully))
          }
          file.delete()
        }
      } finally {
        val process3 = Runtime.getRuntime().exec("hdiutil unmount /Volumes/SimpleX").onExit().join()
      }
    }
  }
}
