package chat.simplex.app

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.width
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.collectAsState
import androidx.compose.ui.Modifier
import androidx.compose.ui.layout.onSizeChanged
import androidx.compose.ui.unit.DpSize
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.WindowState
import chat.simplex.common.AppPreview
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel
import chat.simplex.common.simplexWindowState
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.PreviewTerminalLayout
import chat.simplex.common.views.call.PreviewIncomingCallAlertLayout
import chat.simplex.common.views.chat.*
import chat.simplex.common.views.chat.group.*
import chat.simplex.common.views.chat.item.*
import chat.simplex.common.views.chatlist.*
import chat.simplex.common.views.database.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.*
import chat.simplex.common.views.onboarding.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.common.views.usersettings.networkAndServers.*
import kotlinx.serialization.Serializable
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import org.jetbrains.skia.EncodedImageFormat
import org.jetbrains.skia.Image
import org.jetbrains.skia.Surface
import java.io.File
import java.util.Locale
import java.util.TimeZone
import kotlin.math.roundToInt
import kotlin.test.Test
import kotlin.test.assertEquals

private const val SCENE_WIDTH = 402
private const val SCENE_MAX_HEIGHT = 874

private val chatItemFontScales = listOf(1f, 1.4f, 1.5f)

private class SnapshotScene(val name: String, val fontScales: List<Float> = listOf(1f), val content: @Composable () -> Unit)

@Serializable
private class Snapshot(val scene: String, val theme: String, val fontScale: Float, val file: String, val width: Int, val height: Int)

private val indexJson = Json { prettyPrint = true }

private val previewScenes = listOf(
  SnapshotScene("TerminalView.PreviewTerminalLayout") { PreviewTerminalLayout() },
  SnapshotScene("IncomingCallAlertView.PreviewIncomingCallAlertLayout") { PreviewIncomingCallAlertLayout() },
  SnapshotScene("ChatInfoView.PreviewChatInfoLayout") { chat.simplex.common.views.chat.PreviewChatInfoLayout() },
  SnapshotScene("ChatView.PreviewChatLayout") { PreviewChatLayout() },
  SnapshotScene("ChatView.PreviewGroupChatLayout") { PreviewGroupChatLayout() },
  SnapshotScene("ComposeFileView.PreviewComposeFileView") { PreviewComposeFileView() },
  SnapshotScene("ComposeVoiceView.PreviewComposeAudioView") { PreviewComposeAudioView() },
  SnapshotScene("ContextItemView.PreviewContextItemView") { PreviewContextItemView() },
  SnapshotScene("SendMsgView.PreviewSendMsgView") { PreviewSendMsgView() },
  SnapshotScene("SendMsgView.PreviewSendMsgViewEditing") { PreviewSendMsgViewEditing() },
  SnapshotScene("SendMsgView.PreviewSendMsgViewInProgress") { PreviewSendMsgViewInProgress() },
  SnapshotScene("AddGroupMembersView.PreviewAddGroupMembersLayout") { PreviewAddGroupMembersLayout() },
  SnapshotScene("GroupChatInfoView.PreviewGroupChatInfoLayout") { PreviewGroupChatInfoLayout() },
  SnapshotScene("GroupMemberInfoView.PreviewGroupMemberInfoLayout") { PreviewGroupMemberInfoLayout() },
  SnapshotScene("CIEventView.CIEventViewPreview", chatItemFontScales) { CIEventViewPreview() },
  SnapshotScene("CIGroupInvitationView.PendingCIGroupInvitationViewPreview", chatItemFontScales) { PendingCIGroupInvitationViewPreview() },
  SnapshotScene("CIGroupInvitationView.CIGroupInvitationViewAcceptedPreview", chatItemFontScales) { CIGroupInvitationViewAcceptedPreview() },
  SnapshotScene("CIGroupInvitationView.CIGroupInvitationViewLongNamePreview", chatItemFontScales) { CIGroupInvitationViewLongNamePreview() },
  SnapshotScene("CIMetaView.PreviewCIMetaView", chatItemFontScales) { SimpleXTheme { PreviewCIMetaView() } },
  SnapshotScene("CIMetaView.PreviewCIMetaViewUnread", chatItemFontScales) { SimpleXTheme { PreviewCIMetaViewUnread() } },
  SnapshotScene("CIMetaView.PreviewCIMetaViewSendFailed", chatItemFontScales) { SimpleXTheme { PreviewCIMetaViewSendFailed() } },
  SnapshotScene("CIMetaView.PreviewCIMetaViewSendNoAuth", chatItemFontScales) { SimpleXTheme { PreviewCIMetaViewSendNoAuth() } },
  SnapshotScene("CIMetaView.PreviewCIMetaViewSendSent", chatItemFontScales) { SimpleXTheme { PreviewCIMetaViewSendSent() } },
  SnapshotScene("CIMetaView.PreviewCIMetaViewEdited", chatItemFontScales) { SimpleXTheme { PreviewCIMetaViewEdited() } },
  SnapshotScene("CIMetaView.PreviewCIMetaViewEditedUnread", chatItemFontScales) { SimpleXTheme { PreviewCIMetaViewEditedUnread() } },
  SnapshotScene("CIMetaView.PreviewCIMetaViewEditedSent", chatItemFontScales) { SimpleXTheme { PreviewCIMetaViewEditedSent() } },
  SnapshotScene("CIMetaView.PreviewCIMetaViewDeletedContent", chatItemFontScales) { SimpleXTheme { PreviewCIMetaViewDeletedContent() } },
  SnapshotScene("ChatItemView.PreviewChatItemView", chatItemFontScales) { SimpleXTheme { PreviewChatItemView() } },
  SnapshotScene("ChatItemView.PreviewChatItemViewDeletedContent", chatItemFontScales) { PreviewChatItemViewDeletedContent() },
  SnapshotScene("DeletedItemView.PreviewDeletedItemView", chatItemFontScales) { PreviewDeletedItemView() },
  SnapshotScene("IntegrityErrorItemView.IntegrityErrorItemViewPreview", chatItemFontScales) { IntegrityErrorItemViewPreview() },
  SnapshotScene("MarkedDeletedItemView.PreviewMarkedDeletedItemView", chatItemFontScales) { PreviewMarkedDeletedItemView() },
  SnapshotScene("ChatHelpView.PreviewChatHelpLayout") { PreviewChatHelpLayout() },
  SnapshotScene("ChatListNavLinkView.PreviewChatListNavLinkDirect") { PreviewChatListNavLinkDirect() },
  SnapshotScene("ChatListNavLinkView.PreviewChatListNavLinkGroup") { PreviewChatListNavLinkGroup() },
  SnapshotScene("ChatListNavLinkView.PreviewChatListNavLinkContactRequest") { PreviewChatListNavLinkContactRequest() },
  SnapshotScene("ChatPreviewView.PreviewChatPreviewView") { PreviewChatPreviewView() },
  SnapshotScene("DatabaseEncryptionView.PreviewDatabaseEncryptionLayout") { PreviewDatabaseEncryptionLayout() },
  SnapshotScene("DatabaseErrorView.PreviewChatInfoLayout") { chat.simplex.common.views.database.PreviewChatInfoLayout() },
  SnapshotScene("DatabaseView.PreviewDatabaseLayout") { PreviewDatabaseLayout() },
  SnapshotScene("ChatInfoImage.PreviewChatInfoImage") { PreviewChatInfoImage() },
  SnapshotScene("LinkPreviews.PreviewChatItemLinkView") { PreviewChatItemLinkView() },
  SnapshotScene("LinkPreviews.PreviewComposeLinkView") { PreviewComposeLinkView() },
  SnapshotScene("LinkPreviews.PreviewComposeLinkViewLoading") { PreviewComposeLinkViewLoading() },
  SnapshotScene("SimpleButton.PreviewShareButton") { PreviewShareButton() },
  SnapshotScene("ContactConnectionInfoView.PreviewContactConnectionInfoView") { PreviewContactConnectionInfoView() },
  SnapshotScene("NewChatSheet.PreviewNewChatSheet") { PreviewNewChatSheet() },
  SnapshotScene("QRCode.PreviewQRCode") { PreviewQRCode() },
  SnapshotScene("HowItWorks.PreviewHowItWorks") { PreviewHowItWorks() },
  SnapshotScene("SimpleXInfo.PreviewSimpleXInfo") { PreviewSimpleXInfo() },
  SnapshotScene("WhatsNewView.PreviewWhatsNewView") { PreviewWhatsNewView() },
  SnapshotScene("HelpView.PreviewHelpView") { PreviewHelpView() },
  SnapshotScene("MarkdownHelpView.PreviewMarkdownHelpView") { PreviewMarkdownHelpView() },
  SnapshotScene("SettingsView.PreviewSettingsLayout") { PreviewSettingsLayout() },
  SnapshotScene("UserAddressView.PreviewUserAddressLayoutNoAddress") { PreviewUserAddressLayoutNoAddress() },
  SnapshotScene("UserAddressView.PreviewUserAddressLayoutAddressCreated") { PreviewUserAddressLayoutAddressCreated() },
  SnapshotScene("AdvancedNetworkSettings.PreviewAdvancedNetworkSettingsLayout") { PreviewAdvancedNetworkSettingsLayout() },
  SnapshotScene("NetworkAndServers.PreviewNetworkAndServersLayout") { PreviewNetworkAndServersLayout() },
  SnapshotScene("DesktopApp.AppPreview") { AppPreview() },
)

private val previewsCallingCore = listOf(
  "AddChannelView.PreviewAddChannelView",
  "AddGroupView.PreviewAddGroupLayout",
  "GroupProfileView.PreviewGroupProfileLayout",
  "UserProfileView.PreviewUserProfileLayoutEditOff",
  "UserProfileView.PreviewUserProfileLayoutEditOn",
)

private val previewFunction = Regex("""@Preview\s+(?:@Composable\s+)?(?:\w+\s+)*fun\s+(\w+)""")

class SnapshotTest {
  @Test
  fun everyPreviewIsRendered() {
    assertEquals(previewsInSources(), (previewScenes.map { it.name } + previewsCallingCore).sorted())
  }

  @Test
  fun renderPreviews() {
    val dir = File(System.getenv("SNAPSHOT_DIR") ?: "build/snapshots").apply { mkdirs() }
    val locale = Locale.getDefault()
    val timeZone = TimeZone.getDefault()
    val theme = appPrefs.currentTheme.get()!!
    val themeOverrides = appPrefs.themeOverrides.get()
    val fontScale = appPrefs.fontScale.get()
    val index = mutableListOf<Snapshot>()
    val failures = mutableListOf<String>()
    try {
      Locale.setDefault(Locale.US)
      TimeZone.setDefault(TimeZone.getTimeZone("UTC"))
      simplexWindowState.windowState = WindowState(size = DpSize(SCENE_WIDTH.dp, SCENE_MAX_HEIGHT.dp))
      appPrefs.themeOverrides.set(listOf(ThemeOverrides(base = DefaultTheme.LIGHT), ThemeOverrides(base = DefaultTheme.DARK)))
      for (base in listOf(DefaultTheme.LIGHT, DefaultTheme.DARK)) {
        ThemeManager.applyTheme(base.themeName)
        val themeName = base.name.lowercase()
        for (scene in previewScenes) {
          for (scale in scene.fontScales) {
            appPrefs.fontScale.set(scale)
            ChatModel.terminalItems.value = listOf()
            val file = "${scene.name}-$themeName-font-${(scale * 100).roundToInt()}.png"
            try {
              val image = renderSnapshot(scene.content)
              File(dir, file).writeBytes(image.encodeToData(EncodedImageFormat.PNG)!!.bytes)
              index += Snapshot(scene.name, themeName, scale, file, image.width, image.height)
            } catch (e: Throwable) {
              failures += "$file: ${e.stackTraceToString()}"
            }
          }
        }
      }
      File(dir, "index-desktop.json").writeText(indexJson.encodeToString(index))
    } finally {
      Locale.setDefault(locale)
      TimeZone.setDefault(timeZone)
      appPrefs.themeOverrides.set(themeOverrides)
      appPrefs.fontScale.set(fontScale)
      ThemeManager.applyTheme(theme)
    }
    assertEquals(emptyList(), failures)
  }
}

private fun renderSnapshot(content: @Composable () -> Unit): Image {
  var height = 0
  val frame = renderScene(width = SCENE_WIDTH, height = SCENE_MAX_HEIGHT, frames = 4) {
    Box(Modifier.width(SCENE_WIDTH.dp).background(CurrentColors.collectAsState().value.colors.background).onSizeChanged { height = it.height }) {
      CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler(), content = content)
    }
  }
  return Surface.makeRasterN32Premul(SCENE_WIDTH, height).run {
    canvas.drawImage(frame, 0f, 0f)
    makeImageSnapshot()
  }
}

private fun previewsInSources(): List<String> =
  listOf("src/commonMain/kotlin", "src/desktopMain/kotlin")
    .flatMap { dir -> File(dir).walk().filter { it.extension == "kt" } }
    .flatMap { file -> previewFunction.findAll(withoutComments(file.readText())).map { "${file.nameWithoutExtension}.${it.groupValues[1]}" } }
    .sorted()

private fun withoutComments(source: String): String {
  val code = StringBuilder()
  var i = 0
  while (i < source.length) {
    val end = tokenEnd(source, i)
    if (source.startsWith("//", i) || source.startsWith("/*", i)) code.append(' ') else code.append(source, i, end)
    i = end
  }
  return code.toString()
}

private fun tokenEnd(source: String, start: Int): Int = when {
  source.startsWith("//", start) -> source.indexOf('\n', start).takeIf { it >= 0 } ?: source.length
  source.startsWith("/*", start) -> {
    var depth = 0
    var i = start
    do {
      when {
        source.startsWith("/*", i) -> { depth++; i += 2 }
        source.startsWith("*/", i) -> { depth--; i += 2 }
        else -> i++
      }
    } while (depth > 0)
    i
  }
  source.startsWith("\"\"\"", start) -> {
    var i = source.indexOf("\"\"\"", start + 3)
    while (source.startsWith("\"\"\"\"", i)) i++
    i + 3
  }
  source[start] == '"' || source[start] == '\'' -> {
    var i = start + 1
    while (source[i] != source[start]) i += if (source[i] == '\\') 2 else 1
    i + 1
  }
  else -> start + 1
}
