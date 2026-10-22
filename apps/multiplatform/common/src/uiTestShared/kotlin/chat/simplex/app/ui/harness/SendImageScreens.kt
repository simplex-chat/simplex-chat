package chat.simplex.app.ui.harness

import androidx.compose.ui.graphics.ImageBitmap
import chat.simplex.common.platform.compressImageData
import chat.simplex.common.ui.Ui
import java.io.File

private const val JPEG_SIZE = 64
private val COMPRESSED_IMAGE_OFFER = Regex("""(?:\d{2}:\d{2} )?(.+)sends file (IMG_\S+\.jpg) \(.+ / \d+ bytes\)""")

fun UiScope.jpegFile(name: String): File =
  File(exchangeDir(), name).apply { writeBytes(compressImageData(ImageBitmap(JPEG_SIZE, JPEG_SIZE), usePng = false).toByteArray()) }

fun UiScope.attachImage(file: File) {
  openAttachmentSheet()
  openFile(file) { click(Ui.Chat.Attach("image")) }
  poll(what = "the preview of ${file.name} with `Cancel image preview`") { seesDescription("Cancel image preview") && seesDescription("preview image") }
}

fun CliPeer.receivesCompressedImage(sender: String, fileId: Int, timeoutMs: Long = CONNECT_TIMEOUT_MS): String {
  val line = nextLine(timeoutMs)
  val m = COMPRESSED_IMAGE_OFFER.matchEntire(line)?.takeIf { it.groupValues[1] == sender }
    ?: throw AssertionError("$name: expected `${sender}sends file IMG_….jpg (…)`, got `$line`; next: ${pending()}")
  this expects "use /fr $fileId [<dir>/ | <path>] to receive it"
  return m.groupValues[2]
}
