package chat.simplex.app.ui.desktop

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.CliPeer
import chat.simplex.app.ui.harness.UiEnv
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.described
import chat.simplex.app.ui.harness.onEdt
import chat.simplex.app.ui.harness.openAttachmentSheet
import chat.simplex.app.ui.harness.receivesFile
import chat.simplex.common.simplexWindowState
import chat.simplex.common.ui.Ui
import org.junit.Test
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import kotlin.random.Random

class SendAttachmentDesktopTest : UiTest() {
  @Test fun attachImage_textOnLastImageOnly() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    val first = writeLargePhoto(File(exchangeDir(), "first.jpg"))
    val second = File(UiEnv.repoRoot, "tests/fixtures/test.jpg").copyTo(File(exchangeDir(), "second.jpg"), overwrite = true)
    alice.chatList.open("bob")
    waitFor(Ui.Chat.Message)
    check(!seesDescription("Cancel image preview")) { "an image preview is shown before an image is picked" }
    openAttachmentSheet()
    openFiles(listOf(first)) { click(Ui.Chat.Attach("image")) }
    poll(what = "the preview of first.jpg") { seesDescription("Cancel image preview") && seesDescription("preview image") }
    click(described("Cancel image preview"))
    poll(what = "the image preview removed") { !seesDescription("Cancel image preview") && !seesDescription("preview image") }
    openAttachmentSheet()
    openFiles(listOf(first, second)) { click(Ui.Chat.Attach("image")) }
    poll(what = "the previews of first.jpg and second.jpg") { seesDescription("Cancel image preview") && seesDescription("preview image") }
    typeInto(Ui.Chat.Message, "caption")
    click(Ui.Chat.Send)
    poll(what = "the previews and the caption cleared after Send") { !seesDescription("Cancel image preview") && editableText(Ui.Chat.Message).isNullOrEmpty() }
    val firstName = bob.receivesImageOffer(1)
    bob expectsMessage "alice> caption"
    val secondName = bob.receivesImageOffer(2)
    bob.receivesFile(1, firstName, "alice")
    bob.receivesFile(2, secondName, "alice")
  }

  private fun writeLargePhoto(file: File): File {
    val image = BufferedImage(3000, 2000, BufferedImage.TYPE_INT_RGB)
    val random = Random(1)
    image.setRGB(0, 0, image.width, image.height, IntArray(image.width * image.height) { random.nextInt() }, 0, image.width)
    ImageIO.write(image, "jpg", file)
    check(file.length() > MAX_COMPRESSED_BYTES) { "${file.name} is ${file.length()} bytes, not above the $MAX_COMPRESSED_BYTES bytes image limit" }
    return file
  }

  private fun UiScope.openFiles(files: List<File>, action: () -> Unit) {
    val dialog = simplexWindowState.openMultipleDialog
    action()
    poll(what = "the open files dialog") { dialog.isAwaiting }
    onEdt { dialog.onResult(files) }
    poll(what = "the open files dialog to close") { !dialog.isAwaiting }
  }

  private fun CliPeer.receivesImageOffer(fileId: Int): String {
    val line = nextLine(CONNECT_TIMEOUT_MS)
    val m = IMAGE_OFFER.matchEntire(line) ?: throw AssertionError("$name: expected a compressed image offer, got `$line`; next: ${pending()}")
    check(m.groupValues[2].toLong() <= MAX_COMPRESSED_BYTES) { "${m.groupValues[1]} was sent uncompressed: ${m.groupValues[2]} bytes" }
    this expects "use /fr $fileId [<dir>/ | <path>] to receive it"
    return m.groupValues[1]
  }

  private companion object {
    val IMAGE_OFFER = Regex("""(?:\d{2}:\d{2} )?alice> sends file (IMG_\S+\.jpg) \(.+ / (\d+) bytes\)""")
    const val MAX_COMPRESSED_BYTES = 255L * 1024
  }
}
