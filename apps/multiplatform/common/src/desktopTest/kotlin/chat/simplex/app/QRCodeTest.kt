package chat.simplex.app

import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.test.ComposeUiTest
import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.assertCountEquals
import androidx.compose.ui.test.onAllNodesWithContentDescription
import androidx.compose.ui.test.onNodeWithContentDescription
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.compose.ui.test.runComposeUiTest
import boofcv.alg.fiducial.qrcode.QrCode
import chat.simplex.common.model.*
import chat.simplex.common.views.chat.ChatInfoView
import chat.simplex.common.views.helpers.LocalAppBarHandler
import chat.simplex.common.views.helpers.rememberAppBarHandler
import chat.simplex.common.views.newchat.LocalFixedQRCode
import chat.simplex.common.views.newchat.SimpleXLinkQRCode
import chat.simplex.common.views.newchat.qrCodeBitmap
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withContext
import kotlin.test.AfterTest
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertNotNull
import kotlin.test.assertNull

@OptIn(ExperimentalTestApi::class)
class QRCodeTest {
  private val link = "https://simplex.chat/contact#/?v=2-7&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FWHV0YU1sYlU7NqiEHkHDB6gxO1ofTync%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAWbebOqVYuBXaiqHcXYjEHCpYi6VzDlu6CVaijDTmsQU%253D"

  @AfterTest
  fun tearDown() {
    ChatModel.chatsContext.chats.value.clear()
    ChatModel.currentUser.value = null
  }

  @Test
  fun testQRCodeBitmapOfLinkWithinCapacity() {
    assertNotNull(qrCodeBitmap(link, errorLevel = QrCode.ErrorLevel.L))
    assertNotNull(qrCodeBitmap(link, errorLevel = QrCode.ErrorLevel.M))
  }

  @Test
  fun testNoQRCodeBitmapOfLinkAboveCapacity() {
    assertNull(qrCodeBitmap("$link&data=${"a".repeat(3000)}", errorLevel = QrCode.ErrorLevel.L))
    assertNull(qrCodeBitmap("$link&data=${"a".repeat(2400)}", errorLevel = QrCode.ErrorLevel.M))
  }

  @Test
  fun testContactInfoShowsQRCodeOfAddress() = contactInfo(link) {
    onNodeWithText(ADDRESS_FOOTER).assertExists()
    onAllNodesWithContentDescription("QR Code").assertCountEquals(1)
  }

  @Test
  fun testContactInfoWithoutQRCodeOfAddressAboveCapacity() = contactInfo("$link&data=${"a".repeat(3000)}") {
    onNodeWithText(ADDRESS_FOOTER).assertExists()
    onAllNodesWithContentDescription("QR Code").assertCountEquals(0)
  }

  @Test
  fun testNoShareOfQRCodeAboveCapacity() = runComposeUiTest {
    var shared = false
    setContent {
      CompositionLocalProvider(LocalFixedQRCode provides qrCodeBitmap(link, errorLevel = QrCode.ErrorLevel.L)) {
        SimpleXLinkQRCode("$link&data=${"a".repeat(3000)}", onShare = { shared = true })
      }
    }
    onNodeWithContentDescription("QR Code").performClick()
    waitForIdle()
    assertFalse(shared)
  }

  private fun contactInfo(address: String, test: ComposeUiTest.() -> Unit) = runComposeUiTest {
    val contact = Contact.sampleData.copy(profile = LocalProfile.sampleData.copy(contactLink = address))
    ChatModel.currentUser.value = User.sampleData
    runBlocking { withContext(Dispatchers.Main) { ChatModel.chatsContext.updateChat(null, ChatInfo.Direct(contact)) } }
    setContent {
      CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
        ChatInfoView(ChatModel.chatsContext, ChatModel, contact, connectionStats = null, customUserProfile = null, localAlias = "", connectionCode = null, close = {}, onSearchClicked = {})
      }
    }
    test()
  }

  private companion object {
    const val ADDRESS_FOOTER = "You can share this address with your contacts to let them connect with alice."
  }
}
