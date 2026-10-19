package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.chat.providerForGallery
import kotlinx.datetime.Clock
import kotlin.test.AfterTest
import kotlin.test.BeforeTest
import kotlin.test.Test
import kotlin.test.assertEquals

// Regression for PR #6869: scrollToStart() must not rewrite initialChatId.
class ProviderForGalleryTest {

  // Synthetic items pass canShowMedia only when chatModel.connectedToRemote() is true.
  @BeforeTest
  fun connectChatModelToRemote() {
    chatModel.currentRemoteHost.value = RemoteHostInfo(
      remoteHostId = 0L,
      hostDeviceName = "",
      storePath = "",
      bindAddress_ = null,
      bindPort_ = null,
      sessionState = null,
    )
  }

  @AfterTest
  fun resetChatModel() {
    chatModel.currentRemoteHost.value = null
  }

  @Test
  fun testScrollToStartPreservesAnchor() {
    val items = listOf(imageItem(1L), imageItem(2L), imageItem(3L))
    var scrolledTo: Int? = null
    val provider = providerForGallery(items, cItemId = 3L) { scrolledTo = it }

    provider.currentPageChanged(provider.initialIndex - 1)
    provider.scrollToStart()
    provider.onDismiss(0)

    assertEquals(1, scrolledTo)
  }

  // Pins the onDismiss early-return contract that testScrollToStartPreservesAnchor
  // relies on to read the anchor back through the scrollTo callback.
  @Test
  fun testOnDismissOnActiveItemDoesNotScroll() {
    val items = listOf(imageItem(1L), imageItem(2L), imageItem(3L))
    var scrolledTo: Int? = null
    val provider = providerForGallery(items, cItemId = 3L) { scrolledTo = it }

    provider.onDismiss(provider.initialIndex)

    assertEquals(null, scrolledTo)
  }

  private fun imageItem(id: Long): ChatItem =
    ChatItem(
      chatDir = CIDirection.DirectRcv(),
      meta = CIMeta.getSample(id, Clock.System.now(), text = ""),
      content = CIContent.RcvMsgContent(MsgContent.MCImage(text = "", image = "")),
      reactions = emptyList(),
      file = CIFile.getSample(fileId = id, fileName = "img-$id.jpg", filePath = "img-$id.jpg"),
    )
}
