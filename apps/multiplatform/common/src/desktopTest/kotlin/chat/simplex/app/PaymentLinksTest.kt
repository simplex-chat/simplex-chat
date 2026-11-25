package chat.simplex.app

import androidx.compose.foundation.layout.Column
import androidx.compose.runtime.Composable
import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.runComposeUiTest
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatSendCmdRetry
import chat.simplex.common.ui.theme.SimpleXTheme
import chat.simplex.common.views.chat.item.MarkdownText
import chat.simplex.common.views.chat.item.PreviewChatItemView
import chat.simplex.common.views.chat.item.itemDisplay
import chat.simplex.common.views.chat.item.previewDate
import chat.simplex.common.views.chat.item.segmentDisplayText
import chat.simplex.common.views.helpers.ParsedFormattedText
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.jsonObject
import kotlin.test.*

private const val BTC = "bc1q2gy6f02nn6vvcxs0pnu29tpnpyz0qf66505d4u"
private const val XMR = "8A3ZWAXrrQddvnT1fPrtbK86ZAoM4nai3Gjg1LEow3JWcryJtovMnHYZnxTJpCLmAbfWbnPMeTzPmMBjAhyd4xoM89hYq1c"
private const val ETH = "0xD7047Fe3Eecb2f2FF78d839dD927Be27Bc12c86a"
private const val INVOICE = "lnbc2500u1pvjluezsp5zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zygspp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5xysxxatsyp3k7enxv4jsxqzpu9qrsgquk0rl77nj30yxdy8j9vdx85fkpmdla2087ne0xh8nhedh8w27kyke0lp53ut353s06fv3qfegext0eh0ymjpf39tuven09sam30g4vgpfna3rh"

private val pageMessages = listOf("Donations:", "Suggested donation:", "For the coffee:", "If you want to support the project:", "Or this exact amount:", "On Ethereum:", "Cheaper on Arbitrum:", "Other networks:")

@OptIn(ExperimentalTestApi::class)
class PaymentLinksTest {
  private val sendCmd = chatSendCmdRetry

  @AfterTest
  fun tearDown() {
    chatSendCmdRetry = sendCmd
  }

  @Test
  fun linksShowNetworkSignAndShortenedAddress() {
    val shown = recordedMarkdown().mapValues { (_, parsed) -> parsed.formattedText!!.joinToString("") { segmentDisplayText(it, null, SimplexLinkMode.DESCRIPTION) } }
    assertEquals(
      mapOf(
        "<bitcoin:$BTC?amount=0.001&label=SimpleX>" to "₿\u00A00.001 · SimpleX",
        "Cheaper on Arbitrum: ethereum:$ETH@42161?value=1e16" to "Cheaper on Arbitrum: Ξ\u00A00.01 · Arbitrum One",
        "Donations: $BTC. Cheers" to "Donations: ₿\u00A0bc1q2g…5d4u. Cheers",
        "For the coffee: $INVOICE thanks!" to "For the coffee: ϟ\u00A00.0025 BTC thanks!",
        "If you want to support the project: [Donate](monero:$XMR). Cheers" to "If you want to support the project: ɱ\u00A0Donate. Cheers",
        "On Ethereum: $ETH" to "On Ethereum: Ξ\u00A00xD704…c86a",
        "Or this exact amount: monero:$XMR?tx_amount=0.25&recipient_name=SimpleX" to "Or this exact amount: ɱ\u00A00.25 · SimpleX",
        "Other networks: t1fwjQW5gpFhDqXNhxqDWyF9j9WeKvVS5Jg, ltc1q2gy6f02nn6vvcxs0pnu29tpnpyz0qf66snwfdv, D99pV4n9TrPxBPCkQGx4w4SMSa6QjRBxPf, qq6c8vfvxqrk6rhdysgvkhqc24sggkfsx5nqvdlqcg" to
          "Other networks: ⓩ\u00A0t1fwjQ…S5Jg, Ł\u00A0ltc1q2…wfdv, Ð\u00A0D99pV4…BxPf, ₿\u00A0qq6c8v…lqcg",
        "Suggested donation: bitcoin:$BTC?amount=0.001&label=SimpleX Thank you!" to "Suggested donation: ₿\u00A00.001 · SimpleX Thank you!",
        "[Donate](monero:$XMR)" to "ɱ\u00A0Donate",
        "[$BTC](bitcoin:$BTC)" to "₿\u00A0bc1q2g…5d4u",
        BTC to "₿\u00A0bc1q2g…5d4u",
        "bitcoin:$BTC?amount=0.001&label=SimpleX&message=Thank%20you" to "₿\u00A00.001 · SimpleX",
        "ethereum:0xd7047fe3eecb2f2ff78d839dd927be27bc12c86a" to "Ξ\u00A00xD704…c86a",
        "ethereum:0xdAC17F958D2ee523a2206206994597C13D831ec7@1/transfer?address=$ETH&uint256=5000000" to "Ξ\u00A00xD704…c86a",
        "lightning:LNURL1DP68GURN8GHJ7UM9WFMXJCM99E3K7MF0V9CXJ0M385EKVCENXC6R2C35XVUKXEFCV5MKVV34X5EKZD3EV56NYD3HXQURZEPEXEJXXEPNXSCRVWFNV9NXZCN9XQ6XYEFHVGCXXCMYXYMNSERXFQ5FNS" to "ϟ\u00A0lnurl1…5fns",
        "lightning:lno1pgx9getnwss8vetrw3hhyuckyypwa3eyt44h6txtxquqh7lz5djge4afgfjn7k4rgrkuag0jsd5xvxg" to "ϟ\u00A0lno1pg…xvxg",
        INVOICE to "ϟ\u00A00.0025 BTC",
        "solana:7JCf5m3TiHmYKZVr6jCu1KeZVtb9Y1jRMQDU69p5ARnu?amount=0.5" to "◎\u00A00.5"
      ),
      shown
    )
  }

  @Test
  fun receivedMessageIsDrawnAsItIsSelected() = runComposeUiTest {
    val commands = recorded().getValue("commands").jsonObject
    chatSendCmdRetry = { _, cmd, _ -> commands.getValue(cmd).toString() }
    val item = runBlocking { ChatController.apiGetChat(null, ChatType.Direct, 2, null, pagination = ChatPagination.Last(1)) }!!.first.chatItems.single()
    val shown = "donate ₿\u00A0bc1q2g…5d4u, ɱ\u00A0Donate or ϟ\u00A00.0025 BTC"
    setContent { MarkdownText(item.text, item.formattedText, toggleSecrets = false, linkMode = SimplexLinkMode.DESCRIPTION) }
    onNodeWithText(shown).assertExists()
    assertEquals(shown, itemDisplay(item, SimplexLinkMode.DESCRIPTION)!!.parts.joinToString("") { it.first })
  }
}

@Composable
fun PaymentLinkMessages() = SimpleXTheme {
  val recorded = recordedMarkdown()
  Column {
    pageMessages.forEachIndexed { i, prefix ->
      val (text, parsed) = recorded.entries.first { it.key.startsWith(prefix) }
      PreviewChatItemView(ChatItem.getSampleData(i + 1L, CIDirection.DirectRcv(), previewDate, text, CIStatus.RcvRead()).copy(formattedText = parsed.formattedText))
    }
  }
}

private fun recorded(): JsonObject = Json.parseToJsonElement(fixtureText("payments/links")).jsonObject

private fun recordedMarkdown(): Map<String, ParsedFormattedText> =
  recorded().getValue("markdown").jsonObject.mapValues { (_, parsed) -> json.decodeFromString(ParsedFormattedText.serializer(), parsed.toString()) }
