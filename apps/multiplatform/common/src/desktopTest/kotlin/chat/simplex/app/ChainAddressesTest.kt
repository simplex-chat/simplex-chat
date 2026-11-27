package chat.simplex.app

import androidx.compose.foundation.layout.Column
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.platform.ClipboardManager
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.platform.UriHandler
import androidx.compose.ui.semantics.SemanticsProperties
import androidx.compose.ui.test.ComposeUiTest
import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.SemanticsMatcher
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.SpanStyle
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.style.TextDecoration
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.SimpleXTheme
import chat.simplex.common.ui.theme.WarningOrange
import chat.simplex.common.views.chat.item.*
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.AlertPreview
import chat.simplex.common.views.helpers.ModalManager
import chat.simplex.common.views.helpers.ParsedFormattedText
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.jsonObject
import kotlin.test.*

private const val BTC = "bc1q2gy6f02nn6vvcxs0pnu29tpnpyz0qf66505d4u"
private const val XMR = "8A3ZWAXrrQddvnT1fPrtbK86ZAoM4nai3Gjg1LEow3JWcryJtovMnHYZnxTJpCLmAbfWbnPMeTzPmMBjAhyd4xoM89hYq1c"
private const val ETH = "0xD7047Fe3Eecb2f2FF78d839dD927Be27Bc12c86a"
private const val INVOICE = "lnbc2500u1pvjluezsp5zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zygspp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5xysxxatsyp3k7enxv4jsxqzpu9qrsgquk0rl77nj30yxdy8j9vdx85fkpmdla2087ne0xh8nhedh8w27kyke0lp53ut353s06fv3qfegext0eh0ymjpf39tuven09sam30g4vgpfna3rh"
private const val LNURL = "LNURL1DP68GURN8GHJ7UM9WFMXJCM99E3K7MF0V9CXJ0M385EKVCENXC6R2C35XVUKXEFCV5MKVV34X5EKZD3EV56NYD3HXQURZEPEXEJXXEPNXSCRVWFNV9NXZCN9XQ6XYEFHVGCXXCMYXYMNSERXFQ5FNS"
private const val USDT_POLYGON = "ethereum:0xc2132D05D31c914a87C6611C10748AEb04B58e8F@137/transfer?address=$ETH&uint256=10000000"
private const val UNKNOWN_TOKEN = "ethereum:0x1111111111111111111111111111111111111111@137/transfer?address=$ETH&uint256=10000000"
private const val UNKNOWN_CHAIN_AND_TOKEN = "ethereum:0xdAC17F958D2ee523a2206206994597C13D831ec7@99999/transfer?address=$ETH&uint256=10000000"
private const val LABELED_TOKEN = "[Dinner]($USDT_POLYGON)"
private const val LABELED_UNKNOWN_CHAIN = "[Donate](ethereum:$ETH@99999)"

private val pageMessages = listOf("Donations:", "Suggested donation:", "For the coffee:", "If you want to support the project:", "Or this exact amount:", "On Ethereum:", "Cheaper on Arbitrum:", "Other networks:")

private val previewChainAddress = ChainAddress(ChainCurrency.Bitcoin("0.0012"), BTC, null, "bitcoin:$BTC?amount=0.0012")

@OptIn(ExperimentalTestApi::class)
class ChainAddressesTest : RecordedApiTest() {
  @Test
  fun linkTextOfEachCurrency() {
    val shown = recordedMarkdown().mapValues { (_, parsed) -> parsed.formattedText!!.joinToString("") { segmentDisplayText(it, null, SimplexLinkMode.DESCRIPTION) } }
    assertEquals(
      mapOf(
        "<bitcoin:$BTC?amount=0.001&label=SimpleX>" to "SimpleX ₿\u00A00.001",
        "Cheaper on Arbitrum: ethereum:$ETH@42161?value=1e16" to "Cheaper on Arbitrum: ETH\u00A00.01 (Arbitrum One)",
        "Donations: $BTC. Cheers" to "Donations: ₿\u00A0bc1q2g…\u20605d4u. Cheers",
        "For the coffee: $INVOICE thanks!" to "For the coffee: ϟ\u00A00.0025 thanks!",
        "If you want to support the project: [Donate](monero:$XMR). Cheers" to "If you want to support the project: Donate XMR\u00A08A3ZWA…\u2060Yq1c. Cheers",
        "On Ethereum: $ETH" to "On Ethereum: ⚠\u00A00xD704…\u2060c86a (unknown chain)",
        "Or this exact amount: monero:$XMR?tx_amount=0.25&recipient_name=SimpleX" to "Or this exact amount: SimpleX XMR\u00A00.25",
        "Other networks: t1fwjQW5gpFhDqXNhxqDWyF9j9WeKvVS5Jg, ltc1q2gy6f02nn6vvcxs0pnu29tpnpyz0qf66snwfdv, D99pV4n9TrPxBPCkQGx4w4SMSa6QjRBxPf, qq6c8vfvxqrk6rhdysgvkhqc24sggkfsx5nqvdlqcg, solana:7JCf5m3TiHmYKZVr6jCu1KeZVtb9Y1jRMQDU69p5ARnu" to
          "Other networks: ZEC\u00A0t1fwjQ…\u2060S5Jg, LTC\u00A0ltc1q2…\u2060wfdv, DOGE\u00A0D99pV4…\u2060BxPf, BCH\u00A0qq6c8v…\u2060lqcg, SOL\u00A07JCf5m…\u2060ARnu",
        "Suggested donation: bitcoin:$BTC?amount=0.001&label=SimpleX Thank you!" to "Suggested donation: SimpleX ₿\u00A00.001 Thank you!",
        "[Donate](monero:$XMR)" to "Donate XMR\u00A08A3ZWA…\u2060Yq1c",
        LABELED_TOKEN to "Dinner USDT\u00A010 (Polygon)",
        LABELED_UNKNOWN_CHAIN to "Donate ⚠\u00A00xD704…\u2060c86a (unknown chain)",
        "[$BTC](bitcoin:$BTC)" to "₿\u00A0bc1q2g…\u20605d4u",
        BTC to "₿\u00A0bc1q2g…\u20605d4u",
        "bitcoin:$BTC?amount=0.001&label=SimpleX&message=Thank%20you" to "SimpleX ₿\u00A00.001",
        UNKNOWN_TOKEN to "⚠\u00A00xD704…\u2060c86a (unknown token)",
        "ethereum:$ETH@100?value=1e16" to "⚠\u00A00xD704…\u2060c86a (unknown chain)",
        USDT_POLYGON to "USDT\u00A010 (Polygon)",
        "ethereum:0xd7047fe3eecb2f2ff78d839dd927be27bc12c86a" to "⚠\u00A00xD704…\u2060c86a (unknown chain)",
        "ethereum:0xdAC17F958D2ee523a2206206994597C13D831ec7@1/transfer?address=$ETH&uint256=5000000" to "USDT\u00A05",
        UNKNOWN_CHAIN_AND_TOKEN to "⚠\u00A00xD704…\u2060c86a (unknown chain and token)",
        "flux:t1fwjQW5gpFhDqXNhxqDWyF9j9WeKvVS5Jg?amount=5&message=Thanks" to "FLUX\u00A05",
        "lightning:$LNURL" to "ϟ\u00A0lnurl1…\u20605fns",
        "lightning:lno1pgx9getnwss8vetrw3hhyuckyypwa3eyt44h6txtxquqh7lz5djge4afgfjn7k4rgrkuag0jsd5xvxg" to "ϟ\u00A0lno1pg…\u2060xvxg",
        INVOICE to "ϟ\u00A00.0025",
        "solana:7JCf5m3TiHmYKZVr6jCu1KeZVtb9Y1jRMQDU69p5ARnu?amount=0.5" to "SOL\u00A00.5",
        "solana:7JCf5m3TiHmYKZVr6jCu1KeZVtb9Y1jRMQDU69p5ARnu?amount=10&spl-token=EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v" to "USDC\u00A010 (Solana)",
        "za19w5tpzrsd8x250l6h8qdxv70kg4an9ywljf4cqs5qk2hkw5ej5jw0zc0htg7qxws8vgm6xs3pj3" to "FLUX\u00A0za19w5…\u20603pj3",
        "zel:t1fwjQW5gpFhDqXNhxqDWyF9j9WeKvVS5Jg?amount=5" to "FLUX\u00A05"
      ),
      shown
    )
  }

  @Test
  fun notSignedKeepsLabelAndUnknownReason() {
    val markdown = recordedMarkdown()
    assertEquals(
      listOf("SimpleX ⚠\u00A0bc1q2g…\u20605d4u (not signed)", "⚠\u00A00xD704…\u2060c86a (unknown chain, not signed)"),
      listOf("<bitcoin:$BTC?amount=0.001&label=SimpleX>", "ethereum:$ETH@100?value=1e16").map { key ->
        markdown.getValue(key).formattedText!!.joinToString("") { segmentDisplayText(it, null, SimplexLinkMode.DESCRIPTION, messageSigned = false) }
      }
    )
  }

  @Test
  fun signAfterLabelIsUnderlinedAndChainNameIsItalic() = recorded("chain-addresses/links") {
    val markdown = recordedMarkdown()
    setContent {
      Column {
        listOf("Donations: $BTC. Cheers", "<bitcoin:$BTC?amount=0.001&label=SimpleX>", LABELED_TOKEN).forEach { key ->
          MarkdownText(key, markdown.getValue(key).formattedText, toggleSecrets = false, linkMode = SimplexLinkMode.DESCRIPTION)
        }
      }
    }
    assertEquals(listOf(listOf("₿\u00A0"), listOf<String>(), listOf<String>()), spanTexts { it.textDecoration == TextDecoration.None })
    assertEquals(listOf(listOf<String>(), listOf<String>(), listOf(" (Polygon)")), spanTexts { it.fontStyle == FontStyle.Italic })
  }

  @Test
  fun groupAddressWithoutVerifiedSignatureIsNotSigned() = recorded("chain-addresses/group") {
    val chat = recordedChat("chain-addresses/group", "/_get chat #1 count=2")
    val senders = chat.chatItems.map { chainAddressSender(it, chat.chatInfo) }
    assertEquals(listOf(true, false), senders.map { it.messageSigned })
    assertEquals(
      listOf("donate ₿\u00A0bc1q2g…\u20605d4u", "thanks, or ⚠\u00A0ltc1q2…\u2060wfdv (not signed)"),
      chat.chatItems.map { item -> itemDisplay(item, SimplexLinkMode.DESCRIPTION)!!.parts.joinToString("") { it.first } }
    )
    setContent {
      Column {
        chat.chatItems.zip(senders).forEach { (item, sender) ->
          MarkdownText(item.text, item.formattedText, chainAddressSender = sender, toggleSecrets = false, linkMode = SimplexLinkMode.DESCRIPTION)
        }
        AlertManager.shared.showInView()
      }
    }
    assertEquals(listOf(listOf<String>(), listOf("⚠\u00A0", "ltc1q2…\u2060wfdv (not signed)")), orangeTexts())
    assertEquals(
      listOf("alice: ₿\u00A0bc1q2g…\u20605d4u", "alice: ⚠\u00A0ltc1q2…\u2060wfdv (not signed)"),
      chat.chatItems.zip(senders).flatMap { (item, sender) -> chainLinks(item.formattedText).map { alertMessage(sender, it) } }
    )
  }

  @Test
  fun unknownCurrencyIsOrange() = recorded("chain-addresses/links") {
    val markdown = recordedMarkdown()
    setContent {
      Column {
        listOf("Donations: $BTC. Cheers", "On Ethereum: $ETH").forEach { key ->
          MarkdownText(key, markdown.getValue(key).formattedText, toggleSecrets = false, linkMode = SimplexLinkMode.DESCRIPTION)
        }
      }
    }
    assertEquals(listOf(listOf<String>(), listOf("⚠\u00A0", "0xD704…\u2060c86a (unknown chain)")), orangeTexts())
  }

  @Test
  fun alertNamesSenderLinkAndAddress() = recordedAlerts("chain-addresses/links") {
    val chat = recordedChat("chain-addresses/links", "/_get chat @2 count=1")
    val item = chat.chatItems.single()
    val sender = chainAddressSender(item, chat.chatInfo)
    assertEquals(
      listOf("alice: ₿\u00A0bc1q2g…\u20605d4u", "alice: Donate XMR\u00A08A3ZWA…\u2060Yq1c", "alice: ϟ\u00A00.0025"),
      chainLinks(item.formattedText).map { alertMessage(sender, it) }
    )
    val markdown = recordedMarkdown()
    val alice = ChainAddressSender("alice", forwarded = false, messageSigned = true)
    val forwarded = ChainAddressSender("alice", forwarded = true, messageSigned = true)
    assertEquals(
      listOf(
        "alice: SimpleX ₿\u00A00.001 to bc1q2g…\u20605d4u",
        "alice forwarded: SimpleX ₿\u00A00.001 to bc1q2g…\u20605d4u",
        "alice: USDT\u00A010 (Polygon) to 0xD704…\u2060c86a",
        "alice: ⚠\u00A00xD704…\u2060c86a (unknown chain)",
        "alice: ϟ\u00A0lnurl1…\u20605fns"
      ),
      listOf(
        "<bitcoin:$BTC?amount=0.001&label=SimpleX>" to alice,
        "<bitcoin:$BTC?amount=0.001&label=SimpleX>" to forwarded,
        USDT_POLYGON to alice,
        "ethereum:$ETH@100?value=1e16" to alice,
        "lightning:$LNURL" to alice
      ).flatMap { (key, s) -> chainLinks(markdown.getValue(key).formattedText).map { alertMessage(s, it) } }
    )
  }

  @Test
  fun openWithoutWalletOffersCopyAndQRCode() = recorded("chain-addresses/links") {
    val clipboard = TestClipboard()
    val wallet = NoWalletApp()
    setContent {
      CompositionLocalProvider(LocalClipboardManager provides clipboard) {
        MarkdownText(BTC, recordedMarkdown().getValue(BTC).formattedText, chainAddressSender = ChainAddressSender("alice", forwarded = false, messageSigned = true), toggleSecrets = false, linkMode = SimplexLinkMode.DESCRIPTION, uriHandler = wallet)
        AlertManager.shared.showInView()
      }
    }
    press("₿\u00A0bc1q2g…\u20605d4u")
    onNodeWithText("Open in wallet?").assertExists()
    onNodeWithText("alice: ₿\u00A0bc1q2g…\u20605d4u").assertExists()
    press("Open")
    assertEquals(listOf("bitcoin:$BTC"), wallet.opened)
    onNodeWithText("No wallet app").assertExists()
    onNodeWithText("Copy the address or scan the QR code with a wallet.").assertExists()
    press("Copy")
    assertEquals(listOf(BTC), clipboard.copied)
    showNoWalletAlert(previewChainAddress, clipboard)
    waitForIdle()
    press("QR code")
    val qrCodeShown = ModalManager.end.hasModalsOpen()
    ModalManager.end.closeModals()
    assertTrue(qrCodeShown)
  }

  private fun ComposeUiTest.alertMessage(sender: ChainAddressSender, link: Format.ChainAddressLink): String {
    val before = shownTexts()
    showOpenWalletAlert(sender, link, NoWalletApp(), TestClipboard())
    waitForIdle()
    val alert = shownTexts().filter { it !in before }
    AlertManager.shared.hideAllAlerts()
    waitForIdle()
    assertEquals(listOf("Open in wallet?", "Cancel", "Open"), alert.filterIndexed { i, _ -> i != 1 })
    return alert[1]
  }

  private fun ComposeUiTest.orangeTexts(): List<List<String>> = spanTexts { it.color == WarningOrange }

  private fun ComposeUiTest.spanTexts(style: (SpanStyle) -> Boolean): List<List<String>> =
    onAllNodes(SemanticsMatcher.keyIsDefined(SemanticsProperties.Text), useUnmergedTree = true)
      .fetchSemanticsNodes()
      .flatMap { node -> node.config[SemanticsProperties.Text] }
      .map { s -> s.spanStyles.filter { style(it.item) }.map { s.text.substring(it.start, it.end) } }
}

private class NoWalletApp : UriHandler {
  val opened = mutableListOf<String>()

  override fun openUri(uri: String) {
    opened += uri
    throw IllegalArgumentException("no app for $uri")
  }
}

private class TestClipboard : ClipboardManager {
  val copied = mutableListOf<String>()

  override fun setText(annotatedString: AnnotatedString) { copied += annotatedString.text }
  override fun getText(): AnnotatedString? = null
}

private fun chainLinks(formattedText: List<FormattedText>?): List<Format.ChainAddressLink> =
  formattedText.orEmpty().mapNotNull { it.format as? Format.ChainAddressLink }

private fun recordedJson(fixture: String): JsonObject = Json.parseToJsonElement(fixtureText(fixture)).jsonObject

private fun recordedMarkdown(): Map<String, ParsedFormattedText> =
  recordedJson("chain-addresses/links").getValue("markdown").jsonObject.mapValues { (_, parsed) -> json.decodeFromString(ParsedFormattedText.serializer(), parsed.toString()) }

private fun recordedChat(fixture: String, cmd: String): Chat {
  val response = recordedJson(fixture).getValue("commands").jsonObject.getValue(cmd).toString()
  return ((json.decodeFromString<API>(response) as API.Result).res as CR.ApiChat).chat
}

@Composable
fun ChainAddressMessages() = SimpleXTheme {
  val markdown = recordedMarkdown()
  val texts = pageMessages.map { prefix -> markdown.keys.first { it.startsWith(prefix) } } + listOf(USDT_POLYGON, UNKNOWN_TOKEN, UNKNOWN_CHAIN_AND_TOKEN, LABELED_TOKEN, LABELED_UNKNOWN_CHAIN)
  Column {
    texts.forEachIndexed { i, text ->
      PreviewChatItemView(ChatItem.getSampleData(i + 1L, CIDirection.DirectRcv(), previewDate, text, CIStatus.RcvRead()).copy(formattedText = markdown.getValue(text).formattedText))
    }
    recordedChat("chain-addresses/group", "/_get chat #1 count=2").chatItems.forEach { PreviewChatItemView(it) }
  }
}

@Composable
fun OpenInWalletAlertPreview() = AlertPreview {
  showOpenWalletAlert(ChainAddressSender("Tom", forwarded = false, messageSigned = true), Format.ChainAddressLink("Dinner", previewChainAddress), NoWalletApp(), TestClipboard())
}

@Composable
fun NoWalletAlertPreview() = AlertPreview { showNoWalletAlert(previewChainAddress, TestClipboard()) }
