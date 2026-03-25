package chat.simplex.common.views.invitation_redesign

import SectionBottomSpacer
import SectionItemView
import SectionSpacer
import SectionView
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.item.CIFileViewScope
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.*
import chat.simplex.res.MR

@Composable
fun ModalData.ConnectViewLinkOrQrModal(rhId: Long?, close: () -> Unit) {
  val showQRCodeScanner = remember { stateGetOrPut("showQRCodeScanner") { true } }
  val pastedLink = rememberSaveable { mutableStateOf("") }

  DisposableEffect(Unit) {
    onDispose {
      connectProgressManager.cancelConnectProgress()
    }
  }
  Column(
    Modifier
      .fillMaxWidth()
      .wrapContentHeight()
      .verticalScroll(rememberScrollState()),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    Spacer(Modifier.height(24.dp))

    Image(
      painterResource(MR.images.ic_invitation_connect_link),
      contentDescription = null,
      contentScale = ContentScale.Fit,
      modifier = Modifier.size(160.dp)
    )

    Spacer(Modifier.height(24.dp))

    SectionView(
      title = stringResource(MR.strings.paste_the_link_you_received).uppercase(),
      headerBottomPadding = 0.dp
    ) {
      ConnectPasteLinkView(rhId, pastedLink, showQRCodeScanner, close)
    }

    if (appPlatform.isAndroid) {
      Spacer(Modifier.height(10.dp))

      SectionView(
        title = stringResource(MR.strings.or_scan_qr_code).uppercase(),
        headerBottomPadding = 0.dp
      ) {
        QRCodeScanner(
          showQRCodeScanner = showQRCodeScanner,
          padding = PaddingValues(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF),
          clipShape = RoundedCornerShape(12.dp)
        ) { text ->
          val linkVerified = strIsSimplexLink(text)
          if (!linkVerified) {
            AlertManager.shared.showAlertMsg(
              title = generalGetString(MR.strings.invalid_qr_code),
              text = generalGetString(MR.strings.code_you_scanned_is_not_simplex_link_qr_code)
            )
          }
          connectFromScanner(rhId, text, close)
        }
      }
    }
    SectionBottomSpacer()
    SectionSpacer()
  }
}

@Composable
private fun ConnectPasteLinkView(rhId: Long?, pastedLink: MutableState<String>, showQRCodeScanner: MutableState<Boolean>, close: () -> Unit) {
  if (pastedLink.value.isEmpty()) {
    val clipboard = LocalClipboardManager.current
    SectionItemView({
      val str = clipboard.getText()?.text ?: return@SectionItemView
      val link = strHasSingleSimplexLink(str.trim())
      if (link != null) {
        pastedLink.value = link.text
        showQRCodeScanner.value = false
        withBGApi {
          connectFromPaste(rhId, link.text, close) { pastedLink.value = "" }
        }
      } else {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.invalid_contact_link),
          text = generalGetString(MR.strings.the_text_you_pasted_is_not_a_link)
        )
      }
    }) {
      Box(
        Modifier
          .fillMaxWidth()
          .clip(RoundedCornerShape(12.dp))
          .background(MaterialTheme.colors.background),
        contentAlignment = Alignment.Center
      ) {
        Text(
          stringResource(MR.strings.tap_to_paste_link),
          modifier = Modifier.padding(vertical = 12.dp),
          color = Color.LightGray,
          fontSize = 16.sp
        )
        if (connectProgressManager.showConnectProgress != null) {
          CIFileViewScope.progressIndicator(sizeMultiplier = 0.6f)
        }
      }
    }
  } else {
    Row(
      Modifier.padding(end = DEFAULT_PADDING),
      verticalAlignment = Alignment.CenterVertically
    ) {
      Box(Modifier.weight(1f)) {
        LinkTextView(pastedLink.value, false)
      }
      if (connectProgressManager.showConnectProgress != null) {
        CIFileViewScope.progressIndicator(sizeMultiplier = 0.6f)
      }
    }
  }
}

private suspend fun connectFromScanner(rhId: Long?, text: String?, close: () -> Unit): Boolean {
  if (text != null && strIsSimplexLink(text)) {
    return connectFromPaste(rhId, text, close)
  }
  return false
}

private suspend fun connectFromPaste(rhId: Long?, link: String, close: () -> Unit, cleanup: (() -> Unit)? = null): Boolean =
  planAndConnect(
    rhId,
    link,
    close = close,
    cleanup = cleanup
  ).await()

@Preview
@Composable
fun PreviewConnectViewLinkOrQrModal() {
  SimpleXTheme {
    ModalData().ConnectViewLinkOrQrModal(rhId = null, close = {})
  }
}
