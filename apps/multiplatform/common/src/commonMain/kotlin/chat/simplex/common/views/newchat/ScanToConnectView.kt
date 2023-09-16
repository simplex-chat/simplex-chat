package chat.simplex.common.views.newchat

import SectionBottomSpacer
import SectionTextFooter
import androidx.compose.desktop.ui.tooling.preview.Preview
import chat.simplex.common.platform.Log
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.buildAnnotatedString
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.TAG
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.res.MR
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import java.net.URI

@Composable
expect fun ScanToConnectView(chatModel: ChatModel, close: () -> Unit)

enum class ConnectionLinkType {
  CONTACT, INVITATION, GROUP
}

@Serializable
sealed class CReqClientData {
  @Serializable @SerialName("group") data class Group(val groupLinkId: String): CReqClientData()
}

fun withUriAction(uri: URI, run: suspend (ConnectionLinkType) -> Unit) {
  val action = uri.path?.drop(1)?.replace("/", "")
  val data = URI(uri.toString().replaceFirst("#/", "/")).getQueryParameter("data")
  val type = when {
    data != null -> {
      val parsed = runCatching {
        json.decodeFromString(CReqClientData.serializer(), data)
      }
      when {
        parsed.getOrNull() is CReqClientData.Group -> ConnectionLinkType.GROUP
        else -> null
      }
    }

    action == "contact" -> ConnectionLinkType.CONTACT
    action == "invitation" -> ConnectionLinkType.INVITATION
    else -> null
  }
  if (type != null) {
    withApi { run(type) }
  } else {
    AlertManager.shared.showAlertMsg(
      title = generalGetString(MR.strings.invalid_contact_link),
      text = generalGetString(MR.strings.this_link_is_not_a_valid_connection_link)
    )
  }
}

suspend fun connectViaUri(chatModel: ChatModel, action: ConnectionLinkType, uri: URI, incognito: Boolean): Boolean {
  val r = chatModel.controller.apiConnect(incognito, uri.toString())
  if (r) {
    AlertManager.shared.showAlertMsg(
      title = generalGetString(MR.strings.connection_request_sent),
      text =
      when (action) {
        ConnectionLinkType.CONTACT -> generalGetString(MR.strings.you_will_be_connected_when_your_connection_request_is_accepted)
        ConnectionLinkType.INVITATION -> generalGetString(MR.strings.you_will_be_connected_when_your_contacts_device_is_online)
        ConnectionLinkType.GROUP -> generalGetString(MR.strings.you_will_be_connected_when_group_host_device_is_online)
      }
    )
  }
  return r
}

@Composable
fun ConnectContactLayout(
  chatModel: ChatModel,
  incognitoPref: SharedPreference<Boolean>,
  close: () -> Unit
) {
  val incognito = remember { mutableStateOf(incognitoPref.get()) }

  @Composable
  fun QRCodeScanner(close: () -> Unit) {
    QRCodeScanner { connReqUri ->
      try {
        val uri = URI(connReqUri)
        withUriAction(uri) { linkType ->
          val action = suspend {
            Log.d(TAG, "connectViaUri: connecting")
            if (connectViaUri(ChatModel, linkType, uri, incognito = incognito.value)) {
              close()
            }
          }
          if (linkType == ConnectionLinkType.GROUP) {
            AlertManager.shared.showAlertDialog(
              title = generalGetString(MR.strings.connect_via_group_link),
              text = generalGetString(MR.strings.you_will_join_group),
              confirmText = if (incognito.value) generalGetString(MR.strings.connect_via_link_incognito) else generalGetString(MR.strings.connect_via_link_verb),
              onConfirm = { withApi { action() } }
            )
          } else action()
        }
      } catch (e: RuntimeException) {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.invalid_QR_code),
          text = generalGetString(MR.strings.this_QR_code_is_not_a_link)
        )
      }
    }
  }

  Column(
    Modifier.verticalScroll(rememberScrollState()).padding(horizontal = DEFAULT_PADDING),
    verticalArrangement = Arrangement.SpaceBetween
  ) {
    AppBarTitle(stringResource(MR.strings.scan_QR_code), false)
    Box(
      Modifier
        .fillMaxWidth()
        .aspectRatio(ratio = 1F)
        .padding(bottom = 12.dp)
    ) { QRCodeScanner(close) }

    IncognitoToggle(incognitoPref, incognito) { ModalManager.start.showModal { IncognitoView() } }

    SectionTextFooter(
      buildAnnotatedString {
        append(sharedProfileInfo(chatModel, incognito.value))
        append("\n\n")
        append(annotatedStringResource(MR.strings.if_you_cannot_meet_in_person_scan_QR_in_video_call_or_ask_for_invitation_link))
      }
    )

    SectionBottomSpacer()
  }
}

fun URI.getQueryParameter(param: String): String? {
  if (!query.contains("$param=")) return null
  return query.substringAfter("$param=").substringBefore("&")
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
fun PreviewConnectContactLayout() {
  SimpleXTheme {
    ConnectContactLayout(
      chatModel = ChatModel,
      incognitoPref = SharedPreference({ false }, {}),
      close = {},
    )
  }
}
