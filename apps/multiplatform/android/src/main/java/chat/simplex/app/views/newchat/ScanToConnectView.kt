package chat.simplex.app.views.newchat

import SectionBottomSpacer
import android.Manifest
import android.content.res.Configuration
import android.net.Uri
import android.util.Log
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.core.net.toUri
import chat.simplex.app.R
import chat.simplex.app.TAG
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.json
import chat.simplex.app.ui.theme.DEFAULT_PADDING
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.*
import com.google.accompanist.permissions.rememberPermissionState
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Composable
fun ScanToConnectView(chatModel: ChatModel, close: () -> Unit) {
  val cameraPermissionState = rememberPermissionState(permission = Manifest.permission.CAMERA)
  LaunchedEffect(Unit) {
    cameraPermissionState.launchPermissionRequest()
  }
  ConnectContactLayout(
    chatModelIncognito = chatModel.incognito.value,
    qrCodeScanner = {
      QRCodeScanner { connReqUri ->
        try {
          val uri = Uri.parse(connReqUri)
          withUriAction(uri) { linkType ->
            val action = suspend {
              Log.d(TAG, "connectViaUri: connecting")
              if (connectViaUri(chatModel, linkType, uri)) {
                close()
              }
            }
            if (linkType == ConnectionLinkType.GROUP) {
              AlertManager.shared.showAlertDialog(
                title = generalGetString(R.string.connect_via_group_link),
                text = generalGetString(R.string.you_will_join_group),
                confirmText = generalGetString(R.string.connect_via_link_verb),
                onConfirm = { withApi { action() } }
              )
            } else action()
          }
        } catch (e: RuntimeException) {
          AlertManager.shared.showAlertMsg(
            title = generalGetString(R.string.invalid_QR_code),
            text = generalGetString(R.string.this_QR_code_is_not_a_link)
          )
        }
      }
    },
  )
}

enum class ConnectionLinkType {
  CONTACT, INVITATION, GROUP
}

@Serializable
sealed class CReqClientData {
  @Serializable @SerialName("group") data class Group(val groupLinkId: String): CReqClientData()
}

fun withUriAction(uri: Uri, run: suspend (ConnectionLinkType) -> Unit) {
  val action = uri.path?.drop(1)?.replace("/", "")
  val data = uri.toString().replaceFirst("#/", "/").toUri().getQueryParameter("data")
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
      title = generalGetString(R.string.invalid_contact_link),
      text = generalGetString(R.string.this_link_is_not_a_valid_connection_link)
    )
  }
}

suspend fun connectViaUri(chatModel: ChatModel, action: ConnectionLinkType, uri: Uri): Boolean {
  val r = chatModel.controller.apiConnect(uri.toString())
  if (r) {
    AlertManager.shared.showAlertMsg(
      title = generalGetString(R.string.connection_request_sent),
      text =
      when (action) {
        ConnectionLinkType.CONTACT -> generalGetString(R.string.you_will_be_connected_when_your_connection_request_is_accepted)
        ConnectionLinkType.INVITATION -> generalGetString(R.string.you_will_be_connected_when_your_contacts_device_is_online)
        ConnectionLinkType.GROUP -> generalGetString(R.string.you_will_be_connected_when_group_host_device_is_online)
      }
    )
  }
  return r
}

@Composable
fun ConnectContactLayout(chatModelIncognito: Boolean, qrCodeScanner: @Composable () -> Unit) {
  Column(
    Modifier.verticalScroll(rememberScrollState()).padding(horizontal = DEFAULT_PADDING),
    verticalArrangement = Arrangement.spacedBy(12.dp)
  ) {
    AppBarTitle(stringResource(R.string.scan_QR_code), false)
    InfoAboutIncognito(
      chatModelIncognito,
      true,
      generalGetString(R.string.incognito_random_profile_description),
      generalGetString(R.string.your_profile_will_be_sent)
    )
    Box(
      Modifier
        .fillMaxWidth()
        .aspectRatio(ratio = 1F)
        .padding(bottom = 12.dp)
    ) { qrCodeScanner() }
    Text(
      annotatedStringResource(R.string.if_you_cannot_meet_in_person_scan_QR_in_video_call_or_ask_for_invitation_link),
      lineHeight = 22.sp
    )
    SectionBottomSpacer()
  }
}

@Preview
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewConnectContactLayout() {
  SimpleXTheme {
    ConnectContactLayout(
      chatModelIncognito = false,
      qrCodeScanner = { Surface {} },
    )
  }
}
