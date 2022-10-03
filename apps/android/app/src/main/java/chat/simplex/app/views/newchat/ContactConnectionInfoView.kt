package chat.simplex.app.views.chatlist

import android.content.res.Configuration
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.chat.LocalAliasEditor
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.newchat.InfoAboutIncognito
import chat.simplex.app.views.newchat.QRCode

@Composable
fun ContactConnectionInfoView(chatModel: ChatModel, connReqInvitation: String, contactConnection: PendingContactConnection, close: () -> Unit) {
  val cxt = LocalContext.current
  LaunchedEffect(Unit) {
    if (contactConnection.connReqInv != null) {
      chatModel.connReqInv.value = contactConnection.connReqInv
    }
  }
  DisposableEffect(Unit) {
    onDispose { chatModel.connReqInv.value = null }
  }
  ContactConnectionInfoLayout(
    chatModelIncognito = chatModel.incognito.value,
    connReq = connReqInvitation,
    contactConnection.localAlias,
    share = { shareText(cxt, connReqInvitation) },
    deleteConnection = { deleteContactConnectionAlert(contactConnection, chatModel, close) },
    onLocalAliasChanged = { setContactAlias(contactConnection, it, chatModel) }
  )
}

@Composable
private fun ContactConnectionInfoLayout(
  chatModelIncognito: Boolean,
  connReq: String,
  localAlias: String,
  share: () -> Unit,
  deleteConnection: () -> Unit,
  onLocalAliasChanged: (String) -> Unit
) {
  BoxWithConstraints {
    val screenHeight = maxHeight
    var showQr by remember { mutableStateOf(false) }
    Column(
      Modifier
        .verticalScroll(rememberScrollState())
        .padding(start = DEFAULT_PADDING, end = DEFAULT_PADDING , bottom = DEFAULT_PADDING),
      verticalArrangement = Arrangement.SpaceBetween,
    ) {
      AppBarTitle(stringResource(R.string.shared_one_time_link), false)

      LocalAliasEditor(localAlias, updateValue = onLocalAliasChanged)

      Text(
        stringResource(R.string.show_QR_code_for_your_contact_to_scan_from_the_app__multiline),
      )
      Row {
        InfoAboutIncognito(
          chatModelIncognito,
          true,
          generalGetString(R.string.incognito_random_profile_description),
          generalGetString(R.string.your_profile_will_be_sent)
        )
      }
      if (connReq.isNotEmpty() && showQr) {
        QRCode(
          connReq, Modifier
            .aspectRatio(1f)
            .padding(vertical = 3.dp)
        )

        Text(
          annotatedStringResource(R.string.if_you_cannot_meet_in_person_show_QR_in_video_call_or_via_another_channel),
          lineHeight = 22.sp,
          modifier = Modifier
            .padding(bottom = if (screenHeight > 600.dp) 8.dp else 0.dp)
        )
      }
      if (!showQr) {
        Row(
          Modifier.fillMaxWidth(),
          horizontalArrangement = Arrangement.Center
        ) {
          SimpleButton(stringResource(R.string.show_QR_code), icon = Icons.Outlined.QrCode, click = { showQr = true })
        }
      }
      Row(
        Modifier.fillMaxWidth(),
        horizontalArrangement = Arrangement.Center
      ) {
        SimpleButton(stringResource(R.string.share_invitation_link), icon = Icons.Outlined.Share, click = share)
      }
      Row(
        Modifier.fillMaxWidth(),
        horizontalArrangement = Arrangement.Center
      ) {
        SimpleButton(stringResource(R.string.delete_verb), icon = Icons.Outlined.Delete, color = Color.Red, click = deleteConnection)
      }
    }
  }
}

private fun setContactAlias(contactConnection: PendingContactConnection, localAlias: String, chatModel: ChatModel) = withApi {
  chatModel.controller.apiSetConnectionAlias(contactConnection.pccConnId, localAlias)?.let {
    chatModel.updateContactConnection(it)
  }
}

@Preview
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
private fun PreviewAddContactView() {
  SimpleXTheme {
    ContactConnectionInfoLayout(
      chatModelIncognito = false,
      localAlias = "",
      connReq = "https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D",
      share = {},
      deleteConnection = {},
      onLocalAliasChanged = {},
    )
  }
}
