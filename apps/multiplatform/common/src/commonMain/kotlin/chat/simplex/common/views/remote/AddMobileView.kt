package chat.simplex.common.views.remote

import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.QRCode
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.serialization.encodeToString

@Composable
fun AddMobileView(
  m: ChatModel,
  connecting: MutableState<Boolean>,
  close: () -> Unit
) {
  val remoteHost = remember { mutableStateOf<RemoteHostInfo?>(null) }
  val invitation = remember { mutableStateOf<String?>(null) }
  LaunchedEffect(Unit) {
    withBGApi {
      val r = m.controller.startRemoteHost(null)
      if (r != null) {
        val (rh_, inv) = r
        connecting.value = true
        remoteHost.value = rh_
        invitation.value = inv
      }
    }
  }
  AddMobileViewLayout(
    remoteHost = remoteHost,
    invitation = invitation
  )
}

@Composable
private fun AddMobileViewLayout(
  remoteHost: MutableState<RemoteHostInfo?>,
  invitation: MutableState<String?>,
) {
  Column(
    Modifier.fillMaxWidth().verticalScroll(rememberScrollState()),
    verticalArrangement = Arrangement.spacedBy(8.dp)
  ) {
    AppBarTitle(stringResource(MR.strings.add_mobile_device))
    SectionView {
      val rh = remoteHost.value
      val inv = invitation.value
      if (inv != null) {
        QRCode(
          inv, Modifier
            .padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF)
            .aspectRatio(1f)
        )
      }
    }
  }
}
