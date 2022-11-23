package chat.simplex.app.views.usersettings

import android.content.res.Configuration
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.Text
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.remember
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.UserContactLinkRec
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.newchat.QRCode

@Composable
fun UserAddressView(chatModel: ChatModel) {
  val cxt = LocalContext.current
  UserAddressLayout(
    userAddress = remember { chatModel.userAddress }.value,
    createAddress = {
      withApi {
        val connReqContact = chatModel.controller.apiCreateUserAddress()
        if (connReqContact != null) {
          chatModel.userAddress.value = UserContactLinkRec(connReqContact)
        }
      }
    },
    share = { userAddress: String -> shareText(cxt, userAddress) },
    acceptRequests = {
      chatModel.userAddress.value?.let { address ->
        ModalManager.shared.showModal(settings = true) { AcceptRequestsView(chatModel, address) }
      }
    },
    deleteAddress = {
      AlertManager.shared.showAlertMsg(
        title = generalGetString(R.string.delete_address__question),
        text = generalGetString(R.string.all_your_contacts_will_remain_connected),
        confirmText = generalGetString(R.string.delete_verb),
        onConfirm = {
          withApi {
            chatModel.controller.apiDeleteUserAddress()
            chatModel.userAddress.value = null
          }
        }
      )
    }
  )
}

@Composable
fun UserAddressLayout(
  userAddress: UserContactLinkRec?,
  createAddress: () -> Unit,
  share: (String) -> Unit,
  acceptRequests: () -> Unit,
  deleteAddress: () -> Unit
) {
  Column(
    Modifier.verticalScroll(rememberScrollState()),
    horizontalAlignment = Alignment.Start,
    verticalArrangement = Arrangement.Top
  ) {
    AppBarTitle(stringResource(R.string.your_contact_address), false)
    Text(
      stringResource(R.string.you_can_share_your_address_anybody_will_be_able_to_connect),
      Modifier.padding(bottom = 12.dp),
      lineHeight = 22.sp
    )
    Column(
      Modifier.fillMaxWidth().padding(bottom = DEFAULT_PADDING_HALF),
      horizontalAlignment = Alignment.CenterHorizontally,
      verticalArrangement = Arrangement.SpaceEvenly
    ) {
      if (userAddress == null) {
        SimpleButton(stringResource(R.string.create_address), icon = Icons.Outlined.QrCode, click = createAddress)
      } else {
        QRCode(userAddress.connReqContact, Modifier.aspectRatio(1f))
        Row(
          horizontalArrangement = Arrangement.spacedBy(10.dp),
          verticalAlignment = Alignment.CenterVertically,
          modifier = Modifier.padding(vertical = 16.dp)
        ) {
          SimpleButton(
            stringResource(R.string.share_link),
            icon = Icons.Outlined.Share,
            click = { share(userAddress.connReqContact) })
          SimpleButtonIconEnded(
            stringResource(R.string.contact_requests),
            icon = Icons.Outlined.ChevronRight,
            click = acceptRequests
          )
        }
        SimpleButton(
          stringResource(R.string.delete_address),
          icon = Icons.Outlined.Delete,
          color = Color.Red,
          click = deleteAddress
        )
      }
    }
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewUserAddressLayoutNoAddress() {
  SimpleXTheme {
    UserAddressLayout(
      userAddress = null,
      createAddress = {},
      share = { _ -> },
      acceptRequests = {},
      deleteAddress = {},
    )
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewUserAddressLayoutAddressCreated() {
  SimpleXTheme {
    UserAddressLayout(
      userAddress = UserContactLinkRec("https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D"),
      createAddress = {},
      share = { _ -> },
      acceptRequests = {},
      deleteAddress = {},
    )
  }
}
