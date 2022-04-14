package chat.simplex.app.views.usersettings

import android.content.res.Configuration
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.SimpleButton
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.newchat.QRCode

@Composable
fun UserAddressView(chatModel: ChatModel) {
  val cxt = LocalContext.current
  UserAddressLayout(
    userAddress = chatModel.userAddress.value,
    createAddress = {
      withApi {
        chatModel.userAddress.value = chatModel.controller.apiCreateUserAddress()
      }
    },
    share = { userAddress: String -> shareText(cxt, userAddress) },
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
  userAddress: String?,
  createAddress: () -> Unit,
  share: (String) -> Unit,
  deleteAddress: () -> Unit
) {
  Column(
    horizontalAlignment = Alignment.Start,
    verticalArrangement = Arrangement.Top
  ) {
    Text(
      generalGetString(R.string.your_chat_address),
      Modifier.padding(bottom = 16.dp),
      style = MaterialTheme.typography.h1,
    )
    Text(
      generalGetString(R.string.you_can_share_your_address_anybody_will_be_able_to_connect_deletion_wont_lose_contacts),
      Modifier.padding(bottom = 12.dp),
    )
    Column(
      Modifier.fillMaxWidth(),
      horizontalAlignment = Alignment.CenterHorizontally,
      verticalArrangement = Arrangement.SpaceEvenly
    ) {
      if (userAddress == null) {
        SimpleButton(generalGetString(R.string.create_address), icon = Icons.Outlined.QrCode, click = createAddress)
      } else {
        QRCode(userAddress, Modifier.weight(1f, fill = false).aspectRatio(1f))
        Row(
          horizontalArrangement = Arrangement.spacedBy(10.dp),
          verticalAlignment = Alignment.CenterVertically,
          modifier = Modifier.padding(vertical = 10.dp)
        ) {
          SimpleButton(
            generalGetString(R.string.share_link),
            icon = Icons.Outlined.Share,
            click = { share(userAddress) })
          SimpleButton(
            generalGetString(R.string.share_link),
            icon = Icons.Outlined.Delete,
            color = Color.Red,
            click = deleteAddress
          )
        }
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
      userAddress = "https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D",
      createAddress = {},
      share = { _ -> },
      deleteAddress = {},
    )
  }
}
