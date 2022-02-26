package chat.simplex.app.views.usersettings

import android.content.res.Configuration
import androidx.compose.foundation.background
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
import androidx.navigation.NavController
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.SimpleButton
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.newchat.QRCode

@Composable
fun UserAddressView(chatModel: ChatModel, nav: NavController) {
  val cxt = LocalContext.current
  UserAddressLayout(
    userAddress = chatModel.userAddress.value,
    back = { nav.popBackStack() },
    createAddress = {
      withApi {
        chatModel.userAddress.value = chatModel.controller.apiCreateUserAddress()
      }
    },
    share = { userAddress: String -> shareText(cxt, userAddress) },
    deleteAddress = {
      chatModel.alertManager.showAlertMsg(
        title = "Delete address?",
        text = "All your contacts will remain connected",
        confirmText = "Delete",
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
  back: () -> Unit,
  createAddress: () -> Unit,
  share: (String) -> Unit,
  deleteAddress: () -> Unit
) {
  Column(
    modifier = Modifier
      .fillMaxSize()
      .background(MaterialTheme.colors.background)
      .padding(horizontal = 8.dp),
    horizontalAlignment = Alignment.Start,
    verticalArrangement = Arrangement.Top
  ) {
    CloseSheetBar(back)
    Text(
      "Your chat address",
      Modifier.padding(bottom = 24.dp),
      style = MaterialTheme.typography.h1,
      color = MaterialTheme.colors.onBackground
    )
    Text(
      "You can share your address as a link or as a QR code - anybody will be able to connect to you, " +
          "and if you later delete it - you won't lose your contacts.",
      Modifier.padding(bottom = 24.dp),
      color = MaterialTheme.colors.onBackground
    )
    Column(
      Modifier
        .fillMaxWidth()
        .padding(top = 12.dp),
      horizontalAlignment = Alignment.CenterHorizontally,
      verticalArrangement = Arrangement.spacedBy(24.dp)
    ) {
      if (userAddress == null) {
        SimpleButton("Create address", icon = Icons.Outlined.QrCode, click = createAddress)
      } else {
        QRCode(userAddress)
        Row(
          horizontalArrangement = Arrangement.spacedBy(12.dp)
        ) {
          SimpleButton(
            "Share link",
            icon = Icons.Outlined.Share,
            click = { share(userAddress) })
          SimpleButton(
            "Delete address",
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
      back = {},
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
      back = {},
      createAddress = {},
      share = { _ -> },
      deleteAddress = {},
    )
  }
}
