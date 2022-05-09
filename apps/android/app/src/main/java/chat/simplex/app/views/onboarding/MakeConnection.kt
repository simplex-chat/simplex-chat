package chat.simplex.app.views.onboarding

import android.Manifest
import android.content.res.Configuration
import androidx.annotation.StringRes
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.User
import chat.simplex.app.ui.theme.SimpleButton
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.newchat.*
import chat.simplex.app.views.usersettings.simplexTeamUri
import com.google.accompanist.permissions.rememberPermissionState

@Composable
fun MakeConnection(chatModel: ChatModel) {
  val cameraPermissionState = rememberPermissionState(permission = Manifest.permission.CAMERA)
  MakeConnectionLayout(
    chatModel.currentUser.value,
    createLink = {
      withApi {
        //        show spinner
        chatModel.connReqInvitation = chatModel.controller.apiAddContact()
        //        hide spinner
        if (chatModel.connReqInvitation != null) {
          ModalManager.shared.showModal { AddContactView(chatModel) }
        }
      }
    },
    pasteLink = {
      ModalManager.shared.showCustomModal { close -> PasteToConnectView(chatModel, close) }
    },
    scanCode = {
      ModalManager.shared.showCustomModal { close -> ScanToConnectView(chatModel, close) }
      cameraPermissionState.launchPermissionRequest()
    },
    about = {
      chatModel.onboardingStage.value = OnboardingStage.Step1_SimpleXInfo
    }
  )
}

@Composable
fun MakeConnectionLayout(
  user: User?,
  createLink: () -> Unit,
  pasteLink: () -> Unit,
  scanCode: () -> Unit,
  about: () -> Unit
) {
  Surface {
    Column(
      Modifier
        .fillMaxSize()
        .background(color = MaterialTheme.colors.background)
        .padding(20.dp)
    ) {
      Text(
        if (user == null) stringResource(R.string.make_private_connection)
        else String.format(stringResource(R.string.personal_welcome), user.profile.displayName),
        style = MaterialTheme.typography.h1,
        modifier = Modifier.padding(bottom = 8.dp)
      )
      Text(
        annotatedStringResource(R.string.to_make_your_first_private_connection_choose),
        modifier = Modifier.padding(bottom = 16.dp)
      )
      ActionRow(
        Icons.Outlined.QrCode,
        R.string.create_1_time_link_qr_code,
        R.string.it_s_secure_to_share__only_one_contact_can_use_it,
        createLink
      )
      ActionRow(
        Icons.Outlined.Link,
        R.string.paste_the_link_you_received,
        R.string.or_open_the_link_in_the_browser_and_tap_open_in_mobile,
        pasteLink
      )
      ActionRow(
        Icons.Outlined.QrCodeScanner,
        R.string.scan_contact_s_qr_code,
        R.string.in_person_or_via_a_video_call__the_most_secure_way_to_connect,
        scanCode
      )
      Box(Modifier.fillMaxWidth().padding(bottom = 16.dp), contentAlignment = Alignment.Center) {
        Text(stringResource(R.string.or))
      }
      val uriHandler = LocalUriHandler.current
      ActionRow(
        Icons.Outlined.Tag,
        R.string.connect_with_the_developers,
        R.string.to_ask_any_questions_and_to_receive_simplex_chat_updates,
        { uriHandler.openUri(simplexTeamUri) }
      )
      Spacer(Modifier.fillMaxHeight().weight(1f))
      SimpleButton(
        text = stringResource(R.string.about_simplex),
        icon = Icons.Outlined.ArrowBackIosNew,
        click = about
      )
    }
  }
}

@Composable
private fun ActionRow(icon: ImageVector, @StringRes titleId: Int, @StringRes textId: Int, action: () -> Unit) {
  Row(
    Modifier
      .clickable { action() }
      .padding(bottom = 16.dp)
  ) {
    Icon(icon, stringResource(titleId), tint = MaterialTheme.colors.primary,
      modifier = Modifier.padding(end = 10.dp).size(40.dp))
    Column {
      Text(stringResource(titleId), color = MaterialTheme.colors.primary)
      Text(annotatedStringResource(textId))
    }
  }
//  Button(action: action, label: {
//    HStack(alignment: .top, spacing: 20) {
//    Image(systemName: icon)
//      .resizable()
//      .scaledToFit()
//      .frame(width: 30, height: 30)
//    .padding(.leading, 4)
//    .padding(.top, 6)
//    VStack(alignment: .leading) {
//    Group {
//      Text(title).font(.headline)
//      Text(text).foregroundColor(.primary)
//    }
//      .multilineTextAlignment(.leading)
//  }
//  }
//  })
//  .padding(.bottom)
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewMakeConnection() {
  SimpleXTheme {
    MakeConnectionLayout(
      user = User.sampleData,
      createLink = {},
      pasteLink = {},
      scanCode = {},
      about = {}
    )
  }
}
