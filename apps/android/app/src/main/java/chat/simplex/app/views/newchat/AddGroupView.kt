package chat.simplex.app.views.newchat

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.ArrowForwardIos
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.SimplexService
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.views.ProfileNameField
import chat.simplex.app.views.helpers.ModalView
import chat.simplex.app.views.helpers.withApi
import chat.simplex.app.views.isValidDisplayName
import chat.simplex.app.views.onboarding.OnboardingStage
import chat.simplex.app.views.onboarding.ReadableText

@Composable
fun AddGroupView(chatModel: ChatModel, close: () -> Unit) {
  AddGroupLayout(
    createGroup = {},
    close
  )
}

@Composable
fun AddGroupLayout(createGroup: (GroupProfile) -> Unit, close: () -> Unit) {
  val image = remember { mutableStateOf(null) }
  val displayName = remember { mutableStateOf("") }
  val fullName = remember { mutableStateOf("") }
  val focusRequester = remember { FocusRequester() }

  ModalView(close) {
    Surface(Modifier.background(MaterialTheme.colors.onBackground)) {
      Column(
        modifier = Modifier.fillMaxSize()
      ) {
        Row(
          Modifier.fillMaxWidth(),
          horizontalArrangement = Arrangement.Center
        ) {
          Text(
            stringResource(R.string.create_secret_group_title),
            style = MaterialTheme.typography.h4,
            modifier = Modifier.padding(vertical = 5.dp)
          )
        }
        ReadableText(R.string.group_is_decentralized)
        Spacer(Modifier.height(10.dp))
        Text(
          stringResource(R.string.group_display_name_field),
          style = MaterialTheme.typography.h6,
          modifier = Modifier.padding(bottom = 3.dp)
        )
        ProfileNameField(displayName, focusRequester)
        val errorText = if (!isValidDisplayName(displayName.value)) stringResource(R.string.display_name_cannot_contain_whitespace) else ""
        Text(
          errorText,
          fontSize = 15.sp,
          color = MaterialTheme.colors.error
        )
        Spacer(Modifier.height(3.dp))
        Text(
          stringResource(R.string.group_full_name_field),
          style = MaterialTheme.typography.h6,
          modifier = Modifier.padding(bottom = 5.dp)
        )
        ProfileNameField(fullName)

        Spacer(Modifier.fillMaxHeight().weight(1f))

        val enabled = displayName.value.isNotEmpty() && isValidDisplayName(displayName.value)
        val createModifier: Modifier
        val createColor: Color
        if (enabled) {
          //          createModifier = Modifier.clickable { chat.simplex.app.views.createProfile(displayName.value, fullName.value) }.padding(8.dp)
          createModifier = Modifier.clickable { }.padding(8.dp)
          createColor = MaterialTheme.colors.primary
        } else {
          createModifier = Modifier.padding(8.dp)
          createColor = HighOrLowlight
        }
        Row(
          Modifier.fillMaxWidth(),
          horizontalArrangement = Arrangement.End
        ) {
          Surface(shape = RoundedCornerShape(20.dp)) {
            Row(
              createModifier,
              verticalAlignment = Alignment.CenterVertically,
            ) {
              Text(stringResource(R.string.create_profile_button), style = MaterialTheme.typography.caption, color = createColor)
              Icon(Icons.Outlined.ArrowForwardIos, stringResource(R.string.create_profile_button), tint = createColor)
            }
          }
        }

        LaunchedEffect(Unit) {
          focusRequester.requestFocus()
        }
      }
    }
  }
}

//func createGroup() {
//  hideKeyboard()
//  do {
//    let groupInfo = try apiNewGroup(profile)
//      m.addChat(Chat(chatInfo: .group(groupInfo: groupInfo), chatItems: []))
//      openedSheet = nil
//      DispatchQueue.main.async {
//        m.chatId = groupInfo.id
//      }
//    } catch {
//      openedSheet = nil
//      AlertManager.shared.showAlert(
//        Alert(
//          title: Text("Error creating group"),
//          message: Text(responseError(error))
//        )
//      )
//    }
//  }

//fun createProfile(chatModel: ChatModel, displayName: String, fullName: String) {
//  withApi {
//    val user = chatModel.controller.apiCreateActiveUser(
//      Profile(displayName, fullName, null)
//    )
//    chatModel.controller.startChat(user)
//    chatModel.controller.showBackgroundServiceNoticeIfNeeded()
//    SimplexService.start(chatModel.controller.appContext)
//    chatModel.onboardingStage.value = OnboardingStage.OnboardingComplete
//  }
//}
