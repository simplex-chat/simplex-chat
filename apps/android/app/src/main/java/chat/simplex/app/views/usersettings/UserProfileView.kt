package chat.simplex.app.views.usersettings

import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.tooling.preview.Preview
import androidx.navigation.NavController
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.Profile
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.withApi

@Composable
fun UserProfileView(chatModel: ChatModel, nav: NavController) {
  val user = chatModel.currentUser.value
  if (user != null) {
    var editProfile by remember { mutableStateOf(false) }
    var profile by remember { mutableStateOf(user.profile) }
    UserProfileLayout(
      editProfile = editProfile,
      profile = profile,
      back = { nav.popBackStack() },
      editProfileOff = { editProfile = false },
      editProfileOn = { editProfile = true },
      saveProfile = { displayName: String, fullName: String ->
        withApi {
          val newProfile = chatModel.controller.apiUpdateProfile(
            profile = Profile(displayName, fullName)
          )
          if (newProfile != null) {
            chatModel.updateUserProfile(newProfile)
            profile = newProfile
            editProfile = false
          }
        }
      }
    )
  }
}

@Composable
fun UserProfileLayout(
  editProfile: Boolean,
  profile: Profile,
  back: () -> Unit,
  editProfileOff: () -> Unit,
  editProfileOn: () -> Unit,
  saveProfile: (String, String) -> Unit,
) {
  Column {
    Button(onClick = back) { Text("Back") }
    Text(
      "Your profile is stored on your device and shared only with your contacts.\n" +
          "SimpleX servers cannot see your profile."
    )
    if (editProfile) {
      var displayName by remember { mutableStateOf(profile.displayName) }
      var fullName by remember { mutableStateOf(profile.fullName) }
      Column {
        TextField(value = displayName, onValueChange = { displayName = it })
        TextField(value = fullName, onValueChange = { fullName = it })
        Row {
          Button(onClick = editProfileOff) { Text("Cancel") }
          Button(
            onClick = { saveProfile(displayName, fullName) },
            enabled = displayName.isNotEmpty()
          ) {
            Text("Save (and notify contacts)")
          }
        }
      }
    } else {
      Column {
        Text("Display name: ${profile.displayName}")
        Text("Full name: ${profile.fullName}")
        Button(onClick = editProfileOn) { Text("Edit") }
      }
    }
  }
}

@Preview(showBackground = true)
@Composable
fun PreviewUserProfileLayoutEditOff() {
  SimpleXTheme {
    UserProfileLayout(
      profile = Profile.sampleData,
      editProfile = false,
      back = {},
      editProfileOff = {},
      editProfileOn = {},
      saveProfile = { _, _ -> }
    )
  }
}

@Preview(showBackground = true)
@Composable
fun PreviewUserProfileLayoutEditOn() {
  SimpleXTheme {
    UserProfileLayout(
      profile = Profile.sampleData,
      editProfile = true,
      back = {},
      editProfileOff = {},
      editProfileOn = {},
      saveProfile = { _, _ -> }
    )
  }
}
