package chat.simplex.app.views.usersettings

import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.navigation.NavController
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.Profile
import chat.simplex.app.views.helpers.withApi

@Composable
fun UserProfileView(chatModel: ChatModel, nav: NavController) {
  val user = chatModel.currentUser.value
  if (user != null) {
    var editProfile by remember { mutableStateOf(false) }
    var profile by remember { mutableStateOf(user.profile) }
    Column {
      Button(onClick = { nav.popBackStack() }) { Text("Back") }
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
            Button(onClick = { editProfile = false }) { Text("Cancel") }
            Button(
              onClick = {
                withApi {
                  val newProfile = chatModel.controller.apiUpdateProfile(
                    profile = Profile(displayName, fullName)
                  )
                  if (newProfile != null) {
                    chatModel.updateUserProfile(newProfile)
                    profile = newProfile
                  }
                  editProfile = false
                }
              },
              enabled = displayName.isNotEmpty()
            ) { Text("Save (and notify contacts)") }
          }
        }
      } else {
        Column {
          Text("Display name: ${profile.displayName}")
          Text("Full name: ${profile.fullName}")
          Button(onClick = { editProfile = true }) { Text("Edit") }
        }
      }
    }
  }
}
