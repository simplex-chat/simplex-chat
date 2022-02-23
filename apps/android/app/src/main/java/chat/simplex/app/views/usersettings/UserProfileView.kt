package chat.simplex.app.views.usersettings

import android.content.res.Configuration
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.KeyboardCapitalization
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.Profile
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.CloseSheetBar
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
          }
          editProfile = false
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
  Column(
    modifier = Modifier
      .fillMaxSize()
      .background(MaterialTheme.colors.background)
      .padding(horizontal = 8.dp),
    horizontalAlignment = Alignment.Start
  ) {
    CloseSheetBar(back)
    Text(
      "Your chat profile",
      Modifier.padding(bottom = 24.dp),
      style = MaterialTheme.typography.h1,
      color = MaterialTheme.colors.onBackground
    )
    Text(
      "Your profile is stored on your device and shared only with your contacts.\n" +
          "SimpleX servers cannot see your profile.",
      Modifier.padding(bottom = 24.dp),
      color = MaterialTheme.colors.onBackground
    )
    if (editProfile) {
      var displayName by remember { mutableStateOf(profile.displayName) }
      var fullName by remember { mutableStateOf(profile.fullName) }
      Column(
        modifier = Modifier.fillMaxWidth(),
        horizontalAlignment = Alignment.Start
      ) {
        // TODO hints
        BasicTextField(
          value = displayName,
          onValueChange = { displayName = it },
          modifier = Modifier
            .padding(bottom = 24.dp)
            .fillMaxWidth(),
          textStyle = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground),
          keyboardOptions = KeyboardOptions(
            capitalization = KeyboardCapitalization.None,
            autoCorrect = false
          ),
          singleLine = true
        )
        BasicTextField(
          value = fullName,
          onValueChange = { fullName = it },
          modifier = Modifier
            .padding(bottom = 24.dp)
            .fillMaxWidth(),
          textStyle = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground),
          keyboardOptions = KeyboardOptions(
            capitalization = KeyboardCapitalization.None,
            autoCorrect = false
          ),
          singleLine = true
        )
        Row {
          Text(
            "Cancel",
            color = MaterialTheme.colors.primary,
            modifier = Modifier
              .clickable(onClick = editProfileOff),
          )
          Spacer(Modifier.padding(horizontal = 8.dp))
          Text(
            "Save (and notify contacts)",
            color = MaterialTheme.colors.primary,
            modifier = Modifier
              .clickable(onClick = { saveProfile(displayName, fullName) })
          )
        }
      }
    } else {
      Column(
        modifier = Modifier.fillMaxWidth(),
        horizontalAlignment = Alignment.Start
      ) {
        Row(
          Modifier.padding(bottom = 24.dp)
        ) {
          Text(
            "Display name:",
            color = MaterialTheme.colors.onBackground
          )
          Spacer(Modifier.padding(horizontal = 4.dp))
          Text(
            profile.displayName,
            fontWeight = FontWeight.Bold,
            color = MaterialTheme.colors.onBackground
          )
        }
        Row(
          Modifier.padding(bottom = 24.dp)
        ) {
          Text(
            "Full name:",
            color = MaterialTheme.colors.onBackground
          )
          Spacer(Modifier.padding(horizontal = 4.dp))
          Text(
            profile.fullName,
            fontWeight = FontWeight.Bold,
            color = MaterialTheme.colors.onBackground
          )
        }
        Text(
          "Edit",
          color = MaterialTheme.colors.primary,
          modifier = Modifier
            .clickable(onClick = editProfileOn)
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
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
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
