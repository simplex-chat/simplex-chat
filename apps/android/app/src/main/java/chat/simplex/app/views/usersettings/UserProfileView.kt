package chat.simplex.app.views.usersettings

import android.content.res.Configuration
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.KeyboardCapitalization
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.Profile
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.*
import kotlinx.coroutines.launch

@Composable
fun UserProfileView(chatModel: ChatModel) {
  val user = chatModel.currentUser.value
  if (user != null) {
    var editProfile by remember { mutableStateOf(false) }
    var profile by remember { mutableStateOf(user.profile) }
    UserProfileLayout(
      editProfile = editProfile,
      profile = profile,
      editProfileOff = { editProfile = false },
      editProfileOn = { editProfile = true },
      saveProfile = { displayName: String, fullName: String ->
        withApi {
          val newProfile = chatModel.controller.apiUpdateProfile(
            profile = Profile(displayName, fullName, null)
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
  editProfileOff: () -> Unit,
  editProfileOn: () -> Unit,
  saveProfile: (String, String) -> Unit,
) {
  val bottomSheetModalState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden)
  var displayName = remember { mutableStateOf(profile.displayName) }
  var fullName = remember { mutableStateOf(profile.fullName) }
  var profileImageStr = remember { mutableStateOf(profile.displayImage) }
  val coroutineScope = rememberCoroutineScope()

  ModalBottomSheetLayout(
    scrimColor=MaterialTheme.colors.onSurface.copy(alpha = 0.0f),
    modifier = Modifier.fillMaxWidth(),
    sheetContent = {GetImageOptions(bottomSheetModalState, profileImageStr)},
    sheetState = bottomSheetModalState,
  ) {
    Column(horizontalAlignment = Alignment.Start) {
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
      // TODO hints
      Row(modifier = Modifier.fillMaxWidth()) {
        Column(
          horizontalAlignment = Alignment.Start,
          modifier = Modifier.padding(5.dp)
        ) {
          ProfileImage(80.dp, profileImageStr.value)
        }
        Column(horizontalAlignment = Alignment.Start) {
          Row(
            Modifier.padding(bottom = 24.dp)
          ) {
            EditableDisplayName(editProfile, profile, displayName)
          }
          Row(
            Modifier.padding(bottom = 24.dp)
          ) {
            EditableFullName(editProfile, profile, fullName)
          }
        }
      }
        if (editProfile) {

          Button(
            onClick = {
              coroutineScope.launch {
                if (!bottomSheetModalState.isVisible) {
                  bottomSheetModalState.show()
                } else {
                  bottomSheetModalState.hide()
                }
              }
            },
            modifier = Modifier
              .padding(8.dp)
              .fillMaxWidth(),
          ) {
            Text(
              text = "Update Profile Image",
              modifier = Modifier.padding(8.dp),
              textAlign = TextAlign.Center,
            )
          }

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
                .clickable(onClick = { saveProfile(displayName.value, fullName.value) })
            )
          }
        } else {
          Column(
            modifier = Modifier.fillMaxWidth(),
            horizontalAlignment = Alignment.Start
          ) {
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
  }


@Composable
fun EditableDisplayName(editMode: Boolean, profile: Profile, displayName: MutableState<String>){
  if (editMode) {
    BasicTextField(
      value = displayName.value,
      onValueChange = { displayName.value = it },
      modifier = Modifier
        .fillMaxWidth(),
      textStyle = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground),
      keyboardOptions = KeyboardOptions(
        capitalization = KeyboardCapitalization.None,
        autoCorrect = false
      ),
      singleLine = true
    )
  }
  else {
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
}


@Composable
fun EditableFullName(editMode: Boolean, profile: Profile, fullName: MutableState<String>){
  if (editMode) {
    BasicTextField(
      value = fullName.value,
      onValueChange = { fullName.value = it },
      modifier = Modifier
        .fillMaxWidth(),
      textStyle = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground),
      keyboardOptions = KeyboardOptions(
        capitalization = KeyboardCapitalization.None,
        autoCorrect = false
      ),
      singleLine = true
    )
  }
  else {
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
      editProfileOff = {},
      editProfileOn = {},
      saveProfile = { _, _ -> }
    )
  }
}
