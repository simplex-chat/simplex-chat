package chat.simplex.app.views.usersettings

import android.content.res.Configuration
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Close
import androidx.compose.material.icons.outlined.PhotoCamera
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.KeyboardCapitalization
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.Profile
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.*
import com.google.accompanist.insets.ProvideWindowInsets
import com.google.accompanist.insets.navigationBarsWithImePadding
import kotlinx.coroutines.launch

@Composable
fun UserProfileView(chatModel: ChatModel, close: () -> Unit) {
  val user = chatModel.currentUser.value
  if (user != null) {
    var editProfile = remember { mutableStateOf(false) }
    var profile by remember { mutableStateOf(user.profile) }
    UserProfileLayout(
      close = close,
      editProfile = editProfile,
      profile = profile,
      saveProfile = { displayName, fullName, image ->
        withApi {
          val p = Profile(displayName, fullName, image)
          val newProfile = chatModel.controller.apiUpdateProfile(p)
          if (newProfile != null) {
            chatModel.updateUserProfile(newProfile)
            profile = newProfile
          }
          editProfile.value = false
        }
      }
    )
  }
}

@Composable
fun UserProfileLayout(
  close: () -> Unit,
  editProfile: MutableState<Boolean>,
  profile: Profile,
  saveProfile: (String, String, String?) -> Unit,
) {
  val bottomSheetModalState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden)
  val displayName = remember { mutableStateOf(profile.displayName) }
  val fullName = remember { mutableStateOf(profile.fullName) }
  val profileImage = remember { mutableStateOf(profile.image) }
  val scope = rememberCoroutineScope()
  val scrollState = rememberScrollState()
  val keyboardState by getKeyboardState()
  var savedKeyboardState by remember { mutableStateOf(keyboardState) }

  ProvideWindowInsets(windowInsetsAnimationsEnabled = true) {
    ModalBottomSheetLayout(
      scrimColor = Color.Black.copy(alpha = 0.12F),
      modifier = Modifier.navigationBarsWithImePadding(),
      sheetContent = {
        GetImageBottomSheet(profileImage, hideBottomSheet = {
          scope.launch { bottomSheetModalState.hide() }
        })
      },
      sheetState = bottomSheetModalState,
      sheetShape = RoundedCornerShape(topStart = 18.dp, topEnd = 18.dp)
    ) {
      ModalView(close = close) {
        Column(
          Modifier
            .verticalScroll(scrollState)
            .padding(bottom = 16.dp),
          horizontalAlignment = Alignment.Start
        ) {
          Text(
            generalGetString(R.string.your_chat_profile),
            Modifier.padding(bottom = 24.dp),
            style = MaterialTheme.typography.h1,
            color = MaterialTheme.colors.onBackground
          )
          Text(
            generalGetString(R.string.your_profile_is_stored_on_device_and_shared_only_with_contacts_simplex_cannot_see_it),
            Modifier.padding(bottom = 24.dp),
            color = MaterialTheme.colors.onBackground
          )
          if (editProfile.value) {
            Column(
              Modifier.fillMaxWidth(),
              horizontalAlignment = Alignment.Start
            ) {
              Box(
                Modifier
                  .fillMaxWidth()
                  .padding(bottom = 24.dp),
                contentAlignment = Alignment.Center
              ) {
                Box(contentAlignment = Alignment.TopEnd) {
                  Box(contentAlignment = Alignment.Center) {
                    ProfileImage(192.dp, profileImage.value)
                    EditImageButton { scope.launch { bottomSheetModalState.show() } }
                  }
                  if (profileImage.value != null) {
                    DeleteImageButton { profileImage.value = null }
                  }
                }
              }
              ProfileNameTextField(displayName)
              ProfileNameTextField(fullName)
              Row {
                TextButton(generalGetString(R.string.cancel)) {
                  displayName.value = profile.displayName
                  fullName.value = profile.fullName
                  profileImage.value = profile.image
                  editProfile.value = false
                }
                Spacer(Modifier.padding(horizontal = 8.dp))
                TextButton(generalGetString(R.string.save_and_notify_contacts)) {
                  saveProfile(displayName.value, fullName.value, profileImage.value)
                }
              }
            }
          } else {
            Column(
              modifier = Modifier.fillMaxWidth(),
              horizontalAlignment = Alignment.Start
            ) {
              Box(
                Modifier
                  .fillMaxWidth()
                  .padding(bottom = 24.dp), contentAlignment = Alignment.Center
              ) {
                ProfileImage(192.dp, profile.image)
                if (profile.image == null) {
                  EditImageButton {
                    editProfile.value = true
                    scope.launch { bottomSheetModalState.show() }
                  }
                }
              }
              ProfileNameRow(generalGetString(R.string.display_name__field), profile.displayName)
              ProfileNameRow(generalGetString(R.string.full_name__field), profile.fullName)
              TextButton(generalGetString(R.string.edit)) { editProfile.value = true }
            }
          }
          if (savedKeyboardState != keyboardState) {
            LaunchedEffect(keyboardState) {
              scope.launch {
                savedKeyboardState = keyboardState
                scrollState.animateScrollTo(scrollState.maxValue)
              }
            }
          }
        }
      }
    }
  }
}

@Composable
private fun ProfileNameTextField(name: MutableState<String>) {
  BasicTextField(
    value = name.value,
    onValueChange = { name.value = it },
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
}

@Composable
private fun ProfileNameRow(label: String, text: String) {
  Row(Modifier.padding(bottom = 24.dp)) {
    Text(
      label,
      color = MaterialTheme.colors.onBackground
    )
    Spacer(Modifier.padding(horizontal = 4.dp))
    Text(
      text,
      fontWeight = FontWeight.Bold,
      color = MaterialTheme.colors.onBackground
    )
  }
}

@Composable
private fun TextButton(text: String, click: () -> Unit) {
  Text(
    text,
    color = MaterialTheme.colors.primary,
    modifier = Modifier.clickable(onClick = click),
  )
}

@Composable
fun EditImageButton(click: () -> Unit) {
  IconButton(
    onClick = click,
    modifier = Modifier.background(Color(1f, 1f, 1f, 0.2f), shape = CircleShape)
  ) {
    Icon(
      Icons.Outlined.PhotoCamera,
      contentDescription = generalGetString(R.string.edit_image),
      tint = MaterialTheme.colors.primary,
      modifier = Modifier.size(36.dp)
    )
  }
}

@Composable
fun DeleteImageButton(click: () -> Unit) {
  IconButton(onClick = click) {
    Icon(
      Icons.Outlined.Close,
      contentDescription = generalGetString(R.string.delete_image),
      tint = MaterialTheme.colors.primary,
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
      close = {},
      profile = Profile.sampleData,
      editProfile = remember { mutableStateOf(false) },
      saveProfile = { _, _, _ -> }
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
      close = {},
      profile = Profile.sampleData,
      editProfile = remember { mutableStateOf(true) },
      saveProfile = {_, _, _ ->}
    )
  }
}
