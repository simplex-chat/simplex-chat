package chat.simplex.app.views.usersettings

import SectionBottomSpacer
import android.content.res.Configuration
import android.net.Uri
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.ProfileNameField
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.isValidDisplayName
import chat.simplex.app.views.onboarding.ReadableText
import com.google.accompanist.insets.ProvideWindowInsets
import com.google.accompanist.insets.navigationBarsWithImePadding
import kotlinx.coroutines.launch

@Composable
fun UserProfileView(chatModel: ChatModel, close: () -> Unit) {
  val user = chatModel.currentUser.value
  if (user != null) {
    var profile by remember { mutableStateOf(user.profile.toProfile()) }
    UserProfileLayout(
      profile = profile,
      close,
      saveProfile = { displayName, fullName, image ->
        withApi {
          val newProfile = chatModel.controller.apiUpdateProfile(profile.copy(displayName = displayName, fullName = fullName, image = image))
          if (newProfile != null) {
            chatModel.updateCurrentUser(newProfile)
            profile = newProfile
          }
          close()
        }
      }
    )
  }
}

@Composable
fun UserProfileLayout(
  profile: Profile,
  close: () -> Unit,
  saveProfile: (String, String, String?) -> Unit,
) {
  val bottomSheetModalState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden)
  val displayName = remember { mutableStateOf(profile.displayName) }
  val fullName = remember { mutableStateOf(profile.fullName) }
  val chosenImage = rememberSaveable { mutableStateOf<Uri?>(null) }
  val profileImage = rememberSaveable { mutableStateOf(profile.image) }
  val scope = rememberCoroutineScope()
  val scrollState = rememberScrollState()
  val keyboardState by getKeyboardState()
  var savedKeyboardState by remember { mutableStateOf(keyboardState) }
  val focusRequester = remember { FocusRequester() }
  ProvideWindowInsets(windowInsetsAnimationsEnabled = true) {
    ModalBottomSheetLayout(
      scrimColor = Color.Black.copy(alpha = 0.12F),
      modifier = Modifier.navigationBarsWithImePadding(),
      sheetContent = {
        GetImageBottomSheet(
          chosenImage,
          onImageChange = { bitmap -> profileImage.value = resizeImageToStrSize(cropToSquare(bitmap), maxDataSize = 12500) },
          hideBottomSheet = {
            scope.launch { bottomSheetModalState.hide() }
          })
      },
      sheetState = bottomSheetModalState,
      sheetShape = RoundedCornerShape(topStart = 18.dp, topEnd = 18.dp)
    ) {
      val dataUnchanged =
        displayName.value == profile.displayName &&
            fullName.value == profile.fullName &&
            profile.image == profileImage.value

      val closeWithAlert = {
        if (dataUnchanged || !(displayName.value.isNotEmpty() && isValidDisplayName(displayName.value))) {
          close()
        } else {
          showUnsavedChangesAlert({ saveProfile(displayName.value, fullName.value, profileImage.value) }, close)
        }
      }
      ModalView(close = closeWithAlert) {
        Column(
          Modifier
            .verticalScroll(scrollState)
            .padding(horizontal = DEFAULT_PADDING),
        ) {
          AppBarTitle(stringResource(R.string.your_current_profile))
          ReadableText(generalGetString(R.string.your_profile_is_stored_on_device_and_shared_only_with_contacts_simplex_cannot_see_it), TextAlign.Center)
          Column(
            Modifier
              .fillMaxWidth()
          ) {
            Box(
              Modifier
                .fillMaxWidth()
                .padding(bottom = 24.dp),
              contentAlignment = Alignment.Center
            ) {
              Box(contentAlignment = Alignment.TopEnd) {
                Box(contentAlignment = Alignment.Center) {
                  ProfileImage(108.dp, profileImage.value, color = MaterialTheme.colors.secondary.copy(alpha = 0.1f))
                  EditImageButton { scope.launch { bottomSheetModalState.show() } }
                }
                if (profileImage.value != null) {
                  DeleteImageButton { profileImage.value = null }
                }
              }
            }
            Row(Modifier.padding(bottom = DEFAULT_PADDING_HALF).fillMaxWidth(), horizontalArrangement = Arrangement.SpaceBetween) {
              Text(
                stringResource(R.string.display_name__field),
                fontSize = 16.sp
              )
              if (!isValidDisplayName(displayName.value)) {
                Spacer(Modifier.size(DEFAULT_PADDING_HALF))
                Text(
                  stringResource(R.string.no_spaces),
                  fontSize = 16.sp,
                  color = Color.Red
                )
              }
            }
            ProfileNameField(displayName, "", ::isValidDisplayName, focusRequester)
            Spacer(Modifier.height(DEFAULT_PADDING))
            Text(
              stringResource(R.string.full_name__field),
              fontSize = 16.sp,
              modifier = Modifier.padding(bottom = DEFAULT_PADDING_HALF)
            )
            ProfileNameField(fullName)

            Spacer(Modifier.height(DEFAULT_PADDING))
            val enabled = !dataUnchanged && displayName.value.isNotEmpty() && isValidDisplayName(displayName.value)
            val saveModifier: Modifier
            val saveColor: Color
            if (enabled) {
              saveModifier = Modifier
                .clickable { saveProfile(displayName.value, fullName.value, profileImage.value) }
              saveColor = MaterialTheme.colors.primary
            } else {
              saveModifier = Modifier
              saveColor = MaterialTheme.colors.secondary
            }
            Text(
              stringResource(R.string.save_and_notify_contacts),
              modifier = saveModifier,
              color = saveColor
            )
          }
          Spacer(Modifier.height(DEFAULT_BOTTOM_BUTTON_PADDING))
          if (savedKeyboardState != keyboardState) {
            LaunchedEffect(keyboardState) {
              scope.launch {
                savedKeyboardState = keyboardState
                scrollState.animateScrollTo(scrollState.maxValue)
              }
            }
          }
          SectionBottomSpacer()
        }
      }
    }
  }
}

@Composable
fun EditImageButton(click: () -> Unit) {
  IconButton(
    onClick = click,
    modifier = Modifier.size(30.dp)
  ) {
    Icon(
      painterResource(R.drawable.ic_photo_camera),
      contentDescription = stringResource(R.string.edit_image),
      tint = MaterialTheme.colors.primary,
      modifier = Modifier.size(30.dp)
    )
  }
}

@Composable
fun DeleteImageButton(click: () -> Unit) {
  IconButton(onClick = click) {
    Icon(
      painterResource(R.drawable.ic_close),
      contentDescription = stringResource(R.string.delete_image),
      tint = MaterialTheme.colors.primary,
    )
  }
}

private fun showUnsavedChangesAlert(save: () -> Unit, revert: () -> Unit) {
  AlertManager.shared.showAlertDialogStacked(
    title = generalGetString(R.string.save_preferences_question),
    confirmText = generalGetString(R.string.save_and_notify_contacts),
    dismissText = generalGetString(R.string.exit_without_saving),
    onConfirm = save,
    onDismiss = revert,
  )
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
      close = {},
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
      profile = Profile.sampleData,
      close = {},
      saveProfile = { _, _, _ -> }
    )
  }
}
