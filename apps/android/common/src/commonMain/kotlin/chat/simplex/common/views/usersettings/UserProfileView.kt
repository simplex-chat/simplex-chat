package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import androidx.compose.desktop.ui.tooling.preview.Preview
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
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.icerockdev.library.MR
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.ProfileNameField
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.isValidDisplayName
import chat.simplex.common.views.onboarding.ReadableText
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.Profile
import chat.simplex.common.platform.*
import kotlinx.coroutines.launch
import java.net.URI

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
  val chosenImage = rememberSaveable { mutableStateOf<URI?>(null) }
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
          AppBarTitle(stringResource(MR.strings.your_current_profile))
          ReadableText(generalGetString(MR.strings.your_profile_is_stored_on_device_and_shared_only_with_contacts_simplex_cannot_see_it), TextAlign.Center)
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
                stringResource(MR.strings.display_name__field),
                fontSize = 16.sp
              )
              if (!isValidDisplayName(displayName.value)) {
                Spacer(Modifier.size(DEFAULT_PADDING_HALF))
                Text(
                  stringResource(MR.strings.no_spaces),
                  fontSize = 16.sp,
                  color = Color.Red
                )
              }
            }
            ProfileNameField(displayName, "", ::isValidDisplayName, focusRequester)
            Spacer(Modifier.height(DEFAULT_PADDING))
            Text(
              stringResource(MR.strings.full_name__field),
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
              stringResource(MR.strings.save_and_notify_contacts),
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
      painterResource(MR.images.ic_photo_camera),
      contentDescription = stringResource(MR.strings.edit_image),
      tint = MaterialTheme.colors.primary,
      modifier = Modifier.size(30.dp)
    )
  }
}

@Composable
fun DeleteImageButton(click: () -> Unit) {
  IconButton(onClick = click) {
    Icon(
      painterResource(MR.images.ic_close),
      contentDescription = stringResource(MR.strings.delete_image),
      tint = MaterialTheme.colors.primary,
    )
  }
}

private fun showUnsavedChangesAlert(save: () -> Unit, revert: () -> Unit) {
  AlertManager.shared.showAlertDialogStacked(
    title = generalGetString(MR.strings.save_preferences_question),
    confirmText = generalGetString(MR.strings.save_and_notify_contacts),
    dismissText = generalGetString(MR.strings.exit_without_saving),
    onConfirm = save,
    onDismiss = revert,
  )
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
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

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
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
