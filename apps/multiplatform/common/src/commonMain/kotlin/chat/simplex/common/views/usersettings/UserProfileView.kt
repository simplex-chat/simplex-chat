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
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.ReadableText
import chat.simplex.common.platform.*
import chat.simplex.common.views.*
import chat.simplex.res.MR
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import java.net.URI

@Composable
fun UserProfileView(chatModel: ChatModel, close: () -> Unit) {
  val u = remember {chatModel.currentUser}
  val user = u.value
  KeyChangeEffect(u.value?.remoteHostId, u.value?.userId) {
    close()
  }

  if (user != null) {
    var profile by remember { mutableStateOf(user.profile.toProfile()) }
    UserProfileLayout(
      profile = profile,
      close = close,
      saveProfile = { displayName, fullName, shortDescr, description, image ->
        withBGApi {
          val updatedProfile = profile.copy(displayName = displayName.trim(), fullName = fullName.trim(), shortDescr = shortDescr.trim().ifEmpty { null }, description = description.trim().ifEmpty { null }, image = image)
          val updated = chatModel.controller.apiUpdateProfile(user.remoteHostId, updatedProfile)
          if (updated != null) {
            val (newProfile, _) = updated
            chatModel.updateCurrentUser(user.remoteHostId, newProfile)
            profile = newProfile
            close()
          }
        }
      }
    )
  }
}

@Composable
fun UserProfileLayout(
  profile: Profile,
  close: () -> Unit,
  saveProfile: (String, String, String, String, String?) -> Unit,
) {
  val bottomSheetModalState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden)
  val displayName = remember { mutableStateOf(profile.displayName) }
  val fullName = remember { mutableStateOf(profile.fullName) }
  val shortDescr = remember { mutableStateOf(profile.shortDescr ?: "") }
  val description = remember { mutableStateOf(profile.description ?: "") }
  val chosenImage = rememberSaveable { mutableStateOf<URI?>(null) }
  val profileImage = rememberSaveable { mutableStateOf(profile.image) }
  val scope = rememberCoroutineScope()
  val scrollState = rememberScrollState()
  val keyboardState by getKeyboardState()
  var savedKeyboardState by remember { mutableStateOf(keyboardState) }
  val focusRequester = remember { FocusRequester() }
  val descrFocusRequester = remember { FocusRequester() }
  var editingDescription by remember { mutableStateOf(false) }
    ModalBottomSheetLayout(
      scrimColor = Color.Black.copy(alpha = 0.12F),
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
        displayName.value.trim() == profile.displayName &&
            fullName.value.trim() == profile.fullName &&
            shortDescr.value.trim() == (profile.shortDescr ?: "") &&
            description.value.trim() == (profile.description ?: "") &&
            profile.image == profileImage.value
      val closeWithAlert = {
        if (dataUnchanged || !canSaveProfile(displayName.value, shortDescr.value, profile)) {
          close()
        } else {
          showUnsavedChangesAlert({ saveProfile(displayName.value, fullName.value, shortDescr.value, description.value, profileImage.value) }, close)
        }
      }
      LaunchedEffect(editingDescription) {
        if (editingDescription) {
          delay(200)
          descrFocusRequester.requestFocus()
        }
      }
      ModalView(close = if (editingDescription) ({ editingDescription = false }) else closeWithAlert) {
        if (editingDescription) {
          // app bar is top (default) or bottom (one-handed) — mirror ColumnWithScrollBar's spacers
          // so the entry area never runs under the app bar, keyboard, or system bars
          val oneHandUI = remember { ChatController.appPrefs.oneHandUI.state }
          Column(Modifier.fillMaxSize().imePadding().padding(horizontal = DEFAULT_PADDING)) {
            if (oneHandUI.value) {
              Spacer(Modifier.padding(top = DEFAULT_PADDING + 5.dp).windowInsetsTopHeight(WindowInsets.statusBars))
            } else {
              Spacer(Modifier.statusBarsPadding().padding(top = AppBarHeight * fontSizeSqrtMultiplier))
            }
            AppBarTitle(stringResource(MR.strings.profile_description__field), withPadding = false)
            // weight goes on the Box (a direct Column child); TextEditor forwards its modifier
            // to the inner BasicTextField, where weight would be ignored
            Box(Modifier.weight(1f, fill = false).padding(bottom = DEFAULT_PADDING)) {
              TextEditor(
                description,
                Modifier.heightIn(min = 140.dp),
                placeholder = stringResource(MR.strings.enter_description_optional),
                contentPadding = PaddingValues(),
                focusRequester = descrFocusRequester,
                maxLines = Int.MAX_VALUE
              )
            }
            if (oneHandUI.value) {
              Spacer(Modifier.navigationBarsPadding().padding(bottom = AppBarHeight * fontSizeSqrtMultiplier))
            } else {
              Spacer(Modifier.windowInsetsBottomHeight(WindowInsets.systemBars))
            }
          }
          return@ModalView
        }
        ColumnWithScrollBar(
          Modifier
            .padding(horizontal = DEFAULT_PADDING),
        ) {
          AppBarTitle(stringResource(MR.strings.your_current_profile), withPadding = false)
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
              if (!isValidNewProfileName(displayName.value, profile)) {
                Spacer(Modifier.width(DEFAULT_PADDING_HALF))
                IconButton({ showInvalidNameAlert(mkValidName(displayName.value), displayName) }, Modifier.size(20.dp)) {
                  Icon(painterResource(MR.images.ic_info), null, tint = MaterialTheme.colors.error)
                }
              }
            }
            ProfileNameField(displayName, "", { isValidNewProfileName(it, profile) }, focusRequester)
            if (showFullName(profile)) {
              Spacer(Modifier.height(DEFAULT_PADDING))
              Text(
                stringResource(MR.strings.full_name__field),
                fontSize = 16.sp,
                modifier = Modifier.padding(bottom = DEFAULT_PADDING_HALF)
              )
              ProfileNameField(fullName)
            }

            Spacer(Modifier.height(DEFAULT_PADDING))

            Row(Modifier.padding(bottom = DEFAULT_PADDING_HALF).fillMaxWidth(), horizontalArrangement = Arrangement.SpaceBetween) {
              Text(
                stringResource(MR.strings.short_descr__field),
                fontSize = 16.sp,
              )
              if (!bioFitsLimit(shortDescr.value)) {
                Spacer(Modifier.size(DEFAULT_PADDING_HALF))
                IconButton(
                  onClick = { AlertManager.shared.showAlertMsg(title = generalGetString(MR.strings.bio_too_large)) },
                  Modifier.size(20.dp)
                ) {
                  Icon(painterResource(MR.images.ic_info), null, tint = MaterialTheme.colors.error)
                }
              }
            }
            ProfileNameField(shortDescr)

            Spacer(Modifier.height(DEFAULT_PADDING))
            Text(
              stringResource(if (description.value.isBlank()) MR.strings.add_description else MR.strings.edit_description),
              color = MaterialTheme.colors.primary,
              modifier = Modifier.clickable { editingDescription = true }
            )

            Spacer(Modifier.height(DEFAULT_PADDING))
            val enabled = !dataUnchanged && canSaveProfile(displayName.value, shortDescr.value, profile)
            val saveModifier: Modifier = Modifier.clickable(enabled) { saveProfile(displayName.value, fullName.value, shortDescr.value, description.value, profileImage.value) }
            val saveColor: Color = if (enabled) MaterialTheme.colors.primary else MaterialTheme.colors.secondary
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

private fun isValidNewProfileName(displayName: String, profile: Profile): Boolean =
  displayName == profile.displayName || isValidDisplayName(displayName.trim())

private fun showFullName(profile: Profile): Boolean =
  profile.fullName.trim().isNotEmpty() && profile.fullName.trim() != profile.displayName.trim()

private fun canSaveProfile(displayName: String, shortDescr: String, profile: Profile): Boolean =
  displayName.trim().isNotEmpty() && isValidNewProfileName(displayName, profile) && bioFitsLimit(shortDescr)

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
      saveProfile = { _, _, _, _, _ -> }
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
      saveProfile = { _, _, _, _, _ -> }
    )
  }
}
