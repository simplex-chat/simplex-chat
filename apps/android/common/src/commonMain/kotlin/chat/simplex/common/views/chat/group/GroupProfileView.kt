package chat.simplex.common.views.chat.group

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
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.icerockdev.library.MR
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.ProfileNameField
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.isValidDisplayName
import chat.simplex.common.views.onboarding.ReadableText
import chat.simplex.common.views.usersettings.*
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import java.net.URI

@Composable
fun GroupProfileView(groupInfo: GroupInfo, chatModel: ChatModel, close: () -> Unit) {
  GroupProfileLayout(
    close = close,
    groupProfile = groupInfo.groupProfile,
    saveProfile = { p ->
      withApi {
        val gInfo = chatModel.controller.apiUpdateGroup(groupInfo.groupId, p)
        if (gInfo != null) {
          chatModel.updateGroup(gInfo)
          close.invoke()
        }
      }
    }
  )
}

@Composable
fun GroupProfileLayout(
  close: () -> Unit,
  groupProfile: GroupProfile,
  saveProfile: (GroupProfile) -> Unit,
) {
  val bottomSheetModalState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden)
  val displayName = rememberSaveable { mutableStateOf(groupProfile.displayName) }
  val fullName = rememberSaveable { mutableStateOf(groupProfile.fullName) }
  val chosenImage = rememberSaveable { mutableStateOf<URI?>(null) }
  val profileImage = rememberSaveable { mutableStateOf(groupProfile.image) }
  val scope = rememberCoroutineScope()
  val scrollState = rememberScrollState()
  val focusRequester = remember { FocusRequester() }
  val dataUnchanged =
    displayName.value == groupProfile.displayName &&
        fullName.value == groupProfile.fullName &&
        groupProfile.image == profileImage.value
  val closeWithAlert = {
    if (dataUnchanged || !(displayName.value.isNotEmpty() && isValidDisplayName(displayName.value))) {
      close()
    } else {
      showUnsavedChangesAlert({
        saveProfile(
          groupProfile.copy(
            displayName = displayName.value,
            fullName = fullName.value,
            image = profileImage.value
          )
        )
      }, close)
    }
  }
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
      ModalView(close = closeWithAlert) {
        Column(
          Modifier
            .verticalScroll(scrollState)
        ) {
          Column(
            Modifier.fillMaxWidth()
              .padding(horizontal = DEFAULT_PADDING)
          ) {
            ReadableText(MR.strings.group_profile_is_stored_on_members_devices, TextAlign.Center)
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
                stringResource(MR.strings.group_display_name_field),
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
              stringResource(MR.strings.group_full_name_field),
              fontSize = 16.sp,
              modifier = Modifier.padding(bottom = DEFAULT_PADDING_HALF)
            )
            ProfileNameField(fullName)
            Spacer(Modifier.height(DEFAULT_PADDING))
            val enabled = !dataUnchanged && displayName.value.isNotEmpty() && isValidDisplayName(displayName.value)
            if (enabled) {
              Text(
                stringResource(MR.strings.save_group_profile),
                modifier = Modifier.clickable {
                  saveProfile(
                    groupProfile.copy(
                      displayName = displayName.value,
                      fullName = fullName.value,
                      image = profileImage.value
                    )
                  )
                },
                color = MaterialTheme.colors.primary
              )
            } else {
              Text(
                stringResource(MR.strings.save_group_profile),
                color = MaterialTheme.colors.secondary
              )
            }
          }

          SectionBottomSpacer()

          LaunchedEffect(Unit) {
            delay(300)
            focusRequester.requestFocus()
          }
        }
      }
    }
  }
}

private fun showUnsavedChangesAlert(save: () -> Unit, revert: () -> Unit) {
  AlertManager.shared.showAlertDialogStacked(
    title = generalGetString(MR.strings.save_preferences_question),
    confirmText = generalGetString(MR.strings.save_and_notify_group_members),
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
fun PreviewGroupProfileLayout() {
  SimpleXTheme {
    GroupProfileLayout(
      close = {},
      groupProfile = GroupProfile.sampleData,
      saveProfile = { _ -> }
    )
  }
}
