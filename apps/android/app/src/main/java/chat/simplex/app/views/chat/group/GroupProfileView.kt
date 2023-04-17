package chat.simplex.app.views.chat.group

import android.content.res.Configuration
import android.graphics.Bitmap
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
import chat.simplex.app.views.usersettings.*
import com.google.accompanist.insets.ProvideWindowInsets
import com.google.accompanist.insets.navigationBarsWithImePadding
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch

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
  val chosenImage = rememberSaveable { mutableStateOf<Uri?>(null) }
  val profileImage = rememberSaveable { mutableStateOf(groupProfile.image) }
  val scope = rememberCoroutineScope()
  val scrollState = rememberScrollState()
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
      ModalView(close = close) {
        Column(
          Modifier
            .verticalScroll(scrollState)
            .padding(horizontal = DEFAULT_PADDING),
          horizontalAlignment = Alignment.Start
        ) {
          Text(
            stringResource(R.string.group_profile_is_stored_on_members_devices),
            Modifier.padding(bottom = 24.dp),
            color = MaterialTheme.colors.onBackground,
            lineHeight = 22.sp
          )
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
            Row(Modifier.padding(bottom = DEFAULT_PADDING_HALF).fillMaxWidth(), horizontalArrangement = Arrangement.SpaceBetween) {
              Text(
                stringResource(R.string.group_display_name_field),
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
              stringResource(R.string.group_full_name_field),
              fontSize = 16.sp,
              modifier = Modifier.padding(bottom = DEFAULT_PADDING_HALF)
            )
            ProfileNameField(fullName)
            Spacer(Modifier.height(DEFAULT_PADDING))
            Row {
              TextButton(stringResource(R.string.cancel_verb)) {
                close.invoke()
              }
              Spacer(Modifier.padding(horizontal = 8.dp))
              val enabled = displayName.value.isNotEmpty() && isValidDisplayName(displayName.value)
              if (enabled) {
                Text(
                  stringResource(R.string.save_group_profile),
                  modifier = Modifier.clickable {
                    saveProfile(groupProfile.copy(
                      displayName = displayName.value,
                      fullName = fullName.value,
                      image = profileImage.value
                    ))
                  },
                  color = MaterialTheme.colors.primary
                )
              } else {
                Text(
                  stringResource(R.string.save_group_profile),
                  color = HighOrLowlight
                )
              }
            }
          }
          Spacer(Modifier.height(DEFAULT_BOTTOM_BUTTON_PADDING))

          LaunchedEffect(Unit) {
            delay(300)
            focusRequester.requestFocus()
          }
        }
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
fun PreviewGroupProfileLayout() {
  SimpleXTheme {
    GroupProfileLayout(
      close = {},
      groupProfile = GroupProfile.sampleData,
      saveProfile = { _ -> }
    )
  }
}
