package chat.simplex.app.views.chat.group

import android.content.res.Configuration
import android.graphics.Bitmap
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.ProfileNameField
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.isValidDisplayName
import chat.simplex.app.views.usersettings.*
import com.google.accompanist.insets.ProvideWindowInsets
import com.google.accompanist.insets.navigationBarsWithImePadding
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
  val displayName = remember { mutableStateOf(groupProfile.displayName) }
  val fullName = remember { mutableStateOf(groupProfile.fullName) }
  val chosenImage = remember { mutableStateOf<Bitmap?>(null) }
  val profileImage = remember { mutableStateOf(groupProfile.image) }
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
            .padding(bottom = 16.dp),
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
            Text(
              stringResource(R.string.group_display_name_field),
              Modifier.padding(bottom = 3.dp)
            )
            ProfileNameField(displayName, focusRequester)
            val errorText = if (!isValidDisplayName(displayName.value)) stringResource(R.string.display_name_cannot_contain_whitespace) else ""
            Text(
              errorText,
              fontSize = 15.sp,
              color = MaterialTheme.colors.error
            )
            Spacer(Modifier.height(3.dp))
            Text(
              stringResource(R.string.group_full_name_field),
              Modifier.padding(bottom = 5.dp)
            )
            ProfileNameField(fullName)
            Spacer(Modifier.height(16.dp))
            Row {
              TextButton(stringResource(R.string.cancel_verb)) {
                close.invoke()
              }
              Spacer(Modifier.padding(horizontal = 8.dp))
              val enabled = displayName.value.isNotEmpty() && isValidDisplayName(displayName.value)
              if (enabled) {
                Text(
                  stringResource(R.string.save_group_profile),
                  modifier = Modifier.clickable { saveProfile(GroupProfile(displayName.value, fullName.value, profileImage.value)) },
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

          LaunchedEffect(Unit) {
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
