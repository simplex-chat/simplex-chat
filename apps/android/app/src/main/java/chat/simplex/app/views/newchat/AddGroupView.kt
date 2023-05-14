package chat.simplex.app.views.newchat

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
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.ProfileNameField
import chat.simplex.app.views.chat.group.AddGroupMembersView
import chat.simplex.app.views.chatlist.setGroupMembers
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.isValidDisplayName
import chat.simplex.app.views.onboarding.ReadableText
import chat.simplex.app.views.usersettings.DeleteImageButton
import chat.simplex.app.views.usersettings.EditImageButton
import com.google.accompanist.insets.ProvideWindowInsets
import com.google.accompanist.insets.navigationBarsWithImePadding
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch

@Composable
fun AddGroupView(chatModel: ChatModel, close: () -> Unit) {
  AddGroupLayout(
    createGroup = { groupProfile ->
      withApi {
        val groupInfo = chatModel.controller.apiNewGroup(groupProfile)
        if (groupInfo != null) {
          chatModel.addChat(Chat(chatInfo = ChatInfo.Group(groupInfo), chatItems = listOf()))
          chatModel.chatItems.clear()
          chatModel.chatId.value = groupInfo.id
          setGroupMembers(groupInfo, chatModel)
          close.invoke()
          ModalManager.shared.showModalCloseable(true) { close ->
            AddGroupMembersView(groupInfo, true, chatModel, close)
          }
        }
      }
    },
    close
  )
}

@Composable
fun AddGroupLayout(createGroup: (GroupProfile) -> Unit, close: () -> Unit) {
  val bottomSheetModalState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden)
  val scope = rememberCoroutineScope()
  val displayName = rememberSaveable { mutableStateOf("") }
  val fullName = rememberSaveable { mutableStateOf("") }
  val chosenImage = rememberSaveable { mutableStateOf<Uri?>(null) }
  val profileImage = rememberSaveable { mutableStateOf<String?>(null) }
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
            .fillMaxSize()
            .verticalScroll(rememberScrollState())
            .padding(horizontal = DEFAULT_PADDING)
        ) {
          AppBarTitle(stringResource(R.string.create_secret_group_title))
          ReadableText(R.string.group_is_decentralized, TextAlign.Center)
          Box(
            Modifier
              .fillMaxWidth()
              .padding(bottom = 24.dp),
            contentAlignment = Alignment.Center
          ) {
            Box(contentAlignment = Alignment.TopEnd) {
              Box(contentAlignment = Alignment.Center) {
                ProfileImage(108.dp, image = profileImage.value)
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
          ProfileNameField(fullName, "")
          Spacer(Modifier.height(8.dp))
          val enabled = displayName.value.isNotEmpty() && isValidDisplayName(displayName.value)
          if (enabled) {
            CreateGroupButton(MaterialTheme.colors.primary, Modifier
              .clickable {
                createGroup(GroupProfile(
                  displayName = displayName.value,
                  fullName = fullName.value,
                  image = profileImage.value
                ))
              }
              .padding(8.dp))
          } else {
            CreateGroupButton(MaterialTheme.colors.secondary, Modifier.padding(8.dp))
          }
          LaunchedEffect(Unit) {
            delay(300)
            focusRequester.requestFocus()
          }
        }
      }
    }
  }
}

@Composable
fun CreateGroupButton(color: Color, modifier: Modifier) {
  Row(
    Modifier.fillMaxWidth(),
    horizontalArrangement = Arrangement.End
  ) {
    Surface(shape = RoundedCornerShape(20.dp), color = Color.Transparent) {
      Row(modifier, verticalAlignment = Alignment.CenterVertically) {
        Text(stringResource(R.string.create_profile_button), style = MaterialTheme.typography.caption, color = color, fontWeight = FontWeight.Bold)
        Icon(painterResource(R.drawable.ic_arrow_forward_ios), stringResource(R.string.create_profile_button), tint = color)
      }
    }
  }
}

@Preview
@Composable
fun PreviewAddGroupLayout() {
  SimpleXTheme {
    AddGroupLayout(
      createGroup = {},
      close = {}
    )
  }
}
