package chat.simplex.common.views.newchat

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
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.icerockdev.library.MR
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.ProfileNameField
import chat.simplex.common.views.chat.group.AddGroupMembersView
import chat.simplex.common.views.chatlist.setGroupMembers
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.isValidDisplayName
import chat.simplex.common.views.onboarding.ReadableText
import chat.simplex.common.views.usersettings.DeleteImageButton
import chat.simplex.common.views.usersettings.EditImageButton
import chat.simplex.common.platform.*
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import java.net.URI

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
  val chosenImage = rememberSaveable { mutableStateOf<URI?>(null) }
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
          AppBarTitle(stringResource(MR.strings.create_secret_group_title))
          ReadableText(MR.strings.group_is_decentralized, TextAlign.Center)
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
          ProfileNameField(fullName, "")
          Spacer(Modifier.height(8.dp))
          val enabled = displayName.value.isNotEmpty() && isValidDisplayName(displayName.value)
          if (enabled) {
            CreateGroupButton(MaterialTheme.colors.primary, Modifier
              .clickable {
                createGroup(
                  GroupProfile(
                  displayName = displayName.value,
                  fullName = fullName.value,
                  image = profileImage.value
                )
                )
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
        Text(stringResource(MR.strings.create_profile_button), style = MaterialTheme.typography.caption, color = color, fontWeight = FontWeight.Bold)
        Icon(painterResource(MR.images.ic_arrow_forward_ios), stringResource(MR.strings.create_profile_button), tint = color)
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
