package chat.simplex.common.views.newchat

import SectionTextFooter
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
import androidx.compose.ui.text.buildAnnotatedString
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.group.AddGroupMembersView
import chat.simplex.common.views.chatlist.setGroupMembers
import chat.simplex.common.views.helpers.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.*
import chat.simplex.common.views.chat.group.GroupLinkView
import chat.simplex.common.views.usersettings.*
import chat.simplex.res.MR
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import java.net.URI

@Composable
fun AddGroupView(chatModel: ChatModel, rh: RemoteHostInfo?, close: () -> Unit) {
  val rhId = rh?.remoteHostId
  AddGroupLayout(
    createGroup = { incognito, groupProfile ->
      withBGApi {
        val groupInfo = chatModel.controller.apiNewGroup(rhId, incognito, groupProfile)
        if (groupInfo != null) {
          chatModel.addChat(Chat(remoteHostId = rhId, chatInfo = ChatInfo.Group(groupInfo), chatItems = listOf()))
          chatModel.chatItems.clear()
          chatModel.chatItemStatuses.clear()
          chatModel.chatId.value = groupInfo.id
          setGroupMembers(rhId, groupInfo, chatModel)
          close.invoke()
          if (!groupInfo.incognito) {
            ModalManager.end.showModalCloseable(true) { close ->
              AddGroupMembersView(rhId, groupInfo, creatingGroup = true, chatModel, close)
            }
          } else {
            ModalManager.end.showModalCloseable(true) { close ->
              GroupLinkView(chatModel, rhId, groupInfo, connReqContact = null, memberRole = null, onGroupLinkUpdated = null, creatingGroup = true, close)
            }
          }
        }
      }
    },
    incognitoPref = chatModel.controller.appPrefs.incognito,
    rhId,
    close
  )
}

@Composable
fun AddGroupLayout(
  createGroup: (Boolean, GroupProfile) -> Unit,
  incognitoPref: SharedPreference<Boolean>,
  rhId: Long?,
  close: () -> Unit
) {
  val bottomSheetModalState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden)
  val scope = rememberCoroutineScope()
  val displayName = rememberSaveable { mutableStateOf("") }
  val chosenImage = rememberSaveable { mutableStateOf<URI?>(null) }
  val profileImage = rememberSaveable { mutableStateOf<String?>(null) }
  val focusRequester = remember { FocusRequester() }
  val incognito = remember { mutableStateOf(incognitoPref.get()) }

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
        ColumnWithScrollBar(
          Modifier
            .fillMaxSize()
            .padding(horizontal = DEFAULT_PADDING)
        ) {
          AppBarTitle(stringResource(MR.strings.create_secret_group_title), hostDevice(rhId))
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
            if (!isValidDisplayName(displayName.value.trim())) {
              Spacer(Modifier.size(DEFAULT_PADDING_HALF))
              IconButton({ showInvalidNameAlert(mkValidName(displayName.value.trim()), displayName) }, Modifier.size(20.dp)) {
                Icon(painterResource(MR.images.ic_info), null, tint = MaterialTheme.colors.error)
              }
            }
          }
          ProfileNameField(displayName, "", { isValidDisplayName(it.trim()) }, focusRequester)
          Spacer(Modifier.height(8.dp))

          SettingsActionItem(
            painterResource(MR.images.ic_check),
            stringResource(MR.strings.create_group_button),
            click = {
              createGroup(incognito.value, GroupProfile(
                displayName = displayName.value.trim(),
                fullName = "",
                image = profileImage.value,
                groupPreferences = GroupPreferences(history = GroupPreference(GroupFeatureEnabled.ON))
              ))
            },
            textColor = MaterialTheme.colors.primary,
            iconColor = MaterialTheme.colors.primary,
            disabled = !canCreateProfile(displayName.value)
          )

          IncognitoToggle(incognitoPref, incognito) { ModalManager.start.showModal { IncognitoView() } }

          SectionTextFooter(
            buildAnnotatedString {
              append(sharedProfileInfo(chatModel, incognito.value))
              append("\n")
              append(annotatedStringResource(MR.strings.group_is_decentralized))
            }
          )

          LaunchedEffect(Unit) {
            delay(300)
            focusRequester.requestFocus()
          }
        }
      }
    }
  }
}

fun canCreateProfile(displayName: String): Boolean = displayName.trim().isNotEmpty() && isValidDisplayName(displayName.trim())

@Preview
@Composable
fun PreviewAddGroupLayout() {
  SimpleXTheme {
    AddGroupLayout(
      createGroup = { _, _ -> },
      incognitoPref = SharedPreference({ false }, {}),
      close = {},
      rhId = null,
    )
  }
}
