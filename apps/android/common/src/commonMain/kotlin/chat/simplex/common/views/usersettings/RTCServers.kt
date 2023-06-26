package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import SectionItemViewSpaceBetween
import androidx.compose.runtime.Composable
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalUriHandler
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.icerockdev.library.MR
import chat.simplex.common.model.ChatModel
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.call.parseRTCIceServers
import chat.simplex.common.views.helpers.*

@Composable
fun RTCServersView(
  chatModel: ChatModel
) {
  var userRTCServers by remember {
    mutableStateOf(chatModel.controller.appPrefs.webrtcIceServers.get()?.split("\n") ?: listOf())
  }
  var isUserRTCServers by remember { mutableStateOf(userRTCServers.isNotEmpty()) }
  var editRTCServers by remember { mutableStateOf(!isUserRTCServers) }
  val userRTCServersStr = remember { mutableStateOf(if (isUserRTCServers) userRTCServers.joinToString(separator = "\n") else "") }
  fun saveUserRTCServers() {
    val srvs = userRTCServersStr.value.split("\n")
    if (srvs.isNotEmpty() && srvs.toSet().size == srvs.size && parseRTCIceServers(srvs) != null) {
      userRTCServers = srvs
      chatModel.controller.appPrefs.webrtcIceServers.set(srvs.joinToString(separator = "\n"))
      editRTCServers = false
    } else {
      AlertManager.shared.showAlertMsg(
        generalGetString(MR.strings.error_saving_ICE_servers),
        generalGetString(MR.strings.ensure_ICE_server_address_are_correct_format_and_unique)
      )
    }
  }

  fun resetRTCServers() {
    isUserRTCServers = false
    userRTCServers = listOf()
    chatModel.controller.appPrefs.webrtcIceServers.set(null)
  }

  RTCServersLayout(
    isUserRTCServers = isUserRTCServers,
    editRTCServers = editRTCServers,
    userRTCServersStr = userRTCServersStr,
    isUserRTCServersOnOff = { switch ->
      if (switch) {
        isUserRTCServers = true
      } else if (userRTCServers.isNotEmpty()) {
          AlertManager.shared.showAlertDialog(
            title = generalGetString(MR.strings.use_simplex_chat_servers__question),
            text = generalGetString(MR.strings.saved_ICE_servers_will_be_removed),
            confirmText = generalGetString(MR.strings.confirm_verb),
            onConfirm = {
              resetRTCServers()
              isUserRTCServers = false
              userRTCServersStr.value = ""
            },
            destructive = true,
          )
        } else {
        isUserRTCServers = false
        userRTCServersStr.value = ""
      }
    },
    cancelEdit = {
      isUserRTCServers = userRTCServers.isNotEmpty()
      editRTCServers = !isUserRTCServers
      userRTCServersStr.value = if (isUserRTCServers) userRTCServers.joinToString(separator = "\n") else ""
    },
    saveRTCServers = ::saveUserRTCServers,
    editOn = { editRTCServers = true },
  )
}

@Composable
fun RTCServersLayout(
  isUserRTCServers: Boolean,
  editRTCServers: Boolean,
  userRTCServersStr: MutableState<String>,
  isUserRTCServersOnOff: (Boolean) -> Unit,
  cancelEdit: () -> Unit,
  saveRTCServers: () -> Unit,
  editOn: () -> Unit,
) {
  Column {
    AppBarTitle(stringResource(MR.strings.your_ICE_servers))
    Column(
      Modifier
        .fillMaxWidth()
        .verticalScroll(rememberScrollState())
        .padding(horizontal = DEFAULT_PADDING),
      verticalArrangement = Arrangement.spacedBy(8.dp)
    ) {
      SectionItemViewSpaceBetween(padding = PaddingValues()) {
        Text(stringResource(MR.strings.configure_ICE_servers), Modifier.padding(end = 24.dp))
        DefaultSwitch(
          checked = isUserRTCServers,
          onCheckedChange = isUserRTCServersOnOff,
        )
      }

      if (!isUserRTCServers) {
        Text(stringResource(MR.strings.using_simplex_chat_servers), lineHeight = 22.sp)
      } else {
        Text(stringResource(MR.strings.enter_one_ICE_server_per_line))
        if (editRTCServers) {
          TextEditor(userRTCServersStr, Modifier.height(160.dp), contentPadding = PaddingValues())

          Row(
            Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.SpaceBetween,
            verticalAlignment = Alignment.CenterVertically
          ) {
            Column {
              Row {
                Text(
                  stringResource(MR.strings.cancel_verb),
                  color = MaterialTheme.colors.primary,
                  modifier = Modifier
                    .clickable(onClick = cancelEdit)
                )
                Spacer(Modifier.padding(horizontal = 8.dp))
                Text(
                  stringResource(MR.strings.save_servers_button),
                  color = MaterialTheme.colors.primary,
                  modifier = Modifier.clickable(onClick = {
                    saveRTCServers()
                  })
                )
              }
            }
            Column(horizontalAlignment = Alignment.End) {
              howToButton()
            }
          }
        } else {
          Surface(
            modifier = Modifier
              .height(160.dp)
              .fillMaxWidth(),
            shape = RoundedCornerShape(10.dp),
            border = BorderStroke(1.dp, MaterialTheme.colors.secondaryVariant)
          ) {
            SelectionContainer(
              Modifier.verticalScroll(rememberScrollState())
            ) {
              Text(
                userRTCServersStr.value,
                Modifier
                  .padding(vertical = 5.dp, horizontal = 7.dp),
                style = TextStyle(fontFamily = FontFamily.Monospace, fontSize = 14.sp),
              )
            }
          }
          Row(
            Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.SpaceBetween,
            verticalAlignment = Alignment.CenterVertically
          ) {
            Column {
              Text(
                stringResource(MR.strings.edit_verb),
                color = MaterialTheme.colors.primary,
                modifier = Modifier
                  .clickable(onClick = editOn)
              )
            }
            Column(horizontalAlignment = Alignment.End) {
              howToButton()
            }
          }
        }
      }
      SectionBottomSpacer()
    }
  }
}

@Composable
private fun howToButton() {
  val uriHandler = LocalUriHandler.current
  Row(
    verticalAlignment = Alignment.CenterVertically,
    modifier = Modifier.clickable { uriHandler.openUriCatching("https://simplex.chat/docs/webrtc.html#configure-mobile-apps") }
  ) {
    Text(stringResource(MR.strings.how_to), color = MaterialTheme.colors.primary)
    Icon(
      painterResource(MR.images.ic_open_in_new), stringResource(MR.strings.how_to), tint = MaterialTheme.colors.primary,
      modifier = Modifier.padding(horizontal = 5.dp)
    )
  }
}
