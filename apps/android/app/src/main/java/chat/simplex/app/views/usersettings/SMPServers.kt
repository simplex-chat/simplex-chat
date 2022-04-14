package chat.simplex.app.views.usersettings

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.OpenInNew
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.SolidColor
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.input.KeyboardCapitalization
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.*

@Composable
fun SMPServersView(chatModel: ChatModel) {
  val userSMPServers = chatModel.userSMPServers.value
  if (userSMPServers != null) {
    var isUserSMPServers by remember { mutableStateOf(userSMPServers.isNotEmpty()) }
    var editSMPServers by remember { mutableStateOf(!isUserSMPServers) }
    var userSMPServersStr by remember { mutableStateOf(if (isUserSMPServers) userSMPServers.joinToString(separator = "\n") else "") }
    fun saveSMPServers(smpServers: List<String>) {
      withApi {
        val r = chatModel.controller.setUserSMPServers(smpServers = smpServers)
        if (r) {
          chatModel.userSMPServers.value = smpServers
          if (smpServers.isEmpty()) {
            isUserSMPServers = false
            editSMPServers = true
          } else {
            editSMPServers = false
          }
        }
      }
    }

    SMPServersLayout(
      isUserSMPServers = isUserSMPServers,
      editSMPServers = editSMPServers,
      userSMPServersStr = userSMPServersStr,
      isUserSMPServersOnOff = { switch ->
        if (switch) {
          isUserSMPServers = true
        } else {
          val userSMPServers = chatModel.userSMPServers.value
          if (userSMPServers != null) {
            if (userSMPServers.isNotEmpty()) {
              AlertManager.shared.showAlertMsg(
                title = generalGetString(R.string.use_simplex_chat_servers__question),
                text = generalGetString(R.string.saved_SMP_servers_will_br_removed),
                confirmText = generalGetString(R.string.confirm_verb),
                onConfirm = {
                  saveSMPServers(listOf())
                  isUserSMPServers = false
                  userSMPServersStr = ""
                }
              )
            } else {
              isUserSMPServers = false
              userSMPServersStr = ""
            }
          }
        }
      },
      editUserSMPServersStr = { userSMPServersStr = it },
      cancelEdit = {
        val userSMPServers = chatModel.userSMPServers.value
        if (userSMPServers != null) {
          isUserSMPServers = userSMPServers.isNotEmpty()
          editSMPServers = !isUserSMPServers
          userSMPServersStr = if (isUserSMPServers) userSMPServers.joinToString(separator = "\n") else ""
        }
      },
      saveSMPServers = { saveSMPServers(it) },
      editOn = { editSMPServers = true },
    )
  }
}

@Composable
fun SMPServersLayout(
  isUserSMPServers: Boolean,
  editSMPServers: Boolean,
  userSMPServersStr: String,
  isUserSMPServersOnOff: (Boolean) -> Unit,
  editUserSMPServersStr: (String) -> Unit,
  cancelEdit: () -> Unit,
  saveSMPServers: (List<String>) -> Unit,
  editOn: () -> Unit,
) {
  Column(
    Modifier.fillMaxWidth(),
    horizontalAlignment = Alignment.Start,
    verticalArrangement = Arrangement.spacedBy(8.dp)
  ) {
    Text(
      generalGetString(R.string.your_SMP_servers),
      Modifier.padding(bottom = 24.dp),
      style = MaterialTheme.typography.h1
    )
    Row(
      verticalAlignment = Alignment.CenterVertically
    ) {
      Text(generalGetString(R.string.configure_SMP_servers), Modifier.padding(end = 24.dp))
      Switch(
        checked = isUserSMPServers,
        onCheckedChange = isUserSMPServersOnOff,
        colors = SwitchDefaults.colors(
          checkedThumbColor = MaterialTheme.colors.primary,
          uncheckedThumbColor = HighOrLowlight
        ),
      )
    }

    if (!isUserSMPServers) {
      Text(generalGetString(R.string.using_simplex_chat_servers))
    } else {
      Text(generalGetString(R.string.enter_one_SMP_server_per_line))
      if (editSMPServers) {
        BasicTextField(
          value = userSMPServersStr,
          onValueChange = editUserSMPServersStr,
          textStyle = TextStyle(
            fontFamily = FontFamily.Monospace, fontSize = 14.sp,
            color = MaterialTheme.colors.onBackground
          ),
          keyboardOptions = KeyboardOptions.Default.copy(
            capitalization = KeyboardCapitalization.None,
            autoCorrect = false
          ),
          modifier = Modifier.height(160.dp),
          cursorBrush = SolidColor(HighOrLowlight),
          decorationBox = { innerTextField ->
            Surface(
              shape = RoundedCornerShape(10.dp),
              border = BorderStroke(1.dp, MaterialTheme.colors.secondary)
            ) {
              Row(
                Modifier.background(MaterialTheme.colors.background),
                verticalAlignment = Alignment.Top
              ) {
                Box(
                  Modifier
                    .weight(1f)
                    .padding(vertical = 5.dp, horizontal = 7.dp)
                ) {
                  innerTextField()
                }
              }
            }
          }
        )

        Row(
          Modifier.fillMaxWidth(),
          horizontalArrangement = Arrangement.SpaceBetween,
          verticalAlignment = Alignment.CenterVertically
        ) {
          Column(horizontalAlignment = Alignment.Start) {
            Row {
              Text(
                generalGetString(R.string.cancel_verb),
                color = MaterialTheme.colors.primary,
                modifier = Modifier
                  .clickable(onClick = cancelEdit)
              )
              Spacer(Modifier.padding(horizontal = 8.dp))
              Text(
                generalGetString(R.string.save_servers_button),
                color = MaterialTheme.colors.primary,
                modifier = Modifier.clickable(onClick = {
                  val servers = userSMPServersStr.split("\n")
                  saveSMPServers(servers)
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
          border = BorderStroke(1.dp, MaterialTheme.colors.secondary)
        ) {
          SelectionContainer(
            Modifier.verticalScroll(rememberScrollState())
          ) {
            Text(
              userSMPServersStr,
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
          Column(horizontalAlignment = Alignment.Start) {
            Text(
              generalGetString(R.string.edit_verb),
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
  }
}

@Composable
fun howToButton() {
  val uriHandler = LocalUriHandler.current
  Row(
    verticalAlignment = Alignment.CenterVertically,
    modifier = Modifier.clickable { uriHandler.openUri("https://github.com/simplex-chat/simplexmq#using-smp-server-and-smp-agent") }
  ) {
    Text(generalGetString(R.string.how_to), color = MaterialTheme.colors.primary)
    Icon(
      Icons.Outlined.OpenInNew, generalGetString(R.string.how_to), tint = MaterialTheme.colors.primary,
      modifier = Modifier.padding(horizontal = 5.dp)
    )
  }
}

@Preview(showBackground = true)
@Composable
fun PreviewSMPServersLayoutDefaultServers() {
  SimpleXTheme {
    SMPServersLayout(
      isUserSMPServers = false,
      editSMPServers = true,
      userSMPServersStr = "",
      isUserSMPServersOnOff = {},
      editUserSMPServersStr = {},
      cancelEdit = {},
      saveSMPServers = {},
      editOn = {},
    )
  }
}

@Preview(showBackground = true)
@Composable
fun PreviewSMPServersLayoutUserServersEditOn() {
  SimpleXTheme {
    SMPServersLayout(
      isUserSMPServers = true,
      editSMPServers = true,
      userSMPServersStr = "smp://u2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU=@smp4.simplex.im\nsmp://hpq7_4gGJiilmz5Rf-CswuU5kZGkm_zOIooSw6yALRg=@smp5.simplex.im",
      isUserSMPServersOnOff = {},
      editUserSMPServersStr = {},
      cancelEdit = {},
      saveSMPServers = {},
      editOn = {},
    )
  }
}

@Preview(showBackground = true)
@Composable
fun PreviewSMPServersLayoutUserServersEditOff() {
  SimpleXTheme {
    SMPServersLayout(
      isUserSMPServers = true,
      editSMPServers = false,
      userSMPServersStr = "smp://u2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU=@smp4.simplex.im\nsmp://hpq7_4gGJiilmz5Rf-CswuU5kZGkm_zOIooSw6yALRg=@smp5.simplex.im",
      isUserSMPServersOnOff = {},
      editUserSMPServersStr = {},
      cancelEdit = {},
      saveSMPServers = {},
      editOn = {},
    )
  }
}
