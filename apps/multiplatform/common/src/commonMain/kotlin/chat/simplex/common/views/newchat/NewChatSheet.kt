package chat.simplex.common.views.newchat

import SectionItemView
import TextIconSpaced
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.*
import androidx.compose.ui.graphics.painter.Painter
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.RemoteHostInfo
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.contacts.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR

@Composable
fun NewChatButton(icon: Painter, text: String, click: () -> Unit, textColor: Color = Color.Unspecified, iconColor: Color = MaterialTheme.colors.secondary, disabled: Boolean = false, extraPadding: Boolean = false) {
  SectionItemView(click, disabled = disabled) {
    Icon(icon, text, tint = if (disabled) MaterialTheme.colors.secondary else iconColor)
    TextIconSpaced(extraPadding)
    Text(text, color = if (disabled) MaterialTheme.colors.secondary else textColor)
  }
}

@Composable
fun NewChatOptions(addContact: () -> Unit, scanPaste: () -> Unit, createGroup: () -> Unit) {
  val actions = remember { listOf(addContact, scanPaste, createGroup) }

  Column {
    actions.forEachIndexed { index, _ ->
      NewChatButton(
        icon = painterResource(icons[index]),
        text = stringResource(titles[index]),
        click = actions[index],
        extraPadding = true)
    }
  }
}
@Composable
fun ModalData.NewChatSheet(rh: RemoteHostInfo?, close: () -> Unit) {
  Column(
    Modifier.fillMaxSize(),
  ) {
    Box(contentAlignment = Alignment.Center) {
      val bottomPadding = DEFAULT_PADDING
      AppBarTitle(
        stringResource(MR.strings.new_chat),
        hostDevice(rh?.remoteHostId),
        bottomPadding = bottomPadding
      )
    }

    val closeAll = { ModalManager.closeAllModalsEverywhere() }

    NewChatSheetLayout(
      addContact = {
        ModalManager.center.showModalCloseable { _ -> NewChatView(chatModel.currentRemoteHost.value, NewChatOption.INVITE, close = closeAll ) }
      },
      scanPaste = {
        ModalManager.center.showModalCloseable { _ -> NewChatView(chatModel.currentRemoteHost.value, NewChatOption.CONNECT, showQRCodeScanner = true, close = closeAll) }
      },
      createGroup = {
        ModalManager.center.showCustomModal { close -> AddGroupView(chatModel, chatModel.currentRemoteHost.value, close, closeAll) }
      },
      rh = rh,
      close = close
    )
  }
}

private val titles = listOf(
  MR.strings.add_contact_tab,
  MR.strings.scan_paste_link,
  MR.strings.create_group_button
)
private val icons = listOf(MR.images.ic_add_link, MR.images.ic_qr_code, MR.images.ic_group)

@Composable
fun NewChatSheetLayout(
  rh: RemoteHostInfo?,
  addContact: () -> Unit,
  scanPaste: () -> Unit,
  createGroup: () -> Unit,
  close: () -> Unit,
) {
  ContactsView(
    contactActions = {
      NewChatOptions(
        addContact = addContact,
        scanPaste = scanPaste,
        createGroup = createGroup
      )
    },
    rh = rh,
    close = close
  )
}

@Composable
fun ActionButton(
  text: String?,
  comment: String?,
  icon: Painter,
  disabled: Boolean = false,
  click: () -> Unit = {}
) {
  Surface(shape = RoundedCornerShape(18.dp), color = Color.Transparent, contentColor = LocalContentColor.current) {
    Column(
      Modifier
        .clickable(onClick = click)
        .padding(8.dp),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      val tint = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
      Icon(
        icon, text,
        tint = tint,
        modifier = Modifier
          .size(40.dp)
          .padding(bottom = 8.dp)
      )
      if (text != null) {
        Text(
          text,
          textAlign = TextAlign.Center,
          fontWeight = FontWeight.Bold,
          color = tint,
          modifier = Modifier.padding(bottom = 4.dp)
        )
      }
      if (comment != null) {
        Text(
          comment,
          textAlign = TextAlign.Center,
          style = MaterialTheme.typography.body2
        )
      }
    }
  }
}

@Composable
fun ActionButton(
  modifier: Modifier,
  text: String?,
  comment: String?,
  icon: Painter,
  tint: Color = MaterialTheme.colors.primary,
  disabled: Boolean = false,
  click: () -> Unit = {}
) {
  Surface(modifier, shape = RoundedCornerShape(18.dp), contentColor = LocalContentColor.current) {
    Column(
      Modifier
        .fillMaxWidth()
        .clickable(onClick = click)
        .padding(8.dp),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      val tint = if (disabled) MaterialTheme.colors.secondary else tint
      Icon(
        icon, text,
        tint = tint,
        modifier = Modifier
          .size(40.dp)
          .padding(bottom = 8.dp)
      )
      if (text != null) {
        Text(
          text,
          textAlign = TextAlign.Center,
          fontWeight = FontWeight.Bold,
          color = tint,
          modifier = Modifier.padding(bottom = 4.dp)
        )
      }
      if (comment != null) {
        Text(
          comment,
          textAlign = TextAlign.Center,
          style = MaterialTheme.typography.body2
        )
      }
    }
  }
}

@Preview
@Composable
private fun PreviewNewChatSheet() {
  SimpleXTheme {
    NewChatSheetLayout(rh = null, scanPaste = {}, addContact = {}, createGroup = {}, close = {})
  }
}
