package chat.simplex.app.views.newchat

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.DEFAULT_PADDING
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.views.helpers.ModalManager
import chat.simplex.app.views.helpers.withApi
import chat.simplex.app.views.usersettings.UserAddressView

enum class CreateLinkTab {
  ONE_TIME, LONG_TERM
}

@Composable
fun CreateLinkView(m: ChatModel, initialSelection: CreateLinkTab) {
  val selection = remember { mutableStateOf(initialSelection) }
  val connReqInvitation = rememberSaveable { m.connReqInv }
  val creatingConnReq = rememberSaveable { mutableStateOf(false) }
  LaunchedEffect(selection.value) {
    if (selection.value == CreateLinkTab.ONE_TIME && connReqInvitation.value.isNullOrEmpty() && !creatingConnReq.value) {
      createInvitation(m, creatingConnReq, connReqInvitation)
    }
  }
  /** When [AddContactView] is open, we don't need to drop [chatModel.connReqInv].
   * Otherwise, it will be called here AFTER [AddContactView] is launched and will clear the value too soon.
   * It will be dropped automatically when connection established or when user goes away from this screen.
   **/
  DisposableEffect(Unit) {
    onDispose {
      if (!ModalManager.shared.hasModalsOpen()) {
        m.connReqInv.value = null
      }
    }
  }
  val tabTitles = CreateLinkTab.values().map {
    when {
      it == CreateLinkTab.ONE_TIME && connReqInvitation.value.isNullOrEmpty() -> stringResource(R.string.create_one_time_link)
      it == CreateLinkTab.ONE_TIME -> stringResource(R.string.one_time_link)
      it == CreateLinkTab.LONG_TERM -> stringResource(R.string.your_contact_address)
      else -> ""
    }
  }
  Column(
    Modifier
      .fillMaxHeight()
      .padding(horizontal = DEFAULT_PADDING),
    verticalArrangement = Arrangement.SpaceBetween
  ) {
    Column(Modifier.weight(1f)) {
      when (selection.value) {
        CreateLinkTab.ONE_TIME -> {
          AddContactView(connReqInvitation.value ?: "", m.incognito.value)
        }
        CreateLinkTab.LONG_TERM -> {
          UserAddressView(m)
        }
      }
    }
    TabRow(
      selectedTabIndex = selection.value.ordinal,
      backgroundColor = MaterialTheme.colors.background,
      contentColor = MaterialTheme.colors.primary,
    ) {
      tabTitles.forEachIndexed { index, it ->
        Tab(
          selected = selection.value.ordinal == index,
          onClick = {
            selection.value = CreateLinkTab.values()[index]
          },
          text = { Text(it, fontSize = 13.sp) },
          icon = {
            Icon(
              if (CreateLinkTab.ONE_TIME.ordinal == index) Icons.Outlined.RepeatOne else Icons.Outlined.AllInclusive,
              it
            )
          },
          selectedContentColor = MaterialTheme.colors.primary,
          unselectedContentColor = HighOrLowlight,
        )
      }
    }
  }
}

private fun createInvitation(m: ChatModel, creatingConnReq: MutableState<Boolean>, connReqInvitation: MutableState<String?>) {
  creatingConnReq.value = true
  withApi {
    val connReq = m.controller.apiAddContact()
    if (connReq != null) {
      connReqInvitation.value = connReq
    } else {
      creatingConnReq.value = false
    }
  }
}
