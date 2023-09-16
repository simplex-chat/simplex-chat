package chat.simplex.common.views.newchat

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.PendingContactConnection
import chat.simplex.common.views.helpers.ModalManager
import chat.simplex.common.views.helpers.withApi
import chat.simplex.common.views.usersettings.UserAddressView
import chat.simplex.res.MR

enum class CreateLinkTab {
  ONE_TIME, LONG_TERM
}

@Composable
fun CreateLinkView(m: ChatModel, initialSelection: CreateLinkTab) {
  val selection = remember { mutableStateOf(initialSelection) }
  val connReqInvitation = rememberSaveable { m.connReqInv }
  val contactConnection: MutableState<PendingContactConnection?> = rememberSaveable { mutableStateOf(null) }
  val creatingConnReq = rememberSaveable { mutableStateOf(false) }
  LaunchedEffect(selection.value) {
    if (
      selection.value == CreateLinkTab.ONE_TIME
      && connReqInvitation.value.isNullOrEmpty()
      && contactConnection.value == null
      && !creatingConnReq.value
    ) {
      createInvitation(m, creatingConnReq, connReqInvitation, contactConnection)
    }
  }
  /** When [AddContactView] is open, we don't need to drop [chatModel.connReqInv].
   * Otherwise, it will be called here AFTER [AddContactView] is launched and will clear the value too soon.
   * It will be dropped automatically when connection established or when user goes away from this screen.
   **/
  DisposableEffect(Unit) {
    onDispose {
      if (!ModalManager.center.hasModalsOpen()) {
        m.connReqInv.value = null
      }
    }
  }
  val tabTitles = CreateLinkTab.values().map {
    when {
      it == CreateLinkTab.ONE_TIME && connReqInvitation.value.isNullOrEmpty() && contactConnection.value == null ->
        stringResource(MR.strings.create_one_time_link)
      it == CreateLinkTab.ONE_TIME ->
        stringResource(MR.strings.one_time_link)
      it == CreateLinkTab.LONG_TERM ->
        stringResource(MR.strings.your_simplex_contact_address)
      else -> ""
    }
  }
  Column(
    Modifier
      .fillMaxHeight(),
    verticalArrangement = Arrangement.SpaceBetween
  ) {
    Column(Modifier.weight(1f)) {
      when (selection.value) {
        CreateLinkTab.ONE_TIME -> {
          AddContactView(m, connReqInvitation.value ?: "", contactConnection)
        }
        CreateLinkTab.LONG_TERM -> {
          UserAddressView(m, viaCreateLinkView = true, close = {})
        }
      }
    }
    TabRow(
      selectedTabIndex = selection.value.ordinal,
      backgroundColor = Color.Transparent,
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
              if (CreateLinkTab.ONE_TIME.ordinal == index) painterResource(MR.images.ic_repeat_one) else painterResource(MR.images.ic_all_inclusive),
              it
            )
          },
          selectedContentColor = MaterialTheme.colors.primary,
          unselectedContentColor = MaterialTheme.colors.secondary,
        )
      }
    }
  }
}

private fun createInvitation(
  m: ChatModel,
  creatingConnReq: MutableState<Boolean>,
  connReqInvitation: MutableState<String?>,
  contactConnection: MutableState<PendingContactConnection?>
) {
  creatingConnReq.value = true
  withApi {
    val r = m.controller.apiAddContact(incognito = m.controller.appPrefs.incognito.get())
    if (r != null) {
      connReqInvitation.value = r.first
      contactConnection.value = r.second
    } else {
      creatingConnReq.value = false
    }
  }
}
