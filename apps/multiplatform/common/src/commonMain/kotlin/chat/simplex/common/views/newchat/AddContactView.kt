package chat.simplex.common.views.newchat

import SectionBottomSpacer
import SectionTextFooter
import SectionView
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import dev.icerock.moko.resources.compose.painterResource
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalClipboardManager
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.shareText
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.res.MR

@Composable
fun AddContactView(
  chatModel: ChatModel,
  connReqInvitation: String,
  contactConnection: MutableState<PendingContactConnection?>
) {
  val clipboard = LocalClipboardManager.current
  AddContactLayout(
    chatModel = chatModel,
    incognitoPref = chatModel.controller.appPrefs.incognito,
    connReq = connReqInvitation,
    contactConnection = contactConnection,
    share = { clipboard.shareText(connReqInvitation) },
    learnMore = {
      ModalManager.center.showModal {
        Column(
          Modifier
            .fillMaxHeight()
            .padding(horizontal = DEFAULT_PADDING),
          verticalArrangement = Arrangement.SpaceBetween
        ) {
          AddContactLearnMore()
        }
      }
    }
  )
}

@Composable
fun AddContactLayout(
  chatModel: ChatModel,
  incognitoPref: SharedPreference<Boolean>,
  connReq: String,
  contactConnection: MutableState<PendingContactConnection?>,
  share: () -> Unit,
  learnMore: () -> Unit
) {
  val incognito = remember { mutableStateOf(incognitoPref.get()) }

  LaunchedEffect(incognito.value) {
    withApi {
      val contactConnVal = contactConnection.value
      if (contactConnVal != null) {
        chatModel.controller.apiSetConnectionIncognito(contactConnVal.pccConnId, incognito.value)?.let {
          contactConnection.value = it
          chatModel.updateContactConnection(it)
        }
      }
    }
  }

  Column(
    Modifier
      .verticalScroll(rememberScrollState()),
    verticalArrangement = Arrangement.SpaceBetween,
  ) {
    AppBarTitle(stringResource(MR.strings.add_contact))

    SectionView(stringResource(MR.strings.one_time_link_short).uppercase()) {
      if (connReq.isNotEmpty()) {
        QRCode(
          connReq, Modifier
            .padding(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF)
            .aspectRatio(1f)
        )
      } else {
        CircularProgressIndicator(
          Modifier
            .size(36.dp)
            .padding(4.dp)
            .align(Alignment.CenterHorizontally),
          color = MaterialTheme.colors.secondary,
          strokeWidth = 3.dp
        )
      }

      IncognitoToggle(incognitoPref, incognito) { ModalManager.start.showModal { IncognitoView() } }
      ShareLinkButton(share)
      OneTimeLinkLearnMoreButton(learnMore)
    }
    SectionTextFooter(sharedProfileInfo(chatModel, incognito.value))

    SectionBottomSpacer()
  }
}

@Composable
fun ShareLinkButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_share),
    stringResource(MR.strings.share_invitation_link),
    onClick,
    iconColor = MaterialTheme.colors.primary,
    textColor = MaterialTheme.colors.primary,
  )
}

@Composable
fun OneTimeLinkLearnMoreButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(MR.images.ic_info),
    stringResource(MR.strings.learn_more),
    onClick,
  )
}

@Composable
fun IncognitoToggle(
  incognitoPref: SharedPreference<Boolean>,
  incognito: MutableState<Boolean>,
  onClickInfo: () -> Unit
) {
  SettingsActionItemWithContent(
    icon = if (incognito.value) painterResource(MR.images.ic_theater_comedy_filled) else painterResource(MR.images.ic_theater_comedy),
    text = null,
    click = onClickInfo,
    iconColor = if (incognito.value) Indigo else MaterialTheme.colors.secondary,
    extraPadding = false
  ) {
    SharedPreferenceToggleWithIcon(
      stringResource(MR.strings.incognito),
      painterResource(MR.images.ic_info),
      stopped = false,
      onClickInfo = onClickInfo,
      preference = incognitoPref,
      preferenceState = incognito
    )
  }
}

fun sharedProfileInfo(
  chatModel: ChatModel,
  incognito: Boolean
): String {
  val name = chatModel.currentUser.value?.displayName ?: ""
  return if (incognito) {
    generalGetString(MR.strings.connect__a_new_random_profile_will_be_shared)
  } else {
    String.format(generalGetString(MR.strings.connect__your_profile_will_be_shared), name)
  }
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
fun PreviewAddContactView() {
  SimpleXTheme {
    AddContactLayout(
      chatModel = ChatModel,
      incognitoPref = SharedPreference({ false }, {}),
      connReq = "https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D",
      contactConnection = mutableStateOf(PendingContactConnection.getSampleData()),
      share = {},
      learnMore = {},
    )
  }
}
