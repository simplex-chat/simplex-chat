package chat.simplex.common.views.newchat

import SectionBottomSpacer
import SectionSpacer
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
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import chat.simplex.common.platform.shareText
import com.icerockdev.library.MR
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.SettingsActionItem

@Composable
fun AddContactView(connReqInvitation: String, connIncognito: Boolean) {
  val clipboard = LocalClipboardManager.current
  AddContactLayout(
    connReq = connReqInvitation,
    connIncognito = connIncognito,
    share = { clipboard.shareText(connReqInvitation) },
    learnMore = {
      ModalManager.shared.showModal {
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
fun AddContactLayout(connReq: String, connIncognito: Boolean, share: () -> Unit, learnMore: () -> Unit) {
  Column(
    Modifier
      .verticalScroll(rememberScrollState()),
    verticalArrangement = Arrangement.SpaceBetween,
  ) {
    AppBarTitle(stringResource(MR.strings.add_contact))
    OneTimeLinkProfileText(connIncognito)

    SectionSpacer()
    SectionView(stringResource(MR.strings.one_time_link_short).uppercase()) {
      OneTimeLinkSection(connReq, share, learnMore)
    }
    SectionBottomSpacer()
  }
}

@Composable
fun OneTimeLinkProfileText(connIncognito: Boolean) {
  Row(Modifier.padding(horizontal = DEFAULT_PADDING)) {
    InfoAboutIncognito(
      connIncognito,
      true,
      generalGetString(MR.strings.incognito_random_profile_description),
      generalGetString(MR.strings.your_profile_will_be_sent)
    )
  }
}

@Composable
fun ColumnScope.OneTimeLinkSection(connReq: String, share: () -> Unit, learnMore: () -> Unit) {
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
  ShareLinkButton(share)
  OneTimeLinkLearnMoreButton(learnMore)
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
fun InfoAboutIncognito(chatModelIncognito: Boolean, supportedIncognito: Boolean = true, onText: String, offText: String, centered: Boolean = false) {
  if (chatModelIncognito) {
    Row(
      Modifier
        .fillMaxWidth()
        .padding(vertical = 4.dp),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = if (centered) Arrangement.Center else Arrangement.Start
    ) {
      Icon(
        if (supportedIncognito) painterResource(MR.images.ic_theater_comedy_filled) else painterResource(MR.images.ic_info),
        stringResource(MR.strings.incognito),
        tint = if (supportedIncognito) Indigo else WarningOrange,
        modifier = Modifier.padding(end = 10.dp).size(20.dp)
      )
      Text(onText, textAlign = if (centered) TextAlign.Center else TextAlign.Left, style = MaterialTheme.typography.body2)
    }
  } else {
    Row(
      Modifier
        .fillMaxWidth()
        .padding(vertical = 4.dp),
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = if (centered) Arrangement.Center else Arrangement.Start
    ) {
      Icon(
        painterResource(MR.images.ic_info),
        stringResource(MR.strings.incognito),
        tint = MaterialTheme.colors.secondary,
        modifier = Modifier.padding(end = 10.dp).size(20.dp)
      )
      Text(offText, textAlign = if (centered) TextAlign.Center else TextAlign.Left, style = MaterialTheme.typography.body2)
    }
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
      connReq = "https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D",
      connIncognito = false,
      share = {},
      learnMore = {},
    )
  }
}
