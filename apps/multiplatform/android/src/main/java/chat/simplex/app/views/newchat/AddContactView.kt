package chat.simplex.app.views.newchat

import SectionBottomSpacer
import SectionSpacer
import SectionView
import android.content.res.Configuration
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.ui.res.painterResource
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.usersettings.SettingsActionItem

@Composable
fun AddContactView(connReqInvitation: String, connIncognito: Boolean) {
  val cxt = LocalContext.current
  AddContactLayout(
    connReq = connReqInvitation,
    connIncognito = connIncognito,
    share = { shareText(cxt, connReqInvitation) },
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
    AppBarTitle(stringResource(R.string.add_contact))
    OneTimeLinkProfileText(connIncognito)

    SectionSpacer()
    SectionView(stringResource(R.string.one_time_link_short).uppercase()) {
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
      generalGetString(R.string.incognito_random_profile_description),
      generalGetString(R.string.your_profile_will_be_sent)
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
    painterResource(R.drawable.ic_share),
    stringResource(R.string.share_invitation_link),
    onClick,
    iconColor = MaterialTheme.colors.primary,
    textColor = MaterialTheme.colors.primary,
  )
}

@Composable
fun OneTimeLinkLearnMoreButton(onClick: () -> Unit) {
  SettingsActionItem(
    painterResource(R.drawable.ic_info),
    stringResource(R.string.learn_more),
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
        if (supportedIncognito) painterResource(R.drawable.ic_theater_comedy_filled) else painterResource(R.drawable.ic_info),
        stringResource(R.string.incognito),
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
        painterResource(R.drawable.ic_info),
        stringResource(R.string.incognito),
        tint = MaterialTheme.colors.secondary,
        modifier = Modifier.padding(end = 10.dp).size(20.dp)
      )
      Text(offText, textAlign = if (centered) TextAlign.Center else TextAlign.Left, style = MaterialTheme.typography.body2)
    }
  }
}

@Preview
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
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
