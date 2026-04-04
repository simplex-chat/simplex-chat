package chat.simplex.common.views.invitation_redesign

import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.Image
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.ui.theme.*
import chat.simplex.res.MR

@Composable
fun ConnectBannerCard(onInviteClick: () -> Unit, onScanPasteClick: () -> Unit) {
  Column {
    IconButton(
      onClick = { appPrefs.connectBannerCardShown.set(true) },
      modifier = Modifier.align(Alignment.End)
    ) {
      Icon(
        painterResource(MR.images.ic_close),
        stringResource(MR.strings.back),
        tint = MaterialTheme.colors.secondary
      )
    }
    Surface(
      shape = RoundedCornerShape(18.dp),
      color = MaterialTheme.appColors.sentMessage,
      modifier = Modifier.fillMaxWidth()
    ) {
      Column {
        Row(
          Modifier.fillMaxWidth(),
          horizontalArrangement = Arrangement.SpaceEvenly
        ) {
          Box(
            modifier = Modifier
              .weight(1f)
              .aspectRatio(1.6f)
              .clickable { onInviteClick() },
          ) {
            Image(
              painterResource(MR.images.ic_invitation_card_invite_someone),
              contentDescription = stringResource(MR.strings.create_link_or_qr),
              modifier = Modifier.fillMaxSize(),
              contentScale = ContentScale.Crop
            )
          }
          Box(
            modifier = Modifier
              .weight(1f)
              .aspectRatio(1.6f)
              .clickable { onScanPasteClick() },
          ) {
            Image(
              painterResource(MR.images.ic_invitation_card_one_time_link),
              contentDescription = stringResource(MR.strings.paste_link_scan),
              modifier = Modifier.fillMaxSize(),
              contentScale = ContentScale.Crop
            )
          }
        }
        Divider(color = MaterialTheme.colors.onSurface.copy(alpha = 0.06f))
        Row(
          Modifier.fillMaxWidth().padding(vertical = DEFAULT_PADDING_HALF),
          horizontalArrangement = Arrangement.SpaceEvenly
        ) {
          Row(
            Modifier
              .weight(1f)
              .clickable { onInviteClick() },
            horizontalArrangement = Arrangement.Center,
            verticalAlignment = Alignment.CenterVertically
          ) {
            Icon(
              painterResource(MR.images.ic_repeat_one),
              contentDescription = null,
              modifier = Modifier.size(18.dp),
              tint = MaterialTheme.colors.secondary
            )
            Spacer(Modifier.width(DEFAULT_PADDING_HALF))
            Text(
              stringResource(MR.strings.create_link_or_qr),
              style = MaterialTheme.typography.body2.copy(fontWeight = FontWeight.Medium),
            )
          }
          Row(
            Modifier
              .weight(1f)
              .clickable { onScanPasteClick() },
            horizontalArrangement = Arrangement.Center,
            verticalAlignment = Alignment.CenterVertically
          ) {
            Icon(
              painterResource(MR.images.ic_qr_code),
              contentDescription = null,
              modifier = Modifier.size(18.dp),
              tint = MaterialTheme.colors.secondary
            )
            Spacer(Modifier.width(DEFAULT_PADDING_HALF))
            Text(
              stringResource(MR.strings.paste_link_scan),
              style = MaterialTheme.typography.body2.copy(fontWeight = FontWeight.Medium),
            )
          }
        }
      }
    }
  }
}

@Preview
@Composable
fun PreviewConnectBannerCard() {
  SimpleXTheme {
    ConnectBannerCard(onInviteClick = {}, onScanPasteClick = {})
  }
}
