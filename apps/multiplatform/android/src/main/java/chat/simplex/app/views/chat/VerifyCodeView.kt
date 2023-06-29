package chat.simplex.app.views.chat

import SectionBottomSpacer
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.newchat.QRCode

@Composable
fun VerifyCodeView(
  displayName: String,
  connectionCode: String?,
  connectionVerified: Boolean,
  verify: suspend (String?) -> Pair<Boolean, String>?,
  close: () -> Unit,
) {
  if (connectionCode != null) {
    VerifyCodeLayout(
      displayName,
      connectionCode,
      connectionVerified,
      verifyCode = { newCode, cb ->
        withBGApi {
          val res = verify(newCode)
          if (res != null) {
            val (verified) = res
            cb(verified)
            if (verified) close()
          }
        }
      }
    )
  }
}

@Composable
private fun VerifyCodeLayout(
  displayName: String,
  connectionCode: String,
  connectionVerified: Boolean,
  verifyCode: (String?, cb: (Boolean) -> Unit) -> Unit,
) {
  Column(
    Modifier
      .fillMaxSize()
      .verticalScroll(rememberScrollState())
      .padding(horizontal = DEFAULT_PADDING)
  ) {
    AppBarTitle(stringResource(R.string.security_code), false)
    val splitCode = splitToParts(connectionCode, 24)
    Row(Modifier.fillMaxWidth().padding(bottom = DEFAULT_PADDING_HALF), horizontalArrangement = Arrangement.Center) {
      if (connectionVerified) {
        Icon(painterResource(R.drawable.ic_verified_user), null, Modifier.padding(end = 4.dp).size(22.dp), tint = MaterialTheme.colors.secondary)
        Text(String.format(stringResource(R.string.is_verified), displayName))
      } else {
        Text(String.format(stringResource(R.string.is_not_verified), displayName))
      }
    }

    SectionView {
      QRCode(connectionCode, Modifier.aspectRatio(1f))
    }

    Row(Modifier.fillMaxWidth(), verticalAlignment = Alignment.CenterVertically) {
      Spacer(Modifier.weight(2f))
      SelectionContainer(Modifier.padding(vertical = DEFAULT_PADDING_HALF, horizontal = DEFAULT_PADDING_HALF)) {
        Text(
          splitCode,
          fontFamily = FontFamily.Monospace,
          fontSize = 18.sp,
          maxLines = 20
        )
      }
      val context = LocalContext.current
      Box(Modifier.weight(1f)) {
        IconButton({ shareText(context, connectionCode) }, Modifier.size(20.dp).align(Alignment.CenterStart)) {
          Icon(painterResource(R.drawable.ic_share_filled), null, tint = MaterialTheme.colors.primary)
        }
      }
      Spacer(Modifier.weight(1f))
    }

    Text(
      generalGetString(R.string.to_verify_compare),
      Modifier.padding(bottom = DEFAULT_PADDING)
    )

    Row(
      Modifier.padding(bottom = DEFAULT_PADDING).align(Alignment.CenterHorizontally),
      horizontalArrangement = Arrangement.spacedBy(10.dp)
    ) {
      if (connectionVerified) {
        SimpleButton(generalGetString(R.string.clear_verification), painterResource(R.drawable.ic_shield)) {
          verifyCode(null) {}
        }
      } else {
        SimpleButton(generalGetString(R.string.scan_code), painterResource(R.drawable.ic_qr_code)) {
          ModalManager.shared.showModal {
            ScanCodeView(verifyCode) { }
          }
        }
        SimpleButton(generalGetString(R.string.mark_code_verified), painterResource(R.drawable.ic_verified_user)) {
          verifyCode(connectionCode) { verified ->
            if (!verified) {
              AlertManager.shared.showAlertMsg(
                title = generalGetString(R.string.incorrect_code)
              )
            }
          }
        }
      }
    }
    SectionBottomSpacer()
  }
}

private fun splitToParts(s: String, length: Int): String {
  if (length >= s.length) return s
  return (0..(s.length - 1) / length)
    .map { s.drop(it * length).take(length) }
    .joinToString(separator = "\n")
}
