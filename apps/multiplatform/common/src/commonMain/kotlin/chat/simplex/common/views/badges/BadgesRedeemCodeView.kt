package chat.simplex.common.views.badges

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.SolidColor
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.text.TextRange
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.KeyboardCapitalization
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.QRCodeScanner
import chat.simplex.common.views.onboarding.OnboardingActionButton
import chat.simplex.common.views.onboarding.TextButtonBelowOnboardingButton
import chat.simplex.res.MR

private const val badgeCodePrefix = "SB"
private const val badgeCodeBodyLength = 20
private const val badgeCodeGroupLength = 5

// Regroups what was typed; validity and the folding of ambiguous characters are core's alone.
private fun formatBadgeCodeInput(s: String): String {
  val normalized = StringBuilder()
  for (c in s.take(256).uppercase()) {
    if (!c.isLetterOrDigit()) continue
    normalized.append(c)
    if (normalized.length == badgeCodePrefix.length + badgeCodeBodyLength) break
  }
  if (!normalized.startsWith(badgeCodePrefix)) return normalized.toString()
  val groups = mutableListOf(badgeCodePrefix)
  var i = badgeCodePrefix.length
  while (i < normalized.length) {
    val j = minOf(i + badgeCodeGroupLength, normalized.length)
    groups.add(normalized.substring(i, j))
    i = j
  }
  return groups.joinToString("-")
}

@Composable
fun BadgesRedeemCodeView() {
  val rhId = remember { chatModel.remoteHostId() }
  val supporterBannerShown = remember { appPrefs.supporterBannerShown }
  val code = remember { mutableStateOf(TextFieldValue("")) }
  val canonicalCode = remember { mutableStateOf<String?>(null) }
  val submitting = remember { mutableStateOf(false) }

  // when the text is unchanged, the field's own value is kept: it carries the cursor position and the
  // keyboard's composition state, which BasicTextField loses unless they are passed back to it
  fun applyCodeInput(v: TextFieldValue) {
    val formatted = formatBadgeCodeInput(v.text)
    code.value = if (formatted != v.text) TextFieldValue(formatted, selection = TextRange(formatted.length)) else v
    canonicalCode.value = parseBadgeCode(formatted)
  }

  fun redeem() {
    val sending = canonicalCode.value ?: return
    val user = chatModel.currentUser.value ?: return
    submitting.value = true
    withBGApi {
      when (val result = chatModel.controller.apiRedeemBadgeCode(rhId, user.userId, sending)) {
        null -> withContext(Dispatchers.Main) { submitting.value = false }
        is BadgeRedeemResult.Redeemed -> {
          val badgeState = result.badgeState
          withContext(Dispatchers.Main) {
            submitting.value = false
            // written before the pop: BadgesView swaps its content under this pushed view, so
            // the pop reveals Your Badge already in place rather than animating it afterwards
            BadgeModel.set(rhId, user.userId, badgeState)
            // the response is the only carrier: redeeming raises no event that refreshes the
            // profile, so without this the badge beside the name is the one from before
            chatModel.updateUser(result.user)
            if (badgeState != null && !badgeState.shown) {
              // a replay adds no purchase; a fresh code's badge can be retired on arrival
              AlertManager.shared.showAlertMsg(
                title = generalGetString(MR.strings.badges_error_title),
                text = generalGetString(if (result.newBadge) MR.strings.badges_error_badge_ended else MR.strings.badges_error_code_used)
              )
            } else {
              supporterBannerShown.set(true)
              ModalManager.start.closeModal()
            }
          }
        }
        is BadgeRedeemResult.Failed -> {
          Log.e(TAG, "apiRedeemBadgeCode: ${result.err?.string}")
          withContext(Dispatchers.Main) {
            submitting.value = false
            AlertManager.shared.showAlertMsg(
              title = generalGetString(MR.strings.badges_error_title),
              text = chatModel.controller.redeemErrorText(result.err)
            )
          }
        }
      }
    }
  }

  ColumnWithScrollBar(
    Modifier.padding(horizontal = 25.dp).padding(top = 8.dp, bottom = 20.dp),
    verticalArrangement = Arrangement.spacedBy(16.dp),
    horizontalAlignment = Alignment.CenterHorizontally,
    maxIntrinsicSize = true,
  ) {
    Text(
      stringResource(MR.strings.badges_redeem_code_title),
      style = MaterialTheme.typography.h1,
      fontWeight = FontWeight.Bold,
      color = MaterialTheme.colors.primary,
      textAlign = TextAlign.Center,
      modifier = Modifier.fillMaxWidth()
    )

    Text(
      stringResource(MR.strings.badges_redeem_code_body),
      style = MaterialTheme.typography.body1,
      textAlign = TextAlign.Center,
      modifier = Modifier.fillMaxWidth()
    )

    CodeField(code, submitting.value, ::applyCodeInput)

    PasteButton(submitting.value, ::applyCodeInput)

    if (appPlatform.isAndroid) {
      QRCodeScanner(padding = PaddingValues(start = 16.dp, top = 12.dp, end = 16.dp)) { text ->
        val formatted = formatBadgeCodeInput(text)
        when {
          submitting.value -> false
          parseBadgeCode(formatted) == null -> {
            AlertManager.shared.showAlertMsg(
              title = generalGetString(MR.strings.invalid_qr_code),
              text = generalGetString(MR.strings.badges_code_you_scanned_is_not_badge_code)
            )
            false
          }
          else -> {
            applyCodeInput(TextFieldValue(formatted, selection = TextRange(formatted.length)))
            redeem()
            true
          }
        }
      }
    }

    Spacer(Modifier.weight(1f))

    Column(horizontalAlignment = Alignment.CenterHorizontally) {
      SubmitButton(enabled = canonicalCode.value != null && !submitting.value, onClick = ::redeem)
      TextButtonBelowOnboardingButton("", null)
    }
  }
}

@Composable
private fun CodeField(code: MutableState<TextFieldValue>, submitting: Boolean, applyCodeInput: (TextFieldValue) -> Unit) {
  val colors = TextFieldDefaults.textFieldColors(
    backgroundColor = MaterialTheme.appColors.sentMessage,
    textColor = MaterialTheme.colors.onBackground,
    focusedIndicatorColor = Color.Unspecified,
    unfocusedIndicatorColor = Color.Unspecified,
  )
  BasicTextField(
    value = code.value,
    onValueChange = applyCodeInput,
    enabled = !submitting,
    singleLine = true,
    textStyle = TextStyle.Default.copy(
      color = MaterialTheme.colors.onBackground,
      fontFamily = FontFamily.Monospace,
      fontSize = MaterialTheme.typography.body1.fontSize,
      textAlign = TextAlign.Center
    ),
    keyboardOptions = KeyboardOptions(capitalization = KeyboardCapitalization.Characters, autoCorrect = false),
    cursorBrush = SolidColor(colors.cursorColor(false).value),
    modifier = Modifier
      .fillMaxWidth()
      .clip(RoundedCornerShape(10.dp))
      .background(colors.backgroundColor(!submitting).value)
      .padding(start = 12.dp, top = 14.dp, end = 12.dp, bottom = 14.dp),
    decorationBox = { innerTextField ->
      Box(contentAlignment = Alignment.Center) {
        if (code.value.text.isEmpty()) {
          Text(stringResource(MR.strings.badges_redeem_code_placeholder), color = MaterialTheme.colors.secondary, fontFamily = FontFamily.Monospace)
        }
        innerTextField()
      }
    }
  )
}

@Composable
private fun PasteButton(submitting: Boolean, applyCodeInput: (TextFieldValue) -> Unit) {
  val clipboard = LocalClipboardManager.current
  TextButton(
    onClick = { clipboard.getText()?.text?.let { applyCodeInput(TextFieldValue(it, selection = TextRange(it.length))) } },
    enabled = !submitting
  ) {
    Text(stringResource(MR.strings.paste_button), color = MaterialTheme.colors.primary, fontWeight = FontWeight.Medium)
  }
}

@Composable
private fun SubmitButton(enabled: Boolean, onClick: () -> Unit) {
  OnboardingActionButton(
    modifier = if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING).fillMaxWidth() else Modifier.widthIn(min = 300.dp),
    labelId = MR.strings.badges_redeem,
    onboarding = null,
    enabled = enabled,
    onclick = onClick
  )
}
