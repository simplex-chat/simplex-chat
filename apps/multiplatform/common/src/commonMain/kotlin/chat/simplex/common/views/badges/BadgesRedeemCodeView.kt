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
import dev.icerock.moko.resources.StringResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.OnboardingActionButton
import chat.simplex.common.views.onboarding.TextButtonBelowOnboardingButton
import chat.simplex.res.MR

private const val badgeCodePrefix = "SB"
private const val badgeCodeBodyLength = 20
private const val badgeCodeGroupLength = 5

// The code as core will accept it - prefix and 20 characters, no separators - or null if it does not
// parse. Validity is decided only here: a second check-character implementation would drift.
fun parseBadgeCode(s: String): String? {
  val canonical = chatParseBadgeCode(s)
  return if (canonical.isEmpty()) null else canonical
}

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
  val failure = remember { mutableStateOf<BadgeRedeemError?>(null) }

  fun applyCodeInput(s: String) {
    val formatted = formatBadgeCodeInput(s)
    if (formatted != code.value.text) code.value = TextFieldValue(formatted, selection = TextRange(formatted.length))
    canonicalCode.value = parseBadgeCode(formatted)
    failure.value = null
  }

  fun redeem() {
    val sending = canonicalCode.value ?: return
    val user = chatModel.currentUser.value ?: return
    submitting.value = true
    failure.value = null
    withBGApi {
      when (val result = chatModel.controller.apiRedeemBadgeCode(rhId, user.userId, sending)) {
        is BadgeRedeemResult.Redeemed -> {
          val badgeState = try { chatModel.controller.apiGetBadgeState(rhId, user.userId) } catch (e: Exception) { null }
          withContext(Dispatchers.Main) {
            submitting.value = false
            // written before the pop: BadgesView swaps its content under this pushed view, so
            // the pop reveals Your Badge already in place rather than animating it afterwards
            if (badgeState != null) {
              BadgeModel.set(rhId, user.userId, badgeState)
            }
            // the response is the only carrier: redeeming raises no event that refreshes the
            // profile, so without this the badge beside the name is the one from before
            chatModel.updateUser(result.user)
            if (badgeState != null && !badgeState.shown) {
              // a replay adds no purchase; a fresh code's badge can be retired on arrival
              failure.value = if (result.newBadge) BadgeRedeemError.BadgeEnded else BadgeRedeemError.CodeUsed
            } else {
              supporterBannerShown.set(true)
              ModalManager.start.closeModal()
            }
          }
        }
        is BadgeRedeemResult.Failed -> {
          // the mapped case only - core embeds the service's response in some of these messages
          Log.e(TAG, "apiRedeemBadgeCode: ${result.error}")
          withContext(Dispatchers.Main) {
            submitting.value = false
            failure.value = result.error
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

    val f = failure.value
    if (f != null) {
      Text(
        stringResource(failureMessage(f)),
        style = MaterialTheme.typography.body2,
        color = Color.Red,
        textAlign = TextAlign.Center,
        modifier = Modifier.fillMaxWidth()
      )
    }

    Spacer(Modifier.weight(1f))

    Column(horizontalAlignment = Alignment.CenterHorizontally) {
      SubmitButton(enabled = canonicalCode.value != null && !submitting.value, onClick = ::redeem)
      TextButtonBelowOnboardingButton("", null)
    }
  }
}

@Composable
private fun CodeField(code: MutableState<TextFieldValue>, submitting: Boolean, applyCodeInput: (String) -> Unit) {
  val colors = TextFieldDefaults.textFieldColors(
    backgroundColor = MaterialTheme.appColors.sentMessage,
    textColor = MaterialTheme.colors.onBackground,
    focusedIndicatorColor = Color.Unspecified,
    unfocusedIndicatorColor = Color.Unspecified,
  )
  BasicTextField(
    value = code.value,
    onValueChange = { applyCodeInput(it.text) },
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
private fun PasteButton(submitting: Boolean, applyCodeInput: (String) -> Unit) {
  val clipboard = LocalClipboardManager.current
  TextButton(
    onClick = { clipboard.getText()?.text?.let { applyCodeInput(it) } },
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

private fun failureMessage(failure: BadgeRedeemError): StringResource = when (failure) {
  BadgeRedeemError.InvalidCode -> MR.strings.badges_error_invalid_code
  BadgeRedeemError.ServiceNotConfigured -> MR.strings.badges_error_service_not_configured
  BadgeRedeemError.AlreadyActive -> MR.strings.badges_error_already_active
  BadgeRedeemError.CodeInvalid -> MR.strings.badges_error_code_invalid
  BadgeRedeemError.CodeUsed -> MR.strings.badges_error_code_used
  BadgeRedeemError.CodeExpired -> MR.strings.badges_error_code_expired
  BadgeRedeemError.RateLimited -> MR.strings.badges_error_rate_limited
  BadgeRedeemError.ServiceFailed -> MR.strings.badges_error_service_failed
  BadgeRedeemError.BadServiceResponse -> MR.strings.badges_error_bad_service_response
  BadgeRedeemError.CredentialNotVerified -> MR.strings.badges_error_credential_not_verified
  BadgeRedeemError.UnsupportedVersion -> MR.strings.badges_error_unsupported_version
  BadgeRedeemError.NetworkError -> MR.strings.badges_error_network
  BadgeRedeemError.BadgeEnded -> MR.strings.badges_error_badge_ended
  BadgeRedeemError.Unknown -> MR.strings.badges_error_unknown
}
