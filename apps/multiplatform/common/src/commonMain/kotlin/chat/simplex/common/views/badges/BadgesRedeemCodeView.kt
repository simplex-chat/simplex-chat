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

sealed class BadgeRedeemOutcome {
  object Redeemed: BadgeRedeemOutcome()
  class Refused(val message: String): BadgeRedeemOutcome()
  object Cancelled: BadgeRedeemOutcome()
}

// sets the badge before returning, so a caller that dismisses on Redeemed lands on Your Badge
// instead of showing the switch from Support. Cancelled when the user cancels the retry alert.
suspend fun redeemBadgeCode(rhId: Long?, user: User, code: String): BadgeRedeemOutcome =
  when (val result = chatModel.controller.apiRedeemBadgeCode(rhId, user.userId, code)) {
    null -> BadgeRedeemOutcome.Cancelled
    is BadgeRedeemResult.Redeemed -> withContext(Dispatchers.Main) {
      val badgeState = result.badgeState
      BadgeModel.set(rhId, user.userId, badgeState)
      chatModel.updateUser(result.user)
      if (badgeState != null && !badgeState.shown) {
        // a replay adds no purchase; a fresh code's badge can be retired on arrival
        BadgeRedeemOutcome.Refused(
          generalGetString(if (result.newBadge) MR.strings.badges_error_badge_ended else MR.strings.badges_error_code_used)
        )
      } else {
        appPrefs.supporterBannerShown.set(true)
        BadgeRedeemOutcome.Redeemed
      }
    }
    is BadgeRedeemResult.Failed -> {
      Log.e(TAG, "apiRedeemBadgeCode: ${result.err?.string}")
      BadgeRedeemOutcome.Refused(chatModel.controller.redeemErrorText(result.err, purchase = false))
    }
  }

fun showCannotRedeemAlert(message: String) {
  AlertManager.shared.showAlertMsg(title = generalGetString(MR.strings.badges_error_title), text = message)
}

@Composable
fun BadgesRedeemCodeView(modalManager: ModalManager) {
  val rhId = remember { chatModel.remoteHostId() }
  val code = remember { mutableStateOf(TextFieldValue("")) }
  val canonicalCode = remember { mutableStateOf<String?>(null) }
  val submitting = remember { mutableStateOf(false) }
  CloseWhenSupportGivesWay(modalManager)

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
      val outcome = redeemBadgeCode(rhId, user, sending)
      withContext(Dispatchers.Main) {
        submitting.value = false
        when (outcome) {
          // a shown badge closes this screen itself, and a second close would take the screen beneath
          is BadgeRedeemOutcome.Redeemed -> if (currentShownBadge() == null) modalManager.closeModal()
          is BadgeRedeemOutcome.Refused -> showCannotRedeemAlert(outcome.message)
          is BadgeRedeemOutcome.Cancelled -> {}
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

  if (submitting.value) {
    Box(
      Modifier.fillMaxSize(),
      contentAlignment = Alignment.Center
    ) {
      Surface(Modifier.size(50.dp), color = MaterialTheme.colors.background.copy(0.9f), contentColor = LocalContentColor.current, shape = RoundedCornerShape(50)){}
      CircularProgressIndicator(
        Modifier
          .padding(horizontal = 2.dp)
          .size(30.dp),
        color = MaterialTheme.colors.secondary,
        strokeWidth = 3.dp
      )
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

// the open screen's own state, not a copy of its step, so the two cannot disagree
private var badgeLinkStep: MutableState<BadgeLinkStep>? = null

private fun isBadgeLinkOpen(): Boolean =
  ModalManager.end.hasModalOpen(ModalViewId.BADGE_LINK)

fun isBadgeLinkIssuing(): Boolean =
  isBadgeLinkOpen() && badgeLinkStep?.value == BadgeLinkStep.Issuing

fun openBadgeLink(rhId: Long?, codeText: String) {
  val code = parseBadgeCode(codeText)
    ?: return showCannotRedeemAlert(generalGetString(MR.strings.badges_error_invalid_code))
  // opens over the chat list, as iOS dismisses every sheet before a link
  ModalManager.closeAllModalsEverywhere()
  // held by the modal, not remembered: a modal is composed only while on top, and rotation recreates the activity,
  // either of which would reset remembered state to Confirming with a request in flight
  val step = mutableStateOf(BadgeLinkStep.Confirming)
  ModalManager.end.showCustomModal(id = ModalViewId.BADGE_LINK) { close ->
    BadgesRedeemLinkView(rhId, code, step, close)
  }
  badgeLinkStep = step
}

enum class BadgeLinkStep {
  Confirming,
  Issuing,
  Redeemed,
  ViewingBadge
}

// Any web page can send a badge link, and a profile holds one badge at a time,
// so this screen asks before redeeming, names the profile, and offers nothing but the redemption.
// It is a code redemption with the code hidden, so it inherits the redeem screen's behaviour: an
// interrupted request is not resumed, and the code stays on the page that issued the link.
@Composable
fun BadgesRedeemLinkView(rhId: Long?, code: String, step: MutableState<BadgeLinkStep>, close: () -> Unit) {
  fun isOpen(): Boolean =
    badgeLinkStep === step && isBadgeLinkOpen()

  fun isShowing(): Boolean =
    isOpen() && ModalManager.end.isLastModalOpen(ModalViewId.BADGE_LINK)

  // An outcome can arrive after this screen was closed or covered: it must not close another screen,
  // and a covered one goes back to asking, so it is never left locked on the spinner.
  fun closeIfShowing() {
    if (isShowing()) {
      close()
    } else if (isOpen()) {
      step.value = BadgeLinkStep.Confirming
    }
  }

  fun redeemFromLink() {
    // a second tap before the screen changes must not send the code again
    if (step.value != BadgeLinkStep.Confirming) return
    val user = chatModel.currentUser.value ?: return closeIfShowing()
    step.value = BadgeLinkStep.Issuing
    withBGApi {
      val outcome = redeemBadgeCode(rhId, user, code)
      withContext(Dispatchers.Main) {
        when (outcome) {
          is BadgeRedeemOutcome.Redeemed -> {
            if (!isShowing()) {
              AlertManager.shared.showAlertMsg(
                title = generalGetString(MR.strings.badges_link_added_title),
                text = String.format(generalGetString(MR.strings.badges_link_added_profile), user.displayName)
              )
            }
            // a covered screen is not left on the spinner; a closed one is not written to
            if (isOpen()) step.value = BadgeLinkStep.Redeemed
          }
          is BadgeRedeemOutcome.Refused -> {
            closeIfShowing()
            showCannotRedeemAlert(outcome.message)
          }
          is BadgeRedeemOutcome.Cancelled -> closeIfShowing()
        }
      }
    }
  }

  // read for the profile the screen names, when it renders, so the two cannot disagree;
  // core refuses a code while the profile shows a badge
  val profileHasBadge = BadgeModel.isCurrent(rhId, chatModel.currentUser.value?.userId) && BadgeModel.badgeState.value?.shown == true

  when (step.value) {
    BadgeLinkStep.Confirming -> ModalView(::closeIfShowing) {
      if (profileHasBadge) {
        BadgeHeld(onViewBadge = { step.value = BadgeLinkStep.ViewingBadge }, onCancel = ::closeIfShowing)
      } else {
        Confirming(onConfirm = ::redeemFromLink, onCancel = ::closeIfShowing)
      }
    }
    BadgeLinkStep.Issuing -> ModalView(::closeIfShowing) { BeingIssued(onDismiss = ::closeIfShowing) }
    BadgeLinkStep.Redeemed, BadgeLinkStep.ViewingBadge -> BadgesView(ModalManager.end, close)
  }
}

// the screen cannot tell a repeated link for the badge it shows from a code for another badge,
// so it asserts neither
@Composable
private fun BadgeHeld(onViewBadge: () -> Unit, onCancel: () -> Unit) {
  LinkStep(
    MR.strings.badges_link_held_title,
    primary = MR.strings.badges_link_view_badge to onViewBadge,
    textButton = MR.strings.cancel_verb to onCancel
  ) {
    LinkText(stringResource(MR.strings.badges_link_held_other_profile))
    LinkText(stringResource(MR.strings.badges_link_held_page))
  }
}

@Composable
private fun Confirming(onConfirm: () -> Unit, onCancel: () -> Unit) {
  LinkStep(
    MR.strings.badges_link_confirm_title,
    primary = MR.strings.badges_link_add_badge to onConfirm,
    textButton = MR.strings.cancel_verb to onCancel
  ) {
    LinkText(String.format(stringResource(MR.strings.badges_link_confirm_profile), chatModel.currentUser.value?.displayName ?: ""))
  }
}

// leaving does not cancel: the badge is still added after the screen closes
@Composable
private fun BeingIssued(onDismiss: () -> Unit) {
  LinkStep(MR.strings.badges_being_issued, textButton = MR.strings.badges_dismiss to onDismiss) {
    Spacer(Modifier.weight(1f))
    CircularProgressIndicator(
      Modifier.size(30.dp),
      color = MaterialTheme.colors.secondary,
      strokeWidth = 3.dp
    )
  }
}

// with no primary, the text button takes its place, so a button does not move between steps
@Composable
private fun LinkStep(
  title: StringResource,
  primary: Pair<StringResource, () -> Unit>? = null,
  textButton: Pair<StringResource, () -> Unit>,
  content: @Composable ColumnScope.() -> Unit
) {
  ColumnWithScrollBar(
    Modifier.padding(horizontal = 25.dp).padding(top = 8.dp, bottom = 20.dp),
    verticalArrangement = Arrangement.spacedBy(16.dp),
    horizontalAlignment = Alignment.CenterHorizontally,
    maxIntrinsicSize = true,
  ) {
    Text(
      stringResource(title),
      style = MaterialTheme.typography.h1,
      fontWeight = FontWeight.Bold,
      color = MaterialTheme.colors.primary,
      textAlign = TextAlign.Center,
      modifier = Modifier.fillMaxWidth()
    )

    content()

    Spacer(Modifier.weight(1f))

    Column(horizontalAlignment = Alignment.CenterHorizontally) {
      if (primary != null) {
        OnboardingActionButton(
          modifier = if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING).fillMaxWidth() else Modifier.widthIn(min = 300.dp),
          labelId = primary.first,
          onboarding = null,
          onclick = primary.second
        )
        TextButtonBelowOnboardingButton(stringResource(textButton.first), textButton.second)
      } else {
        TextButtonBelowOnboardingButton(stringResource(textButton.first), textButton.second)
        TextButtonBelowOnboardingButton("", null)
      }
    }
  }
}

@Composable
private fun LinkText(text: String) {
  Text(
    text,
    style = MaterialTheme.typography.body1,
    textAlign = TextAlign.Center,
    modifier = Modifier.fillMaxWidth()
  )
}
