package chat.simplex.common.views.badges

import InfoRow
import SectionItemView
import SectionSpacer
import SectionTextFooter
import SectionView
import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.BadgeIssueFailure
import chat.simplex.common.model.BadgeState
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.localTimestamp
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.helpers.AppBarTitle
import chat.simplex.common.views.helpers.ModalManager
import chat.simplex.common.views.helpers.badgeImage
import chat.simplex.common.views.helpers.badgeTypeName
import chat.simplex.common.views.helpers.openVerifiedSimplexUri
import chat.simplex.common.views.usersettings.SettingsActionItem
import chat.simplex.common.views.usersettings.simplexTeamUri
import chat.simplex.res.MR

@Composable
fun BadgesYourBadgeView(badgeState: BadgeState) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.badges_your_badge))

    SectionView {
      BadgeSummary(badgeState)
    }
    SectionSpacer()
    SectionView(stringResource(MR.strings.badges_ends)) {
      Text(badgeState.paidThroughText, Modifier.padding(horizontal = DEFAULT_PADDING, vertical = 12.dp))
    }
    SectionTextFooter(stringResource(MR.strings.badges_prepaid_footer))
    SectionSpacer()
    SectionView {
      SettingsActionItem(
        painterResource(MR.images.ic_info),
        stringResource(MR.strings.badges_how_it_works_button),
        { ModalManager.start.showModal { BadgesHowItWorksView() } },
      )
    }
    SectionSpacer()
    val issueError = badgeState.issueError
    if (issueError != null) {
      val uriHandler = LocalUriHandler.current
      SectionView(title = stringResource(MR.strings.error), icon = painterResource(MR.images.ic_warning), iconTint = Color.Red, leadingIcon = true) {
        SectionItemView {
          Text(badgeIssueFailureText(issueError.reason), color = MaterialTheme.colors.secondary)
        }
        InfoRow(stringResource(MR.strings.badges_error_since), localTimestamp(issueError.failedSince))
        val nextWakeAt = badgeState.nextWakeAt
        if (nextWakeAt != null) {
          InfoRow(stringResource(MR.strings.badges_error_next_attempt), localTimestamp(nextWakeAt))
        }
        SettingsActionItem(
          painterResource(MR.images.ic_tag),
          stringResource(MR.strings.badges_contact_team),
          { uriHandler.openVerifiedSimplexUri(simplexTeamUri) },
          textColor = MaterialTheme.colors.primary
        )
      }
      SectionSpacer()
    }
    if (appPrefs.developerTools.get()) {
      val clipboard = LocalClipboardManager.current
      SectionView(stringResource(MR.strings.badges_credential)) {
        val badge = chatModel.currentUser.value?.profile?.localBadge
        if (badge != null) {
          InfoRow(stringResource(MR.strings.badges_credential_status), badge.status.name)
          InfoRow(stringResource(MR.strings.badges_credential_expires), localTimestamp(badge.badge.badgeExpiry))
        }
        InfoRow(stringResource(MR.strings.badges_credential_months_left), badgeState.monthsLeft.toString())
        InfoRow(stringResource(MR.strings.badges_credential_purchase_id), badgeState.badgePurchaseId.toString())
        val nextCheckAt = badgeState.nextWakeAt
        if (nextCheckAt != null) {
          InfoRow(stringResource(MR.strings.badges_credential_next_check), localTimestamp(nextCheckAt))
        }
        if (issueError != null) {
          InfoRow(stringResource(MR.strings.badges_credential_last_attempt), localTimestamp(issueError.lastAttemptAt))
          InfoRow(stringResource(MR.strings.badges_credential_last_error), issueError.reason.tag)
        }
        SectionItemView({ clipboard.setText(AnnotatedString(badgeState.purchaseKey)) }) {
          Text(stringResource(MR.strings.badges_copy_purchase_key), color = MaterialTheme.colors.primary)
        }
        SectionItemView({ ModalManager.start.showCustomModal { close -> BadgesLedgerView(badgeState, close) } }) {
          Text(stringResource(MR.strings.badges_ledger))
        }
      }
      SectionSpacer()
    }
  }
}

@Composable
private fun badgeIssueFailureText(reason: BadgeIssueFailure): String = when (reason) {
  is BadgeIssueFailure.ServiceError -> String.format(stringResource(MR.strings.badges_error_service_refused), reason.code.text)
  is BadgeIssueFailure.Timeout -> stringResource(MR.strings.badges_error_no_response)
  is BadgeIssueFailure.Network -> stringResource(MR.strings.badges_error_unreachable)
  is BadgeIssueFailure.InvalidCredential -> stringResource(MR.strings.badges_error_credential_invalid)
  is BadgeIssueFailure.Unexpected -> String.format(stringResource(MR.strings.badges_error_unexpected_response), reason.message)
}

@Composable
fun BadgeSummary(badgeState: BadgeState) {
  Column(
    Modifier.fillMaxWidth().padding(vertical = 20.dp),
    horizontalAlignment = Alignment.CenterHorizontally,
    verticalArrangement = Arrangement.spacedBy(4.dp)
  ) {
    Image(
      painterResource(badgeImage(badgeState.badgeType)),
      contentDescription = null,
      contentScale = ContentScale.Fit,
      modifier = Modifier.padding(bottom = 8.dp).size(68.dp)
    )

    Text(badgeTypeName(badgeState.badgeType), style = MaterialTheme.typography.h3, fontWeight = FontWeight.SemiBold)

    Text(
      stringResource(MR.strings.badges_shown_on_your_profile),
      style = MaterialTheme.typography.body2,
      color = MaterialTheme.colors.secondary
    )
  }
}
