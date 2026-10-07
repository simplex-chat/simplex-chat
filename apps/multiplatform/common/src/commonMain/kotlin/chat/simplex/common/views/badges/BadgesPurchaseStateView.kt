package chat.simplex.common.views.badges

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.StringResource
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.BadgeIssueFailure
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.views.onboarding.TextButtonBelowOnboardingButton
import chat.simplex.res.MR

@Composable
fun BadgesPurchaseStateView(title: StringResource, message: StringResource?, failure: BadgeIssueFailure? = null, onDismiss: () -> Unit) {
  ColumnWithScrollBar(
    Modifier.background(MaterialTheme.colors.background).padding(horizontal = 25.dp).padding(top = 8.dp, bottom = 20.dp),
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

    if (message != null) {
      Text(
        stringResource(message),
        style = MaterialTheme.typography.body1,
        textAlign = TextAlign.Center,
        modifier = Modifier.fillMaxWidth()
      )
    }

    if (failure != null) {
      Column(horizontalAlignment = Alignment.CenterHorizontally, verticalArrangement = Arrangement.spacedBy(6.dp)) {
        Icon(painterResource(MR.images.ic_warning), contentDescription = null, tint = Color.Red)
        Text(
          failure.purchaseText,
          style = MaterialTheme.typography.body1,
          color = MaterialTheme.colors.secondary,
          textAlign = TextAlign.Center,
          modifier = Modifier.fillMaxWidth()
        )
      }
    }

    Spacer(Modifier.weight(1f))

    CircularProgressIndicator(
      Modifier.size(30.dp),
      color = MaterialTheme.colors.secondary,
      strokeWidth = 3.dp
    )

    Spacer(Modifier.weight(1f))

    Column(horizontalAlignment = Alignment.CenterHorizontally) {
      TextButtonBelowOnboardingButton(stringResource(MR.strings.badges_dismiss), onDismiss)
      TextButtonBelowOnboardingButton("", null)
    }
  }
}

val BadgePurchaseState.title: StringResource
  get() = when (this) {
    BadgePurchaseState.Issuing -> MR.strings.badges_issuing_title
    BadgePurchaseState.WaitingForApproval -> MR.strings.badges_waiting_for_approval_title
  }

val BadgePurchaseState.message: StringResource
  get() = when (this) {
    BadgePurchaseState.Issuing -> MR.strings.badges_issuing_body
    BadgePurchaseState.WaitingForApproval -> MR.strings.badges_waiting_for_approval_body
  }
