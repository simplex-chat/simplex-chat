package chat.simplex.common.views.badges

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.StringResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.res.MR

@Composable
fun BadgesPurchaseStateView(purchaseState: BadgePurchaseState) {
  ColumnWithScrollBar(
    Modifier.background(MaterialTheme.colors.background).padding(horizontal = 25.dp).padding(top = 8.dp, bottom = 20.dp),
    verticalArrangement = Arrangement.spacedBy(16.dp),
    horizontalAlignment = Alignment.CenterHorizontally,
    maxIntrinsicSize = true,
  ) {
    Text(
      stringResource(purchaseState.title),
      style = MaterialTheme.typography.h1,
      fontWeight = FontWeight.Bold,
      color = MaterialTheme.colors.primary,
      textAlign = TextAlign.Center,
      modifier = Modifier.fillMaxWidth()
    )

    Text(
      stringResource(purchaseState.message),
      style = MaterialTheme.typography.body1,
      textAlign = TextAlign.Center,
      modifier = Modifier.fillMaxWidth()
    )

    Spacer(Modifier.weight(1f))

    CircularProgressIndicator(
      Modifier.size(30.dp),
      color = MaterialTheme.colors.secondary,
      strokeWidth = 3.dp
    )

    Spacer(Modifier.weight(1f))
  }
}

private val BadgePurchaseState.title: StringResource
  get() = when (this) {
    BadgePurchaseState.Issuing -> MR.strings.badges_issuing_title
    BadgePurchaseState.WaitingForApproval -> MR.strings.badges_waiting_for_approval_title
    BadgePurchaseState.Checking -> MR.strings.badges_checking_title
  }

private val BadgePurchaseState.message: StringResource
  get() = when (this) {
    BadgePurchaseState.Issuing -> MR.strings.badges_issuing_body
    BadgePurchaseState.WaitingForApproval -> MR.strings.badges_waiting_for_approval_body
    BadgePurchaseState.Checking -> MR.strings.badges_checking_body
  }
