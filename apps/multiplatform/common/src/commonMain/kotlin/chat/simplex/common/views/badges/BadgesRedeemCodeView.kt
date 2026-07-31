package chat.simplex.common.views.badges

import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.res.MR

// Draft entry-point for redeeming an investor badge code. TODO [badges]: implement input field,
// server verification and success/failure states when the redeem API is defined.
@Composable
fun BadgesRedeemCodeView() {
  ColumnWithScrollBar(
    Modifier.padding(horizontal = 25.dp).padding(top = 8.dp),
    horizontalAlignment = Alignment.Start
  ) {
    Text(
      stringResource(MR.strings.badges_redeem_code_button),
      style = MaterialTheme.typography.h1,
      fontWeight = FontWeight.Bold,
      color = MaterialTheme.colors.primary
    )
  }
}
