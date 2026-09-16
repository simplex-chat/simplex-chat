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

// TODO [badges]: replace lorem ipsum with the real copy once the badge protocol and privacy properties are documented.
@Composable
fun BadgesHowItWorksView() {
  ColumnWithScrollBar(
    Modifier.padding(horizontal = 25.dp).padding(top = 8.dp),
    horizontalAlignment = Alignment.Start,
    verticalArrangement = Arrangement.spacedBy(12.dp)
  ) {
    Text(
      stringResource(MR.strings.badges_how_it_works_title),
      style = MaterialTheme.typography.h1,
      fontWeight = FontWeight.Bold,
      color = MaterialTheme.colors.primary,
      modifier = Modifier.padding(bottom = 16.dp)
    )
    Text(stringResource(MR.strings.badges_how_it_works_p1), style = MaterialTheme.typography.body1)
    Text(stringResource(MR.strings.badges_how_it_works_p2), style = MaterialTheme.typography.body1)
    Text(stringResource(MR.strings.badges_how_it_works_p3), style = MaterialTheme.typography.body1)
  }
}
