package chat.simplex.common.views.badges

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
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.BadgeState
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.views.helpers.AppBarTitle
import chat.simplex.common.views.helpers.ModalManager
import chat.simplex.common.views.helpers.badgeImage
import chat.simplex.common.views.helpers.badgeTypeName
import chat.simplex.common.views.usersettings.SettingsActionItem
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
  }
}

@Composable
fun BadgeSummary(badgeState: BadgeState) {
  Column(
    Modifier.fillMaxWidth().padding(vertical = 8.dp),
    horizontalAlignment = Alignment.CenterHorizontally,
    verticalArrangement = Arrangement.spacedBy(4.dp)
  ) {
    Image(
      painterResource(badgeImage(badgeState.badgeType)),
      contentDescription = null,
      contentScale = ContentScale.Fit,
      modifier = Modifier.size(68.dp).padding(bottom = 8.dp)
    )

    Text(badgeTypeName(badgeState.badgeType), style = MaterialTheme.typography.h3, fontWeight = FontWeight.SemiBold)

    Text(
      stringResource(MR.strings.badges_shown_on_your_profile),
      style = MaterialTheme.typography.body2,
      color = MaterialTheme.colors.secondary
    )
  }
}
