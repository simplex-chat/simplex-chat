package chat.simplex.common.views.badges

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.StringResource
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.BadgeType
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.OnboardingActionButton
import chat.simplex.res.MR

// Draft levels used by the badges UI while the API/state machine is still being designed. TODO [badges]:
// replace with types produced by the badge purchase API when it lands.
enum class BadgeLevel {
  Supporter,
  Legend;

  val title: StringResource
    get() = when (this) {
      Supporter -> MR.strings.badges_level_supporter
      Legend -> MR.strings.badges_level_legend
    }

  val filesDescription: StringResource
    get() = when (this) {
      Supporter -> MR.strings.badges_level_supporter_files
      Legend -> MR.strings.badges_level_legend_files
    }

  val monthlyPrice: StringResource
    get() = when (this) {
      Supporter -> MR.strings.badges_level_supporter_monthly
      Legend -> MR.strings.badges_level_legend_monthly
    }

  val oneMonthPrice: StringResource
    get() = when (this) {
      Supporter -> MR.strings.badges_level_supporter_one_month
      Legend -> MR.strings.badges_level_legend_one_month
    }

  val payMonthlyLabel: StringResource
    get() = when (this) {
      Supporter -> MR.strings.badges_pay_supporter_monthly
      Legend -> MR.strings.badges_pay_legend_monthly
    }

  val payOnceLabel: StringResource
    get() = when (this) {
      Supporter -> MR.strings.badges_pay_supporter_once
      Legend -> MR.strings.badges_pay_legend_once
    }

  val tagline: StringResource
    get() = when (this) {
      Supporter -> MR.strings.badges_level_supporter_tagline
      Legend -> MR.strings.badges_level_legend_tagline
    }

  val badgeAsset: dev.icerock.moko.resources.ImageResource
    get() = when (this) {
      Supporter -> MR.images.badge_supporter
      Legend -> MR.images.badge_legend
    }

  val badgeType: BadgeType
    get() = when (this) {
      Supporter -> BadgeType.Supporter
      Legend -> BadgeType.Legend
    }
}

@Composable
fun BadgesYourLevelView() {
  var selectedLevel by remember { mutableStateOf(BadgeLevel.Supporter) }

  ColumnWithScrollBar(
    Modifier.padding(horizontal = 25.dp).padding(top = 8.dp, bottom = 20.dp),
    verticalArrangement = Arrangement.spacedBy(16.dp),
    horizontalAlignment = Alignment.CenterHorizontally,
    maxIntrinsicSize = true,
  ) {
    Text(
      stringResource(MR.strings.badges_your_level_title),
      style = MaterialTheme.typography.h1,
      fontWeight = FontWeight.Bold,
      color = MaterialTheme.colors.primary,
      textAlign = TextAlign.Center,
      modifier = Modifier.fillMaxWidth()
    )

    BadgeUserPreview(level = selectedLevel, modifier = Modifier.padding(top = 4.dp)) {
      Icon(
        painterResource(MR.images.ic_keyboard_arrow_down),
        contentDescription = null,
        tint = MaterialTheme.colors.primary
      )
    }

    Row(
      Modifier.fillMaxWidth().padding(top = 8.dp),
      horizontalArrangement = Arrangement.spacedBy(12.dp)
    ) {
      LevelCard(BadgeLevel.Supporter, selectedLevel, Modifier.weight(1f)) { selectedLevel = it }
      LevelCard(BadgeLevel.Legend, selectedLevel, Modifier.weight(1f)) { selectedLevel = it }
    }

    Spacer(Modifier.weight(1f).heightIn(min = 20.dp))

    ContinueButton(selectedLevel)

    HowItWorksButton(Modifier.padding(top = 4.dp))
  }
}

@Composable
private fun LevelCard(level: BadgeLevel, selectedLevel: BadgeLevel, modifier: Modifier, onSelect: (BadgeLevel) -> Unit) {
  val isSelected = level == selectedLevel
  val borderColor = if (isSelected) MaterialTheme.colors.primary else MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.92f)
  val shape = RoundedCornerShape(16.dp)
  Column(
    modifier
      .clip(shape)
      .background(MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.97f), shape)
      .border(2.dp, borderColor, shape)
      .clickable { onSelect(level) }
      .padding(vertical = 20.dp),
    horizontalAlignment = Alignment.CenterHorizontally,
    verticalArrangement = Arrangement.spacedBy(10.dp)
  ) {
    Image(
      painterResource(level.badgeAsset),
      contentDescription = null,
      contentScale = ContentScale.Fit,
      modifier = Modifier.size(60.dp)
    )
    Text(stringResource(level.title), style = MaterialTheme.typography.h3, fontWeight = FontWeight.Bold)
    Text(stringResource(level.filesDescription), style = MaterialTheme.typography.body2, color = MaterialTheme.colors.secondary)
    Text(stringResource(level.monthlyPrice), style = MaterialTheme.typography.body1)
  }
}

@Composable
private fun ContinueButton(selectedLevel: BadgeLevel) {
  OnboardingActionButton(
    modifier = if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING).fillMaxWidth() else Modifier.widthIn(min = 300.dp),
    labelId = MR.strings.badges_continue,
    onboarding = null,
    onclick = {
      ModalManager.start.showModal { BadgesPayView(selectedLevel) }
    }
  )
}

@Composable
private fun HowItWorksButton(modifier: Modifier = Modifier) {
  TextButton(
    onClick = { ModalManager.start.showModal { BadgesHowItWorksView() } },
    modifier = modifier
  ) {
    Row(verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(6.dp)) {
      Icon(painterResource(MR.images.ic_info), null, tint = MaterialTheme.colors.primary)
      Text(stringResource(MR.strings.badges_how_it_works_button), color = MaterialTheme.colors.primary, fontWeight = FontWeight.Medium)
    }
  }
}
