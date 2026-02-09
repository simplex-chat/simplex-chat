package chat.simplex.common.views.onboarding

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.ui.theme.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource

@Composable
fun IntroPage(
  headline: String,
  subtitle: String,
  centralContent: @Composable BoxScope.() -> Unit,
  showButtons: Boolean = false,
  onCreateProfile: (() -> Unit)? = null,
  onMigrate: (() -> Unit)? = null,
) {
  Column(
    modifier = Modifier
      .fillMaxSize()
      .padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING),
    horizontalAlignment = Alignment.CenterHorizontally,
    verticalArrangement = Arrangement.Top,
  ) {

    Box(
      modifier = Modifier
        .weight(0.5f)
        .fillMaxWidth(),
      contentAlignment = Alignment.Center
    ) {
      centralContent()
    }

    Spacer(Modifier.height(32.dp))

    Text(
      text = headline,
      style = MaterialTheme.typography.h1,
      fontWeight = FontWeight.Bold,
      color = MaterialTheme.colors.onBackground,
      textAlign = TextAlign.Center,
      lineHeight = 38.sp,
    )
    Spacer(Modifier.height(16.dp))
    Text(
      text = subtitle,
      style = MaterialTheme.typography.body1,
      color = MaterialTheme.colors.secondary,
      textAlign = TextAlign.Center,
      lineHeight = 24.sp,
    )

    if (showButtons && (onCreateProfile != null || onMigrate != null)) {
      Spacer(Modifier.height(DEFAULT_PADDING * 2))
      Column(
        Modifier.widthIn(max = if (appPlatform.isAndroid) 450.dp else 1000.dp),
        horizontalAlignment = Alignment.CenterHorizontally,
      ) {
        if (onCreateProfile != null) {
          OnboardingActionButton(
            modifier = Modifier.fillMaxWidth(),
            labelId = MR.strings.create_your_profile,
            onboarding = OnboardingStage.Step2_CreateProfile,
            onclick = onCreateProfile,
          )
        }
        if (onMigrate != null) {
          TextButtonBelowOnboardingButton(
            text = stringResource(MR.strings.migrate_from_another_device),
            onClick = onMigrate,
          )
        }
      }
    }
  }
}
