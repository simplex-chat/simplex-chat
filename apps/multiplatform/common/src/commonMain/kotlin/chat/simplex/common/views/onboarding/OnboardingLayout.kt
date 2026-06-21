package chat.simplex.common.views.onboarding

import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.Divider
import androidx.compose.material.Icon
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.draw.clipToBounds
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.layout.*
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.ui.theme.isInDarkTheme
import chat.simplex.common.views.helpers.ModalManager
import chat.simplex.common.views.helpers.mixWith
import chat.simplex.common.views.newchat.darkStops
import chat.simplex.common.views.newchat.gradientPoints
import chat.simplex.common.views.newchat.lightStops
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.compose.painterResource

/**
 * A layout for onboarding screens: image + content + spacer + button.
 * The spacer shrinks first (down to [minSpacerHeight]), then the image shrinks.
 * Button is always at the bottom.
 */
@Composable
fun OnboardingShrinkingLayout(
    modifier: Modifier = Modifier,
    topPadding: Dp = 0.dp,
    minSpacerHeight: Dp = 20.dp,
    image: @Composable () -> Unit,
    content: @Composable () -> Unit,
    button: @Composable () -> Unit
) {
    Layout(
        contents = listOf(image, content, button),
        modifier = modifier
    ) { (imageMeasurables, contentMeasurables, buttonMeasurables), constraints ->
        val width = constraints.maxWidth
        val height = constraints.maxHeight
        val childConstraints = constraints.copy(minWidth = 0, minHeight = 0)

        // 1. Measure fixed content (texts) and button first
        val contentPlaceable = contentMeasurables.first().measure(childConstraints)
        val buttonPlaceable = buttonMeasurables.first().measure(childConstraints)
        val minSpacer = minSpacerHeight.roundToPx()

        // 2. Image gets remaining after top padding + content + button + minimum spacer
        val topPad = topPadding.roundToPx()
        val reservedHeight = topPad + contentPlaceable.height + buttonPlaceable.height + minSpacer
        val imageMaxHeight = (height - reservedHeight).coerceAtLeast(0)
        val imagePlaceable = imageMeasurables.first().measure(
            childConstraints.copy(maxWidth = width, maxHeight = imageMaxHeight)
        )

        // 3. Spacer fills whatever is left between content and button
        val usedHeight = topPad + imagePlaceable.height + contentPlaceable.height + buttonPlaceable.height
        val spacerHeight = (height - usedHeight).coerceAtLeast(minSpacer)

        // 4. Place: image centered horizontally, rest below
        layout(width, height) {
            var y = topPad
            imagePlaceable.placeRelative((width - imagePlaceable.width) / 2, y)
            y += imagePlaceable.height
            contentPlaceable.placeRelative((width - contentPlaceable.width) / 2, y)
            y += contentPlaceable.height
            y += spacerHeight
            buttonPlaceable.placeRelative((width - buttonPlaceable.width) / 2, y)
        }
    }
}

@Composable
fun OnboardingImage(
    lightImage: ImageResource,
    darkImage: ImageResource,
    fallbackIcon: ImageResource,
    modifier: Modifier = Modifier,
    aspectRatio: Float = 1f
) {
    if (BuildConfigCommon.SIMPLEX_ASSETS) {
        Image(
            painterResource(if (isInDarkTheme()) darkImage else lightImage),
            contentDescription = null,
            contentScale = ContentScale.Fit,
            modifier = Modifier.fillMaxWidth().then(modifier)
        )
    } else {
        val isDark = isInDarkTheme()
        val stops = if (isDark) darkStops else lightStops
        val scale = if (isDark) 1.5f else 1.2f
        Box(
            modifier
                .aspectRatio(aspectRatio)
                .clip(RoundedCornerShape(24.dp))
                .drawBehind {
                    val gp = gradientPoints(size.height / size.width, scale)
                    drawRect(
                        Brush.linearGradient(
                            colorStops = stops,
                            start = Offset(gp.startX * size.width, gp.startY * size.height),
                            end = Offset(gp.endX * size.width, gp.endY * size.height)
                        )
                    )
                },
            contentAlignment = Alignment.Center
        ) {
            Icon(
                painterResource(fallbackIcon),
                contentDescription = null,
                modifier = Modifier.size(80.dp),
                tint = MaterialTheme.colors.primary
            )
        }
    }
}

@Composable
fun DesktopOnboardingShell(stage: OnboardingStage, content: @Composable () -> Unit) {
    Row(Modifier.fillMaxSize()) {
        Box(
            Modifier.weight(0.382f).fillMaxHeight()
                .background(MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.985f))
                .padding(horizontal = DEFAULT_PADDING),
            contentAlignment = Alignment.Center
        ) {
            when (stage) {
                OnboardingStage.Step1_SimpleXInfo ->
                    OnboardingImage(MR.images.intro, MR.images.intro_light, MR.images.ic_forum, Modifier.fillMaxWidth())
                OnboardingStage.Step2_CreateProfile,
                OnboardingStage.Step2_5_SetupDatabasePassphrase,
                OnboardingStage.LinkAMobile ->
                    OnboardingImage(MR.images.your_profile, MR.images.your_profile_light, MR.images.ic_person, Modifier.fillMaxWidth())
                OnboardingStage.Step3_ChooseServerOperators,
                OnboardingStage.Step3_CreateSimpleXAddress,
                OnboardingStage.Step4_SetNotificationsMode ->
                    OnboardingImage(MR.images.your_network, MR.images.your_network_light, MR.images.ic_dns, Modifier.fillMaxWidth())
                OnboardingStage.Step4_NetworkCommitments ->
                    OnboardingImage(MR.images.network_commitments, MR.images.network_commitments_light, MR.images.ic_shield, Modifier.fillMaxWidth(), aspectRatio = 1.5f)
                else -> {}
            }
        }
        Divider(Modifier.fillMaxHeight().width(1.dp))
        Box(Modifier.weight(0.618f).fillMaxHeight().clipToBounds()) {
            content()
            ModalManager.fullscreen.showInView()
        }
    }
}
