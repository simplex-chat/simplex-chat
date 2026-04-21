package chat.simplex.common.views.onboarding

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.ui.theme.isInDarkTheme

@Composable
fun PageIndicator(
  pageCount: Int,
  currentPage: Int,
  modifier: Modifier = Modifier,
  dotSize: Dp = 8.dp,
  spacing: Dp = 8.dp,
) {
  val activeColor = MaterialTheme.colors.primary
  val inactiveColor = if (isInDarkTheme()) {
    MaterialTheme.colors.secondary.copy(alpha = 0.5f)
  } else {
    MaterialTheme.colors.secondary.copy(alpha = 0.4f)
  }

  Row(
    modifier = modifier.padding(vertical = DEFAULT_PADDING),
    horizontalArrangement = Arrangement.spacedBy(spacing, Alignment.CenterHorizontally),
  ) {
    repeat(pageCount) { index ->
      Box(
        modifier = Modifier
          .size(dotSize)
          .clip(CircleShape)
          .background(if (index == currentPage) activeColor else inactiveColor)
      )
    }
  }
}
