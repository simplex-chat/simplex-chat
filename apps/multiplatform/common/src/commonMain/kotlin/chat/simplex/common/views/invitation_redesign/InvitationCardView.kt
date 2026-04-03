package chat.simplex.common.views.invitation_redesign

import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.Icon
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Surface
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.common.ui.theme.appColors
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.compose.painterResource

@Composable
fun InvitationCardView(
  modifier: Modifier = Modifier,
  mainImageResource: ImageResource,
  iconResource: ImageResource,
  title: String,
  description: String? = null
) {
  Surface(
    shape = RoundedCornerShape(18.dp),
    color = MaterialTheme.appColors.sentMessage,
    modifier = modifier
  ) {
    Column(
      Modifier.fillMaxWidth().padding(bottom = DEFAULT_PADDING_HALF),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      Image(
        painterResource(mainImageResource),
        contentDescription = null,
        modifier = Modifier.fillMaxWidth()
      )
      Spacer(Modifier.height(DEFAULT_PADDING_HALF))
      Row(verticalAlignment = Alignment.CenterVertically) {
        Icon(
          painterResource(iconResource),
          contentDescription = null,
          modifier = Modifier.size(24.dp),
          tint = MaterialTheme.colors.primary
        )
        Spacer(Modifier.width(DEFAULT_PADDING_HALF))
        Text(
          text = title,
          style = MaterialTheme.typography.h6.copy(fontWeight = FontWeight.Medium),
        )
      }
      description?.let {
        Text(
          text = description,
          style = MaterialTheme.typography.body2.copy(fontWeight = FontWeight.Medium),
          color = MaterialTheme.colors.secondary
        )
      }
    }
  }
}