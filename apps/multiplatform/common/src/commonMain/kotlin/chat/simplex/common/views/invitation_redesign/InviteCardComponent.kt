package chat.simplex.common.views.invitation_redesign

import androidx.compose.foundation.Image
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.Card
import androidx.compose.material.Icon
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.ui.theme.DEFAULT_PADDING

@Composable
fun InviteCardComponent(
  image: Painter,
  titleIcon: Painter,
  title: String,
  description: String? = null,
  onClick: () -> Unit
) {
  Card(
    shape = RoundedCornerShape(24.dp),
    backgroundColor = Color(0xfff5f5f6),
    elevation = 0.dp,
    modifier = Modifier
      .fillMaxWidth()
      .padding(horizontal = DEFAULT_PADDING)
      .clickable(onClick = onClick)
  ) {
    Column(
      Modifier.fillMaxWidth(),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      Image(
        image,
        contentDescription = null,
        contentScale = ContentScale.FillWidth,
        modifier = Modifier.fillMaxWidth()
      )

      Row(
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.Center,
        modifier = Modifier
          .fillMaxWidth()
          .padding(vertical = 16.dp)
      ) {
        Icon(
          titleIcon,
          contentDescription = null,
          tint = MaterialTheme.colors.primary,
          modifier = Modifier.size(24.dp)
        )
        Spacer(Modifier.width(8.dp))
        Text(
          title,
          style = MaterialTheme.typography.body1.copy(
            fontSize = 18.sp,
            fontWeight = FontWeight.Medium
          ),
          color = Color.Black
        )
      }
      if (description != null) {
        Text(
          description,
          style = MaterialTheme.typography.body1.copy(
            fontSize = 14.sp,
            fontWeight = FontWeight.Medium
          ),
          color = Color.Black
        )
        Spacer(Modifier.height(8.dp))
      }
    }
  }
}
