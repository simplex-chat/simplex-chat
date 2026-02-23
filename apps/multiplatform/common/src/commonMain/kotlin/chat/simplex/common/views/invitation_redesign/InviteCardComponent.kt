package chat.simplex.common.views.invitation_redesign

import androidx.compose.foundation.Image
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.Button
import androidx.compose.material.ButtonDefaults
import androidx.compose.material.Card
import androidx.compose.material.Icon
import androidx.compose.material.IconButton
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
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

const val SHOW_PICTURES = false

@Composable
fun InviteCardComponent(
  image: Painter,
  titleIcon: Painter,
  title: String,
  description: String? = null,
  bulletPoints: List<String> = emptyList(),
  buttonText: String? = null,
  onDismiss: (() -> Unit)? = null,
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
    Box {
      Column(
        Modifier.fillMaxWidth(),
        horizontalAlignment = Alignment.CenterHorizontally
      ) {
        if (SHOW_PICTURES) {
          Image(
            image,
            contentDescription = null,
            contentScale = ContentScale.FillWidth,
            modifier = Modifier.fillMaxWidth()
          )
        } else {
          Spacer(Modifier.height(DEFAULT_PADDING))
        }

        Row(
          verticalAlignment = Alignment.CenterVertically,
          horizontalArrangement = Arrangement.Center,
          modifier = Modifier
            .fillMaxWidth()
            .padding(horizontal = DEFAULT_PADDING)
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
          Spacer(Modifier.height(4.dp))
          Text(
            description,
            style = MaterialTheme.typography.body2.copy(fontSize = 14.sp),
            color = Color.Gray
          )
        }

        if (bulletPoints.isNotEmpty()) {
          Spacer(Modifier.height(DEFAULT_PADDING))
          Column(
            Modifier
              .fillMaxWidth()
              .padding(horizontal = DEFAULT_PADDING * 1.5f)
          ) {
            bulletPoints.forEach { point ->
              Row(Modifier.padding(bottom = 4.dp)) {
                Text(
                  "\u2022 ",
                  style = MaterialTheme.typography.body1.copy(fontWeight = FontWeight.Bold),
                  color = Color.Black
                )
                Text(
                  point,
                  style = MaterialTheme.typography.body1,
                  color = Color.Black
                )
              }
            }
          }
        }

        if (buttonText != null) {
          Spacer(Modifier.height(DEFAULT_PADDING))
          Button(
            onClick = onClick,
            shape = RoundedCornerShape(20.dp),
            colors = ButtonDefaults.buttonColors(backgroundColor = MaterialTheme.colors.primary),
            modifier = Modifier
              .fillMaxWidth()
              .height(56.dp)
              .padding(horizontal = DEFAULT_PADDING)
          ) {
            Text(
              buttonText,
              color = Color.White,
              style = MaterialTheme.typography.body1.copy(
                fontSize = 17.sp,
                fontWeight = FontWeight.Medium
              )
            )
          }
          Spacer(Modifier.height(DEFAULT_PADDING))
        } else {
          Spacer(Modifier.height(8.dp))
        }
      }

      if (!SHOW_PICTURES && onDismiss != null) {
        IconButton(
          onClick = onDismiss,
          modifier = Modifier
            .align(Alignment.TopEnd)
            .padding(4.dp)
        ) {
          Icon(
            painterResource(MR.images.ic_close),
            stringResource(MR.strings.back),
            tint = MaterialTheme.colors.secondary
          )
        }
      }
    }
  }
}
