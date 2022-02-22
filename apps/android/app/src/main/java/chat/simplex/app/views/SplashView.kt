package chat.simplex.app.views

import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.unit.dp
import chat.simplex.app.R

@Composable
fun SplashView() {
  Box(modifier = Modifier
    .fillMaxSize()
    .background(color = Color.White)
  ) {
//    Image(
//      painter = painterResource(R.drawable.logo),
//      contentDescription = "Simplex Icon",
//      modifier = Modifier
//        .height(230.dp)
//        .align(Alignment.Center)
//    )
  }
}
